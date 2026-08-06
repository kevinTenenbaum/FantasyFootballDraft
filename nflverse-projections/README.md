# nflverse projection data

This directory is an isolated starting point for building independent,
season-long NFL player projections. The collector downloads raw nflverse inputs;
it does not yet fit projection models or change the Shiny app.

## What it collects

The files are separated by source and season under `data/raw/` so interrupted
runs can resume and individual seasons can be refreshed.

| Dataset | Projection use | Required? |
|---|---|---|
| Weekly player statistics | Component outcomes and player usage | Yes |
| Weekly team statistics | Team pass/rush opportunity pools | Yes |
| Season and weekly rosters | Team assignments, age, experience, availability | Yes / helpful |
| Depth charts | Current and historical role priors | Helpful |
| Snap counts | Distinguish starters, injured players, and low-snap backups | Helpful |
| Players and fantasy-ID crosswalk | Stable joins across nflverse, PFR, and fantasy sources | Yes |
| Schedules | Bye weeks and later matchup features | Yes |
| Draft picks and combine | Rookie priors | Yes / helpful |
| Injuries | Historical availability features | Helpful; known source gaps are tolerated |
| Play-by-play | Red-zone, goal-line, air-yard, and situational usage | Yes for the full model |
| Participation | Routes/alignment and richer role features | Optional; historical coverage varies |
| ffopportunity expected points | Opportunity-quality features | Optional |

Participation data from 2023 onward is provided by **FTN Data via nflverse** and
is CC-BY-SA 4.0. Preserve that attribution in derived datasets and published
outputs.

Depth-chart files are normalized across nflverse's source change. The legacy
weekly GSIS files (through 2024) and dated ESPN snapshots (2025 onward) are both
stored with the same columns:

```text
season, week, snapshot_date, team, player_id, player_name,
position, depth_position, depth_rank, formation, source
```

`week` is unavailable for dated ESPN snapshots, while `snapshot_date` is
unavailable for legacy weekly rows. Existing cached depth-chart files with the
old schema are detected and refreshed automatically.

## Setup

From R:

```r
install.packages(c("nflreadr", "arrow", "jsonlite", "quantreg", "lpSolve"))
```

The collector does not install packages automatically or write outside this
subdirectory.

## Run

From the repository root:

```sh
Rscript nflverse-projections/collect_data.R
```

Defaults live near the top of `collect_data.R`. Existing non-empty parquet files
are reused. Each run writes `data/manifests/latest.csv` plus a timestamped
manifest containing row counts, file sizes, and source errors.

Useful command-line options:

```sh
# Quick core-data collection without the largest/least-complete sources
Rscript nflverse-projections/collect_data.R \
  --skip-pbp --skip-participation --skip-expected-points

# Override the historical window and target season
Rscript nflverse-projections/collect_data.R --start=2018 --target=2026

# Replace existing cached files
Rscript nflverse-projections/collect_data.R --force
```

Available flags are `--start=YEAR`, `--target=YEAR`, `--output=PATH`,
`--skip-pbp`, `--skip-participation`, `--skip-expected-points`,
`--skip-injuries`, `--force`, and `--fail-on-optional`.

When `--output=PATH` is supplied, manifests are written to a `manifests/`
directory alongside that output path.

## Tests

The offline smoke test uses small fake nflverse-shaped tables and makes no
network requests:

```sh
Rscript nflverse-projections/tests/smoke_test.R
```

For a small live check, collect one completed season while skipping heavy data:

```sh
Rscript nflverse-projections/collect_data.R \
  --start=2025 --target=2026 \
  --skip-pbp --skip-participation --skip-expected-points
```

## Intended next layer

The modeling code will read these parquet files and produce raw statistical
projections matching the app's existing schema:

```text
pass_yds, pass_tds, pass_ints,
rush_yds, rush_tds,
receptions, rec_yds, rec_tds, fumbles
```

League-specific scoring and VOR can then remain downstream of the projection
model.

## Calculate historical fantasy points

`score_all_seasons.R` loops through every collected season and calculates
regular-season fantasy points for every QB, RB, WR, and TE. Kickers and defenses
are excluded. Edit the named `scoring_rules` vector near the top of the script,
then run:

```sh
Rscript nflverse-projections/score_all_seasons.R
```

The combined result is written to
`data/derived/fantasy_points_all_seasons.csv`. The flat file has one row per
player-season with season, position rank, teams, games, fantasy points, and each
scored statistical total. PPR can be changed by setting `receptions` to `1`,
`0.5`, or `0`.

An optional season range can be supplied for smaller backtests:

```sh
Rscript nflverse-projections/score_all_seasons.R --start=2020 --end=2025
```

## Simple total-points model

`build_simple_linear_model.R` creates separate QB, RB, WR, and TE models for
next-season total fantasy points. The mean projection uses linear regression,
and P10, P50, and P90 projections use quantile regression so the uncertainty
estimates can be asymmetric. Predictions use the same zero-point floor as the
mean model, and independently fitted quantiles are monotonically rearranged if
they cross. All four models use prior fantasy points, two-year points, availability,
position/depth role shrinkage, depth rank, draft capital, age, experience,
rookie status, and a rookie-by-draft-capital interaction. They also allow prior
scoring rate to have different effects by depth-chart role. The models
deliberately do not include team passing or rushing forecasts yet. Historical
predictions use five-fold cross-validation grouped by player, so every season
for a player is held out from the models used to predict that player's rows.

```sh
Rscript nflverse-projections/build_simple_linear_model.R
```

The target season and scoring rules are editable at the top of the script. It
writes:

```text
data/derived/simple_linear_training_data.csv
data/derived/simple_linear_role_priors.csv
data/derived/simple_linear_projections_2026.csv
data/models/simple_linear_models.rds
data/models/simple_linear_quantile_models.rds
```

The training and projection CSVs store the mean and uncertainty projections in
the same rows:

```text
projected_fantasy_points,
projected_fantasy_points_p10,
projected_fantasy_points_p50,
projected_fantasy_points_p90
```

Evaluate the grouped out-of-fold predictions with:

```sh
Rscript nflverse-projections/evaluate_out_of_fold_predictions.R
```

The evaluator prints overall and position-level calibration and accuracy
statistics. It also writes detailed summaries to:

```text
data/derived/simple_linear_accuracy_summary.csv
data/derived/simple_linear_calibration_summary.csv
data/derived/simple_linear_uncertainty_summary.csv
data/derived/simple_linear_largest_misses.csv
```

## Public draft availability model

`build_availability_model.R` downloads current aggregate ADP data from
[Fantasy Football Calculator](https://fantasyfootballcalculator.com/), joins it
to the independent player projections, and estimates the probability that each
player remains available at every pick. The default availability model is
configured for 2026, 12 teams, 15 rounds, and PPR scoring.

```sh
Rscript nflverse-projections/build_availability_model.R
```

The model starts with each player's lower-bounded ADP distribution and retains
probability beyond the modeled draft instead of forcing every listed player to
be selected. It then calibrates QB, RB, WR, TE, D/ST, and kicker together. Every
pick consumes exactly one expected player, and the expected position totals
match the source sample's draft-frequency mix. This keeps the individual curves
monotone, prevents their cumulative selections from exceeding the available
slots, and preserves realistic late-round and undrafted probability.

Players absent from the public ADP sample are retained with
`match_method = "unmatched"` and availability probability 1 at every pick,
rather than being assigned unsupported draft behavior. Raw dated source
responses are cached under
`data/raw/public_adp/`, and the per-player, per-pick result is written to:

```text
data/derived/player_availability_2026.csv
```

Useful overrides include `--season=YEAR`, `--scoring=FORMAT`, `--teams=N`,
`--rounds=N`, `--adp-json=PATH`, `--projections=PATH`, and `--output=PATH`.
Supplying `--adp-json` rebuilds from a saved response without making a network
request. The requested draft horizon can differ from the source sample's round
count; both values are recorded in the output.

### Historical ADP backfill

`backfill_historical_adp.R` downloads archived Fantasy Football Calculator ADP
without requiring historical projection files. The public archive currently
has usable 12-team PPR results for 2010 onward; 2007–2009 return no data.

```sh
Rscript nflverse-projections/backfill_historical_adp.R \
  --start-season=2010 \
  --end-season=2025 \
  --teams=12 \
  --scoring=ppr
```

Each exact source response is preserved under `data/raw/public_adp/`. The
combined player-season table and a season-level coverage audit are written to:

```text
data/derived/public_adp_history_12_team_ppr.csv
data/derived/public_adp_history_12_team_ppr_summary.csv
```

Review the coverage audit before using the history for model training. Sample
sizes and player coverage vary substantially, and some older seasons are thin.

The web application uses a separate 12-team, 16-round build so its league setup
and availability horizon agree:

```sh
Rscript nflverse-projections/build_availability_model.R \
  --teams=12 --rounds=16 \
  --output=data/derived/player_availability_12_team_2026.csv
```

## Fractional draft optimizer

`optimize_draft.R` combines the mean projections with the 12-team availability
curves to recommend the current pick. The configured league drafts 16 total
players: seven offensive starters, seven offensive bench players, one kicker,
and one D/ST. The optimizer handles the first 14 rounds and reserves rounds 15
and 16 for kicker and D/ST.

The offensive starter slots are one QB, two RB, three WR/TE, and one RB/WR/TE
FLEX. League maximums are QB 4, RB 8, WR 8, and TE 4. Starter roles receive full
projected value. Successive RB, WR, and TE bench roles default to weights 0.45,
0.25, and 0.10; later depth retains the 0.10 weight. Because this is a one-QB
league and replacement quarterbacks are plentiful, QB2 through QB4 use the
larger discounts 0.20, 0.05, and 0.01. These weights can be overridden and are
intended to be replaced by empirical weekly contribution rates later.

At each remaining pick, the model keeps the top five projected players per
position with at least 2% conditional availability, plus a high-survival
fallback at each position. It then uses a continuous linear program to:

- select one fractional player at every remaining pick;
- prevent more than one total share of any player;
- keep each player's selection mass within nested conditional-availability
  limits;
- fill every starter and bench role; and
- enforce the league's position maximums.

Each currently available top-five QB, RB, WR, and TE is fixed as the current
selection in a separate solve. Candidates are ranked by the resulting value of
the complete fractional roster.

For a new draft from slot 3:

```sh
Rscript nflverse-projections/optimize_draft.R --draft-slot=3
```

For an in-progress draft, supply a roster CSV and a CSV of every drafted player.
Both files require a `player_id` column. The roster must contain one offensive
player for every completed round:

```sh
Rscript nflverse-projections/optimize_draft.R \
  --draft-slot=3 \
  --current-round=6 \
  --roster=my_roster.csv \
  --drafted=all_drafted_players.csv
```

The default output files are:

```text
data/derived/draft_recommendations_2026.csv
data/derived/fractional_selection_plan_2026.csv
data/derived/fractional_role_plan_2026.csv
```

Useful overrides are `--candidate-count=3|4|5`,
`--minimum-availability=PROBABILITY`, and
`--bench-weights=0.45,0.25,0.10`. QB discounts can be set separately with
`--qb-bench-weights=0.20,0.05,0.01`. The candidate and fallback pruning makes
the optimization deliberately approximate; it should be rerun from the actual
board at every pick.

### Experimental greedy draft rollouts

`simulate_draft_rollouts.R` is a separate experiment that converts the public
ADP distributions into concrete opponent draft orders. At each of the user's
14 offensive picks, it reveals the players who actually survived, selects one
real player with the optimizer, and then repeats from the newly observed board.
Fractions only emerge by comparing selection frequencies across many completed
rollouts; an individual simulated roster never contains fractional players.

The current opponent-board generator samples each matched player's latent draft
position independently from public ADP and standard deviation, then sorts the
draws into one unique order. This models pick competition and exact removals but
does not yet estimate empirical positional-run correlation. Players unmatched
to the public sample are never selected by simulated opponents and are labeled
`unmatched` in the output.

Run three reproducible drafts for the team selecting second with:

```sh
Rscript nflverse-projections/simulate_draft_rollouts.R \
  --draft-slot=2 --simulations=3 --seed=202602
```

The script prints each roster with position, public ADP, and projected points,
then writes the complete results to:

```text
data/derived/greedy_draft_rollouts_2026.csv
data/derived/greedy_draft_rollout_summary_2026.csv
```

`evaluate_draft_scenarios.R` extends the experiment into a counterfactual
current-pick comparison. Every top-five-per-position candidate is evaluated on
the same small screening set of opponent boards. The five strongest candidates
then receive 500 shared scenarios by default. Future picks do not see the
sampled future order: on each pick they solve the fractional plan from the board
currently visible and select the player with its largest current share.

Candidate rankings report mean, P10, worst-decile average, standard error, an
80% mean plus 20% worst-decile score, expected RB1+RB2 projected points, and the
probability of finishing with two RBs projected for at least 150 points. For a
demonstration where Bijan Robinson was selected first and the user picks second:

```sh
Rscript nflverse-projections/evaluate_draft_scenarios.R \
  --draft-slot=2 --current-round=1 \
  --drafted-ids=00-0038542 \
  --screen-scenarios=20 --final-scenarios=500 --finalists=5 --cores=4
```

The evaluator writes candidate rankings, one row per candidate/scenario, and
the concrete roster selected in each scenario to
`data/derived/draft_scenario_*.csv`. This is still an experimental policy: the
opponent boards are coherent and shared across candidates, but their latent ADP
draws do not yet contain empirically estimated positional-run correlation.

`compare_draft_policies.R` runs the full candidate-fixing fractional optimizer
and the faster largest-current-fraction policy on identical opponent boards.
This makes it possible to measure whether the faster hundreds-of-scenarios
approximation changes roster value, positional allocation, or RB timing. The
default comparison fixes Puka Nacua at pick 2; pass Bijan Robinson as already
drafted to reproduce the documented example:

```sh
Rscript nflverse-projections/compare_draft_policies.R \
  --draft-slot=2 --current-round=1 \
  --candidate-id=00-0039075 --drafted-ids=00-0038542 \
  --simulations=50 --cores=4
```

The comparison writes policy summaries, paired scenario scores, concrete
rosters, and round-level selection agreement to
`data/derived/draft_policy_*.csv`.

Candidate nonlinear terms and interactions can be compared on the same player
folds with:

```sh
Rscript nflverse-projections/compare_model_variants.R
```

This writes variant predictions, accuracy statistics, and calibration deciles
to `data/derived/simple_linear_variant_*.csv`. The production projection model
is not changed automatically by this experiment.
