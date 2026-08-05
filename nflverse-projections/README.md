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
install.packages(c("nflreadr", "arrow", "jsonlite"))
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

`build_simple_linear_model.R` creates separate QB, RB, WR, and TE linear models
for next-season total fantasy points. It uses prior fantasy points, two-year
points, availability, position/depth role shrinkage, depth rank, draft capital,
age, experience, rookie status, and a rookie-by-draft-capital interaction. It
also allows prior scoring rate to have different effects by depth-chart role.
It deliberately does not include team passing or rushing forecasts yet. Historical predictions use five-fold
cross-validation grouped by player, so every season for a player is held out
from the models used to predict that player's rows.

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
data/derived/simple_linear_largest_misses.csv
```

Candidate nonlinear terms and interactions can be compared on the same player
folds with:

```sh
Rscript nflverse-projections/compare_model_variants.R
```

This writes variant predictions, accuracy statistics, and calibration deciles
to `data/derived/simple_linear_variant_*.csv`. The production projection model
is not changed automatically by this experiment.
