# Fantasy Draft Room

A browser-based snake draft room powered by the projection model in `nflverse-projections`.

## Refresh the player pool

From this directory:

```sh
Rscript scripts/export_projections.R
```

The exporter reads `../nflverse-projections/data/derived/simple_linear_projections_2026.csv` by default and writes the browser-ready data to `public/projections.json`. Both paths can be overridden with the first and second command-line arguments.

Refresh the player-name hover briefs after projections or consensus rankings
change:

```sh
Rscript scripts/generate_player_summaries.R
```

This writes `public/player-summaries.json` for every player on the board. It
combines current FantasyPros consensus ranks, the local projection model, and
clearly labeled 2025 nflverse injury-report history. The four approved sample
briefs are maintained as curated overrides in the generator. The app reads the
static output at load time, so no API key or live AI request is needed during a
draft.

Refresh the player performance modal after weekly or seasonal nflverse data
changes:

```sh
Rscript scripts/export_player_history.R
```

This writes `public/player-history.json` from the 2023–2025 weekly player-stat
files. It includes three regular-season summaries and a 2025 game log scored
with the same PPR rules used by the projection model. The history file is
loaded only after a player modal is opened.

Refresh the horizontal model-contribution charts after rebuilding the mean
projection model:

```sh
Rscript scripts/export_model_interpretability.R
```

This writes `public/player-interpretability.json` from the saved
position-specific linear models. Each bar is an input value multiplied by its
fitted coefficient, including factor levels and interaction terms, and the bars
are checked to sum back to the player’s 2026 mean projection.

Refresh the calibrated 12-team, 16-round availability curves after rebuilding
the public ADP model:

```sh
Rscript scripts/export_availability.R
```

The live board conditions each future probability on the player still being
available at the current pick. New drafts must match the model's team count and
round count; otherwise availability remains paused instead of showing estimates
from an incompatible league.

Live drafts support one or two focus teams. When two are selected, the
availability columns automatically follow the team with the earlier upcoming
pick, and the recommendation board activates whenever either focus team is on
the clock. Mock draft mode intentionally supports one focus team so simulated
opponent picks cannot skip a managed roster.

When the focus team is on the clock in Rounds 1–14, the recommendation board
screens the top three projected QB, RB, WR, and TE options. It ranks the best
six complete-roster paths by their fractional availability value, then reports
the mean, 10th percentile, worst-decile mean, and chance of leaving the draft
with two viable running backs across 500 shared survivor-board scenarios. The
calculation runs in a web worker so the player board stays responsive, and any
recommended player can be drafted directly from the recommendation row.

## Run locally

```sh
pnpm install
pnpm run dev
```

Draft progress is saved in the browser's local storage. No account or database is required.

The board estimates a player's standard deviation from the model's 80%
projection interval as `(P90 - P10) / 2.5631`. `Risk+` compares that estimate
with the median volatility of the 30 most similarly projected players at the
same position. The late-round upside sort uses `P90 - replacement points`, so a
wide interval only helps when the player's ceiling would matter to a fantasy
roster.
