# Fantasy Football Draft App

You can use the app here: [link](https://kevintenenbaum.shinyapps.io/FantasyFootball/?_ga=2.186942312.1564715598.1591924257-1494174204.1591924257)

# Rankings Summary

The app ranks players by loading projections from [FantasyPros](https://www.fantasypros.com) and adjusting them based on a value-based drafting philosophy. This philosophy allows you to set *replacement levels* for each position and rank players by the difference in projected points between the player in question and the replacement-level player at their position. You can read more about the method at [Footballguys](https://www.footballguys.com/05vbdrevisited.htm).

## FantasyPros data access

FantasyPros now exposes only a small preview of its projections in the public HTML. The app therefore uses the official FantasyPros API for full QB, RB, WR, and TE projections. Request an API key from [FantasyPros API Data](https://www.fantasypros.com/api-data/) and set it outside the repository. The free API tier returns sample data (currently 10 players per position), which is not enough to build this draft board; use a Premium/HOF or other full-data key.

```r
Sys.setenv(FANTASYPROS_API_KEY = "your-key")
```

For a persistent local setup, add `FANTASYPROS_API_KEY=your-key` to your user-level `.Renviron`. Do not commit the key. A deployed Shiny app should receive the same variable through its secret/environment configuration.

Consensus rankings plus kicker and defense data continue to come from FantasyPros' public pages. The integration reads their embedded structured data and validates the expected fields and row counts.

Run the live integration check from the repository root with:

```sh
Rscript scripts/check_fantasypros.R
```

Without an API key, this command validates the public rankings, kicker, and defense feeds and reports that the full projection check was skipped. With a full-data key, it also builds and validates the complete draft board. A free sample-data key fails with a message explaining the access-tier limitation.
