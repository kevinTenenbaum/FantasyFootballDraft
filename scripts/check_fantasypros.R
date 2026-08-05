#!/usr/bin/env Rscript

source("ff.R")

check <- function(condition, message){
  if(!isTRUE(condition)) stop(message, call. = FALSE)
}

cat("Checking public FantasyPros rankings...\n")
ppr <- fetchFantasyProsRankings(1)
half <- fetchFantasyProsRankings(0.5)
standard <- fetchFantasyProsRankings(0)

check(nrow(ppr) > 100, "PPR rankings returned too few players.")
check(nrow(half) > 100, "Half-PPR rankings returned too few players.")
check(nrow(standard) > 100, "Standard rankings returned too few players.")

cat("Checking public FantasyPros kicker and defense data...\n")
kickers <- fetchFantasyProsADP("k", ppr)
defenses <- fetchFantasyProsADP("dst", ppr)

check(nrow(kickers) > 20, "Kicker data returned too few players.")
check(nrow(defenses) > 20, "Defense data returned too few teams.")
check(all(c("K", "Player", "AVG") %in% names(kickers)), "Kicker columns changed.")
check(all(c("DST", "Player", "AVG") %in% names(defenses)), "Defense columns changed.")

apiKey <- Sys.getenv("FANTASYPROS_API_KEY")
if(!nzchar(apiKey)){
  cat(
    paste0(
      "Public-data checks passed. Full projection check skipped because ",
      "FANTASYPROS_API_KEY is not set.\n"
    )
  )
  quit(status = 0)
}

cat("Checking authenticated FantasyPros projections and complete draft board...\n")
board <- downloadData(apiKey = apiKey)

check(nrow(board$players) > 100, "The draft board returned too few players.")
check(all(c("QB", "RB", "WR", "TE") %in% board$players$Pos), "A position is missing.")
check(!anyNA(board$players$FPTS), "Some players have missing fantasy-point projections.")
check(!anyNA(board$players$VORP), "Some players have missing VORP values.")

cat(
  sprintf(
    "All checks passed: %s players, %s kickers, and %s defenses.\n",
    nrow(board$players),
    nrow(board$kickers),
    nrow(board$def)
  )
)
