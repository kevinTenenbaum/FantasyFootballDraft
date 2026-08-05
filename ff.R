options(stringsAsFactors = FALSE)
require(stringr)
require(rvest)
require(dplyr)
require(purrr)


extractEmbeddedJson <- function(html, variablePattern){
  pattern <- paste0(
    "(?s)(?:var\\s+)?",
    variablePattern,
    "\\s*=\\s*(\\{.*?\\});"
  )
  match <- regexec(pattern, html, perl = TRUE)
  value <- regmatches(html, match)[[1]]

  if(length(value) < 2){
    stop(
      paste("FantasyPros changed the page format; could not find", variablePattern),
      call. = FALSE
    )
  }

  jsonlite::fromJSON(value[[2]], simplifyVector = FALSE)
}


readFantasyProsPage <- function(url){
  if(!requireNamespace("jsonlite", quietly = TRUE)){
    stop("The jsonlite package is required to read FantasyPros data.", call. = FALSE)
  }

  tryCatch(
    as.character(rvest::read_html(url)),
    error = function(error){
      stop(
        sprintf("Could not download FantasyPros page %s: %s", url, error$message),
        call. = FALSE
      )
    }
  )
}


fetchFantasyProsADP <- function(position, rankings = NULL){
  position <- tolower(position)
  if(!position %in% c("k", "dst")){
    stop("FantasyPros ADP position must be either 'k' or 'dst'.", call. = FALSE)
  }

  url <- sprintf("https://www.fantasypros.com/nfl/adp/%s.php", position)
  config <- extractEmbeddedJson(
    readFantasyProsPage(url),
    "window\\.FP\\.reportConfig"
  )

  fields <- config$table$fields
  rows <- config$table$rows
  if(is.null(fields) || is.null(rows) || length(rows) == 0){
    stop(
      sprintf("FantasyPros returned no %s ADP rows.", toupper(position)),
      call. = FALSE
    )
  }

  fieldKeys <- setNames(
    vapply(fields, function(field) field$key, character(1)),
    vapply(fields, function(field) field$label, character(1))
  )

  valueFor <- function(row, label, numeric = TRUE){
    key <- unname(fieldKeys[[label]])
    value <- if(is.null(key) || is.null(row[[key]])) NA else row[[key]]
    if(numeric) suppressWarnings(as.numeric(value)) else as.character(value)
  }

  parsedRows <- lapply(rows, function(row){
    player <- row$player
    data.frame(
      Rank = valueFor(row, toupper(position)),
      Overall = valueFor(row, "Overall"),
      FantasyProsID = as.character(row$id),
      Player = if(is.null(player$name)) NA_character_ else player$name,
      ESPN = valueFor(row, "ESPN"),
      Yahoo = valueFor(row, "Yahoo"),
      CBS = valueFor(row, "CBS"),
      Fantrax = valueFor(row, "Fantrax"),
      Sleeper = valueFor(row, "Sleeper"),
      RTSports = valueFor(row, "RTSports"),
      AVG = valueFor(row, "AVG"),
      `Real-Time` = valueFor(row, "Real-Time"),
      check.names = FALSE
    )
  })

  out <- bind_rows(parsedRows)
  names(out)[1] <- toupper(position)

  if(is.null(rankings)) return(out)

  requiredColumns <- c(
    "FantasyProsID", "PlayerName", "Pos", "ECR", "PositionRank"
  )
  if(!all(requiredColumns %in% names(rankings))){
    stop("Rankings data is missing fields needed to complete K/DST ADP.", call. = FALSE)
  }

  positionRankings <- rankings %>%
    filter(Pos == toupper(position)) %>%
    transmute(
      FantasyProsID,
      PositionRank,
      ECR,
      Player = PlayerName
    )

  if(nrow(positionRankings) == 0){
    stop(
      sprintf("FantasyPros rankings returned no %s players.", toupper(position)),
      call. = FALSE
    )
  }

  rankColumn <- toupper(position)
  preview <- out
  names(preview)[names(preview) == rankColumn] <- "ADPRank"
  names(preview)[names(preview) == "Overall"] <- "ADPOverall"

  completed <- positionRankings %>%
    left_join(preview %>% select(-Player), by = "FantasyProsID") %>%
    transmute(
      Rank = dplyr::coalesce(ADPRank, PositionRank),
      Overall = dplyr::coalesce(ADPOverall, ECR),
      FantasyProsID,
      Player,
      ESPN,
      Yahoo,
      CBS,
      Fantrax,
      Sleeper,
      RTSports,
      AVG,
      `Real-Time`
    ) %>%
    arrange(Rank)

  names(completed)[1] <- rankColumn
  completed
}


fetchFantasyProsRankings <- function(receptionPoints = 1){
  page <- if(isTRUE(all.equal(receptionPoints, 1))){
    "ppr-cheatsheets.php"
  } else if(isTRUE(all.equal(receptionPoints, 0.5))){
    "half-point-ppr-cheatsheets.php"
  } else {
    "consensus-cheatsheets.php"
  }

  url <- paste0("https://www.fantasypros.com/nfl/rankings/", page)
  rankings <- extractEmbeddedJson(readFantasyProsPage(url), "ecrData")

  if(is.null(rankings$players) || length(rankings$players) == 0){
    stop("FantasyPros returned no consensus ranking rows.", call. = FALSE)
  }

  scalarNumber <- function(value){
    if(is.null(value) || length(value) == 0) return(NA_real_)
    suppressWarnings(as.numeric(value[[1]]))
  }

  bind_rows(lapply(rankings$players, function(player){
    ecrDelta <- scalarNumber(player$player_ecr_delta)
    rank <- scalarNumber(player$rank_ecr)
    positionRank <- if(is.null(player$pos_rank) || length(player$pos_rank) == 0){
      NA_real_
    } else {
      suppressWarnings(as.numeric(stringr::str_extract(player$pos_rank[[1]], "[0-9]+")))
    }

    data.frame(
      FantasyProsID = as.character(player$player_id),
      PlayerName = player$player_name,
      Team = player$player_team_id,
      Player = paste(player$player_name, player$player_team_id),
      Pos = player$player_position_id,
      ECR = rank,
      PositionRank = positionRank,
      Best = scalarNumber(player$rank_min),
      Worst = scalarNumber(player$rank_max),
      Avg = scalarNumber(player$rank_ave),
      `Std Dev` = scalarNumber(player$rank_std),
      ADP = if(is.na(ecrDelta)) NA_real_ else rank + ecrDelta,
      check.names = FALSE
    )
  }))
}


projectionStats <- function(player){
  stats <- player$stats
  if(is.null(stats) || length(stats) == 0){
    stop(
      sprintf("FantasyPros returned no projection stats for %s.", player$name),
      call. = FALSE
    )
  }

  if(!is.null(names(stats)) && "points" %in% names(stats)){
    return(stats)
  }

  if(is.list(stats[[1]])) return(stats[[1]])

  stop(
    sprintf("FantasyPros returned an unexpected projection format for %s.", player$name),
    call. = FALSE
  )
}


parseFantasyProsProjections <- function(payload, position){
  players <- payload$players
  if(is.null(players) || length(players) == 0){
    stop(
      sprintf("FantasyPros returned no %s projections.", position),
      call. = FALSE
    )
  }

  reportedCount <- suppressWarnings(as.integer(payload$count))
  if(
    length(reportedCount) == 1 &&
      !is.na(reportedCount) &&
      reportedCount > length(players)
  ){
    stop(
      paste0(
        "FantasyPros reports ", reportedCount, " ", position,
        " projections but returned only ", length(players),
        ". This API key appears limited to free-tier sample data; ",
        "the draft board requires a Premium/HOF or other full-data key."
      ),
      call. = FALSE
    )
  }

  statValue <- function(stats, name){
    value <- stats[[name]]
    if(is.null(value) || length(value) == 0) return(NA_real_)
    suppressWarnings(as.numeric(value[[1]]))
  }

  out <- bind_rows(lapply(players, function(player){
    stats <- projectionStats(player)
    data.frame(
      FantasyProsID = as.character(player$fpid),
      PlayerName = as.character(player$name),
      Team = as.character(player$team_id),
      PlayerFile = as.character(player$filename),
      pass_att = statValue(stats, "pass_att"),
      pass_cmp = statValue(stats, "pass_cmp"),
      pass_yds = statValue(stats, "pass_yds"),
      pass_tds = statValue(stats, "pass_tds"),
      pass_ints = statValue(stats, "pass_ints"),
      rush_att = statValue(stats, "rush_att"),
      rush_yds = statValue(stats, "rush_yds"),
      rush_tds = statValue(stats, "rush_tds"),
      receptions = statValue(stats, "rec_rec"),
      rec_yds = statValue(stats, "rec_yds"),
      rec_tds = statValue(stats, "rec_tds"),
      fumbles = statValue(stats, "fumbles"),
      check.names = FALSE
    )
  }))

  requiredStat <- if(position == "QB") "pass_yds" else "rec_yds"
  if(all(is.na(out[[requiredStat]]))){
    stop(
      sprintf("FantasyPros %s projections are missing %s.", position, requiredStat),
      call. = FALSE
    )
  }

  out
}


fetchFantasyProsProjections <- function(position,
                                        season = as.integer(format(Sys.Date(), "%Y")),
                                        apiKey = Sys.getenv("FANTASYPROS_API_KEY")){
  position <- toupper(position)
  if(!position %in% c("QB", "RB", "WR", "TE")){
    stop("FantasyPros projection position must be QB, RB, WR, or TE.", call. = FALSE)
  }

  if(!nzchar(apiKey)){
    stop(
      paste(
        "FantasyPros now limits its public projection tables to a small preview.",
        "Set FANTASYPROS_API_KEY to use the official full projections API:",
        "https://www.fantasypros.com/api-data/"
      ),
      call. = FALSE
    )
  }
  if(!requireNamespace("httr", quietly = TRUE)){
    stop("The httr package is required to download FantasyPros projections.", call. = FALSE)
  }
  if(!requireNamespace("jsonlite", quietly = TRUE)){
    stop("The jsonlite package is required to parse FantasyPros projections.", call. = FALSE)
  }

  url <- sprintf(
    "https://api.fantasypros.com/public/v2/json/nfl/%s/projections",
    as.integer(season)
  )
  response <- httr::GET(
    url,
    httr::add_headers(`x-api-key` = apiKey),
    query = list(position = position, week = 0),
    httr::timeout(30)
  )

  if(httr::status_code(response) != 200){
    stop(
      sprintf(
        "FantasyPros API request for %s failed with HTTP %s. Check FANTASYPROS_API_KEY and API access.",
        position,
        httr::status_code(response)
      ),
      call. = FALSE
    )
  }

  payload <- jsonlite::fromJSON(
    httr::content(response, as = "text", encoding = "UTF-8"),
    simplifyVector = FALSE
  )
  parseFantasyProsProjections(payload, position)
}


# dat <- downloadData()

# input <- list(qbrepl = 14, rbrepl = 38, wrrepl = 38, terepl = 12,
#               PtsPassYd = 1/20, PtsPassTD = 4, PtsINT = -2, PtsRYDS = .1, PtsRTDs = 6,
#               PtsFL = -2, PtsREC = 1, PtsRecYds = .1, PtsRecTDS = 6)
# 
# attach(list(qbrepl = input$qbrepl, rbrepl = input$rbrepl, wrrepl = input$wrrepl, terepl = input$terepl,
#             PassYds = input$PtsPassYd, PassTD = input$PtsPassTD, INT = input$PtsINT, RYDS = input$PtsRYDS,
#             RTDS = input$PtsRTDs, FL = input$PtsFL, REC = input$PtsREC, RecYds= input$PtsRecYds, RecTDs = input$PtsRecTDS))

# downloadData(qbrepl = input$qbrepl, rbrepl = input$rbrepl, wrrepl = input$wrrepl, terepl = input$terepl,
#              PassYds = input$PtsPassYd, PassTD = input$PtsPassTD, INT = input$PtsINT, RYDS = input$PtsRYDS,
#              RTDS = input$PtsRTDs, FL = input$PtsFL, REC = input$PtsREC, RecYds= input$PtsRecYds, RecTDs = input$PtsRecTDS)$players %>% select(-PlayerLink) %>% arrange(desc(VORP)) #%>% filter(Pos == 'RB')

downloadData <- function(qbrepl = 14, rbrepl = 38, wrrepl = 38, terepl = 12,
                         PassYds = 1/20, PassTD = 4, INT = -2, RYDS = 1/10, 
                         RTDS = 6, FL = -2, REC = 1, RecYds = 1/10, RecTDs = 6,
                         season = as.integer(format(Sys.Date(), "%Y")),
                         apiKey = Sys.getenv("FANTASYPROS_API_KEY")){

  cat("Pulling projections... \n")
  projections <- list(
    QB = fetchFantasyProsProjections("QB", season, apiKey),
    RB = fetchFantasyProsProjections("RB", season, apiKey),
    WR = fetchFantasyProsProjections("WR", season, apiKey),
    TE = fetchFantasyProsProjections("TE", season, apiKey)
  )

  scoreProjection <- function(projection, position){
    fpts <- if(position == "QB"){
      projection$pass_yds * PassYds +
        projection$pass_tds * PassTD +
        projection$pass_ints * INT +
        projection$rush_yds * RYDS +
        projection$rush_tds * RTDS +
        projection$fumbles * FL
    } else {
      projection$rush_yds * RYDS +
        projection$rush_tds * RTDS +
        projection$rec_yds * RecYds +
        projection$rec_tds * RecTDs +
        projection$fumbles * FL +
        projection$receptions * REC
    }

    projection %>%
      transmute(
        FantasyProsID,
        Player = paste(PlayerName, Team),
        PlayerFile,
        FPTS = as.numeric(fpts),
        Pos = position
      )
  }

  qb_fp <- scoreProjection(projections$QB, "QB")
  rb_fp <- scoreProjection(projections$RB, "RB")
  wr_fp <- scoreProjection(projections$WR, "WR")
  te_fp <- scoreProjection(projections$TE, "TE")

  cat("Pulling rankings and ADP... \n")
  experts <- fetchFantasyProsRankings(REC)
  k <- fetchFantasyProsADP("k", experts)
  DST <- fetchFantasyProsADP("dst", experts)
  
  
 
  
  # qbrepl <- 17
  # rbrepl <- 35
  # wrrepl <- 35
  # terepl <-  13
  
  qb_fp <- qb_fp[order(qb_fp$FPTS, decreasing = TRUE),]
  rb_fp <- rb_fp[order(rb_fp$FPTS, decreasing = TRUE),]
  wr_fp <- wr_fp[order(wr_fp$FPTS, decreasing = TRUE),]
  te_fp <- te_fp[order(te_fp$FPTS, decreasing = TRUE),]
  tewr_fp <- bind_rows(wr_fp, te_fp) %>% arrange(desc(FPTS))

  replacementPoints <- function(players, rank, position){
    if(length(rank) != 1 || is.na(rank) || rank <= 1 || rank >= nrow(players)){
      stop(
        sprintf(
          "%s replacement rank %s is invalid; FantasyPros returned %s eligible players.",
          position,
          rank,
          nrow(players)
        ),
        call. = FALSE
      )
    }
    mean(players$FPTS[c(rank - 1, rank, rank + 1)], na.rm = TRUE)
  }

  ## Calculate Replacement Levels
  qbr <- replacementPoints(qb_fp, qbrepl, "QB")
  rbr <- replacementPoints(rb_fp, rbrepl, "RB")
  wrr <- replacementPoints(tewr_fp, wrrepl, "WR/TE")
  ter <- replacementPoints(tewr_fp, terepl, "WR/TE")
  
  qb_fp$VORP <- qb_fp$FPTS - qbr
  rb_fp$VORP <- rb_fp$FPTS - rbr
  wr_fp$VORP <- wr_fp$FPTS - wrr
  te_fp$VORP <- te_fp$FPTS - ter
  
  
  qb_fp$cluster <- clusterPlayers(qb_fp, 10)
  rb_fp$cluster <- clusterPlayers(rb_fp, 10)
  wr_fp$cluster <- clusterPlayers(wr_fp, 10)
  te_fp$cluster <- clusterPlayers(te_fp, 10)
  
  fp_all <- bind_rows(
    qb_fp %>% select(FantasyProsID, Player, PlayerFile, FPTS, Pos, VORP, cluster),
    rb_fp %>% select(FantasyProsID, Player, PlayerFile, FPTS, Pos, VORP, cluster),
    wr_fp %>% select(FantasyProsID, Player, PlayerFile, FPTS, Pos, VORP, cluster),
    te_fp %>% select(FantasyProsID, Player, PlayerFile, FPTS, Pos, VORP, cluster)
  )

  all <- fp_all %>%
    arrange(desc(VORP)) %>%
    left_join(
      experts %>% select(FantasyProsID, Best, Worst, Avg, `Std Dev`, ADP),
      by = "FantasyProsID"
    )

  all <- all[
    order(all$VORP, decreasing = TRUE),
    c(
      'FantasyProsID', 'Player', 'PlayerFile', 'Pos', 'FPTS', 'VORP',
      'Avg', 'Std Dev', 'Best', 'Worst', 'cluster', 'ADP'
    )
  ]
  rownames(all) <- 1:nrow(all)
  all[,c('FPTS','VORP')] <- round(all[,c('FPTS','VORP')])
  qbs <- all[which(all[,'Pos']=='QB'),]
  rbs <- all[which(all[,'Pos']=='RB'),]
  wrs <- all[which(all[,'Pos']=='WR'),]
  tes <- all[which(all[,'Pos']=='TE'),]
  
  
  # all <- cbind(all, 0,0,0)
  # colnames(all)[(ncol(all)-2):ncol(all)] <- c('one','five','ten')
  # for (i in 1:(nrow(all)-10)){
  #   all[i,'one'] <- all[i,'VORP'] - all[i+1,'VORP']
  #   all[i,'five'] <- all[i,'VORP'] - all[i+5,'VORP']
  #   all[i,'ten'] <- all[i,'VORP'] - all[i+10, 'VORP']
  # }
  # 
  # for (i in 1:(nrow(qbs)-10)){
  #   qbs[i,'one'] <- qbs[i,'VORP'] - qbs[i+1,'VORP']
  #   qbs[i,'five'] <- qbs[i,'VORP'] - qbs[i+5,'VORP']
  #   qbs[i,'ten'] <- qbs[i,'VORP'] - qbs[i+10, 'VORP']
  # }
  # 
  # for (i in 1:(nrow(rbs)-10)){
  #   rbs[i,'one'] <- rbs[i,'VORP'] - rbs[i+1,'VORP']
  #   rbs[i,'five'] <- rbs[i,'VORP'] - rbs[i+5,'VORP']
  #   rbs[i,'ten'] <- rbs[i,'VORP'] - rbs[i+10, 'VORP']
  # }
  # 
  # for (i in 1:(nrow(wrs)-10)){
  #   wrs[i,'one'] <- wrs[i,'VORP'] - wrs[i+1,'VORP']
  #   wrs[i,'five'] <- wrs[i,'VORP'] - wrs[i+5,'VORP']
  #   wrs[i,'ten'] <- wrs[i,'VORP'] - wrs[i+10, 'VORP']
  # }
  # 
  # for (i in 1:(nrow(tes)-10)){
  #   tes[i,'one'] <- tes[i,'VORP'] - tes[i+1,'VORP']
  #   tes[i,'five'] <- tes[i,'VORP'] - tes[i+5,'VORP']
  #   tes[i,'ten'] <- tes[i,'VORP'] - tes[i+10, 'VORP']
  # }
  
  
  
  
  all$Pos <- as.character(all$Pos)
  
  all$team <- NA_character_
  DST$team <- NA_character_
  k$team <- NA_character_
  
  all$Rnd <- NA_character_
  all$Pck <- NA_character_
  DST$Rnd <- NA_character_
  DST$Pck <- NA_character_
  k$Rnd <- NA_character_
  k$Pck <- NA_character_
  all$queue <- FALSE
  DST$queue <- FALSE
  k$queue <- FALSE
  
  all$Best <- as.numeric(all$Best)
  all$Worst <- as.numeric(all$Worst)
  all$Avg <- as.numeric(all$Avg)
  all$`Std Dev` <- as.numeric(all$`Std Dev`)
  # all$Avg <- ifelse(is.na(all$Avg), Inf, all$Avg)
  
  all$Rnk <- 1:nrow(all)

  all <- all %>%
    mutate(
      PlayerLink = paste0(
        '<a href = "https://www.fantasypros.com/nfl/projections/',
        PlayerFile,
        '" target="_blank">',
        Player,
        '</a>'
      )
    ) %>%
    select(-FantasyProsID, -PlayerFile)
  
  outList <- list(players = all, 
                  def = DST,
                  kickers = k,
                  qbr = qbr,
                  rbr = rbr,
                  wrr = wrr,
                  ter = ter)
  
  return(outList)
}


nextRndPick <- function(tms, currentTeam, Rnd){
  pickIndex <- which(tms$team == currentTeam)
  direction <- ifelse(Rnd %% 2 == 0, -1, 1)
  
  if(direction == -1){
    pickIndex <- nrow(tms) - pickIndex + 1
  }
  newRnd <- Rnd
  newPick <- pickIndex + 1
  if(direction == 1 & pickIndex >= nrow(tms)){
    newRnd <- Rnd + 1
    newPick <- 1
  } 
  if(direction == -1 & pickIndex >= nrow(tms)){
    newRnd <- Rnd + 1
    newPick <- 1
  }
  
  
  return(c(Rnd = newRnd, Pck = newPick))
}

nextPick <- function(tms, currentTeam, Rnd){
  pickIndex <- which(tms$team == currentTeam)
  direction <- ifelse(Rnd %% 2 == 0, -1, 1)
  if(direction == 1 & pickIndex == nrow(tms)){
    direction <- 0
  } 
  if(direction == -1 & pickIndex == 1){
    direction <-0
  }
  tms[pickIndex + direction, 'team']
}

clusterPlayers <- function(players, n = 10){
  kmeansOut <- kmeans(players[,c('VORP')], n)
  Clusters <- data.frame(orig = 1:nrow(kmeansOut$centers), val = kmeansOut$centers)
  Clusters <- Clusters %>% arrange(desc(val))
  Clusters$new <- 1:nrow(Clusters)
  data.frame(orig = kmeansOut$cluster) %>% inner_join(Clusters, by = 'orig') %>% select(new) %>% unlist()
  
}

# 
# library(RMySQL)
# con <- dbConnect(MySQL(), user="root", password="root", dbname="football", host="localhost", unix.socket="/Applications/MAMP/tmp/mysql/mysql.sock")
# dbGetQuery(con, "drop table if exists Available")
# dbGetQuery(con, "drop table if exists drafted")
# dbGetQuery(con, "drop table if exists TEAMS")
# dbGetQuery(con, "drop table if exists Defense")
# dbGetQuery(con, "drop table if exists Kickers")
# dbGetQuery(con, "drop table if exists Queue")
# dbWriteTable(con, 'Available', all, append=T, row.names=T)
# dbGetQuery(con, "create table drafted(
#            Player varchar(25),
#            Team varchar(10),
#            Pos varchar(5)
#            )")
# dbGetQuery(con, "create table Queue(
#            Player Text(25))
#            ")
# dbGetQuery(con, "create table TEAMS (
#              name varchar(20)
#            )")
# dbWriteTable(con, 'Defense', DST, append=T, row.names=F)
# dbWriteTable(con, "Kickers", k, append=T, row.names=F)
# dbDisconnect(con)
# 
# 
