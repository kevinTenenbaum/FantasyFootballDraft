options(stringsAsFactors = FALSE)


downloadData <- function(qbrepl = 14, rbrepl = 38, wrrepl = 38, terepl = 12){
  
  
  require(XML)
  require(stringr)
  require(rvest)
  require(dplyr)
  
  scrapeTable <- function(url){
    site <- read_html(url)
    tables <- site %>%
      html_nodes("table") %>%
      html_table()
    tab <- tables[which.max(sapply(tables, nrow))][[1]]
    rowOne <- max(which(sapply(tab[,1], function(x) length(strsplit(x, ' ')[[1]])) == 1)) + 1
    return(tab[rowOne:nrow(tab),])
  }
  
  
  scrapeADP <- function(url){
    site <- read_html(url)
    tables <- site %>%
      html_nodes("table") %>%
      html_table()
    tab <- tables[which.max(sapply(tables, nrow))][[1]]
    return(tab)
  }
  
  scrapeExperts <- function(url){
    site <- read_html(url)
    tables <- site %>%
      html_nodes("table") %>%
      html_table(fill = TRUE)
    tab <- tables[which.max(sapply(tables, nrow))][[1]][,1:10]
    Tiers <- which(substring(tab[,1], 1, 4) == 'Tier')
    tab <- tab[-Tiers,]
    return(tab)
  }
  cat("Pulling projections... \n")
  qb_fp <- scrapeTable('https://www.fantasypros.com/nfl/projections/qb.php?week=draft')
  rb_fp <- scrapeTable("https://www.fantasypros.com/nfl/projections/rb.php?week=draft")
  wr_fp <- scrapeTable("https://www.fantasypros.com/nfl/projections/wr.php?week=draft")
  te_fp <- scrapeTable("https://www.fantasypros.com/nfl/projections/te.php?week=draft")
  k <- scrapeADP("http://www.fantasypros.com/nfl/adp/k.php")
  DST <- scrapeADP("http://www.fantasypros.com/nfl/adp/dst.php")
  
  names <- strsplit(k[,'Player Team (Bye)'],' ')
  colnames(qb_fp) <- c('Player','PATT','CMP','YDS','TDS','INTS','ATT','RYDS','RTDS','FL','FPTS')
  colnames(te_fp) <- c('Player','REC','YDS','TDS','FL','FPTS')
  colnames(wr_fp) <- c('Player','ATT','RYDS','RTDS','REC','YDS','TDS','FL','FPTS')
  colnames(rb_fp) <- c('Player','ATT','YDS','TDS','REC','RYDS','RTDS','FL','FPTS')
  qb_fp[,'YDS'] <- gsub(",", "", qb_fp[,'YDS'], fixed = TRUE)
  qb_fp[,'RYDS'] <- gsub(",", "", qb_fp[,'RYDS'], fixed = TRUE) 
  for(i in 1:nrow(qb_fp)){
    qb_fp[i,'FPTS'] <- c(1/20,4,-2,1/10,6,-2)%*%as.numeric(qb_fp[i,c('YDS','TDS','INTS','RYDS','RTDS','FL')])
  }
  
  colnames(rb_fp) <- c('Player','ATT','YDS','TDS','REC','RYDS','RTDS','FL','FPTS')
  rb_fp[,'YDS'] <- gsub(",", "", rb_fp[,'YDS'], fixed = TRUE) 
  rb_fp[,'RYDS']<- gsub(",", "", rb_fp[,'RYDS'], fixed = TRUE) 
  
  for(i in 1:nrow(rb_fp)){
    rb_fp[i,'FPTS'] <- c(1/10,6,1/10,6,-2,1)%*%as.numeric(rb_fp[i,c('YDS','TDS','RYDS','RTDS','FL','REC')])
  }
  
  # colnames(wr_fp) <- colnames(rb_fp)
  wr_fp[,'YDS'] <- gsub(",", "", wr_fp[,'YDS'], fixed = TRUE) 
  wr_fp[,'RYDS']<- gsub(",", "", wr_fp[,'RYDS'], fixed = TRUE) 
  for(i in 1:nrow(wr_fp)){
    wr_fp[i,'FPTS'] <- c(1/10,6,1/10,6,-2,1)%*%as.numeric(wr_fp[i,c('YDS','TDS','RYDS','RTDS','FL','REC')])
  }
  
  te_fp[,'YDS'] <- gsub(",", "", te_fp[,'YDS'], fixed = TRUE) 
  for(i in 1:nrow(te_fp)){
    te_fp[i,'FPTS'] <- c(1/10,6,-2,1)%*%as.numeric(te_fp[i,c('YDS','TDS','FL','REC')])
  }
  
  k.names <- c()
  for (i in 1:length(names)){
    name_first <- names[[i]][1]
    name_last <- names[[i]][2]
    name <- paste(name_first,name_last)
    k.names <- c(k.names, name)
  }
  k$name <- k.names
  names <- strsplit(DST[,'Player Team (Bye)'],' ')
  
  DST.names <- c()
  for (i in 1:length(names)){
    name_first <- names[[i]][1]
    name_last <- names[[i]][2]
    name <- paste(name_first,name_last)
    DST.names <- c(DST.names, name)
  }
  
  DST$name <- DST.names
  
  
  experts <- scrapeExperts("http://www.fantasypros.com/nfl/rankings/consensus-cheatsheets.php")
  names(experts)[1] <- 'Rank'
  qb_fp <- cbind(qb_fp,'QB')
  rb_fp <- cbind(rb_fp,'RB')
  wr_fp <- cbind(wr_fp,'WR')
  te_fp <- cbind(te_fp,'TE')
  names(qb_fp)[ncol(qb_fp)] <- names(rb_fp)[ncol(rb_fp)] <- names(wr_fp)[ncol(wr_fp)] <- names(te_fp)[ncol(te_fp)] <- 'Pos'
  qb_fp$FPTS <- as.numeric(qb_fp$FPTS)
  rb_fp$FPTS <- as.numeric(rb_fp$FPTS)
  wr_fp$FPTS <- as.numeric(wr_fp$FPTS)
  te_fp$FPTS <- as.numeric(te_fp$FPTS)
  
  
 
  
  # qbrepl <- 17
  # rbrepl <- 35
  # wrrepl <- 35
  # terepl <-  13
  
  qb_fp <- qb_fp[order(qb_fp$FPTS, decreasing = TRUE),]
  rb_fp <- rb_fp[order(rb_fp$FPTS, decreasing = TRUE),]
  wr_fp <- wr_fp[order(wr_fp$FPTS, decreasing = TRUE),]
  te_fp <- te_fp[order(te_fp$FPTS, decreasing = TRUE),]
  
  
  ## Calculate Replacement Levels
  qbr <- mean(qb_fp[qbrepl, 'FPTS'], qb_fp[qbrepl+1,'FPTS'], qb_fp[qbrepl-1,'FPTS'])
  rbr <- mean(rb_fp[rbrepl, 'FPTS'], rb_fp[rbrepl+1,'FPTS'], rb_fp[rbrepl-1,'FPTS'])
  wrr <- mean(wr_fp[wrrepl, 'FPTS'], wr_fp[wrrepl+1,'FPTS'], wr_fp[wrrepl-1,'FPTS'])
  ter <- mean(te_fp[terepl, 'FPTS'], te_fp[terepl+1,'FPTS'], te_fp[terepl-1,'FPTS'])
  
  qb_fp$VORP <- qb_fp$FPTS - qbr
  rb_fp$VORP <- rb_fp$FPTS - rbr
  wr_fp$VORP <- wr_fp$FPTS - wrr
  te_fp$VORP <- te_fp$FPTS - ter
  
  
  
  qb_fp$cluster <- clusterPlayers(qb_fp, 10)
  rb_fp$cluster <- clusterPlayers(rb_fp, 10)
  wr_fp$cluster <- clusterPlayers(wr_fp, 10)
  te_fp$cluster <- clusterPlayers(te_fp, 10)
  
  fp_all <- rbind(qb_fp[,c('Player','FPTS','Pos','VORP','cluster')], rb_fp[,c('Player','FPTS','Pos','VORP','cluster')],wr_fp[,c('Player','FPTS','Pos','VORP','cluster')], te_fp[,c('Player','FPTS','Pos','VORP','cluster')])
  
  names <- strsplit(fp_all$Player,' ')
  
  fp_all.names <- c()
  for (i in 1:length(names)){
    name_first <- names[[i]][1]
    name_last <- names[[i]][2]
    name <- paste(name_first,name_last)
    fp_all.names <- c(fp_all.names, name)
  }
  
  fp_all$name <- fp_all.names
  
  names <- strsplit(experts[,'Overall (Team)'], ' ')
  
  experts.names <- c()
  teambyes <- c()
  for (i in 1:length(names)){
    name_first <- names[[i]][1]
    name_last <- names[[i]][3]
    name <- paste(name_first, name_last)
    experts.names <- c(experts.names,name)
    team.bye <- names[[i]][3]
    teambyes <- c(teambyes, team.bye)
  }
  
  experts$name <- experts.names
  colnames(experts)[2] <- 'Player'
  
  experts$team.bye <- teambyes
  
  experts <- experts[,c('Avg', 'Std Dev','Best','Worst','team.bye','name')]
  
  all <- fp_all %>% left_join(experts, by = 'name')
  all <- all[order(all$VORP, decreasing=T),c('Player','team.bye','Pos','FPTS','VORP', 'Avg','Std Dev', 'Best','Worst','cluster')]
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
  
  
  
  
  top <- function(df,n){
    print(df[1:n,])
  }
  
  colnames(DST)[3] <- 'Player'
  colnames(k)[3] <- 'Player'
  
  all$Player <- str_replace(all$Player, "'", '')
  all$Pos <- as.character(all$Pos)
  
  all$team <- NA
  DST$team <- NA
  k$team <- NA
  
  all$Rnd <- NA
  all$Pck <- NA
  DST$Rnd <- NA
  DST$Pck <- NA
  k$Rnd <- NA
  k$Pck <- NA
  all$queue <- FALSE
  DST$queue <- FALSE
  k$queue <- FALSE
  
  all$Rnk <- 1:nrow(all)
  
  all <- all %>% mutate(PlayerName = tolower(str_replace(trimws(substring(Player, 1, nchar(Player)-3)), ' ', '-')),
             PlayerLink = paste0('<a href = "https://www.fantasypros.com/nfl/projections/', PlayerName, '.php" target="_blank">', Player, '</a>'))
  
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
