args <- commandArgs(trailingOnly = TRUE)

numTeams <- as.numeric(args[1])
numRds <- as.numeric(args[2])

# numTeams <- 2
# numRds <- 2

if(numRds %% 2 == 0){
  rds <- rep(c(1:numTeams,numTeams:1), (numRds/2)) - 1
} else {
  rds <- c(rep(c(1:numTeams,numTeams:1), ((numRds-1)/2)),1:numTeams) - 1
}

rdNum <- as.numeric(sapply(1:numRds, function(x) rep(x, numTeams)))
pickNo <- 1:length(rds)
rds <- data.frame(pickNo=pickNo,Round=rdNum, Team=rds)
rds <- rds[-1,]


library(RMySQL)
con <- dbConnect(MySQL(), user="root", password="root", dbname="football", host="localhost", unix.socket="/Applications/MAMP/tmp/mysql/mysql.sock")

dbGetQuery(con, "drop table Rounds")
dbWriteTable(con, 'Rounds', as.data.frame(rds), append=T, row.names=T)
dbDisconnect(con)
