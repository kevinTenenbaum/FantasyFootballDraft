library(shiny)
library(DT)
library(stringr)
library(RColorBrewer)

source('ff.R')

# Published at https://kevintenenbaum.shinyapps.io/FantasyFootball/?_ga=2.186942312.1564715598.1591924257-1494174204.1591924257


# dat <- downloadData()
# dat$Pos <- as.character(dat$Pos)

ui <- fluidPage(
  tags$script(HTML("$(function(){ 

      $(document).keyup(function(e) {
      if (e.which == 13) {
        $('#selectPlayer').click()
      }
      });
      })")),
  sidebarLayout(
      conditionalPanel(
        condition = "input.displayType !== 'LeagueSetup'",
        sidebarPanel(style = "position:fixed;width:inherit;",
          actionButton("selectPlayer", "Draft Player"),
          selectInput("MyTeam", choices = NA, label = "Your Team"),
          selectInput("CurrentTeam", choices = NA, label = "Current"),
          selectInput("ViewTeam", choices = NA, label = "View Roster"),
          downloadButton('downloadData', 'Download Draft Board/Results'),
          actionButton("queue", "Add to Queue"),
          actionButton("remove", "Remove from Queue"),
          DT::dataTableOutput("drafted")
      )),
      
    mainPanel(
      textOutput("Round"),
      textOutput("TeamPos"),
      tabsetPanel(
        id = "displayType",
        tabPanel("LeagueSetup",
                 fluidRow(
                   column(4, 
                          h1("Input Team Names"),
                          textInput("TeamNames", label = "Team Names", "Team1, Team2, Team3, Team4, Team5"),
                          textOutput('nTeams'),
                          h1("Set Replacement Levels"),
                          numericInput("qbrepl", label = "QB Replacement Level", value = 14),
                          numericInput("rbrepl", label = "RB Replacement Level", value = 38),
                          numericInput("wrrepl", label = "WR Replacement Level", value = 38),
                          numericInput("terepl", label = "TE Replacement Level", value = 12),
                          h1("Continue Old Draft"),
                          fileInput("oldData", label = "Upload csv file")
                          
                    ),
                  column(4, 
                         h1("Set Point Values"),
                         
                         
                         
                         numericInput("PtsPassYd", label = "Pts per passing yard", 1/20),
                         numericInput("PtsPassTD", label = "Pts per passing TD", 4),
                         numericInput("PtsINT", label = "Pts per Interception", -2),
                         numericInput("PtsRYDS", label = "Pts per rushing yard", 1/10),
                         numericInput("PtsRTDs", label = "Pts per rushing TD", 6),
                         numericInput("PtsFL", label = "Pts per fumble lost", -2),
                         numericInput("PtsRec", label = "Points per Reception", 1),
                         numericInput("PtsRecYds", label = "Points per receiving yard", 1/10),
                         numericInput("PtsRecTDS", label = "Points per receiving TD", 6)
                    ),
                  column(4,
                         h1("Instructions"),
                         p("Input team names, separated by commas, in the order in which they are drafting and click the 'Generate Board' button."),
                         p("You can change the replacement levels and scoring rules used to generate ranking to the left as well."),
                         p("Once you generate the board, you can see available players on the other tabs."),
                         actionButton("download", "Generate Board")
                    )
                 ),
                 ),
        tabPanel("Available",
                 DT::dataTableOutput('players')
                 ),
        tabPanel("Defense",
                 DT::dataTableOutput('defense')
                 ),
        tabPanel("Kickers",
          DT::dataTableOutput('kickers')
        )
      )
      
  )
)
)
  

# datatable(data = dat$players,
#           # extensions = "Buttons",
#           selection = 'single'
#           )


input <- list(TeamNames = c("Kevin, Caroline, Mary, Alan"),
              qbrepl = 14, rbrepl = 38, wrrepl = 38, terepl = 16)
server <- function(input, output, session){
  
  
  
  v <- reactiveValues(players = data.frame()
                      , defense = data.frame()
                      , kickers = data.frame()
                      , drafted = c()
                      , draftedDef = c()
                      , draftedK = c()
                      , teams = data.frame()
                      , Rnd = 1
                      , Pck = 1
                      , rem = NA
                      )
  
  
  observeEvent(input$download, {
    dat <- downloadData(qbrepl = input$qbrepl, rbrepl = input$rbrepl, wrrepl = input$wrrepl, terepl = input$terepl,
                        PassYds = input$PtsPassYd, PassTD = input$PtsPassTD, INT = input$PtsINT, RYDS = input$PtsRYDS,
                        RTDS = input$PtsRTDs, FL = input$PtsFL, REC = input$PtsRec, RecYds= input$PtsRecYds, RecTDs = input$PtsRecTDS)
    
    
    if(!is.null(input$oldData)){
      oldData <- read.csv(input$oldData$datapath)
      dat$players <- dat$players %>% select(-team, -Rnd, -Pck) %>% inner_join(oldData %>% select(Player, Pos, team = Team, Rnd, Pck), by = c('Player','Pos'))
      dat$def <- dat$def %>% mutate(Pos = 'Def') %>% select(-team, -Rnd, -Pck) %>% inner_join(oldData %>% select(Player, Pos, team = Team, Rnd, Pck), by = c('Player','Pos'))
      dat$kickers <- dat$kickers %>% mutate(Pos = 'K') %>% select(-team, -Rnd, -Pck) %>% inner_join(oldData %>% select(Player, Pos, team = Team, Rnd, Pck), by = c('Player','Pos'))
    }
    
    v$players <- dat$players
    v$defense <- dat$def
    v$kickers <- dat$kickers
    v$drafted <- logical(nrow(dat$players))
    v$draftedDef <- logical(nrow(dat$def))
    v$draftedK <- logical(nrow(dat$kickers))
    
    tms <- str_split(input$TeamNames, ", ")[[1]]
    v$teams <- data.frame(teamNum = 1:length(tms), team = tms)
    
    
    
    updateSelectInput(session, 'MyTeam', choices = tms)
    updateSelectInput(session, 'CurrentTeam', choices = tms, selected = tms[1])
    updateSelectInput(session, 'ViewTeam', choices = c(tms, 'All', 'Queue'), selected = input$MyTeam)
  })
  
  observeEvent(input$queue,{
    if(input$displayType == 'Available'){
      v$players[is.na(v$players$team),][input$players_rows_selected,'queue'] <- TRUE
    } else if(input$displayType == "Defense"){
      v$draftedDef[input$defense_rows_selected] <- TRUE
      v$defense[is.na(v$defense$team),][input$defense_rows_selected,c('team','Rnd','Pck')] <- c(input$CurrentTeam, v$Rnd, v$Pck)
    } else if(input$displayType == 'Kickers'){
      v$kickers[is.na(v$kickers$team),][input$kickers_rows_selected] <- TRUE
      v$kickers[is.na(v$kickers$team),][input$kickers_rows_selected,c('team','Rnd','Pck')] <- c(input$CurrentTeam, v$Rnd, v$Pck)
    }
  })
  
  observeEvent(input$remove, {
    
      pl <- v$players %>% filter(queue) %>% select(Player, Team = team, Pos, VORP, Rnd, Pck) %>% arrange(desc(VORP))
      d  <- v$defense %>% filter(queue) %>% mutate(Pos = 'Def', VORP = 0) %>% select(Player, Team = team, Pos, Rnd, Pck)
      k <- v$kickers  %>% filter(queue) %>% mutate(Pos = 'K', VORP = 0) %>% select(Player, Team = team, Pos, Rnd, Pck) 
      
      out <- bind_rows(pl, d, k)  %>% arrange(desc(Rnd), desc(Pck))
      
    
      player <- (out[input$drafted_rows_selected, 'Player'])
      pos <- (out[input$drafted_rows_selected, 'Pos'])
      
      if(pos == 'Def'){
        v$defense[v$defense$Player == player] <- FALSE
      } else if(pos == 'K'){
        v$kickers[v$kickers$Player == player] <- FALSE
      } else{
        v$players[v$players$Player == player & v$players$Pos == pos, 'queue'] <- FALSE
      }
    
  })
  
  observeEvent(input$selectPlayer, {
    nxt <- nextPick(v$teams, input$CurrentTeam, v$Rnd)
    nxtPickRnd <- nextRndPick(v$teams, input$CurrentTeam, v$Rnd)
    
    if(input$displayType == 'Available'){
      v$players[is.na(v$players$team),][input$players_rows_selected,c('team','Rnd','Pck')] <- c(input$CurrentTeam, v$Rnd, v$Pck)
      v$drafted[input$players_rows_selected] <- TRUE
  
      
    } else if(input$displayType == "Defense"){
      v$draftedDef[input$defense_rows_selected] <- TRUE
      v$defense[is.na(v$defense$team),][input$defense_rows_selected,c('team','Rnd','Pck')] <- c(input$CurrentTeam, v$Rnd, v$Pck)
    } else if(input$displayType == 'Kickers'){
      v$kickers[is.na(v$kickers$team),][input$kickers_rows_selected] <- TRUE
      v$kickers[is.na(v$kickers$team),][input$kickers_rows_selected,c('team','Rnd','Pck')] <- c(input$CurrentTeam, v$Rnd, v$Pck)
    }
    updateVarSelectInput(session, "CurrentTeam", selected = nxt)
    v$Rnd <- as.numeric(nxtPickRnd['Rnd'])
    v$Pck <- as.numeric(nxtPickRnd['Pck'])
    
    
  })
  
  output$Round <- renderText({
    paste0("Round: ", v$Rnd, " Pick: ", v$Pck, " Team: ", input$CurrentTeam)
  })
  
  output$TeamPos <- renderText({
    if(nrow(v$players) > 0){
      pl <- v$players  %>% select(Player, team, Pos, VORP, Rnd, Pck) %>% arrange(desc(VORP))
      d  <- v$defense %>% mutate(Pos = 'Def', VORP = 0) %>% select(Player, team, Pos, VORP, Rnd, Pck)
      k <- v$kickers %>% mutate(Pos = 'K', VORP = 0) %>% select(Player, team, Pos, VORP, Rnd, Pck) 
      
      out <- bind_rows(pl, d, k) %>% filter(team == input$MyTeam) 
      
      positions <- data.frame(Pos = c('QB','RB','WR','TE','Def','K'))
      
      byPos <- positions %>% left_join(out %>% group_by(Pos) %>% summarise(num = n()), by = 'Pos') %>% mutate(num = ifelse(is.na(num), 0, num))
      
      return(paste0('Team by Position: ', 
             paste0(byPos$Pos, ': ', byPos$num, collapse = " "))  )
    } else {
      return("")
    }
    
  }
    )
  
  output$players <- DT::renderDataTable({
    if(nrow(v$players) > 0){
      
      datatable(v$players %>% filter(is.na(team)) %>% select(Rnk, Player = PlayerLink, Position = Pos, Bye, FPTS, VORP, Avg, SD = `Std Dev`, Best, Worst, Cluster = cluster), 
                selection = 'single',
                # filter = 'top',
                rownames = FALSE,
                extensions = c('Responsive','ColReorder','FixedHeader'),
                style = 'default',
                filter = 'top',
                escape = 1,
                options = list(pageLength = 200,
                               colReorder = TRUE,
                               colReorder = TRUE,
                               fixedHeader = TRUE
                               , columnDefs = list(list(visible = FALSE, targets = 10),
                                                   list(searchable = FALSE, targets = c(0, 4, 5, 6, 7, 8, 9)))
                               )   
      ) %>% formatStyle('Player', 'Cluster', backgroundColor = styleEqual(1:10, brewer.pal(n = 10, name = 'Set3'))) %>%
        formatStyle("Position", backgroundColor = styleEqual(c('QB','RB','WR','TE'), brewer.pal(n = 4, name  = 'Set2')))
      
      
      
      
    } else{
      datatable(data.frame(Fail  = 1))
    }
    
    })
  
  output$drafted <- DT::renderDataTable({
    if(nrow(v$players) > 0){
      if(input$ViewTeam == 'All'){
        
        
        pl <- v$players %>% select(Player, Team = team, Pos, VORP, Rnd, Pck) %>% arrange(desc(VORP))
        d  <- v$defense %>% mutate(Pos = 'Def', VORP = 0) %>% select(Player, Team = team, Pos, Rnd, Pck)
        k <- v$kickers  %>% mutate(Pos = 'K', VORP = 0) %>% select(Player, Team = team, Pos, Rnd, Pck) 
        
        out <- bind_rows(pl, d, k) %>% filter(!is.na(Team)) %>% arrange(desc(Rnd), desc(Pck))
        
      } else if(input$ViewTeam == 'Queue'){
        pl <- v$players %>% filter(queue) %>% select(Player, Team = team, Pos, VORP, Rnd, Pck) %>% arrange(desc(VORP))
        d  <- v$defense %>% filter(queue) %>% mutate(Pos = 'Def', VORP = 0) %>% select(Player, Team = team, Pos, Rnd, Pck)
        k <- v$kickers  %>% filter(queue) %>% mutate(Pos = 'K', VORP = 0) %>% select(Player, Team = team, Pos, Rnd, Pck) 
        
        out <- bind_rows(pl, d, k)  %>% arrange(desc(Rnd), desc(Pck))
        
      } else{
        
        pl <- v$players %>% filter(team == input$ViewTeam) %>% select(Player, Pos, VORP, Bye, Rnd) %>% arrange(desc(VORP))
        d  <- v$defense %>% filter(team == input$ViewTeam) %>% mutate(Pos = 'Def', VORP = 0, Bye = NA) %>% select(Player, Pos, VORP, Rnd)
        k <- v$kickers %>% filter(team == input$ViewTeam) %>% mutate(Pos = 'K', VORP = 0, Bye = NA) %>% select(Player, Pos, VORP, Rnd) 
        
        out <- bind_rows(pl, d, k)  
      }
      
      if(input$ViewTeam == 'Queue'){
        datatable(out,
                  selection = 'single')
      } else{
        datatable(out,selection = 'single',
                  rownames = FALSE,
                  extensions = c('FixedColumns','Scroller'),
                  options = list(
                    deferRender = TRUE,
                    scrollY = 200,
                    scroller =TRUE,
                    scrollX = TRUE,
                    fixedColumns = FALSE
                  ))  
      }
      
    }
  })
  
  
  output$defense <- DT::renderDataTable({
    if(nrow(v$defense) > 0){
      datatable(v$defense %>% filter(is.na(team)) %>% select(DST, Player, ESPN, Yahoo, MFL, FFC, AVG), 
                selection = 'single')   
    } else{
      datatable(data.frame(Fail  = 1))
    }
    
  })
  
  output$kickers <- DT::renderDataTable({
    if(nrow(v$kickers) > 0){
      datatable(v$kickers %>% filter(is.na(team)) %>% select(K, Player, ESPN, Yahoo, MFL, FFC, AVG), 
                selection = 'single')   
    } else{
      datatable(data.frame(Fail  = 1))
    }
    
  })

  output$nTeams <- renderText({
    paste(length(str_split(input$TeamNames, ", ")[[1]]), 'Teams')
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('data-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      pl <- v$players %>% select(Player, Team = team, Pos, VORP, Rnd, Pck) %>% arrange(desc(VORP))
      d  <- v$defense %>% mutate(Pos = 'Def', VORP = 0) %>% select(Player, Team = team, Pos, Rnd, Pck)
      k <- v$kickers  %>% mutate(Pos = 'K', VORP = 0) %>% select(Player, Team = team, Pos, Rnd, Pck) 
      
      out <- bind_rows(pl, d, k) %>% arrange(desc(Rnd), desc(Pck))
      write.csv(out, con)
    }
  )
  
  session$onSessionEnded(function() {
    stopApp()
  })
  
}

shinyApp(ui, server)

