library(shiny)
library(DT)
library(stringr)

source('ff.R')



# dat <- downloadData()
# dat$Pos <- as.character(dat$Pos)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      actionButton("download", "Generate Board"),
      actionButton("selectPlayer", "Draft Player"),
      selectInput("MyTeam", choices = NA, label = "Your Team"),
      selectInput("CurrentTeam", choices = NA, label = "Current"),
      selectInput("ViewTeam", choices = NA, label = "View Roster"),
      DT::dataTableOutput("drafted")
    ),
    mainPanel(
      textOutput("Round"),
      tabsetPanel(
        id = "displayType",
        tabPanel("LeagueSetup",
                 textInput("TeamNames", label = "Team Names", "Kevin, Caroline, Mom, Dad, Bumpy, Steve"),
                 numericInput("qbrepl", label = "QB Replacement Level", value = 14),
                 numericInput("rbrepl", label = "QB Replacement Level", value = 38),
                 numericInput("wrrepl", label = "QB Replacement Level", value = 38),
                 numericInput("terepl", label = "QB Replacement Level", value = 12)
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


# input <- list(TeamNames = c("Kevin, Caroline, Mary, Alan"))
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
                      )
  
  
  observeEvent(input$download, {
    dat <- downloadData(qbrepl = input$qbrepl, rbrepl = input$rbrepl, wrrepl = input$wrrepl)
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
    updateSelectInput(session, 'ViewTeam', choices = tms, selected = input$MyTeam)
    
    
    # isolate({
    #   players <- dat$players
    #   kickers <- dat$k
    #   def <- dat$def
    # })
    # players(dat$players)
    # kickers(dat$k)
    # def(dat$def)
  })
  
  observeEvent(input$selectPlayer, {
    nxt <- nextPick(v$teams, input$CurrentTeam, v$Rnd)
    nxtPickRnd <- nextRndPick(v$teams, input$CurrentTeam, v$Rnd)
    
    if(input$displayType == 'Available'){
      v$players[is.na(v$players$team),][input$players_rows_selected,'team'] <- input$CurrentTeam
      v$drafted[input$players_rows_selected] <- TRUE
  
      
    } else if(input$displayType == "Defense"){
      v$draftedDef[input$defense_rows_selected] <- TRUE
      v$defense[is.na(v$defense$team),][input$defense_rows_selected,'team'] <- input$CurrentTeam
    } else if(input$displayType == 'Kickers'){
      v$kickers[is.na(v$kickers$team),][input$kickers_rows_selected] <- TRUE
      v$kickers[is.na(v$kickers$team),][input$kickers_rows_selected,'team'] <- input$CurrentTeam
    }
    updateVarSelectInput(session, "CurrentTeam", selected = nxt)
    v$Rnd <- as.numeric(nxtPickRnd['Rnd'])
    v$Pck <- as.numeric(nxtPickRnd['Pck'])
    
  })
  
  output$Round <- renderText(paste0("Round: ", v$Rnd, " Pick: ", v$Pck))
  
  output$players <- DT::renderDataTable({
    if(nrow(v$players) > 0){
      datatable(v$players %>% filter(is.na(team)) %>% select(Rnk, Player, team, Pos, FPTS, VORP, Avg, `Std Dev`, Best, Worst), 
                selection = 'single',
                filter = 'top')   
    } else{
      datatable(data.frame(Fail  = 1))
    }
    
    })
  
  output$drafted <- DT::renderDataTable({
    if(nrow(v$players) > 0){
      pl <- v$players %>% filter(team == input$ViewTeam) %>% select(Rnk, Player, Pos, VORP) %>% arrange(desc(VORP))
      d  <- v$defense %>% filter(team == input$ViewTeam) %>% mutate(Pos = 'Def', VORP = 0) %>% select(Rnk = DST, Player, Pos, VORP)
      k <- v$kickers %>% filter(team == input$ViewTeam) %>% mutate(Pos = 'K', VORP = 0) %>% select(Rnk = K, Player, Pos, VORP) 
      
      out <- bind_rows(pl, d, k)
      datatable(out)
    }
  })
  
  
  output$defense <- DT::renderDataTable({
    if(nrow(v$defense) > 0){
      datatable(v$defense[!v$draftedDef,] %>% select(DST, Player, ESPN, Yahoo, MFL, FFC, AVG), 
                selection = 'single')   
    } else{
      datatable(data.frame(Fail  = 1))
    }
    
  })
  
  output$kickers <- DT::renderDataTable({
    if(nrow(v$kickers) > 0){
      datatable(v$kickers[!v$draftedK,] %>% select(K, Player, ESPN, Yahoo, MFL, FFC, AVG), 
                selection = 'single')   
    } else{
      datatable(data.frame(Fail  = 1))
    }
    
  })

  
}

shinyApp(ui, server)

