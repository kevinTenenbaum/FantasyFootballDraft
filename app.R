library(shiny)
library(DT)

source('ff.R')

# dat <- downloadData()
# dat$Pos <- as.character(dat$Pos)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      actionButton("download", "Generate Board"),
      actionButton("selectPlayer", "Draft Player")    
    ),
    mainPanel(
      DT::dataTableOutput('players'),
      textOutput("rowNum")
  )
)
)
  

# datatable(data = dat$players,
#           # extensions = "Buttons",
#           selection = 'single'
#           )

server <- function(input, output, session){
  
  v <- reactiveValues(players = data.frame(), drafted = c())
  
  observeEvent(input$download, {
    dat <- downloadData()
    v$players <- dat$players
    v$drafted <- logical(nrow(dat$players))
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
       v$drafted[input$players_rows_selected] <- TRUE
  })
  
  
  output$players <- DT::renderDataTable({
    cat(head(v$drafted))
    if(nrow(v$players) > 0){
      datatable(bind_cols(v$players, Drafted = v$drafted)[!v$drafted,] %>% select(Player, Drafted, Pos, FPTS, Avg, `Std Dev`, Best, Worst), 
                selection = 'single')   
    } else{
      datatable(data.frame(Fail  = 1))
    }
    
    })
  
  # selected row ID from data table
  selected_row_id <- reactive({
    input$players_rows_selected
  })

  output$rowNum <- renderText({
     as.character(input$players_rows_selected) %>% return()
  })
}

shinyApp(ui, server)

