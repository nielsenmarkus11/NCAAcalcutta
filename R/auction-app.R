#' @title Start the NCAA Calcutta Shiny App
#' 
#' @examples 
#' # Input the 2018 teams
#' teams <- import_teams(system.file("extdata", "ncaa-teams.csv", package = "NCAAcalcutta"))
#' start_auction(teams, randomize=TRUE)
#' 
#' @import shiny
#' @import iterators
#' 
#' @export
start_auction <- function(teams, randomize=TRUE){
  
  if (randomize) {
    teams <- randomize_teams(teams)
  }
  
  i <- 1
  
  teams.out <- NULL
  runApp(list(
    # Define UI for application that draws a histogram
    ui = fluidPage(
      
      # Application title
      titlePanel("NCAA Calcutta Competition"),
      
      # Sidebar with a slider input for number of bins 
      wellPanel(
        fluidRow(
          column(4,
                 textInput("owner","Bid Winner:"),
                 textInput("cost","Cost:"),
                 actionButton("submit","Submit")),
          column(4,
                 h2(htmlOutput("team")),
                 h3(textOutput("minbid")),
                 h4(textOutput("rank")),
                 h4(textOutput("region")),
                 h4(textOutput("opponent"))),
          column(4,
                 h2(htmlOutput("timer")),
                 actionButton("nextteam","Next Team"),
                 actionButton("lastteam","Back")
          )
        )
      ),
      
      # Show a plot of the generated distribution
      fluidRow(
        downloadButton('download',"Download the data"),
        dataTableOutput("table")
      )
    )
    ,
    # Define server logic required to draw a histogram
    server = function(input, output, session) {
      
      # curr.row <- reactive({
      #   input$nextteam
      #   
      #   nextElem(iteams)
      # })
      
      observe({
        input$nextteam
        i <<- i+1
      })
      
      observe({
        input$lastteam
        i <<- i-1
      })
      
      last.time <- reactive({
        input$nextteam
        input$lastteam
        # curr.row()
        
        secs <- ifelse(teams$rank[i]<=2,120,
                       ifelse(teams$rank[i]<=4,90,
                              ifelse(teams$rank[i]<=12,60,45)
                       )
        )
        
        eventTime <- Sys.time() + secs
      })
      
      output$team <- renderText({
        input$nextteam
        input$lastteam
        # tmp <- curr.row()
        paste0("Current Team: <font color=\"#FF0000\">",
               teams$team[i],
               "</font>"
               # as.character(tmp$team)
        )
      })
      
      output$minbid <- renderText({
        input$nextteam
        input$lastteam
        minbid <- ifelse(teams$rank[i]<=4,100,
                         ifelse(teams$rank[i]<=10,55,35)
        )
        
        paste0("Minimum Bid: ",minbid)
      })
      
      output$rank <- renderText({
        input$nextteam
        input$lastteam
        # tmp <- curr.row()
        paste0("Rank: ",
               teams$rank[i]
               # as.character(tmp$rank)
        )
      })
      
      output$region <- renderText({
        input$nextteam
        input$lastteam
        # tmp <- curr.row()
        paste0("Region: ",
               teams$region[i]
               # as.character(tmp$region)
        )
      })
      
      output$opponent <- renderText({
        input$nextteam
        input$lastteam
        # tmp <- curr.row()
        paste0("Opponent: ",
               teams$opponent[i]
               # as.character(tmp$region)
        )
      })
      
      output$timer <- renderText({
        eventTime <- last.time()
        invalidateLater(1000,session)
        paste0("Time Remaining: <font color=\"#0000FF\">", 
               round(difftime(eventTime, Sys.time(),units = 'secs')),
               '</font> seconds'
        )
      })
      
      
      thedata <- reactive({
        input$submit
        owner <- isolate(input$owner)
        cost <- isolate(input$cost)
        
        tmp.dat <- data.frame(teams[i,])
        tmp.dat$owner <- owner
        tmp.dat$bid <- cost
        teams.out <<- rbind(teams.out,tmp.dat)
      })
      
      output$table <- renderDataTable({
        thedata()
      },options = list(searching=F, paging = F, sort = F))
      
      output$download <- downloadHandler(
        filename = function(){"teams-out.csv"},
        content = function(fname){
          write.csv(thedata(), fname)
        }
      )
    }),
    launch.browser = T
  )
  
  
}