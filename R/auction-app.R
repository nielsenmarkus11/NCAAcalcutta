#' @title Start the NCAA Calcutta Shiny App
#' 
#' @description This Shiny App will help facilitate the auction
#' 
#' @param teams This should be a dataframe with rank, region, team, and opponent
#' @param players This should be a character vector of all those participating
#' @param points This is the total number of points each player starts with
#' @param randomize  (Default: TRUE) This determines whether you'd like to randomize the order of which teams are auctioned
#' By default this is a block randomization so that you will get a 1-16 seed before you see the next group of 1-16 seeds
#' 
#' @examples 
#' \dontrun{
#' # Input the 2018 teams
#' teams <- import_teams(system.file("extdata", "ncaa-teams.csv", package = "NCAAcalcutta"))
#' players <- c('Mark','Markus','Marko','Marky')
#' points <- 1200
#' start_auction(teams, players, points, randomize=TRUE)
#' }
#' 
#' @import shiny
#' @import iterators
#' @importFrom shinyalert shinyalert
#' 
#' @export
start_auction <- function(teams, players, points, randomize=TRUE){
  
  if (randomize) {
    teams <- randomize_teams(teams)
  }
  
  i <- 1
  
  teams.out <- NULL
  
  player_points <- data.frame(player=players, points=points)
  
  runApp(list(
    # Define UI for application that draws a histogram
    ui = fluidPage(
      
      # Application title
      titlePanel("NCAA Calcutta Competition"),
      
      # Sidebar with a slider input for number of bins 
      wellPanel(
        fluidRow(
          column(4,
                 selectizeInput("owner","Bid Winner:", choices = players),
                 textInput("cost","Cost:"),
                 actionButton("submit","Submit")),
          column(4,
                 h2(htmlOutput("team")),
                 h3(textOutput("minbid")),
                 h4(textOutput("rank")),
                 h4(textOutput("region")),
                 h4(htmlOutput("opponent"))),
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
        paste0("<img src=\"", teams$logo[i], "\"  width=\"100\" height=\"100\"> <font color=\"#FF0000\">",
               teams$team[i],
               "</font>"
               # as.character(tmp$team)
        )
      })
      
      output$minbid <- renderText({
        input$nextteam
        input$lastteam
        minbid <- ifelse(teams$rank[i]<=4,90,
                         ifelse(teams$rank[i]<=10,45,25)
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
        paste0("Opponent: <img src=\"", teams$opponent_logo[i], "\"  width=\"50\" height=\"50\"> ",
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
        
        out_cols <- c("rank", "region", "team", "opponent")
        
        if(is.null(teams.out)){
          
          if (points - coalesce(as.numeric(cost),0) < 0 ){
            shinyalert("Oops!", paste0(owner," has less than ",cost," points left."), type = "error")
          } else {
            tmp.dat <- data.frame(teams[i, out_cols])
            tmp.dat$owner <- owner
            tmp.dat$bid <- cost
            teams.out <<- rbind(teams.out,tmp.dat[FALSE,])
          }
        } else {
          if (points - sum(as.numeric(teams.out[teams.out$owner==owner,"bid"]), na.rm = TRUE) - coalesce(as.numeric(cost),0) < 0 ){
            shinyalert::shinyalert("Oops!", paste0(owner," has less than ",cost," points left."), type = "error")
          } else {
            tmp.dat <- data.frame(teams[i, out_cols])
            tmp.dat$owner <- owner
            tmp.dat$bid <- cost
            teams.out <<- rbind(tmp.dat,teams.out)
          }
        }
        
        if(is.null(teams.out)){
          return(data.frame(rank='',
                            region='',
                            team='',
                            opponent='',
                            owner='',
                            bid=''))
        } else {
          return(teams.out)
        }
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
