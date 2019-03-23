#' @export
results_app <- function(teams, starting_points, randomize=TRUE){
  # starting_points <- 1175
  require('shiny')
  require('shinydashboard')
  require('ggplot2')
  
  scores <- NCAAcalcutta::get_tournament_scores()
  
  t1_scores <- scores %>% 
    select(round, region, rank = team1_seed, team_id = team1_id, score = team1_score) %>% 
    left_join(teams, by=c("rank","region"))
  
  t2_scores <- scores %>% 
    select(round, region, rank = team2_seed, team_id = team2_id, score = team2_score) %>% 
    left_join(teams, by=c("rank","region"))
  
  append_scores <- t1_scores %>% bind_rows(t2_scores)  
  
  shinyApp(
    # Define UI for application that draws a histogram
    ui = dashboardPage(
      dashboardHeader(title = 'NCAA Calcutta'),
      dashboardSidebar(
        sidebarMenu(
          menuItem("Standings", tabName = "standings", icon = icon("dashboard")),
          menuItem("Graphs", tabName = "graphs", icon = icon("th")),
          menuItem("Scores", tabName = "scores", icon = icon("th")),
          menuItem("Owners", tabName = "owners", icon = icon("th"))
        )
      ),
      dashboardBody(
        tabItems(
          # Standings tab content
          tabItem(tabName = "standings",
                  fluidRow(
                    DT::dataTableOutput("ranking")
                  )
          ),
          
          # Graphs tab content
          tabItem(tabName = "graphs",
                  fluidRow(
                    plotOutput("total_points", height = 800)
                  )
          ),
          
          # Scores tab content
          tabItem(tabName = "scores",
                  h2("Widgets tab content")
          ),
          
          # Owners tab content
          tabItem(tabName = "owners",
                  h2("Widgets tab content")
          )
        )
      )
    ),
    server = function(input, output, session) {
      
      output$ranking <- DT::renderDataTable({
        total_points <- append_scores %>% 
          filter(round==1) %>% 
          group_by(owner) %>% 
          summarise(spent = sum(bid),
                    left_over = starting_points - sum(bid),
                    points_scored = sum(score)) %>% 
          mutate(left_over = if_else(left_over < 0, 10*left_over, 1*left_over),
                 final_score = left_over + points_scored,
                 rank = rank(-final_score, ties.method = 'min'))
        
        return(total_points %>% t)
      })
      
      output$total_points <- renderPlot({
        
        # Get remaining points
        left_over <- append_scores %>% 
          filter(round==1) %>% 
          group_by(owner) %>% 
          summarise(total_points = starting_points - sum(bid)) %>% 
          mutate(type = 'Left Over', total_points = if_else(total_points < 0, 10*total_points, 1*total_points))
        
        points_scored <- append_scores %>% 
          filter(!is.na(score)) %>% 
          group_by(owner) %>% 
          summarise(total_points = sum(score)) %>% 
          mutate(type = 'Points Scored') 
        

        combined_points <- left_over %>% 
          bind_rows(points_scored)
        
        ggplot(combined_points, aes(x=owner, y=total_points, fill=type)) + geom_bar(stat="identity")
      })
    }
  )
}