results_app <- function(teams, randomize=TRUE){
  
  require('shiny')
  require('shinydashboard')
  require('ggplot2')
  
  scores <- NCAAcalcutta::get_tournament_scores()
  
  t1_scores <- scores %>% 
    select(round, region, rank = team1_seed, team_id = team1_id, score = team1_score) %>% 
    left_join(teams, by=c("rank","region")) %>% 
    select(-bid)
  
  t2_scores <- scores %>% 
    select(round, region, rank = team2_seed, team_id = team2_id, score = team2_score) %>% 
    left_join(teams, by=c("rank","region")) %>% 
    select(-bid)
  
  append_scores <- t1_scores %>% bind_rows(t2_scores)  
  
  runApp(list(
    # Define UI for application that draws a histogram
    ui = dashboardPage(
      dashboardHeader(title = 'NCAA Calcutta'),
      dashboardSidebar(
        sidebarMenu(
          menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
          menuItem("Widgets", tabName = "widgets", icon = icon("th"))
        )
      ),
      dashboardBody(
        tabItems(
          # First tab content
          tabItem(tabName = "dashboard",
                  fluidRow(
                    plotOutput("total_points", height = 800)
                  )
          ),
          
          # Second tab content
          tabItem(tabName = "widgets",
                  h2("Widgets tab content")
          )
        )
      )
    ),
    server = function(input, output, session) {
      
      output$total_points <- renderPlot({
        append_scores %>% 
          filter(!is.na(score)) %>% 
          group_by(owner) %>% 
          summarise(total_points = sum(score)) %>% 
          ggplot(aes(x=owner, y=total_points, fill=owner)) + geom_bar(stat="identity")
      })
    }
  ))
}