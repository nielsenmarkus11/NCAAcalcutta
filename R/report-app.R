#' @export
results_app <- function(teams, starting_points, randomize=TRUE){
  # starting_points <- 1175
  
  scores <- NCAAcalcutta::get_tournament_scores()
  
  t1_scores <- scores %>% 
    select(round, region, rank = team1_seed, team_id = team1_id, score = team1_score) %>% 
    left_join(teams, by=c("rank","region"))
  
  t2_scores <- scores %>% 
    select(round, region, rank = team2_seed, team_id = team2_id, score = team2_score) %>% 
    left_join(teams, by=c("rank","region"))
  
  append_scores <- t1_scores %>% bind_rows(t2_scores)  
  scores_completed <- append_scores %>% 
    group_by(round) %>% 
    summarize(cnt = n(), scores=sum(!is.na(score)))
  
  current_round <- min(scores_completed$round[scores_completed$scores<scores_completed$cnt])
  
  eliminated <- append_scores %>% 
    filter(!is.na(team)) %>% 
    group_by(team) %>% 
    summarise(eliminated = if_else(max(round) < current_round, TRUE, FALSE),
              round_exited = if_else(max(round) < current_round, max(round), as.numeric(NA)))
  
  final_data <- append_scores %>% left_join(eliminated, by = c("team"))
  
  
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
                  box(title = 'Current Points', status = "primary", solidHeader = TRUE,
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
        total_points <- final_data %>% 
          filter(round==1) %>% 
          group_by(owner) %>% 
          summarise(spent = sum(bid),
                    left_over = starting_points - sum(bid),
                    points_scored = sum(score),
                    teams_purchased = n(),
                    teams_remaining = n() - sum(eliminated, na.rm=TRUE)) %>% 
          mutate(left_over = if_else(left_over < 0, 10*left_over, 1*left_over),
                 final_score = left_over + points_scored,
                 rank = rank(-final_score, ties.method = 'min')) %>% data.frame
        
        row.names(total_points) = total_points$owner
        total_points$owner <- NULL
        
        return(total_points %>% t)
      })
      
      output$total_points <- renderPlot({
        
        # Get remaining points
        left_over <- final_data %>% 
          filter(round==1) %>% 
          group_by(owner) %>% 
          summarise(total_points = starting_points - sum(bid)) %>% 
          mutate(type = 'Left Over', total_points = if_else(total_points < 0, 10*total_points, 1*total_points))
        
        points_scored <- final_data %>% 
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
