#' @export
results_app <- function(teams, starting_points, randomize=TRUE){
  # starting_points <- 1175
  
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
        ),
        actionButton("goButton", "Refresh Data")
      ),
      dashboardBody(
        tabItems(
          # Standings tab content
          tabItem(tabName = "standings",
                  fluidRow(
                    column(12,
                           box(title = 'Current Standings', status = "primary", solidHeader = TRUE,
                               DT::dataTableOutput("ranking"),
                               width = 12
                           )
                    )
                    
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
      
      scores <-  reactive({
        input$goButton
        scores <- NCAAcalcutta::get_tournament_scores() %>% 
          mutate(team1_elim = if_else(team2_win=='W', TRUE, FALSE, FALSE),
                 team2_elim = if_else(team1_win=='W', TRUE, FALSE, FALSE))
        return(scores)
      })
      
      final_data <- reactive({
        scores <- scores()
        t1_scores <- scores %>% 
          select(round, region, rank = team1_seed, team_id = team1_id, score = team1_score, eliminated = team1_elim) %>% 
          left_join(teams, by=c("rank","region"))
        
        t2_scores <- scores %>% 
          select(round, region, rank = team2_seed, team_id = team2_id, score = team2_score, eliminated = team2_elim) %>% 
          left_join(teams, by=c("rank","region"))
        
        append_scores <- t1_scores %>% bind_rows(t2_scores)  
        
        final_data <- append_scores %>% 
          mutate(round_exited = if_else(eliminated, round, as.numeric(NA)))
        return(final_data)
      })
      
      total_points <- reactive({
        final_data <- final_data()
        
        total_points <- final_data %>% 
          group_by(rank, region, team_id, team, owner, bid) %>% 
          summarise(score = sum(score, na.rm = TRUE), eliminated = !all(!eliminated)) %>% 
          mutate(realized_loss_or_gain = if_else(eliminated, score - bid, as.numeric(NA)))
        return(total_points)
      })
      
      output$ranking <- DT::renderDataTable({
        total_points <- total_points()
        
        player_points <- total_points %>% 
          filter(!is.na(owner)) %>% 
          group_by(owner) %>% 
          summarise(spent = sum(bid),
                    left_over = starting_points - sum(bid),
                    points_scored = sum(score),
                    teams_purchased = n(),
                    teams_remaining = n() - sum(eliminated, na.rm=TRUE)) %>% 
          mutate(left_over = if_else(left_over < 0, 10*left_over, 1*left_over),
                 final_score = left_over + points_scored,
                 rank = rank(-final_score, ties.method = 'min')) %>% data.frame
        
        row.names(player_points) = player_points$owner
        player_points$owner <- NULL
        
        new_tp <- player_points %>% t
        row.names(new_tp) <- row.names(new_tp) %>% 
          stringr::str_replace('\\_',' ') %>%
          stringr::str_to_title()
        
        
        datatable(new_tp, options = list(
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
            "}"), 
          searching = FALSE,
          ordering = FALSE,
          paging = FALSE,
          info = FALSE
        ))
      })
      
      output$total_points <- renderPlot({
        
        # Get remaining points
        left_over <- final_data() %>% 
          filter(round==1) %>% 
          group_by(owner) %>% 
          summarise(total_points = starting_points - sum(bid)) %>% 
          mutate(type = 'Left Over', total_points = if_else(total_points < 0, 10*total_points, 1*total_points))
        
        points_scored <- final_data() %>% 
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
