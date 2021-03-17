#' @param auction_results 
#' @param starting_points Starting points given to each member for the
#'
#' @importFrom DT datatable JS formatStyle styleEqual dataTableOutput renderDataTable
#' @import ggplot2
#' @import plotly
#' @import shinydashboard
#' @import shiny
#' @import dplyr
#'
#' @export
results_app <- function(auction_results, starting_points, year=NULL){
  # starting_points <- 1175
  
  shinyApp(
    # Define UI for application that draws a histogram
    ui = dashboardPage(
      dashboardHeader(title = 'NCAA Calcutta'),
      dashboardSidebar(tags$head(tags$style(HTML("tbody > tr:last-child { 
                                               font-weight: bold; 
                                             }"))),
                       sidebarMenu(
                         menuItem("Standings", tabName = "standings", icon = icon("trophy")),
                         menuItem("Graphs", tabName = "graphs", icon = icon("chart-bar")),
                         menuItem("Teams by Owner", tabName = "owners", icon = icon("basketball-ball")),
                         menuItem("Bracket", tabName = "bracket", icon = icon("sitemap"))
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
                  fluidRow(
                    box(title = 'Current Points', status = "primary", solidHeader = TRUE,
                        plotlyOutput("total_points"),
                        width = 12
                    ),
                    box(title = 'Points Paid by Rank', status = "primary", solidHeader = TRUE,
                        plotlyOutput("paid_by_seed"),
                        width = 12
                    ),
                    box(title = 'Points Realized by Team', status = "primary", solidHeader = TRUE,
                        plotlyOutput("realized_return"),
                        width = 12
                    )
                  )
          ),
          
          # Owners tab content
          tabItem(tabName = "owners",
                  fluidRow(
                    column(12,
                           box(
                             title = "Inputs", status = "warning", solidHeader = TRUE,
                             uiOutput("select_owner"),
                             uiOutput("owner_remaining"),
                             uiOutput("owner_total"),
                             uiOutput("owner_rank"),
                             width = 12
                           )
                    )
                  ),
                  fluidRow(
                    column(12,
                           box(title = 'Teams by Owner', status = "primary", solidHeader = TRUE,
                               DT::dataTableOutput("teams_points"),
                               width = 12
                           )
                    )
                  )
          ),
          tabItem(tabName = "bracket",
                  fluidRow(
                    column(12,
                           plotlyOutput('bracket')
                           )
                  )
          )
        )
      )
    ),
    server = function(input, output, session) {
      
      scores <-  reactive({
        input$goButton
        scores <- NCAAcalcutta::get_tournament_scores(year=year) %>% 
          mutate(team1_elim = if_else(team2_win=='W', TRUE, FALSE, FALSE),
                 team2_elim = if_else(team1_win=='W', TRUE, FALSE, FALSE))
        return(scores)
      })
      
      final_data <- reactive({
        scores <- scores()
        
        scores_id1 <- scores %>% 
          filter(round==1) %>% 
          select(region, rank=team1_seed, team_id=team1_id)
        
        scores_id2 <- scores %>% 
          filter(round==1) %>% 
          select(region, rank=team2_seed, team_id=team2_id)
        
        scores_id <- scores_id1 %>% bind_rows(scores_id2)
        
        
        teams_with_id <- auction_results %>% 
          left_join(scores_id, by=c("rank","region")) %>% 
          select(-rank, -region)
        
        t1_scores <- scores %>% 
          select(round, region, rank = team1_seed, team_id = team1_id, score = team1_score, eliminated = team1_elim, win=team1_win) %>% 
          left_join(teams_with_id, by=c("team_id"))
        
        t2_scores <- scores %>% 
          select(round, region, rank = team2_seed, team_id = team2_id, score = team2_score, eliminated = team2_elim, win=team2_win) %>% 
          left_join(teams_with_id, by=c("team_id"))
        
        
        append_scores <- t1_scores %>% bind_rows(t2_scores)  
        
        
        winning_team <- append_scores %>% 
          filter(round==6, win=='W') %>% 
          mutate(round=7, score=100, eliminated=TRUE)
        
        final_data <- append_scores %>% 
          bind_rows(winning_team) %>% 
          mutate(round_exited = if_else(eliminated, round, as.numeric(NA)))
        return(final_data)
      })
      
      total_points <- reactive({
        final_data <- final_data()
        
        total_points <- final_data %>% 
          group_by(rank, team_id, team, owner, bid) %>% 
          summarise(score = sum(score, na.rm = TRUE), eliminated = !all(!eliminated)) %>% 
          mutate(realized_loss_or_gain = if_else(eliminated, score - bid, as.numeric(NA)))
        return(total_points)
      })
      
      output$select_owner <- renderUI({
        total_points <- total_points()
        
        selectizeInput('team_owner', "Select Owner:", unique(total_points$owner))
      })
      
      player_points <- reactive({
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
        return(player_points)
      })
      
      output$ranking <- DT::renderDataTable({
        player_points <- player_points()
        
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
      
      output$total_points <- renderPlotly({
        
        final_data <- final_data()
        
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
        
        g <- ggplot(combined_points, aes(x=owner, y=total_points, fill=type)) + 
          geom_bar(stat="identity") +
          theme(legend.position = "bottom") +
          labs(x = "", y = "Total Points", fill = "")
        ggplotly(g)
      })
      
      
      output$paid_by_seed <- renderPlotly({
        total_points <- total_points()
        
        paid_by_seed <- total_points %>% 
          group_by(rank) %>% 
          summarise(bid = mean(bid))
        
        g <- ggplot() +
          geom_line(aes(x=rank, y=bid), data = paid_by_seed, color = "black", size = 2) +
          geom_point(aes(x=rank, y=bid), data = total_points, color="blue", size = 2) +
          labs(x = "Rank", y = "Bid")
        ggplotly(g)
        
      })
      
      output$realized_return <- renderPlotly({
        total_points <- total_points()
        
        bids <- total_points %>% 
          mutate(bid = -bid, type="Bid") %>% 
          select(rank, team, points=bid, type)
        
        realized_return <- total_points %>% 
          mutate(type = "Points Scored") %>% 
          select(rank, team, points=score, type)
        
        combo <- bids %>% bind_rows(realized_return)
        
        g <- ggplot(combo) +
          geom_bar(aes(x=reorder(abbreviate(team, 12), rank), y=points, fill = type), stat = "identity", show.legend = F) +
          geom_line(aes(x=reorder(abbreviate(team, 12), rank), y=realized_loss_or_gain, group = 1), data = total_points, size = 1) +
          theme(axis.text.x=element_text(angle=90,hjust=1,vjust = 0.5)) + 
          facet_grid(.~rank, scales = "free_x") +
          labs(x = "", y = "Points")
        ggplotly(g)
        
      })
      
      output$owner_remaining <- renderUI({
        # team_owner = "Mark"
        team_owner <- input$team_owner
        player_points <- isolate(player_points())
        
        HTML(paste0('<b>Points Remaining: ',player_points[team_owner,"left_over"],'</b>'))
        
      })
      
      output$owner_total <- renderUI({
        # team_owner = "Mark"
        team_owner <- input$team_owner
        player_points <- isolate(player_points())
        
        HTML(paste0('<h4><b>Total Score: ',player_points[team_owner,"final_score"],'</b></h4>'))
        
      })
      
      output$owner_rank <- renderUI({
        # team_owner = "Mark"
        team_owner <- input$team_owner
        player_points <- isolate(player_points())
        
        HTML(paste0('<h4><b>Rank: ',player_points[team_owner,"rank"],'</b></h4>'))
        
      })
      
      output$teams_points <- DT::renderDataTable({
        # team_owner = "Mark"
        team_owner <- input$team_owner
        total_points <- total_points()
        
        owner_teams <- total_points %>% 
          ungroup() %>% 
          filter(owner == team_owner) %>% 
          select(rank, team, points_paid=bid, points_earned=score, realized_loss_or_gain) %>% 
          arrange(rank, points_paid)  %>%
          janitor::adorn_totals("row") %>% 
          data.frame
        
        names(owner_teams) <- names(owner_teams) %>% 
          stringr::str_replace_all('\\_',' ') %>%
          stringr::str_to_title()
        
        datatable(owner_teams, options = list(
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
            "}"), 
          searching = FALSE,
          ordering = FALSE,
          paging = FALSE,
          info = FALSE
        ),
        rownames= FALSE) %>%
          formatStyle(
            0,
            target = "row",
            fontWeight = styleEqual(1, "bold")
          )
      })
      
      output$bracket <- renderPlotly({
        final_data <- final_data()
        
        bracket_prep <- final_data %>% 
          mutate(old_row = row_number())
        
        translate = read.csv(system.file("extdata", "match.csv", package = "NCAAcalcutta"))
        
        bracket_teams <- bracket_prep %>% 
          inner_join(translate) %>% 
          arrange(new_row)
        
        # bracket_prep$region <- ordered(bracket_prep$region,levels = c('Midwest','South','East','West','Final Four A', 'Final Four'))
        # 
        # bracket_teams <- bracket_prep %>% 
        #   mutate(final=ifelse(round>=6,1,0),
        #          final_four=ifelse(round>=5,1,0),
        #          region_grp = ifelse(region %in% c('Midwest','South','Final Four A'),0,1)) %>% 
        #   arrange(final, region_grp, final_four, round, region, -game, seed) %>% 
        #   mutate(team = row_number())
        
        bracket = tournament_bracket(64)
        bracket$plot %>% add_annotations(data=bracket$data, x=~x, y=~y+0.5, text = ~coalesce(bracket_teams$team,''),
                                         xanchor = 'left',
                                         showarrow = F)
      })
    }
  )
}
