#' @param auction_results 
#' @param starting_points Starting points given to each member for the
#'
#' @importFrom DT datatable JS formatStyle styleEqual dataTableOutput renderDataTable
#' @import ggplot2
#' @import plotly
#' @import shinydashboard
#' @import shiny
#' @import dplyr
#' @importFrom tidyr pivot_wider
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
                       uiOutput("select_owner"),
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
                  ),
                  fluidRow(
                    column(12,
                           box(title = 'Predicted Ranking', status = "primary", solidHeader = TRUE,
                               DT::dataTableOutput("pred_ranking"),
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
                             title = "Owner Summary", status = "warning", solidHeader = TRUE,
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
                           plotlyOutput('bracket', height = '800px')
                           )
                  )
          )
        )
      )
    ),
    server = function(input, output, session) {
      
      scores <-  reactive({
        input$goButton
        scores <- NCAAcalcutta::get_tournament_scores_api(year=year) %>% 
          mutate(team1_elim = if_else(team2_win=='W', TRUE, FALSE, FALSE),
                 team2_elim = if_else(team1_win=='W', TRUE, FALSE, FALSE),
                 game = row_number(),
                 team2_seed = ifelse(is.na(team2_seed),17-team1_seed, team2_seed))
        return(scores)
      })
      
      pred_scores <- reactive({
        scores <- scores()
        
        scores <- scores %>% 
          select(-team1_elim,-team2_elim,-game)
        
        game_ids <- data.frame(game1_id = rep(1:32, each=1),
                               game2_id = rep(1:16, each=2),
                               game3_id = rep(1:8, each=4),
                               game4_id = rep(1:4, each=8),
                               game5_id = rep(1:2, each=16),
                               game6_id = rep(1, each=32))
        
        new_dat <- read.csv(system.file("extdata", "pred-scores.csv", package = "NCAAcalcutta"))
        
        round1 <- scores %>% 
          filter(round==1) %>% 
          bind_cols(game_ids) %>% 
          left_join(new_dat %>% rename(team1_pred_score=pred_score, team1_prob_win=pred_win),
                    by = c("round","region","team1_seed"="team_seed","team2_seed")) %>% 
          left_join(new_dat %>% rename(team2_pred_score=pred_score, team2_prob_win=pred_win),
                    by = c("round","region","team1_seed"="team2_seed","team2_seed"="team_seed")) %>% 
          mutate(team1_pred_win = ifelse(team1_pred_score>team2_pred_score, 'W', NA),
                 team2_pred_win = ifelse(team1_pred_score<team2_pred_score, 'W', NA))
        
        winners_round1 <- round1 %>% 
          filter(!is.na(team1_win) | (is.na(team1_win) & is.na(team2_win) & !is.na(team1_pred_win))) %>% 
          select(game1_id, game2_id, game3_id, game4_id, game5_id, game6_id,
                 seed=team1_seed,
                 id=team1_id,
                 round,
                 region) %>% 
          bind_rows(round1 %>% 
                      filter(!is.na(team2_win) | (is.na(team1_win) & is.na(team2_win) & !is.na(team2_pred_win))) %>% 
                      select(game1_id, game2_id, game3_id, game4_id, game5_id, game6_id,
                             seed=team2_seed,
                             id=team2_id,
                             round,
                             region) ) %>% 
          arrange(game1_id, game2_id) %>% 
          select(-game1_id) %>% 
          arrange(game2_id, seed) %>% 
          mutate(name = paste0('team', rep(1:2,16))) %>% 
          pivot_wider(names_from = name, values_from = c(seed, id), names_glue = "{name}_{.value}") %>% 
          left_join(new_dat %>% rename(team1_pred_score=pred_score, team1_prob_win=pred_win),
                    by = c("round","region","team1_seed"="team_seed","team2_seed")) %>% 
          left_join(new_dat %>% rename(team2_pred_score=pred_score, team2_prob_win=pred_win),
                    by = c("round","region","team1_seed"="team2_seed","team2_seed"="team_seed")) %>% 
          mutate(team1_pred_win = ifelse(team1_pred_score>team2_pred_score, 'W', NA),
                 team2_pred_win = ifelse(team1_pred_score<team2_pred_score, 'W', NA))
        
        round2 <- winners_round1 %>%
          select(-round) %>% 
          left_join(scores %>% 
                      filter(round==2)) %>% 
          mutate(round=2)
        
        winners_round2 <- round2 %>% 
          filter(!is.na(team1_win) | (is.na(team1_win) & is.na(team2_win) & !is.na(team1_pred_win))) %>% 
          select(game2_id, game3_id, game4_id, game5_id, game6_id,
                 seed=team1_seed,
                 id=team1_id,
                 round,
                 region) %>% 
          bind_rows(round2 %>% 
                      filter(!is.na(team2_win) | (is.na(team1_win) & is.na(team2_win) & !is.na(team2_pred_win))) %>% 
                      select(game2_id, game3_id, game4_id, game5_id, game6_id,
                             seed=team2_seed,
                             id=team2_id,
                             round,
                             region)) %>% 
          arrange(game2_id, game3_id) %>% 
          select(-game2_id) %>% 
          arrange(game3_id, seed) %>% 
          mutate(name = paste0('team', rep(1:2,8))) %>% 
          pivot_wider(names_from = name, values_from = c(seed, id), names_glue = "{name}_{.value}") %>% 
          left_join(new_dat %>% rename(team1_pred_score=pred_score, team1_prob_win=pred_win),
                    by = c("round","region","team1_seed"="team_seed","team2_seed")) %>% 
          left_join(new_dat %>% rename(team2_pred_score=pred_score, team2_prob_win=pred_win),
                    by = c("round","region","team1_seed"="team2_seed","team2_seed"="team_seed")) %>% 
          mutate(team1_pred_win = ifelse(team1_pred_score>team2_pred_score, 'W', NA),
                 team2_pred_win = ifelse(team1_pred_score<team2_pred_score, 'W', NA))
        
        round3 <- winners_round2 %>% 
          select(-round) %>% 
          left_join(scores %>% 
                      filter(round==3)) %>% 
          mutate(round=3)
        
        winners_round3 <- round3 %>% 
          filter(!is.na(team1_win) | (is.na(team1_win) & is.na(team2_win) & !is.na(team1_pred_win))) %>% 
          select(game3_id, game4_id, game5_id, game6_id,
                 seed=team1_seed,
                 id=team1_id,
                 round,
                 region) %>% 
          bind_rows(round3 %>% 
                      filter(!is.na(team2_win) | (is.na(team1_win) & is.na(team2_win) & !is.na(team2_pred_win))) %>% 
                      select(game3_id, game4_id, game5_id, game6_id,
                             seed=team2_seed,
                             id=team2_id,
                             round,
                             region)) %>% 
          arrange(game3_id, game4_id) %>% 
          select(-game3_id) %>% 
          arrange(game4_id, seed) %>% 
          mutate(name = paste0('team', rep(1:2,4))) %>% 
          pivot_wider(names_from = name, values_from = c(seed, id), names_glue = "{name}_{.value}") %>% 
          left_join(new_dat %>% rename(team1_pred_score=pred_score, team1_prob_win=pred_win),
                    by = c("round","region","team1_seed"="team_seed","team2_seed")) %>% 
          left_join(new_dat %>% rename(team2_pred_score=pred_score, team2_prob_win=pred_win),
                    by = c("round","region","team1_seed"="team2_seed","team2_seed"="team_seed")) %>% 
          mutate(team1_pred_win = ifelse(team1_pred_score>team2_pred_score, 'W', NA),
                 team2_pred_win = ifelse(team1_pred_score<team2_pred_score, 'W', NA))
        
        round4 <- winners_round3 %>%
          select(-round) %>%  
          left_join(scores %>% 
                      filter(round==4)) %>% 
          mutate(round=4)
        
        winners_round4 <- round4 %>% 
          filter(!is.na(team1_win) | (is.na(team1_win) & is.na(team2_win) & !is.na(team1_pred_win))) %>% 
          select(game4_id, game5_id, game6_id,
                 seed=team1_seed,
                 id=team1_id,
                 round) %>% 
          mutate(region="Final Four") %>% 
          bind_rows(round4 %>% 
                      filter(!is.na(team2_win) | (is.na(team1_win) & is.na(team2_win) & !is.na(team2_pred_win))) %>% 
                      select(game4_id, game5_id, game6_id,
                             seed=team2_seed,
                             id=team2_id,
                             round) %>% 
                      mutate(region="Final Four")) %>% 
          arrange(game4_id, game5_id) %>% 
          select(-game4_id) %>% 
          arrange(game5_id, seed) %>% 
          mutate(name = paste0('team', rep(1:2,2))) %>% 
          pivot_wider(names_from = name, values_from = c(seed, id), names_glue = "{name}_{.value}") %>% 
          left_join(new_dat %>% rename(team1_pred_score=pred_score, team1_prob_win=pred_win),
                    by = c("round","region","team1_seed"="team_seed","team2_seed")) %>% 
          left_join(new_dat %>% rename(team2_pred_score=pred_score, team2_prob_win=pred_win),
                    by = c("round","region","team1_seed"="team2_seed","team2_seed"="team_seed")) %>% 
          mutate(team1_pred_win = ifelse(team1_pred_score>team2_pred_score, 'W', NA),
                 team2_pred_win = ifelse(team1_pred_score<team2_pred_score, 'W', NA),
                 team1_pred_score = 1.75*team1_pred_score,
                 team2_pred_score = 1.75*team2_pred_score)
        
        round5 <- winners_round4 %>% 
          select(-round) %>% 
          left_join(scores %>% 
                      filter(round==5)) %>% 
          mutate(round=5)
        
        pred_scores <- bind_rows(round1 %>% select(-starts_with('game')),
                                            round2 %>% select(-starts_with('game')),
                                            round3 %>% select(-starts_with('game')),
                                            round4 %>% select(-starts_with('game')),
                                            round5 %>% select(-starts_with('game'))) %>% 
          mutate(team1_score = ifelse(coalesce(team1_score,0)==0, team1_pred_score, team1_score),
                 team2_score = ifelse(coalesce(team2_score,0)==0, team2_pred_score, team2_score),
                 team1_win = ifelse(is.na(team1_win) & is.na(team2_win), team1_pred_win, as.character(team1_win)),
                 team2_win = ifelse(is.na(team1_win) & is.na(team2_win), team2_pred_win, as.character(team2_win)))
        
        return(pred_scores)
      })
      
      final_data <- reactive({
        scores <- scores()
        
        scores_id1 <- scores %>% 
          filter(round==1) %>% 
          select(region, rank=team1_seed, team_id=team1_id)
        
        scores_id2 <- scores %>% 
          filter(round==1) %>% 
          select(region, rank=team2_seed, team_id=team2_id)
        
        scores_id <- scores_id1 %>% bind_rows(scores_id2) %>% filter(team_id!=-2)
        
        
        teams_with_id <- auction_results %>% 
          left_join(scores_id, by=c("rank","region")) %>% 
          select(-rank, -region)
        
        t1_scores <- scores %>% 
          select(round, region, game, rank = team1_seed, team_id = team1_id, score = team1_score, eliminated = team1_elim, win=team1_win) %>% 
          left_join(teams_with_id %>% filter(complete.cases(.)), by=c("team_id"))
        
        t2_scores <- scores %>% 
          select(round, region, game, rank = team2_seed, team_id = team2_id, score = team2_score, eliminated = team2_elim, win=team2_win) %>% 
          left_join(teams_with_id %>% filter(complete.cases(.)), by=c("team_id"))
        
        
        append_scores <- t1_scores %>% bind_rows(t2_scores)  
        
        
        winning_team <- append_scores %>% 
          filter(round==6, win=='W') %>% 
          mutate(game=64, round=7, score=100, eliminated=TRUE)
        
        final_data <- append_scores %>% 
          bind_rows(winning_team) %>% 
          mutate(round_exited = if_else(eliminated, round, as.numeric(NA)))
        return(final_data)
      })
      
      pred_final_data <- reactive({
        pred_scores <- pred_scores()
        
        scores_id1 <- pred_scores %>% 
          filter(round==1) %>% 
          select(region, rank=team1_seed, team_id=team1_id)
        
        scores_id2 <- pred_scores %>% 
          filter(round==1) %>% 
          select(region, rank=team2_seed, team_id=team2_id)
        
        scores_id <- scores_id1 %>% bind_rows(scores_id2) %>% filter(team_id!=-2)
        
        
        teams_with_id <- auction_results %>% 
          left_join(scores_id, by=c("rank","region")) %>% 
          select(-rank, -region)
        
        t1_scores <- pred_scores %>% 
          select(round, region, rank = team1_seed, team_id = team1_id, score = team1_score, win=team1_win) %>% 
          left_join(teams_with_id %>% filter(complete.cases(.)), by=c("team_id"))
        
        t2_scores <- pred_scores %>% 
          select(round, region, rank = team2_seed, team_id = team2_id, score = team2_score, win=team2_win) %>% 
          left_join(teams_with_id %>% filter(complete.cases(.)), by=c("team_id"))
        
        pred_final_data <- t1_scores %>% bind_rows(t2_scores)  
        
        return(pred_final_data)
      })
      
      total_points <- reactive({
        final_data <- final_data()
        
        total_points <- final_data %>% 
          group_by(rank, team_id, team, owner, bid) %>% 
          summarise(score = sum(score, na.rm = TRUE), eliminated = !all(!eliminated)) %>% 
          mutate(realized_loss_or_gain = if_else(eliminated, score - bid, as.numeric(NA))) %>% 
          filter(rank!=99)
        return(total_points)
      })
      
      pred_total_points <- reactive({
        pred_final_data <- pred_final_data()
        
        pred_total_points <- pred_final_data %>% 
          group_by(rank, team_id, team, owner, bid) %>% 
          summarise(score = sum(score, na.rm = TRUE))
        return(pred_total_points)
      })
      
      output$select_owner <- renderUI({
        total_points <- total_points()
        
        selectizeInput('team_owner', "Select Owner:", sort(unique(total_points$owner)))
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
      
      pred_player_points <- reactive({
        pred_total_points <- pred_total_points()
        
        pred_player_points <- pred_total_points %>% 
          filter(!is.na(owner)) %>% 
          group_by(owner) %>% 
          summarise(spent = sum(bid),
                    left_over = starting_points - sum(bid),
                    predicted_points = sum(score)) %>% 
          mutate(left_over = if_else(left_over < 0, 10*left_over, 1*left_over),
                 predicted_final_score = left_over + predicted_points,
                 predicted_rank = rank(-predicted_final_score, ties.method = 'min')) %>% data.frame
        
        row.names(pred_player_points) = pred_player_points$owner
        pred_player_points$owner <- NULL
        return(pred_player_points)
      })
      
      
      output$ranking <- DT::renderDataTable({
        player_points <- player_points()
        
        new_tp <- player_points %>% t
        row.names(new_tp) <- row.names(new_tp) %>% 
          stringr::str_replace_all('\\_',' ') %>%
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
      
      
      output$pred_ranking <- DT::renderDataTable({
        pred_player_points <- pred_player_points()
        
        new_tp <- pred_player_points %>% 
          select(predicted_final_score, 
                 predicted_rank) %>% t
        row.names(new_tp) <- row.names(new_tp) %>% 
          stringr::str_replace_all('\\_',' ') %>%
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
          summarise(bid = mean(bid, na.rm=T))
        
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
        team_owner <- input$team_owner
        final_data <- final_data()
        
        final_data$team_abbr <- unname(abbreviate(coalesce(final_data$team,''), 16))
        
        # Fill in missing data
        seed_order = c(1,16,8,9,5,12,4,13,6,11,3,14,7,10,2,15)
        regions = levels(final_data$region)
        round = 1:6
        fill_bracket <- expand.grid(rank=factor(seed_order, levels = seed_order),
                                    region=factor(regions, levels = regions),
                                    round=round)
        fill_bracket$group <- c(1:64,
                                rep(1:32, each=2),
                                rep(1:16, each=4),
                                rep(1:8, each=8),
                                rep(1:4, each=16),
                                rep(1:2, each=32))
        
        fill_wo_na <- fill_bracket %>% 
          inner_join(final_data %>% mutate(rank = factor(rank, levels=seed_order))) %>% 
          select(round, region, group)
        
        fill_w_na <- fill_bracket %>% 
          anti_join(fill_wo_na) %>% 
          mutate(region = factor(ifelse(round==6, ifelse(region %in% c("South","East"), "Final Four", "Fina Four A"),
                                 as.character(region)), levels = c(regions, "Final Four", "Final Four A"))) %>% 
          group_by(round, region, group) %>% 
          summarize(rank = factor(min(as.numeric(as.character(rank))), levels=seed_order)) %>% 
          select(-group)
                   
        bracket_prep <- final_data %>% 
          filter(rank!=99) %>% 
          mutate(rank = factor(rank, levels = seed_order)) %>% 
          bind_rows(fill_w_na) %>% 
          mutate(region = ifelse(coalesce(game,99)==ifelse(is.null(year),62,63),'Final Four A',region),
                 team_abbr = ifelse(owner==team_owner,
                                    paste0('<span style="color:red;"><b>',team_abbr,'</b></span>'),
                                    team_abbr)) %>% 
          arrange(round, region, rank) %>% 
          mutate(old_row = row_number())
        
        translate = read.csv(system.file("extdata", "match.csv", package = "NCAAcalcutta"))
        

        # bracket_prep$region <- ordered(bracket_prep$region,levels = c('Midwest','South','East','West','Final Four A', 'Final Four'))
        # 
        # bracket_teams <- bracket_prep %>%
        #   mutate(final=ifelse(round>=6,1,0),
        #          final_four=ifelse(round>=5,1,0),
        #          region_grp = ifelse(region %in% c('Midwest','South','Final Four A'),0,1)) %>%
        #   arrange(final, region_grp, final_four, round, region, -game, rank) %>%
        #   mutate(new_row = row_number())
        
        bracket = tournament_bracket(64)
        
        bracket_teams <- bracket_prep %>% 
          inner_join(translate, by='old_row') %>% 
          inner_join(bracket$data %>% rename(new_row=team), by='new_row')
        
        
        bracket$plot %>% add_annotations(data=bracket_teams,
                                         x=~x,
                                         y=~y+0.5,
                                         text = ~coalesce(team_abbr,''),
                                         xanchor = 'left',
                                         showarrow = F)
      })
    }
  )
}
