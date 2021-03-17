#' Draw bracket match
#' 
#' This function is a helper function which draws one match on a bracket
#'
#' @param x x coordinate 
#' @param y y coordinate
#' @param l length of line
#' @param w width between two teams lines
#'
#' @return returns a dataframe with coordinates to draw a match
#' @export
#'
#' @examples
bracket_draw_left <- function(x,y,l,w) {
  data.frame(x=c(x,x+l,x+l,x),y=c(y+w,y+w,y-w,y-w))
}

#' Draw tournament bracket
#'
#' @param n_teams Number of teams playing in the tournament (should be a multiple of 4)
#' @param l length of line
#' @param w width between two teams lines
#'
#' @return plotly object with drawn bracket
#' @export
#' 
#' @import plotly
#' @importFrom dplyr '%>%' filter mutate group_by ungroup
#'
#' @examples
tournament_bracket <- function(n_teams, w=0.5, l=5){
  
  cols=log(n_teams,2)-1
  
  pre_bracket_left <- NULL
  for (i in 0:(cols-1)){
    tmp <- cbind(i*l, 2*(2^(i)*1:(n_teams/4/(2^i))-(w*2^i)), l, w*2^i)
    pre_bracket_left <- rbind(pre_bracket_left,tmp)
  }
  
  
  bracket_left <- mapply(bracket_draw_left,
                         pre_bracket_left[,1],
                         pre_bracket_left[,2],
                         pre_bracket_left[,3],
                         pre_bracket_left[,4])
  all_brackets_left <- apply(bracket_left,2,data.frame)
  bracket_df_left <- do.call("rbind",all_brackets_left)
  
  #Right bracket
  bracket_df_right <- bracket_df_left
  
  max_left <- max(bracket_df_left$x)
  bracket_df_right$x <- max_left-1*(bracket_df_left$x-max_left)+3*l
  
  bracket_df <- rbind(bracket_df_right,bracket_df_left)
  bracket_df$grp <- rep(1:(dim(bracket_df)[1]/4),each=4)
  
  #Final
  min_left_final <- min(bracket_df_left[bracket_df_left$x==max_left,'y'])
  max_left_final <- max(bracket_df_left[bracket_df_left$x==max_left,'y'])
  max_grp = max(bracket_df$grp)
  diff <- max_left_final - min_left_final
  final <- data.frame(x=c(max_left, max_left+l, max_left+2*l, max_left+3*l),
                      y=rep(diff*c(3/4,1/4)+min_left_final,each=2),
                      grp=rep(c(max_grp+1,max_grp+2),each=2))
  
  #Champion
  championship <- data.frame(x=max_left+c(l,2*l, 2*l,l,l),
                             y=c(rep(c(diff*1/2+min_left_final,diff*1/2+min_left_final+w*2),each=2),
                                 diff*1/2+min_left_final)-w,
                             grp=max_grp+3)
  
  final_bracket <- rbind(bracket_df,final,championship)
  
  ax <- list(    
    showgrid=FALSE, # thin lines in the background
    zeroline=FALSE, # thick line at x=0
    visible=FALSE
  )
  
  plot_out <- plot_ly(data=final_bracket, 
              x=~x, 
              y=~y,
              mode = 'lines',
              split = ~grp,
              line = list(color = 'black'), 
              showlegend = FALSE) %>% 
    layout(xaxis = ax, yaxis = ax, 
           margin = list(l=0,r=0,t=0,b=0))
  
  data_out <- final_bracket %>% 
    group_by(grp) %>% 
    filter(row_number()==1 | (row_number()==n() & grp<(n_teams-1))) %>% 
    ungroup() %>% 
    mutate(x=ifelse(x > (cols+3)*l, x-l, x))
  
  return(list(plot=plot_out, data=data_out))
  
}