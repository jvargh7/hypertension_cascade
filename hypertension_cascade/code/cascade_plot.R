cascade_plot <- function(df,limits_y = c(0,65),exclude = "Screened",multiplier = 1){
  df2 <- df %>% 
    dplyr::filter(!cascade %in% exclude)
  
  fill_values = c("purple","red","lightblue","darkgreen","lightgreen")
  
  if(exclude == "Screened"){
    fill_values = c("red","lightblue","darkgreen","lightgreen")
  }
  
  fig <- df2 %>% 
    ggplot(data=.,aes(x = group,y=estimate*multiplier,ymax = uci*multiplier,ymin = lci*multiplier,fill=cascade)) +
    geom_col(position = position_dodge(width=0.9)) +
    geom_text(aes(y=uci*multiplier + 2,label=round(estimate*multiplier,1)),
              position = position_dodge(width = 0.9),size=3) +
    geom_errorbar(width = 0.1, position = position_dodge(width = 0.9)) +
    theme_bw() +
    xlab("") +
    ylab("Prevalence (%)") +
    scale_y_continuous(limits = limits_y) + 
    guides(fill = guide_legend(nrow = 1)) +
    scale_fill_manual(name = "",values = fill_values) 
  
  return(fig)
}