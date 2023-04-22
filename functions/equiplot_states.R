equiplot_states <- function(state_diff,national_diff,x_lims=NULL){
  
  if(is.null(x_lims)){
    x_lims = c(min(state_diff$diff),max(state_diff$diff))
    
  }
  
  # ggplot(data=state_diff,aes(x=diff,y=n5_state,xmin=lci_diff,xmax=uci_diff)) +
  #   geom_point() +
  #   geom_errorbarh() +
  #   geom_vline(xintercept=national_diff,col="red",linetype=2) +
  #   theme_bw() +
  #   facet_grid(zone~.,scales="free_y",space="free_y") 
    
    
  
  
  
  
}
