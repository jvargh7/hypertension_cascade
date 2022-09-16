output$nationalmap <- renderTmap({
  
  
  nm <- tm_shape(shp = nm_merge,ext=1.2) +
    tm_fill(title= "",
            col="estimate",
            palette = palette_chr,
            style = "fixed",
            breaks= breaks,
            # midpoint = NA,
            textNA="Data not available",
            colorNA = "white")+ 
    tm_borders(col="black") + 
    tm_text(text="ST_NM",col="black",size=0.5,remove.overlap = TRUE)+
    tm_legend(legend.position = c("right","top"),
              legend.outside=FALSE,
              legend.just=c("left","top"))
  # tm_layout(plot_title,title.size = 2,
  #           legend.text.size = 1,
  #           legend.title.size = 1)
  # 
  nm
  
  
})

output$statemap <- renderTmap({
  
  tmap_mode("view") +
    tm_shape(sm_merge,ext=1.2,bbox = reactive({sm_merge@bbox})) + 
    tm_fill(title= "",
            col="estimate",
            palette = palette_chr,
            style = "fixed",
            breaks= breaks,
            # midpoint = NA,
            textNA="Data not available",
            colorNA = "white")+ 
    tm_borders(col="black") + 
    tm_text(text="ST_NM",col="black",size=0.5,remove.overlap = TRUE)+
    tm_legend(legend.position = c("right","top"),
              legend.outside=FALSE,
              legend.just=c("left","top"))
  # tm_layout(plot_title,title.size = 2,
  #           legend.text.size = 1,
  #           legend.title.size = 1)
  
  
})