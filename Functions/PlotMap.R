PlotMap <- function(df, ShapeData, variable = "LowUsePerc", title = "Map of Region", filtermap = TRUE){
  
  
  Singlevis <- ShapeData %>% 
    left_join(., df, by = c("lsoa11cd"="LSOA_CODE")) %>% 
        ungroup
  
  if(filtermap){
  Singlevis <- Singlevis %>% filter(!is.na(eval(parse(text = paste0("Singlevis$",variable)))
))
  }
  #Just neccessary variables  
  Singlevis <-Singlevis %>% select_("long", "lat", "group", variable)
  
  if(df %>% select_(variable) %>% unlist %>% class == "factor"){
    
    Outplot <- Singlevis %>%
      ggplot(., aes_string("long", "lat", group = "group", fill = variable))+ 
      geom_polygon(colour = alpha("black", 1/2), size = 0.2) + 
      scale_fill_manual(values = c("blue", "white", "salmon","red"), na.value="grey50")+
      theme(plot.title = element_text(hjust = 0.5))+
      theme(axis.title=element_blank(),
            axis.text=element_blank(),
            axis.ticks=element_blank())+ 
      ggtitle(title)   
    
  } else {
    #ONLY SET FITLER MAP TO FALSE WHEN the shape FILES HAVE BEEN PRE FILTERED OTHERWISE PUNISHMENT AND REGRET
    value <- mean(eval(parse(text= paste0("df$", variable))), na.rm = TRUE)
    
        Outplot <- ggplot(Singlevis, aes_string("long", "lat", group = "group", fill = variable)) + 
      geom_polygon(colour = alpha("black", 1/2), size = 0.2) + 
      #scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = value) +
      scale_fill_gradientn(
        #limits = c(0,55),
        colours = c("blue", "white", "red"), 
        values = c(0,.2,.4,1)
        ) +
          theme(plot.title = element_text(hjust = 0.5)) +
      theme(axis.title=element_blank(),
            axis.text=element_blank(),
            axis.ticks=element_blank())+ 
      ggtitle(title) 
  }
  
  
  return(Outplot)  
  
}
