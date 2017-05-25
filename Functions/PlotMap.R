PlotMap <- function(df, name, variable = "LowUsePerc", title = "Map of Region"){
  
  value <- mean(eval(parse(text= paste0("df$", variable))), na.rm = TRUE)
  
  Singlevis <- AG %>% filter(grepl(name, LAD11NM)) %>% 
    left_join(., select(df, LSOA_CODE, LowUsePerc, WardLowUsePerc, LowUse, Empty, Homes), 
              by = c("lsoa11cd"="LSOA_CODE")) %>% 
    ungroup 
  
  ggplot(Singlevis, aes_string("long", "lat", group = "group", fill = variable))+ 
    geom_polygon(colour = alpha("black", 1/2), size = 0.2) + 
    scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = value)+
    theme(plot.title = element_text(hjust = 0.5))+
    theme(axis.title=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank())+ 
    ggtitle(title) 
  
}
