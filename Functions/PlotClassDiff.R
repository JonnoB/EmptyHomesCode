PlotClassDiff <-function(df, LAD = NULL){
   #Takes the Out put of Bootstrap Res and plots a box plot of the difference from expected
   #df: The dataframe of a specific LAD from BootStrapRES
  
  if(is.null(LAD)){
    titlewords <- "Low use homes by Local Authority house price quartile"
  } else{
    titlewords <- paste0("Bootstrapped difference from the expected number of low-use homes in\n each price quartile of ", LAD)
  }
  
   colourTypes <- c( "Lower" = "#F8766D", 
                    "Lower-Mid" = "#A3A500", 
                   "Upper-Mid" ="#00BF7D", 
                   "Upper" = "#00B0F6")
  
  df %>% 
     ggplot(., aes(x= class, y = RatioExvsAct, fill = class)) + 
     geom_boxplot() + 
     labs(title = titlewords, 
          y = "% difference from expected", x= "Price Quartile") +
    scale_fill_manual(values = colourTypes) +
    theme(legend.position = "none") +
    scale_y_continuous(labels = scales::percent)
   
}