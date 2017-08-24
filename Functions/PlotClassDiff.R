PlotClassDiff <-function(df, LAD = NULL){
   #Takes the Out put of Bootstrap Res and plots a box plot of the difference from expected
   #df: The dataframe of a specific LAD from BootStrapRES
  
  if(is.null(LAD)){
    titlewords <- "Observed difference from expected number of low-use homes"
  } else{
    titlewords <- paste0("Observed difference from expected number of low-use homes in ", LAD)
  }
  
   colourTypes <- c( "Lower" = "#F8766D", 
                    "Mid" = "#A3A500", 
                   "Upper" ="#00BF7D", 
                   "Prime" = "#00B0F6", 
                   "Super" = "#E76BF3")
  
  df %>% mutate(RatioExvsAct = (RatioExvsAct-1)*100)%>% 
     ggplot(., aes(x= class, y = RatioExvsAct, fill = class)) + 
     geom_boxplot() + 
     labs(title = titlewords, 
          y = "% difference from expected") +
    scale_fill_manual(values = colourTypes)
   
}