PlotClassDiff <-function(df, Var= "LowUseRatio1"){
   #Takes the Out put of Bootstrap Res and plots a box plot of the difference from expected
   #df: The dataframe of a specific LAD from BootStrapRES
   #Var the column name that you wish to be plotted
  
  df %>% 
    rename_(Ratio = Var) %>%
      ggplot(., aes(x= Quartile, y = Ratio, fill = Quartile)) + 
     geom_boxplot() + 
     labs(y = "% difference from expected", x= "Price Quartile") +
    theme(legend.position = "none", axis.text.x = element_text(angle = 15, hjust = 1)) +
    scale_y_continuous(labels = scales::percent)
   
}