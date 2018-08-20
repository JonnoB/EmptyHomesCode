PlotClassDiff <-function(df, Var= "LowUseRatio1"){
   #Takes the Out put of Bootstrap Res and plots a box plot of the difference from expected
   #df: The dataframe of a specific LAD from BootStrapRES
   #Var the column name that you wish to be plotted
  
   colourTypes <- c( "Lower" = "#F8766D", 
                    "Lower-Mid" = "#A3A500", 
                   "Upper-Mid" ="#00BF7D", 
                   "Upper" = "#00B0F6")
  
  df %>% 
    rename_(Ratio = Var) %>%
      ggplot(., aes(x= Quartile, y = Ratio, fill = Quartile)) + 
     geom_boxplot() + 
     labs(y = "% difference from expected", x= "Price Quartile") +
    scale_fill_manual(values = colourTypes) +
    theme(legend.position = "none", axis.text.x = element_text(angle = 15, hjust = 1)) +
    scale_y_continuous(labels = scales::percent)
   
}