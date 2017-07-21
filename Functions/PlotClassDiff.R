PlotClassDiff <-function(df){
   #Takes the Out put of Bootstrap Res and plots a box plot of the difference from expected
   #df: The dataframe of a specific LAD from BootStrapRES
  df %>% mutate(RatioExvsAct = (RatioExvsAct-1)*100)%>% 
     ggplot(., aes(x= class, y = RatioExvsAct, fill = class)) + 
     geom_boxplot() + 
     labs(title = "Bootstrapped difference from expected number of low-use homes", 
          y = "% difference from expected")
   
}