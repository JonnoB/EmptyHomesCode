CleanForPlotClassDiff <- function(df,...){
  #df, the stacked output of a grouped bootstrap summary
  #A single grouping variable is chosen in the ... argument. Any more and it will have an error
  
  group_var <- quos(...) 
  
  df%>%
    group_by(ID, !!!group_var) %>%
    summarise(Homes = sum(Homes), 
              LowUse = sum(LowUse)) %>%
    group_by(ID)%>%
    mutate(LowUseDistrib = LowUse/sum(LowUse),
           HomesDistrib = Homes/sum(Homes)) %>%
    mutate(LowUseRatio1 = (LowUseDistrib/0.25)-1,
           HomesRatio1 = (HomesDistrib/0.25)-1,
           LowUseRatio2 = (LowUseDistrib-HomesDistrib)/HomesDistrib) %>%
    ungroup %>%
    rename(Quartile = 2) #amazing!
}