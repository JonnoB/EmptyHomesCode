CleanForPlotClassDiff <- function(df){
  df %>%  
    group_by(ID, Class) %>%
    summarise(Homes = sum(Homes), 
              LowUse = sum(LowUse)) %>%
    group_by(ID)%>%
    mutate(LowUseDistrib = LowUse/sum(LowUse),
           HomesDistrib = Homes/sum(Homes)) %>%
    mutate(LowUseRatio1 = (LowUseDistrib/0.25)-1,
           HomesRatio1 = (HomesDistrib/0.25)-1,
           LowUseRatio2 = (LowUseDistrib-HomesDistrib)/HomesDistrib) %>%
    ungroup
}