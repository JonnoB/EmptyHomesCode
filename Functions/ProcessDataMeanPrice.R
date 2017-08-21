ProcessDataMeanPrice <- function(df) {
  df %>% left_join(., MeanWardPrice, by = "Admin_ward_code") %>% 
    mutate(ValLow = LowUse*MeanPrice,
           PercTurnover = counts/WardHomes,
           LowuseClass = cut(LowUsePerc, c(-1,10,20, 30,100), 
                             labels =  c("0-10","11-20","21-30", "30+")),
           WardLowuseClass = cut(WardLowUsePerc, c(0,10,20, 30, 100), 
                                 labels = c("0-10","11-20","21-30", "30+")) ) 
}