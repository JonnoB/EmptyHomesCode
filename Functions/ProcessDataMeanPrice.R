ProcessDataMeanPrice <- function(df) {
  df %>% left_join(., MeanWardPrice, by = "LSOA11CD") %>% 
    mutate(ValLow = LowUse*MeanPrice,
           PercTurnover = counts/MSOAHomes,
           LowuseClass = cut(LowUsePerc, c(-1,10,20, 30,100), 
                             labels =  c("0-10","11-20","21-30", "30+")),
           MSOALowuseClass = cut(MSOALowUsePerc, c(0,10,20, 30, 100), 
                                 labels = c("0-10","11-20","21-30", "30+")) ) 
}