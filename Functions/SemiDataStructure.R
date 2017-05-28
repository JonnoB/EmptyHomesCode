SemiDataStructure <- function(df){
  #does the standardised part of the semi data, useful after the initial cleaning.
  #Is really smilar to the proper cleaning funct but adjusted for the wards only boroughs
  #Should be phased out when better data comes in.
  df <- df %>%
    select(Admin_ward_code, LowUse, Empty) %>% 
    left_join(., EW2, by = "Admin_ward_code") %>%
    group_by(Admin_ward_code)  %>%
    mutate(
      WardLowUsePerc = round(LowUse/sum(Homes)*100),
      LowUsePerc = WardLowUsePerc, 
      WardLowUse = LowUse) %>% 
    ungroup %>% 
    left_join(., MeanWardPrice, by = "Admin_ward_code") %>% 
    mutate(ValLow = LowUse*MeanPrice,
           LowuseClass = cut(LowUsePerc, c(-1,10,20, 30,100), 
                             labels =  c("0-10","11-20","21-30", "30+")),
           WardLowuseClass = cut(WardLowUsePerc, c(0,10,20, 30, 100), 
                                 labels = c("0-10","11-20","21-30", "30+")) ) %>%
    rename(LSOA_CODE = ECODE)
}