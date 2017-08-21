SemiDataStructure <- function(df){
  #does the standardised part of the semi data, useful after the initial cleaning.
  #Is really smilar to the proper cleaning funct but adjusted for the wards only boroughs
  #Should be phased out when better data comes in.
  df <- df %>%
    select(Admin_ward_code, LowUse, Empty) %>% 
    left_join(., EW2, by = "Admin_ward_code") %>%
    group_by(Admin_ward_code) %>%
    rename(LSOA_CODE = ECODE) %>%
    mutate(
      WardLowUsePerc = round(LowUse/sum(Homes)*100),
      WardLowUse = LowUse,
      LowUse = LowUse*(Homes/sum(Homes)),
      LowUsePerc = round(LowUse*100/Homes)) %>% 
    ungroup %>% 
    ProcessDataMeanPrice(.)
}