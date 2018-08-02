CalcLowUse<- function(df){
  #This function calculates all the low use data etc in the processing step.
  #It is a separate function as it allows the selection of what data to include
  #from the Structuredata function. Alterantively it can be used to clean up
  #LSOA codes that overlap between LAD's due to postcode matching.
  
  df %>%
    left_join(., EW2, by = c("LSOA_CODE" = "ECODE")) %>% 
    rename(LSOA11CD = LSOA_CODE) %>%
    group_by(MSOA11CD)  %>%
    mutate(MSOAHomes = sum(Homes),
           LowUsePerc = round(LowUse/Homes *100), 
           MSOALowUse = sum(LowUse) ,
           MSOALowUsePerc = round(MSOALowUse/sum(Homes)*100)) %>% 
    ungroup %>%  
    ProcessDataMeanPrice(.)
}      