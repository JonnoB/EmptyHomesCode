CalcLowUse<- function(df){
  #This function calculates all the low use data etc in the processing step.
  #It is a separate function as it allows the selection of what data to include
  #from the Structuredata function. ALtherantively it can be used to clean up
  #LSOA codes that overlap between LAD's due to postcode matching.
  
  df %>%
    left_join(., EW2, by = c("LSOA_CODE" = "ECODE")) %>% 
    group_by(Admin_ward_code)  %>%
    mutate(LowUsePerc = round(LowUse/Homes *100), 
           WardLowUse = sum(LowUse) ,
           WardLowUsePerc = round(WardLowUse/sum(Homes)*100)) %>% 
    ungroup %>%  
    ProcessDataMeanPrice(.)
}      