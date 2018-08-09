CreateGeogModelData <- function(target, path){
  #this function allows for a cleaner way of aggregating model data at either LAD or MSOA Geography
  #target: the geography you want to use either "LAD" or "MSOA"
  #path: the folder where the bootstrap data is stored
  
  print("Load files")
  Afford <- list.files(path, full.names = TRUE) %>%
    map_df(~readRDS(.x)) %>%
    left_join(select(IncomeEst, Yearly.income, MSOA11CD), by = "MSOA11CD") 
  
  #reverse the selection safely
  if(target=="LAD"){
    target2 <- "MSOA"
  } else if(target=="MSOA") {
    target2 <- "LAD"
  } else{print("error target but be LAD or MSOA")}
  
  #remove the columns that are not needed
  Afford2 <- Afford %>% select(-grep(paste0("(",target2,"H)","|(",target2,"L)"), names(Afford))) #remove columns that have target2 followed by "Homes" and "LowUse"
  names(Afford2) <- gsub(target, "", names(Afford2)) #Remove target one from all columns names
  names(Afford2)[grepl("^11CD",names(Afford2))] <- paste0(target, "11CD") # Add target back in for the 11CD code
  
  target2 <- paste0(target, "11CD")
  
  
  if(target == "LAD"){
    print("Aggregating to LAD")
    Afford3 <- Afford2%>%
      group_by_(target2, "ID") %>%
      summarise(Yearly.income = weighted.mean(Yearly.income, Homes),
                LowUse = sum(LowUse),
                Homes = sum(Homes), #sum(Yearly.income*Homes)/sum(Homes),
                HomesMedian = first(HomesMedian),
                LowUseMedian = first(LowUseMedian)) 
  } else {
    Afford3 <- Afford2
  }
  
  Afford3 <- Afford3 %>%
    mutate(LowUsePerc = LowUse/Homes,
           AffordRatio = HomesMedian/Yearly.income,
           AfforRatioScale = scale(AffordRatio),
           AffordRationScale2 = AfforRatioScale^2,
           AffordRank = percent_rank(AffordRatio),
           HighVal = HomesMedian < LowUseMedian, #Reference Variable
           HighLUP = median(LowUsePerc) < LowUsePerc)  #Reference Variable
  
  LADModelData <- left_join(Afford3, 
                            Tourism %>% #Add in the tourism designed for MSOA
                              group_by_(target2)  %>%
                              summarise(Guest = sum(Guest),
                                        Hotel = sum(Hotel)), 
                            by = target2) %>%
    mutate(Guest = ifelse(is.na(Guest), 0, Guest),
           Hotel = ifelse(is.na(Hotel), 0, Hotel),
           Tourism = Guest + Hotel,
           TourismDensity = Tourism/Homes,
           GuestDensity = Guest/Homes,
           HotelDensity = Hotel/Homes,
           TourismDensityRank = rank(TourismDensity, ties.method = "max"))
  
  
}
