JoinHomesAndLowUse <-function(Homesdf, LowUsedf, GroupVar){
  #This function just tidies up the joining operation across high and low mem modes
  #Homesddf a data frame which is the result of the strapping process
  #LowUse df a data frame which is the result of the strapping process
  #GroupVar the grouping variables, as text, that were used in the strapping process, should include ID if in High mem
  
    left_join(Homesdf, LowUsedf, by = GroupVar) %>%
    rename(LowUse = Counts.y,
           Homes = Counts.x,
           LowUseValue = Value.y, #In that grouping geography
           HomesValue = Value.x, #In that grouping geography
           MSOALowUseMean = GeogMean.y, #In that grouping geography
           MSOAHomesMean =  GeogMean.x, #In that grouping geography
           MSOALowUseMedian = GeogMedian.y, #In that grouping geography
           MSOAHomesMedian =  GeogMedian.x, #In that grouping geography
           LADHomesMean = LADmean.x, #Across whole LAD
           LADHomesMedian = LADmedian.x, #Across whole LAD
           LADLowUseMean = LADmean.y, #Across whole LAD
           LADLowUseMedian = LADmedian.y)  #Across whole LAD

  
}