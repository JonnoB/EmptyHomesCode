StrappedHighMem <- function(dfPrice, dfWard, samples, Grouping){
  #Is fast but uses a lot of memory
  #dfPrice: the prices for the local authority
  #dfWard the data for the local authority
  #samples: the number of bootstraps to be taken. This causes a crash when large due to lack of memory
  #WhichLowUse: The varaiable that will be used to get the number of bootstrap samples to draw.
  #Grouping: the variables that will be used for aggregating the resulting dataframe.
  
  print("Start Bootstrapping Homes distribution")
  
  HomesTemp <-  1:length(unique(dfWard$LSOA11CD)) %>%
    map_df(~{
      LSOA <- unique(dfWard$LSOA11CD)[.x]
      
      print(paste("LSOA",.x, "of",length(dfWard$LSOA11CD)))
      PriceData <- dfPrice %>% 
        filter(LSOA11CD == LSOA)
      #Sometimes an LSOA will have no sales in that year. In these thankfully rare cases the entire LAD is used.
      if(nrow(PriceData)==0){
        PriceData<-dfPrice
      }
      Counts <- filter(dfWard, LSOA11CD == LSOA) %>% pull(Homes) #sample rows from LSOA
      
      SampledData <-sample_n(PriceData, Counts*samples, replace = TRUE) %>%
        mutate(ID = rep(1:samples, nrow(.)/samples))
      
    }
    ) %>%
    group_by(ID) %>%
    mutate(Class = cut(Price, 
                       Price %>% quantile(.) %>%
                         .[2:4] %>% c(0,., Inf), 
                       labels =     c("Lower", "Lower-Mid", "Upper-Mid", "Upper"), 
                       right = F) %>% fct_relevel(., "Upper", after = 3))
  
  print("Start LUP distribution")
  
  #This is annyngky slow as it has to sample from a large number of specific sets.
  #Using a different numbe each time making it difficult to vectorise.
  LowUseTemp <- 1:length(unique(dfWard$LSOA11CD)) %>% map_df(~{
    print(paste("LSOA",.x, "of",length(dfWard$LSOA11CD)))
    
    LSOA <- unique(dfWard$LSOA11CD)[.x]
    CurrentHomes <- HomesTemp %>% filter(LSOA11CD == LSOA) 
    Counts <- filter(dfWard, LSOA11CD == LSOA) %>% pull(LowUse)
    
    SampledData <-CurrentHomes %>%
      # group_by(ID) %>% # sample from each of the ID groups
      sample_n( Counts, replace = FALSE) #This is a strict subset of the Homes data not a bootstrap!
    
  })
  print("LUP distribution Complete")
  
  
  print("Aggregating Data")
  HomesTemp <-HomesTemp %>% 
    group_by(ID) %>%
    mutate(LADmedian = median(Price, na.rm = T),
           LADmean = mean(Price, na.rm = T)) %>%
    group_by_(.dots = c("ID", Grouping)) %>%
    summarise(Counts = n(),
              Value = sum(Price),
              GeogMedian = median(Price),
              GeogMean = mean(Price),
              LADmedian = first(LADmedian),
              LADmean = first(LADmean)) %>%
    ungroup
  
  LowUseTemp <-LowUseTemp %>% 
    group_by(ID) %>%
    mutate(LADmedian = median(Price, na.rm = T),
           LADmean = mean(Price, na.rm = T)) %>%
    group_by_(.dots = c("ID", Grouping)) %>%
    summarise(Counts = n(),
              Value = sum(Price),
              GeogMedian = median(Price),
              GeogMean = mean(Price),
              LADmedian = first(LADmedian),
              LADmean = first(LADmean)) %>%
    ungroup
  
  Out <- HomesTemp   %>%
    left_join(., LowUseTemp, by = c("ID", Grouping)) %>%
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
           LADLowUseMedian = LADmedian.y) %>% #Across whole LAD
    mutate(LAD11CD = unique(dfWard$LAD11CD))
  
  
  Out
}
