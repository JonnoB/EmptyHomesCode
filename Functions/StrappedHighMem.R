StrappedHighMem <- function(dfPrice, dfWard, samples, WhichLowUse, Grouping){
  #Is fast but uses a lot of memory
  #dfPrice: the prices for the local authority
  #dfWard the data for the local authority
  #samples: the number of bootstraps to be taken. This causes a crash when large due to lack of memory
  #WhichLowUse: The varaiable that will be used to get the number of bootstrap samples to draw.
  #Grouping: the variables that will be used for aggregating the resulting dataframe.
  Temp <-  1:length(unique(dfWard$LSOA11CD)) %>%
    map_df(~{
      LSOA <- unique(dfWard$LSOA11CD)[.x]
      
      print(paste("LSOA",.x, "of",length(dfWard$LSOA11CD)))
      PriceData <- dfPrice %>% 
        filter(LSOA11CD == LSOA)
      #Sometimes an LSOA will have no sales in that year. In these thankfully rare cases the entire LAD is used.
      if(nrow(PriceData)==0){
        PriceData<-dfPrice
      }
      LowUseCounts <- sum(filter(dfWard, LSOA11CD == LSOA) %>% select_(WhichLowUse)) #sample rows from LSOA
      
      SampledData <-sample_n(PriceData, LowUseCounts*samples, replace = TRUE) %>%
        mutate(ID = rep(1:samples, nrow(.)/samples))
      
    }
    ) 
  
  print("Aggregating Data")
  Out <-Temp %>% 
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
  
  
  
  
  Out
}
