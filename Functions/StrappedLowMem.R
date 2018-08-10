StrappedLowMem <-  function(dfPrice, dfWard, samples, WhichLowUse, Grouping){
  #Is slow but avoids crashes due to large values of "sample"
  #dfPrice: the prices for the local authority
  #dfWard the data for the local authority
  #samples: the number of bootstraps to be taken. This causes a crash when large due to lack of memory
  #WhichLowUse: The varaiable that will be used to get the number of bootstrap samples to draw.
  #Grouping: the variables that will be used for aggregating the resulting dataframe.
  Out <- 1:samples %>%
    map_df(~{
      print(.x)
      HomesTemp <- 1:length(unique(dfWard$LSOA11CD)) %>%
        map_df(~{
          LSOA <- unique(dfWard$LSOA11CD)[.x]
          
          #print(paste(.x, "of",length(dfWard$LSOA11CD)))
          PriceData <- dfPrice %>% 
            filter(LSOA11CD == LSOA)
          #Sometimes an LSOA will have no sales in that year. In these thankfully rare cases the entire LAD is used.
          if(nrow(PriceData)==0){
            PriceData<-dfPrice
          }
          LowUseCounts <- filter(dfWard, LSOA11CD == LSOA) %>% pull(Homes) #sample rows from LSOA
          
          SampledData <-sample_n(PriceData, LowUseCounts, replace = TRUE)
          
          SampledData  
        }
        ) %>%
        mutate(Class = cut(Price, 
                           Price %>% quantile(.) %>%
                             .[2:4] %>% c(0,., Inf), 
                           labels =     c("Lower", "Lower-Mid", "Upper-Mid", "Upper"), 
                           right = F) %>% fct_relevel(., "Upper", after = 3))
      
      
      LowUseTemp <-  1:length(unique(dfWard$LSOA11CD)) %>% map_df(~{
        
        
        LSOA <- unique(dfWard$LSOA11CD)[.x]
        CurrentHomes <- HomesTemp %>% filter(LSOA11CD == LSOA) 
        Counts <- filter(dfWard, LSOA11CD == LSOA) %>% pull(LowUse)
        
        SampledData <-CurrentHomes %>%
          sample_n( Counts, replace = FALSE) #This is a strict subset of the Homes data not a bootstrap!
        
      })
      
      HomesTemp <- HomesTemp %>% 
        mutate(LADmedian = median(Price, na.rm = T),
               LADmean = mean(Price, na.rm = T))   %>%
        group_by_(.dots = c(Grouping)) %>%
        summarise(Counts = n(),
                  Value = sum(Price),
                  GeogMedian = median(Price),
                  GeogMean = mean(Price),
                  LADmedian = first(LADmedian),
                  LADmean = first(LADmean)) %>%
        ungroup
      
      
      LowUseTemp <- LowUseTemp %>% 
        mutate(LADmedian = median(Price, na.rm = T),
               LADmean = mean(Price, na.rm = T))   %>%
        group_by_(.dots = c(Grouping)) %>%
        summarise(Counts = n(),
                  Value = sum(Price),
                  GeogMedian = median(Price),
                  GeogMean = mean(Price),
                  LADmedian = first(LADmedian),
                  LADmean = first(LADmean)) %>%
        ungroup
      
      
      Out <- HomesTemp   %>%
        left_join(., LowUseTemp, by = c(Grouping)) %>%
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
        mutate(LAD11CD = unique(dfWard$LAD11CD),
               ID = .x)
      
      Out
      
    })
  
  
  Out
}