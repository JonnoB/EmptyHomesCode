WeightedLSOASample <- function(dfWard, dfPrice, Var, samples= 1){
  #Creates a sample of all the properties in a LAD
  #dfWard the dataframe with all the LowUse and property information
  #dfprice the house price dataframe
  #Var the variable that contains the number of properties to draw, usually Homes/LowUse/NonLowUse
  #samples; the number of repititions for each LSOA, seperates high and low memory modes
  1:length(unique(dfWard$LSOA11CD)) %>%
    map_df(~{
      LSOA <- unique(dfWard$LSOA11CD)[.x]
      
      #I think this is no longer necessary as it is much faster now
      # if(samples != 1){
      # print(paste("LSOA",.x, "of",length(dfWard$LSOA11CD)))
      # }
      # 
      PriceData <- dfPrice %>% 
        filter(LSOA11CD == LSOA)
      #Sometimes an LSOA will have no sales in that year. In these thankfully rare cases the entire LAD is used.
      if(nrow(PriceData)==0){
        PriceData<-dfPrice
      }
      Counts <- filter(dfWard, LSOA11CD == LSOA) %>% pull(Var) #sample rows from LSOA
      
      SampledData <-sample_n(PriceData, Counts*samples, replace = TRUE) %>%
        mutate(ID = rep(1:samples, nrow(.)/samples))

    }
    )
  }