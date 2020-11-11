DistribCompareBootstrapper <- function(df, seed, samples=100, type = NULL, PropertyTypes = NULL, GroupVars = c("class", "classVal", "CountryClass")){
  #df:data frame of processed area/s data
  #LADCD: The LAD code to fetch the correct price data
  # Random seed
  #Number of Bootstrap samples.
  #type: whether all purchese or only classes A or b are taken, enter a character "A" or "B" or leave NULL for all
  #GroupVars: the variables that will be used to summarise by group
  
  #The function saves the sample of each LSOA to the working directory. This stops the computer crashing on large LADS All files are deleted aftwords. 
  
  dfPrice <- df %>%
    filter(!is.na(Admin_ward_code)) %>%#filter(!is.na(Pop)) %>%#removes any NA rows which cause a crash
    SubsetPrice(., type, PropertyTypes)
  
  
  dfWard <- df %>% 
    group_by(LSOA11CD) %>%
    summarise_all(funs(first)) %>% 
    group_by(LAD11CD) %>%
    mutate(LADCount = n()) %>% ungroup %>%
    filter(!is.na(Admin_ward_code), LADCount == max(LADCount)) %>% #filter out any stray areas that are classed as a different authority
    select(-LADCount)  
  
  #Function generated within the function to make sure both Homes and LUPs are calculated the same way.
  StrappedLowUse <-  function(WhichLowUse, Grouping){
    Out <- 1:samples %>%
      map_df(~{
        print(.x)
        Temp <- 1:length(unique(dfWard$LSOA11CD)) %>%
          map_df(~{
            LSOA <- unique(dfWard$LSOA11CD)[.x]
            
            #print(paste(.x, "of",length(dfWard$LSOA11CD)))
            PriceData <- dfPrice %>% 
              filter(LSOA11CD == LSOA)
            #Sometimes an LSOA will have no sales in that year. In these thankfully rare cases the entire LAD is used.
            if(nrow(PriceData)==0){
              PriceData<-dfPrice
            }
            LowUseCounts <- sum(filter(dfWard, LSOA11CD == LSOA) %>% select_(WhichLowUse)) #sample rows from LSOA
            
            SampledData <-sample_n(PriceData, LowUseCounts, replace = TRUE)
            
            SampledData  
          }
          ) %>% 
          mutate(ID = .x,
                 LADmedian = median(Price, na.rm = T),
                 LADmean = mean(Price, na.rm = T))   %>%
          group_by_(.dots = c("ID", Grouping)) %>%
          summarise(Counts = n(),
                    Value = sum(Price),
                    GeogMedian = median(Price),
                    GeogMean = mean(Price),
                    LADmedian = first(LADmedian),
                    LADmean = first(LADmean)) %>%
          ungroup
        
        Temp
        
      })
    

    Out
  }
  
  
  print("Start Bootstrapping Homes distribution")
  #bootstraps LAD housing stock.
  set.seed(seed)
  dfWardHomesStrap1 <- StrappedLowUse("Homes", Grouping = GroupVars)
  
  print("Start LUP distribution")
  
  #bootstraps LAD housing stock.
  set.seed(seed)
  dfWardLowUseStrap1 <- StrappedLowUse("LowUse", Grouping = GroupVars)
  
  print("LUP distribution Complete")
  
  test1 <- dfWardHomesStrap1  %>%
    left_join(., dfWardLowUseStrap1, by = c("ID", GroupVars)) %>%
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
  
  #NA values were causing N issues all over the place, the NA's were caused by low levels of super prime, 
  #I have put the ifelse in to convert any NA values into 0, also in the sum function, overkill but whatever
  
  test1 <- test1%>%
    group_by(ID)%>%
    mutate(LowUse = ifelse(is.na(LowUse), 0, LowUse),
           LowUseValue = ifelse(is.na(LowUseValue), 0, LowUseValue),
           HomesValue = ifelse(is.na(HomesValue), 0, HomesValue),
           Homes = ifelse(is.na(Homes), 0, Homes),
           ratio = LowUse/Homes,
           HomesPerc = Homes/sum(Homes, rm.na = T), #percentage of all homes
           LowUsePerc = LowUse/sum(LowUse, rm.na = T) #percentage of all LUPS
    ) %>% 
    ungroup
  
  return(test1)
}
