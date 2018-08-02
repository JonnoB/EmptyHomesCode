DistribCompareBootstrapper2 <- function(df, seed, samples=100, type = NULL, PropertyTypes = NULL, GroupVars = c("class", "classVal", "CountryClass")){
  #df:data frame of processed area/s data
  #LADCD: The LAD code to fetch the correct price data
  # Random seed
  #Number of Bootstrap samples.
  #type: whether all purchese or only classes A or b are taken, enter a character "A" or "B" or leave NULL for all
  #PriceCuts: the cut points for the different classes of housing, used only occaisionally has to start with 0 and end with Inf
  #and in total have 6 elements
  
  dfPrice <- df %>%
    filter(!is.na(Admin_ward_code)) %>%#filter(!is.na(Pop)) %>%#removes any NA rows which cause a crash
    SubsetPrice(., type, PropertyTypes)
  
  dfWard <- df %>% 
    group_by(Admin_ward_code) %>%
    summarise_all(funs(first)) %>% 
    group_by(LAD11CD) %>%
    mutate(LADCount = n()) %>% ungroup %>%
    filter(!is.na(Admin_ward_code), LADCount == max(LADCount)) %>% #filter out any stray areas that are classed as a different authority
    select(-LADCount)  
  
  #Function generated within the function to make sure both Homes and LUPs are calculated the same way.
  StrappedLowUse <- function(WhichLowUse, HighValue = NA, GroupVar= GroupVars){
    test<- 1:samples %>%
      map_df(~{
        
        Out <- unique(dfWard$Admin_ward_code) %>%
          map_df(~{
            #print(.x)
            PriceData <- dfPrice %>% 
              filter(Admin_ward_code == .x)
            #Sometimes a ward will have no sales in that year. In these thankfully rare cases the entire LAD is used.
            if(nrow(PriceData)==0){
              PriceData<-dfPrice
            }
            LowUseCounts <- sum(filter(dfWard, Admin_ward_code == .x) %>% select_(WhichLowUse)) #sample rows from ward
            
            sample_n(PriceData, LowUseCounts, replace = TRUE)
            
          }
          )  %>% mutate(HighVal = Price>HighValue[.x],
                        LADmedian = median(Price, na.rm = T),
                        LADmean = mean(Price, na.rm = T)) %>%
          group_by_(.dots = GroupVar) %>%
          summarise(Counts = n(),
                    Value = sum(Price),
                    HighVal = sum(HighVal, rm.na = TRUE),
                    LADmedian = first(LADmedian),
                    LADmean = first(LADmean)) %>%
          mutate(ID = .x) %>%
          ungroup
        
        Out
        
      }) %>%  
      mutate(
        Price = Value/Counts)
  }
  
  
  print("Start Bootstrapping Homes distribution")
  #bootrapps LAD housing stock.
  set.seed(seed)
  dfWardHomesStrap1 <- StrappedLowUse("WardHomes") %>%
    select(-HighVal)
  
  print("Start LUP distribution")

  HighVal <- dfWardHomesStrap1 %>%
    group_by(ID) %>%
    summarise(LADmedian = first(LADmedian)) %>% .$LADmedian #get median price, this can often be the same throughout the vector

  #bootrapps LAD housing stock.
  set.seed(seed)
  dfWardLowUseStrap1 <- StrappedLowUse("WardLowUse", HighValue = HighVal)

  print("LUP distribution Complete")

  test1 <- dfWardHomesStrap1  %>%
    left_join(., dfWardLowUseStrap1, by = c("ID", GroupVars)) %>%
    rename(LowUse = Counts.y,
           Homes = Counts.x,
           LowUseValue = Value.y, #In that grouping geography
           HomesValue = Value.x, #In that grouping geography
           LowUsePrice = Price.y, #In that grouping geography
           HomesPrice =  Price.x, #In that grouping geography
           MeanHomes = LADmean.x, #Across whole LAD
           MedianHomes = LADmedian.x, #Across whole LAD
           MeanLUPs = LADmean.y, #Across whole LAD
           MedianLUPs = LADmedian.y) %>% #Across whole LAD
    mutate(LAD11CD = unique(dfWard$LAD11CD))

  #NA values were causing N issues all over the place, the NA's were caused by low levels of super prime,
  #I have put the ifelse in to convert any NA values into 0, also in the sum function, overkill but whatever

  test1 <- test1%>%
    group_by(ID)%>%
    mutate(LowUse = ifelse(is.na(LowUse), 0, LowUse),
           LowUseValue = ifelse(is.na(LowUseValue), 0, LowUseValue),
           HomesValue = ifelse(is.na(HomesValue), 0, HomesValue),
           Homes = ifelse(is.na(Homes), 0, Homes),
           MeanLUPs = ifelse(is.na(MeanLUPs), 0, MeanLUPs),
           MedianLUPs = ifelse(is.na(MedianLUPs), 0, MedianLUPs),
           HighVal = ifelse(is.na(HighVal), 0, HighVal),
           ratio = LowUse/Homes,
           HomesPerc = Homes/sum(Homes, rm.na = T),
           LowUsePerc = LowUse/sum(LowUse, rm.na = T),
           ClassPerc = LowUse/Homes,
           ExpectedHomes = HomesPerc*sum(LowUse, rm.na = T),
           RatioExvsAct = LowUse/ExpectedHomes) %>%
    ungroup

  return(test1)
}
