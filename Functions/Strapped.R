Strapped <- function(dfPrice, dfWard, samples, Grouping){
  #Is fast but uses a lot of memory
  #dfPrice: the prices for the local authority
  #dfWard the data for the local authority
  #samples: the number of bootstraps to be taken. This causes a crash when large due to lack of memory
  #WhichLowUse: The varaiable that will be used to get the number of bootstrap samples to draw.
  #Grouping: the variables that will be used for aggregating the resulting dataframe.
  
  print("Start Bootstrapping Homes distribution")
  dfWard <- dfWard %>% mutate(NonLowUse = Homes-LowUse) #this is the number os samples for homes that needs to be created
  
  #sample Homes and LowUse seperately
  print("Sampling Homes")
  HomesTemp <- WeightedLSOASample(dfWard, dfPrice, "NonLowUse", samples) %>% mutate(IsLowUse = FALSE)
  print("Sampling Low-Use")
  LowUseTemp <- WeightedLSOASample(dfWard, dfPrice, "LowUse", samples) %>% mutate(IsLowUse = TRUE)
  
  #Join and get the quartiles
  HomesTemp <- bind_rows(HomesTemp, LowUseTemp)
  
  if("Class" %in% Grouping){
    print("Calculating Price Quartiles")
    HomesTemp <- HomesTemp %>%
      group_by(ID) %>%
      mutate(Class = cut(Price, 
                         Price %>% quantile(.) %>%
                         .[2:4] %>% c(0,., Inf), 
                         labels =     c("Lower", "Lower-Mid", "Upper-Mid", "Upper"), 
                         right = F) %>% fct_relevel(., "Upper", after = 3))
    }
  

  
  if("AffordClass" %in% Grouping){
    print("Calculating Affordability Quartiles")
    HomesTemp <- HomesTemp %>%
      group_by(ID) %>%
      mutate( AffordClass = cut(Affordability,   
                                Affordability %>% quantile(.) %>% .[2:4] %>% c(0,., Inf), 
                               labels =     c("Lower", "Lower-Mid", "Upper-Mid", "Upper"), 
                               right = F) %>% fct_relevel(., "Upper", after = 3))
    
  }
  
  #seperate back into individual groups
  LowUseTemp <-HomesTemp %>% filter(IsLowUse ==TRUE) %>% select(-IsLowUse)
  HomesTemp <- HomesTemp  %>% select(-IsLowUse) #Homes is all properties not just Non-Lups
  
  
  
  print("Aggregating Data")
  HomesTemp <-HomesTemp %>% 
    group_by(ID) %>%
    AggregateStrappedData(., c("ID", Grouping))
  ungroup
  
  LowUseTemp <-LowUseTemp %>% 
    group_by(ID) %>%
    AggregateStrappedData(., c("ID", Grouping))
  
  #Join them together and finish
  Out <- JoinHomesAndLowUse(HomesTemp, LowUseTemp, c("ID", Grouping)) %>% 
    mutate(LAD11CD = unique(dfWard$LAD11CD))
  
  
  Out
}
