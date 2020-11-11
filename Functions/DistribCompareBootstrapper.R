DistribCompareBootstrapper <- function(df, seed, samples=100, type = NULL, PropertyTypes = NULL, GroupVars = c("Class","CountryClass"), Limit = Inf){
  #df:data frame of processed area/s data
  #LADCD: The LAD code to fetch the correct price data
  # Random seed
  #Number of Bootstrap samples.
  #type: whether all purchese or only classes A or b are taken, enter a character "A" or "B" or leave NULL for all
  #GroupVars: the variables that will be used to summarise by group
  
  #The function saves the sample of each LSOA to the working directory. This stops the computer crashing on large LADS All files are deleted aftwords. 
  
  dfWard <- df %>% 
    group_by(LSOA11CD) %>%  #Remove double LSOA that may have sneaked in
    summarise_all(funs(first)) %>% 
    group_by(LAD11CD) %>% #Remove LAD data that is not from this lad. Occaisonly a few lsoa from other LADs get in and cause problems
    mutate(LADCount = n()) %>% ungroup %>%
    filter(!is.na(LAD11CD), LADCount == max(LADCount)) %>% #filter out any stray areas that are classed as a different authority
    select(-LADCount)  
  
  
  dfPrice <- dfWard %>%
    #filter(!is.na(Admin_ward_code)) %>%#filter(!is.na(Pop)) %>%#removes any NA rows which cause a crash
    SubsetPrice(., type, PropertyTypes)

  #Not much difference in them poss around 17% (not tested a lot), which isn't the 10x speed up I'd hoped for
  if(sum(dfWard$Homes, na.rm = T)>Limit){
    print("Low Mem Mode, this is slower")
    StrappedData <- 1:samples %>% map_df(~{
      print(paste("Calculating bootstrap",.x, "of", samples, "for", unique(dfWard$LAD11NM)))
      Strapped(dfPrice, dfWard, 1, Grouping = GroupVars) %>%
        mutate(ID = .x)
      
    })
      

    
  }  else {
    print("High memory usage Mode. can use lots of ram")
    
    StrappedData <- Strapped(dfPrice, dfWard, samples, Grouping = GroupVars)
    
  }

  #NA values were causing N issues all over the place, the NA's were caused by low levels of super prime, 
  #I have put the ifelse in to convert any NA values into 0, also in the sum function, overkill but whatever
  
  Out <- StrappedData %>%
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
  
  print("Bootstrap process complete")
  return(Out)
}
