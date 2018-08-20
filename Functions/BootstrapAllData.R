BootstrapAllData <- function(DATAdf, GroupingVars = c("Class", "CountryClass"), Reps=501, LimitValue, RandomSeed = 1652, PropertyTypes= c("D", "S", "T", "F") ){
  
  #Find the length of each vector so that it can be sampled as quickly as possible
  LADBootInfo <- DATAdf %>%
    group_by(LAD11CD) %>%
    summarise(LAD11NM = first(LAD11NM),
              LSOA = n(),
              Homes = sum(Homes, na.rm = TRUE),
              LowUse = sum(LowUse, na.rm = TRUE)) %>%
    arrange(Homes)
  
  #Find what needs to be bootstrapped
  #The data is saved using LAD codes, this means it is not particularly Human readable but is more reliable.
  newLADsnames <-  unique(DATAdf$LAD11CD)[!(unique(DATAdf$LAD11CD) %in% gsub(".rds", "",list.files(getwd())))] #finds the LAD codes I have and compares them to the LAD codes that are already completed
  
  #only bootstrap if there is something to add.
  #Save each boostrap as you go along to avoid losing it all in a crash
  if(length(newLADsnames)>0){
    1:length(newLADsnames)  %>% walk(~{
      print(paste0("Bootstrapping ", LADBootInfo$LAD11NM[LADBootInfo$LAD11CD==newLADsnames[.x]],". LADs remaining ", length(newLADsnames)-.x))
      DATAdf %>% 
        filter(LAD11CD == newLADsnames[.x], !is.na(Homes)) %>% #removes errors caused by the unassigned empty homes
        DistribCompareBootstrapper(., RandomSeed, Reps, type=NULL, PropertyTypes, Limit = LimitValue, GroupVars = GroupingVars ) %>%
        saveRDS(., file = paste0( newLADsnames[.x], ".rds"))
      gc()
    }) 
  }
  
  
}