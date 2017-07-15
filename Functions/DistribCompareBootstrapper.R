
DistribCompareBootstrapper <-function(df, seed, samples=100, PriceCuts = NULL, type = NULL){
#df:data frame of processed area/s data
#LADCD: The LAD code to fetch the correct price data
# Random seed
#Number of Bootstrap samples.

LADCD <- unique(df$LAD11CD)

if(!is.null(type)){
  prices <- prices %>%
    filter(X15 == type)
}

if(is.null(PriceCuts)){
  PriceCuts<-c(0,  490,      750,   2000, 12000, Inf)*10^3
}


dfPrice <- prices %>% 
  filter( grepl(paste(LADCD, collapse="|"), Admin_district_code)) %>%
  select(Admin_ward_code, lsoa11cd, Price =X2) %>%
  mutate(class = cut(Price, PriceCuts , 
                     labels =     c("Lower", "Mid", "Upper", "Prime", "Super"), 
                     right = F),
         Price = as.numeric(Price)) #there was some number overflow thing changing to numeric solves this
dfWard <- df %>% 
  group_by(Admin_ward_code) %>%
  summarise_all(funs(first)) %>% filter(!is.na(Admin_ward_code))

print("Start Bootstrapping LSOA distribution")

#bootrapps LAD housing stock.
set.seed(seed)
dfWardLowUseStrap1 <- unique(dfPrice$Admin_ward_code)%>% 
  map(~ dfPrice %>% 
        filter(Admin_ward_code == .x) %>% 
        ClassStrapper(., sum((filter(dfWard, Admin_ward_code == .x))$WardLowUse), reps = samples) %>%
        mutate(class = as.character(class),
               Counts = as.integer(Counts),
               ID = as.integer(ID)) %>%
        setNames(c("class", paste0("Counts", .x), paste0("Value", .x) ,"ID"))
  ) %>%  
  Reduce(function(dtf1,dtf2) full_join(dtf1,dtf2, by = c("class", "ID")), .) %>%  
  mutate(Counts = rowSums(.[,grep("Counts", names(.))], na.rm = TRUE),
         Value = rowSums(.[,grep("Value", names(.))], na.rm = TRUE),
         Price = Value/Counts,
         class = fct_relevel(class, "Upper", after = 2)) %>%
  select(class, ID, Counts, Counts,Value, Price )

print("LSOA distribution Complete")
print("Start Bootstrapping LAD distribution")
#bootrapps LAD housing stock.
set.seed(seed)
dfWardHomesStrap1 <- unique(dfPrice$Admin_ward_code)%>% 
  map(~ dfPrice %>% 
        filter(Admin_ward_code == .x) %>% 
         ClassStrapper(., sum((filter(dfWard, Admin_ward_code == .x))$WardHomes), reps = samples) %>%
        mutate(class = as.character(class),
               Counts = as.integer(Counts),
               ID = as.integer(ID)) %>%
        setNames(c("class", paste0("Counts", .x), paste0("Value", .x) ,"ID"))
  ) %>%  
  Reduce(function(dtf1,dtf2) full_join(dtf1,dtf2, by = c("class", "ID")), .) %>%  
  mutate(Counts = rowSums(.[,grep("Counts", names(.))], na.rm = TRUE),
         Value = rowSums(.[,grep("Value", names(.))], na.rm = TRUE),
         Price = Value/Counts,
         class = fct_relevel(class, "Upper", after = 2)) %>%
  select(class, ID, Counts, Counts,Value, Price )

print("LAD distribution Complete")

test1 <- dfWardHomesStrap1  %>% 
  left_join(., dfWardLowUseStrap1, by = c("class", "ID")) %>%
  rename(LowUse = Counts.y, 
         Homes = Counts.x,
         LowUseValue = Value.y,
         HomesValue = Value.x,
         LowUsePrice = Price.y,
         HomesPrice =  Price.x) 

#NA values were causing N issues all over the place, the NA's were caused by low levels of super prime, 
#I have put the ifelse in to convert any NA values into 0, also in the sum function, overkill but whatever

test1 <- test1%>%
  group_by(ID)%>%
  mutate(LowUse = ifelse(is.na(LowUse), 0, LowUse),
         LowUseValue = ifelse(is.na(LowUseValue), 0, LowUseValue),
         HomesValue = ifelse(is.na(HomesValue), 0, HomesValue),
         Homes = ifelse(is.na(Homes), 0, Homes),
         ratio = LowUse/Homes,
         HomesPerc = Homes/sum(Homes, rm.na = T),
         LowUsePerc = LowUse/sum(LowUse, rm.na = T),
         ClassPerc = LowUse/Homes,
         ExpectedHomes = HomesPerc*sum(LowUse, rm.na = T),
         RatioExvsAct = LowUse/ExpectedHomes, rm.na = T)

return(test1)
}