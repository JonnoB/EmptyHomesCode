
DistribCompareBootstrapper <-function(df, seed, samples=100){
#df:data frame of processed area/s data
#LADCD: The LAD code to fetch the correct price data
# Random seed
#Number of Bootstrap samples.

LADCD <- unique(df$LAD11CD)
  
dfPrice <- prices %>% 
  filter( grepl(paste(LADCD, collapse="|"), Admin_district_code)) %>%
  select(Admin_ward_code, lsoa11cd, Price =X2) %>%
  mutate(class = cut(Price, c(0,  490,      750,   2000, 12000, Inf)*10^3, 
                     labels =     c("Lower", "Mid", "Upper", "Prime", "Super"), 
                     right = F))

dfWard <- df %>% 
  group_by(Admin_ward_code) %>%
  summarise_all(funs(first))

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

test1 <- dfWardHomesStrap1  %>% left_join(., dfWardLowUseStrap1, by = c("class", "ID")) %>%
  rename(LowUse = Counts.y,
         Homes = Counts.x,
         LowUseValue = Value.y,
         HomesValue = Value.x,
         LowUsePrice = Price.y,
         HomesPrice =  Price.x) %>%
  group_by(ID)%>%
  mutate(ratio = LowUse/Homes,
         HomesPerc = Homes/sum(Homes),
         LowUsePerc = LowUse/sum(LowUse),
         ClassPerc = LowUse/Homes,
         ExpectedHomes = HomesPerc*sum(LowUse),
         RatioExvsAct = LowUse/ExpectedHomes) 

return(test1)
}