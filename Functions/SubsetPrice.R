
SubsetPrice <- function(df){
  #Subsets the price frame to be just for the specific LAD
  
  #Som LADS have bits of other LADS mixed in this removes those
  Code <- df$LAD11CD[which.max(table(df$LAD11CD) %>% as.matrix)] 
  
  filter(prices, grepl(Code, Admin_district_code)) %>%
    select(Admin_ward_code, lsoa11cd, Price =X2, X15) %>%
    #filter(X15 == "A") %>%
    mutate(class = cut(Price, c(0,  490,      750,   2000, 12000, Inf)*10^3, 
                       labels =     c("Lower", "Mid", "Upper", "Prime", "Super"), 
                       right = F),
           Price = as.numeric(Price)) 
  
  
}

