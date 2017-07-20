
SubsetPrice <- function(df, type = NULL, PropertyTypes = NULL){
  #Subsets the price frame to be just for the specific LAD
  #Takes either the DATA dataframe or the character string LAD11CD
  #this allows the subsetting of the price list without looking up the LAD code
  #PropertyTypes: either a character vector of valid proeprty types to be kept, or left blank for all property types
  
  if(is.character(df)){
    Code <- df
  } else{
    #Som LADS have bits of other LADS mixed in this removes those
    Code <- df$LAD11CD[which.max(table(df$LAD11CD) %>% as.matrix)] 
  }
  
  if(!is.null(type)){
    prices <- prices %>%
      filter(X15 == type)
  }

  #Removes all unwanted property types, useful for removing Other type
  if(!is.null(PropertyTypes)){
    prices <- prices %>%
      filter(X5 %in% PropertyTypes)
  }
  
  
    
  filter(prices, grepl(Code, Admin_district_code)) %>%
    select(Admin_ward_code, lsoa11cd, Price =X2, X15, PropType = X5) %>%
    #filter(X15 == "A") %>%
    mutate(class = cut(Price, c(0,  490,      750,   2000, 12000, Inf)*10^3, 
                       labels =     c("Lower", "Mid", "Upper", "Prime", "Super"), 
                       right = F),
           Price = as.numeric(Price)) 
  
  
}

