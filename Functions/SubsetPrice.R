
SubsetPrice <- function(df, type = NULL, PropertyTypes = NULL, PriceCuts = NULL){
  #Subsets the price frame to be just for the specific LAD
  #Takes either the DATA dataframe or the character string LAD11CD
  #this allows the subsetting of the price list without looking up the LAD code
  #PropertyTypes: either a character vector of valid proeprty types to be kept, or left blank for all property types
  
  if(is.character(df)){
    Code <- df
  } else{
    #Som LADS have bits of other LADS mixed in this removes those
    Code <- names(table(df$LAD11CD))[which.max(table(df$LAD11CD) %>% as.matrix)] 
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
  
  if(is.null(PriceCuts)){
    PriceCuts<-c(0,  490,      750,   2000, 12000, Inf)*10^3
  }
  
  #The  model used to take multiple councils if there was overlap, but this caused some strange
  #Problems So I removed it. The current version only takes a single LAD
  prices %>%
  filter(., grepl(Code, Admin_district_code)) %>%
    select(Admin_ward_code, lsoa11cd, Price =X2, X15, PropType = X5) %>%
    mutate(class = cut(Price, PriceCuts, 
                       labels =     c("Lower", "Mid", "Upper", "Prime", "Super"), 
                       right = F),
           Price = as.numeric(Price)) #there was some number overflow thing changing to numeric solves this
   
}

