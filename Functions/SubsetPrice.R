SubsetPrice <- function(df, type = NULL, PropertyTypes = NULL){
  #Subsets the price frame to be just for the specific LAD
  #Takes either the DATA dataframe or the character string LAD11CD
  #this allows the subsetting of the price list without looking up the LAD code
  #PropertyTypes: either a character vector of valid proeprty types to be kept, or left blank for all property types
  
  if(is.character(df)){
    Code <- df
  } else{
    #Some LADS have bits of other LADS mixed in this removes those
    Code <- names(table(df$LAD11CD))[which.max(table(df$LAD11CD) %>% as.matrix)] 
  }
  
  #filter prices to only relevant rows
  pricesTemp <- prices %>%
  filter(., grepl(Code, LAD11CD)) 
  
  if(!is.null(type)){
    pricesTemp <- pricesTemp %>%
      filter(X15 == type)
  }
  
  #Removes all unwanted property types, useful for removing Other type
  if(!is.null(PropertyTypes)){
    pricesTemp <- pricesTemp %>%
      filter(X5 %in% PropertyTypes)
  }


  #The  model used to take multiple councils if there was overlap, but this caused some strange
  #Problems So I removed it. The current version only takes a single LAD
  pricesTemp %>%
    select( LSOA11CD, Price =X2, X15, PropType = X5, CountryClass, MSOA11CD) %>%
    mutate(Price = as.numeric(Price)) #there was some number overflow thing changing to numeric solves this
  
}
