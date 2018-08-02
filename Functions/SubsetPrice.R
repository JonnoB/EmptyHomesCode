SubsetPrice <- function(df, type = NULL, PropertyTypes = NULL, Quantiles = NULL){
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
  
  if(is.null(Quantiles)){
    Quantiles<-c(0, quantile(pricesTemp$X2)[2:4], Inf)
  }

  #The  model used to take multiple councils if there was overlap, but this caused some strange
  #Problems So I removed it. The current version only takes a single LAD
  pricesTemp %>%
    select(Admin_ward_code, LSOA11CD, Price =X2, X15, PropType = X5, CountryClass, MSOA11CD) %>%
    mutate(class = cut(Price, Quantiles, 
                       labels =     c("Lower", "Lower-Mid", "Upper-Mid", "Upper"), 
                       right = F) %>% fct_relevel(., "Upper", after = 3),
           classVal = cut(Price, Quantiles, 
                          labels =     c(Quantiles[2:4], paste0("x>", Quantiles[5])), 
                          right = F),
           Price = as.numeric(Price)) #there was some number overflow thing changing to numeric solves this
  
}
