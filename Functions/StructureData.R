
StructureData <- function(df, lowuse = NULL, empty = NULL, full = TRUE ){
  #Provides a standardised way of cleaning the data
  #df: data frame of the laoded excel template with only the first 4 columns
  #full: do you want the whole process or partial. a partial process is required to dins out
    #which columns are used for empty and low use.
  #lowuse: the columns of the spread dataframe that contain the discounts used for low use homes
  #empty: Columns used to identityf empty homes. if NULL defaults to the lowuse ones

    if(is.null(lowuse)){
      lowuse <- -1
    } else {
      lowuse <- lowuse
    }
    
    if(is.null(empty)){
      empty <-  lowuse
    } else {
      empty <- empty
    }
    
    df <- df %>%
      setNames(c("Exemption.type", "LSOA_CODE", "X3", "X4")) %>%
      group_by(Exemption.type, LSOA_CODE) %>% summarise(counts = n()) %>% 
      spread(key= Exemption.type, value = counts, fill = 0) %>% 
      setNames(make.names(trimws(names(.))))
    
    if(full){
      df <- df%>%
        mutate(LowUse = rowSums(.[,lowuse]), 
               Empty = rowSums(.[,empty])) %>%
        CalcLowUse(.)
    }
    return(df)
  }
  
  