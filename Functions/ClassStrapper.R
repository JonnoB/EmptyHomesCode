ClassStrapper <- function(PriceData, LowUseCounts, reps = 1000){
  
  #PriceData: A data frame of three columns, the LSOA code, the price of the sale, the class of the sale
  #LowUseCounts: The number of low use homes in that local authority.
  #reps: The number of samples wanted
  
  #generate random indexes to sample
  SampBlock <- sample(1:nrow(PriceData), LowUseCounts*reps, replace = TRUE ) %>% 
    matrix(data = ., ncol = reps)
  

  Out <- 1:reps %>% map_df(~PriceData[,"class"] %>% 
                             slice(SampBlock[,.x]) %>%
                             group_by(class) %>%
                             summarise(Counts = n()) %>%
                             mutate(ID = .x)
  )
  
  return(Out)
}
