ClassStrapper <- function(PriceData, LowUseCounts){
  SampBlock <- sample(1:nrow(PriceData), LowUseCounts*1000, replace = TRUE ) %>% 
    matrix(data = ., ncol = 1000)
  
  Out <- PriceData[,3] %>% 
    slice(SampBlock[,1]) %>%
    group_by(class) %>%
    summarise(Counts = n()) %>%
    mutate(ID = 1)
  
  
  Out <- 1:1000 %>% map_df(~PriceData[,3] %>% 
                             slice(SampBlock[,.x]) %>%
                             group_by(class) %>%
                             summarise(Counts = n()) %>%
                             mutate(ID = .x)
  )
  
  return(Out)
}
