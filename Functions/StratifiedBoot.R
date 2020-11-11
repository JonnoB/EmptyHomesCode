StratifiedBoot <- function(dataList, LSOAcode, SamplesVect, samples = 1000){
  #dataList, A list of vectors of sales prices in each LSOA
  #LSOAcode 
  #LSOAcode a character vector of all the LSOA to be sampled
  #SamplesVect a vector of the number of observed prices to be drawn each round
  #samples the number of times to repeat the process
  
  1: samples %>% map_df(~{
    print(.x)
    SubData <- 1:length(LSOAcode) %>% map(~{
      #print(.x)
      dataList[[LSOAcode[.x]]] %>%
        sample(., SamplesVect[.x], replace = TRUE)
      
    }) %>% unlist 
    
    SubData%>%
      quantile(.) %>% t %>% as.data.frame %>% setNames(names(.) %>% make.names) %>%
      mutate(mean = mean(SubData),
             ID = .x)
  }) 
}