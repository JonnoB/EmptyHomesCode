BootStrapTtest <- function(df, samps = 10000, seed = 1258){
  #This is a function that bootstraps on the processed data files. NA values will be removed
  #df: the processed datafile
  #samples: the number of boostrap resamples
  #seed: the random seed
  
  df <- df %>% filter(!is.na(MeanPrice))
  
  dfMeanPrice <- (sum(df$MeanPrice*df$counts)/sum(df$counts))
  
  df2<- rep(df$MeanPrice, times = df$LowUse)-dfMeanPrice
  
  set.seed(seed)
  
  bootres <- 1:samps %>% map(~data.frame(res =sample(df2, length(df2), replace = T))) %>%
    bind_cols()
  
  sum(colMeans(bootres)>0)/samps
  
}