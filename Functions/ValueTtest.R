ValueTtest <- function(df){
#Performs a t test on the results of the bootstrapping function
  #df: the output from the boostrapping function
  #the percentage difference is relative to regular homes so positvevalue means lowuse more expensive
  #The test performs a difference of means on the mean values of the homes and the low use homes from each bootstrapped simulation
  
  test <- df %>% 
    select(ID, Homes, LowUse, HomesValue, LowUseValue) %>%
    group_by(ID) %>%
    summarise_all(funs(sum)) %>%
    mutate(HomesPrice = HomesValue/Homes,
           LowUsePrice = LowUseValue/LowUse)
  
  res1 <- t.test(x= test$LowUsePrice, y= test$HomesPrice, alternative = "greater")
  
  HomePrice <- (sum(test$HomesValue, na.rm = TRUE)/sum(test$Homes, na.rm = TRUE))
  LowUsePrice <- (sum(test$LowUseValue, na.rm = TRUE)/sum(test$LowUse, na.rm = TRUE))
  
  res2 <- (LowUsePrice - HomePrice)
  res3 <- (res2/HomePrice)*100
  
  Out <-list(test, res1, res2, res3)
  names(Out) <- c("data.frame", "t.test", "absdiff", "Percdiff")
  return(Out)
}
