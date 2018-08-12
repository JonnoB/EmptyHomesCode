LinearResampledModelPerf <- function(Modeldf, TestResample, ModelFormula){
  #Cretes a dataframe with the Classification performance of many resampled models.
  #This allows the same resample set to be used across multiple model builds
  
  #The Outcome variable HAS TO BE CALLED REFERENCE!
  
  #Modeldf: the dataframe that will be used as the data set of the model
  #TestResample, a list of esample_partition objects based on Modeldf
  #ModelFormula the formula used for the model. created using as.formula()
  
  ResOut <- 1:length(TestResample) %>% map_df(~{
    
    #The vector of rows used in this model
    trainrows <- as.integer(TestResample[[.x]]$train)
    testrows <- as.integer(TestResample[[.x]]$test)  
    
    Mod2 <- Modeldf  %>%
      slice(trainrows) %>%
      lm(formula = ModelFormula, data = .)
    
    preds2 <- Modeldf  %>%
      slice(testrows) %>%
      predict(Mod2, newdata = . ) 
    
    Refs <- Modeldf  %>%
      slice(testrows) %>%
      pull(Reference)
    
    ConfOut <-preds2 %>%
      postResample(pred = ., Refs) %>% t %>% data.frame() %>% as.tibble %>%
      mutate(sample = .x)
    
    
  })
  return(ResOut)
  
}