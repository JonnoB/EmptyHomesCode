LinearResampledModelPerf <- function(Modeldf, TestResample, ModelFormula){
  #Cretes a dataframe with the Classification performance of many resampled models.
  #This allows the same resample set to be used across multiple model builds
  
  #The Outcome variable HAS TO BE CALLED REFERENCE!
  
  #Modeldf: the dataframe that will be used as the data set of the model
  #TestResample, a vfold_cv oject
  #ModelFormula the formula used for the model. created using as.formula()
  
  TestResample <- rsample2caret(TestResample)
  
  ResOut <- 1:length(TestResample$index) %>% map_df(~{
    
    #The vector of rows used in this model
    trainrows <- TestResample$index[[.x]]
    testrows <- TestResample$indexOut[[.x]]  
    
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