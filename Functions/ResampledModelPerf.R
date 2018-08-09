ResampledModelPerf <- function(Modeldf, TestResample, ModelFormula){
  #Cretes a dataframe with the Classification performance of many resampled models.
  #This allows the same resample set to be used across multiple model builds
  
  #The Outcome variable HAS TO BE CALLED REFERENCE!
  
  #Modeldf: the dataframe that will be used as the data set of the model
  #TestResample, a list of esample_partition objects based on Modeldf
  #ModelFormula the formula used for the model. created using as.formula()
  
  ResOut <- 1:length(TestResample) %>% map_df(~{
    
    Mod2 <- Modeldf  %>%
      mutate(Reference = as.factor(Reference)) %>%
      slice(as.integer(TestResample[[.x]]$train)) %>%
      glm(formula = ModelFormula, data = ., family=binomial(link='logit'))

    Mod2 <- Modeldf  %>%
      mutate(Reference = as.factor(Reference)) %>%
      slice(as.integer(TestResample[[.x]]$train)) %>%
      glm(formula = ModelFormula, data = ., family=binomial(link='logit'))
    
    
    preds2<- Modeldf  %>%
      mutate(Reference = as.factor(Reference)) %>%
      slice(as.integer(TestResample[[.x]]$test)) %>%
      predict(Mod2, ., type = "response")
    
    ConfOut<-Modeldf  %>%
      mutate(Reference = as.factor(Reference)) %>%
      slice(as.integer(TestResample[[.x]]$test)) %>%
      pull(Reference) %>%
      confusionMatrix(.,preds2>0.5)
    
    ConfOut$overall %>% t %>% data.frame() %>% as.tibble
    
    
  })
  return(ResOut)
  
}
