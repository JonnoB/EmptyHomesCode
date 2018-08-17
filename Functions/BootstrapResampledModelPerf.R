BootstrapResampledModelPerf <- function(Modeldf, trainrows, testrows, ModelFormula, ModsOut =FALSE){
  #Creates a dataframe with the Classification performance of many resampled models.
  #This allows the same resample set to be used across multiple model builds
  
  #The Outcome variable HAS TO BE CALLED REFERENCE!
  
  #Modeldf: the dataframe that will be used as the data set of the model
  #trainrows the rows to be used in training
  #testrows the rows to be used in testing
  #ModelFormula the formula used for the model. created using as.formula()
  #ModsOut if the models will be returned or not
  
  #TestResample <- rsample2caret(TestResample)
  
  ResOut <- 1:max(Modeldf$ID) %>% map(~{
    #print(.x)
    #The vector of rows used in this model
  #  trainrows <- TestResample$index
  #  testrows <- TestResample$indexOut 
    
    Modeldf <- Modeldf %>%
      filter(ID==.x)
    
    Mod2 <- Modeldf  %>%
      slice(trainrows) %>%
      glm(formula = ModelFormula, data = ., family=binomial(link='logit'))
    
      if(ModsOut){
        
        Mod2
        
      }else{
        preds2 <- Modeldf  %>%
        slice(testrows) %>%
        predict(Mod2, newdata = ., type = "response" ) 
    
        data.frame(preds2) %>%
          setNames(paste0("Sim", .x))
        }
    
  }) 
  
  if(!ModsOut){
    
    ResOut <- ResOut %>%
      bind_cols
  }

  
  return(ResOut)
  
}
