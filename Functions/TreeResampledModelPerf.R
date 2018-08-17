TreeResampledModelPerf <- function(Modeldf, TestResample, ModelFormula, LADData = NULL){
  #Cretes a dataframe with the Classification performance of many resampled models.
  #This allows the same resample set to be used across multiple model builds
  
  #The Outcome variable HAS TO BE CALLED REFERENCE!
  
  #Modeldf: the dataframe that will be used as the data set of the model
  #TestResample, a vfold_cv object
  #ModelFormula the formula used for the model. created using as.formula()
  #LADDaata a data frame of the LAD dataset. is used to identify which lads are included when using overall LAD data
  
  TestResample <- rsample2caret(TestResample)
  
  ResOut <- 1:length(TestResample$index) %>% map_df(~{
    
    #The vector of rows used in this model
    trainrows <- TestResample$index[[.x]]
    testrows <- TestResample$indexOut[[.x]]  
    
    if(!is.null(LADData)){
      #Include data by LAD
      LADS <- LADData %>% slice(trainrows) %>% pull(LAD11CD) %>% unique
      trainrows <-(1:nrow(Modeldf))[Modeldf$LAD11CD %in% LADS]
      LADS <- LADData %>% slice(testrows) %>% pull(LAD11CD) %>% unique
      testrows <-(1:nrow(Modeldf))[Modeldf$LAD11CD %in% LADS]
      
    }
    
    print(.x)
    Mod2 <- Modeldf %>%
      slice(trainrows) %>%
      ranger(formula = ModelFormula , data = . )
    
    
    preds2<- Modeldf  %>%
      slice(testrows) %>%
      predict(Mod2, ., type = "response")
    
    preds2 <- preds2$predictions
    
    Refs <- Modeldf  %>%
      slice(testrows) %>%
      pull(Reference)
    
    ConfOut <-confusionMatrix( data =preds2, reference = Refs)
    
    ConfOut$overall %>% t %>% data.frame() %>% as.tibble %>%
      mutate(sample = .x)
    
    
  })
  return(ResOut)
  
}