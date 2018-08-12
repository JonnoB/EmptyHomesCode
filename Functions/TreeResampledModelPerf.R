TreeResampledModelPerf <- function(Modeldf, TestResample, ModelFormula, CombineData = FALSE){
  #Cretes a dataframe with the Classification performance of many resampled models.
  #This allows the same resample set to be used across multiple model builds
  
  #The Outcome variable HAS TO BE CALLED REFERENCE!
  
  #Modeldf: the dataframe that will be used as the data set of the model
  #TestResample, a list of esample_partition objects based on Modeldf
  #ModelFormula the formula used for the model. created using as.formula()
  #CombineData: the data set includes outcome variables for LADs and MSOA meaning the subsetting has to be by LAD
  
  ResOut <- 1:length(TestResample) %>% map_df(~{
    
    if(CombineData){
      
      LADS <- TestResample[[.x]]$train %>% as.data.frame() %>% pull(LAD11CD) %>% unique
      trainrows <-(1:nrow(Modeldf))[Modeldf$LAD11CD %in% LADS]
      LADS <- TestResample[[.x]]$test %>% as.data.frame() %>% pull(LAD11CD) %>% unique
      testrows <-(1:nrow(Modeldf))[Modeldf$LAD11CD %in% LADS]
      
    }else{
    
      trainrows <- as.integer(TestResample[[.x]]$train)
      testrows <- as.integer(TestResample[[.x]]$test)  
      
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