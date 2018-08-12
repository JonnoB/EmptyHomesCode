MSOATreeVoting <- function(Modeldf, TestResample, ModelFormula){
  #This model predicts a LAD outcome at MSOA level, It then uses majority vote to 
  #choos the class of the over all LAD
  #Cretes a dataframe with the Classification performance of many resampled models.
  #This allows the same resample set to be used across multiple model builds
  
  #The Outcome variable HAS TO BE CALLED REFERENCE!
  
  #Modeldf: the dataframe that will be used as the data set of the model
  #TestResample, a list of esample_partition objects based on Modeldf
  #ModelFormula the formula used for the model. created using as.formula()

  
  ResOut <- 1:length(TestResample) %>% map_df(~{
    

      LADS <- TestResample[[.x]]$train %>% as.data.frame() %>% pull(LAD11CD) %>% unique
      trainrows <-(1:nrow(Modeldf))[Modeldf$LAD11CD %in% LADS]
      LADS <- TestResample[[.x]]$test %>% as.data.frame() %>% pull(LAD11CD) %>% unique
      testrows <-(1:nrow(Modeldf))[Modeldf$LAD11CD %in% LADS]

    
    
    print(.x)
    Mod2 <- Modeldf %>%
      slice(trainrows) %>%
      ranger(formula = ModelFormula , data = . )
    
    
    preds2<- Modeldf  %>%
      slice(testrows) %>%
      predict(Mod2, ., type = "response")
    
    preds2 <- Modeldf  %>%
      slice(testrows) %>% 
      select(Reference, LAD11CD) %>%
      mutate(Predictions = ifelse(preds2$predictions=="TRUE",1,0),
             Reference = ifelse(Reference=="TRUE",1,0)) %>%
      group_by(LAD11CD) %>%
      summarise(Predictions = mean(Predictions)>0.5,
                Reference = mean(Reference)>0.5)
    
    ConfOut <-confusionMatrix( data =preds2$Predictions, reference = preds2$Reference)
    
    ConfOut$overall %>% t %>% data.frame() %>% as.tibble %>%
      mutate(sample = .x)
    
    
  })
  return(ResOut)
  
}