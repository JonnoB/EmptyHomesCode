ResampledModelPerfPreds <- function(Modeldf, TestResample, ModelFormula, LADData = NULL){
  #Creates a dataframe with the Classification performance of many resampled models.
  #This allows the same resample set to be used across multiple model builds
  
  #The Outcome variable HAS TO BE CALLED REFERENCE!
  
  #Modeldf: the dataframe that will be used as the data set of the model
  #TestResample, an vfold_cv object
  #ModelFormula the formula used for the model. created using as.formula()
  
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
    
    Mod2 <- Modeldf  %>%
      slice(trainrows) %>%
      glm(formula = ModelFormula, data = ., family=binomial(link='logit'))
    
    preds2 <- Modeldf  %>%
      slice(testrows) %>%
      predict(Mod2, newdata = ., type = "response" ) 
    
    Refs <- Modeldf  %>%
      slice(testrows) %>%
      pull(Reference)
    
    tibble(Predictions = preds2, Reference = Refs, sample = .x) 
    
    
  })
  return(ResOut)
  
}