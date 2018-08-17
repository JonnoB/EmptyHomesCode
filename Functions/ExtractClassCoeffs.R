ExtractClassCoeffs <-function(df, Dependent, TestResample, Formula){
  #SOme sugar to make extracting the model coefficient data easier
  #df: the data frame to build the models
  #TestResample: A resample_partition object
  #Formula: the model formula
  
  TestResample <- rsample2caret(TestResample)
  
  ResOut <- 1:length(TestResample$index) %>% map_df(~{
    
    #The vector of rows used in this model
    trainrows <- TestResample$index[[.x]]
    
    Mod2 <- df  %>%
      rename_(Reference = Dependent) %>%
      slice(trainrows) %>%
      glm(formula = Formula, data = ., family=binomial(link='logit')) %>%
      tidy
    
  })
  
  return(ResOut)
  
}