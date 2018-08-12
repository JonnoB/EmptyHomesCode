ExtractClassCoeffs <-function(df, Dependent, TestResample, Formula){
  #SOme sugar to make extracting the model coefficient data easier
  #df: the data frame to build the models
  #TestResample: A resample_partition object
  #Formula: the model formula
  
  1:length(TestResample) %>% map_df(~{
    Mod2 <- df  %>%
      rename_(Reference = Dependent) %>%
      slice(as.integer(TestResample[[.x]]$train)) %>%
      glm(formula = Formula, data = ., family=binomial(link='logit')) %>%
      tidy
    
  })
  
}