LinearClassifierCombiner <- function(df, Outcome, Formulas, Samples,  Classifier = TRUE){
  #This model allows the creation of multiple glm classifiers
  #The model OutPuts a data frame of model performance
  #df <- the dataset
  #Outcome: a vector of Outcome Variables
  #Formulas: a vector of formular objects
  #Samples <- a resample_partition object
  
  Combos <-expand.grid(1:length(Outcome),1:length(Formulas))
  
  ModelPerf <- map2_df(.x = Outcome[Combos[,1]], .y = Formulas[Combos[,2]],
                       ~{
                         print(paste(.x,.y))
                         Temp <- df %>% 
                           rename_(Reference = .x ) 
                        
                         if(Classifier) {
                          
                          Temp <- Temp  %>%
                             ResampledModelPerf(., Samples,  .y) 
                         } else {
                           
                           Temp <- Temp %>%
                             LinearResampledModelPerf(., Samples,  .y) 
                         }
   
                        Temp %>%
                           mutate(Dependent = .x,
                                  Formula = toString(.y))
                         
                       } )
  

  return(ModelPerf)
  
}