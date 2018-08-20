SummariseModels <- function(df,...){
  #Summarises the output of the resampled model performance function
  #df the out put df from resampled performance
  #Grouing any grouping variable that should be used. This is useful when multiple model params are in the df
 
  group_var <- quos(...) 

  df%>%
    group_by(!!!group_var) %>%
    summarise('Beats NULL' = sum(Accuracy>AccuracyNull)/n(), #How many times did the models beat NULL
              'Mean Accuracy' = mean(Accuracy), #What was the mean accuracy
              'Median Accuracy' = median(Accuracy),
              'sd Accuracy' = sd(Accuracy),
              "PosPred" = mean(Pos.Pred.Value),
              "NegPred" = mean(Neg.Pred.Value),
              'NULL Model' = mean(AccuracyNull),
              'Null Difference' = mean(Accuracy-AccuracyNull) #Mean difference relative to NULL
    ) %>% ungroup
  
  
}