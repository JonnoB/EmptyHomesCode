AggregateStrappedData <- function(df, GroupVar){
  #Sugar to smooth the aggregation between homes and lowuse in both high an low mem modes
  #df the dataframe to aggregate
  #GroupVar the grouping variables for that process, High mem mode needs to include ID
  
    df %>%
      mutate(LADmedian = median(Price, na.rm = T),
             LADmean = mean(Price, na.rm = T)) %>%
      group_by_(.dots = GroupVar) %>%
      summarise(Counts = n(),
                Value = sum(Price),
                GeogMedian = median(Price),
                GeogMean = mean(Price),
                LADmedian = first(LADmedian),
                LADmean = first(LADmean)) %>%
      ungroup
  
}