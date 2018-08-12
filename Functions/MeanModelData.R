MeanModelData <- function(ModelData, target){
  #This aggregates the bootstrap data
  #ModelData the output of the function CreateGeogModelData
  #target: either "LAD" or "MSOA" depending on how CreateGeogModelData was used
  
  ModelData  %>%
    group_by_(paste0(target, "11CD")) %>%
    summarise(Homes = mean(Homes),
              LowUse = mean(LowUse),
              Guest = mean(Guest),
              Hotel = mean(Hotel),
              HomesMedian = mean(HomesMedian),
              LowUseMedian = mean(LowUseMedian),
              Yearly.income = mean(Yearly.income)) %>%
    mutate(AffordRatio = HomesMedian/Yearly.income,
           LowUsePerc = LowUse/Homes,
           LowUseRank = percent_rank(LowUsePerc),
           Tourism = Guest +Hotel,
           HomesMedianRank = percent_rank(HomesMedian),
           LowUseMedianRank = percent_rank(LowUseMedian),
           MedianDiff = LowUseMedian-HomesMedian,
           MedianDiffRatio = (LowUseMedian-HomesMedian)/HomesMedian,
           MedianDiffRank = percent_rank(MedianDiff),
           AffordRank = percent_rank(AffordRatio),
           TourismDensity = percent_rank((Guest+Hotel)/Homes),
           HighVal = as.factor(HomesMedian < LowUseMedian), #Reference Variable
           HighLUP = as.factor(median(LowUsePerc) < LowUsePerc)) #Reference Variable
  
  
}