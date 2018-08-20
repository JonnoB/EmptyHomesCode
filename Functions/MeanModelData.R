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
              Ego1TourismDens = mean(Ego1TourismDens),
              Ego2TourismDens = mean(Ego2TourismDens),
              HomesMedian = mean(HomesMedian),
              HomesMean = mean(HomesMean),
              LowUseMedian = mean(LowUseMedian),
              LowUseMean = mean(LowUseMean),
              Yearly.income = mean(Yearly.income),
              Region = first(Region),
              HomesMedianOld = mean(HomesMedianOld),
              HomesMeanOld = mean(HomesMeanOld),
              HighValCount =  sum(ifelse(HighVal==TRUE,1,0))) %>%
    mutate(AffordRatio = HomesMean/Yearly.income,
           AffordRatioScale2 = (scale(AffordRatio)^2),
           AffordRatioScale3 = (scale(AffordRatio)^3),
           AffordRatioScale = ((AffordRatio)),
           Ego1TourismDens2 = (scale(Ego1TourismDens)^2),
           Ego2TourismDens2 = (scale(Ego2TourismDens)^2),
           LowUsePerc = LowUse/Homes,
           LowUseRank = percent_rank(LowUsePerc),
           Tourism = Guest +Hotel,
           MeanMedianRatio = HomesMean/HomesMedian,
           PercChange = (HomesMean-HomesMeanOld)/HomesMeanOld,
           HomesMedianRank = percent_rank(HomesMedian),
           LowUseMedianRank = percent_rank(LowUseMedian),
           MedianDiff = LowUseMedian-HomesMedian,
           MedianDiffRatio = (LowUseMedian-HomesMedian)/HomesMedian,
           MedianDiffRank = percent_rank(MedianDiff),
           AffordRank = percent_rank(AffordRatio),
           TourismDensity = (Guest+Hotel)/Homes,
           HighVal = as.factor(HomesMedian < LowUseMedian), #Reference Variable
           HighLUP = as.factor(median(LowUsePerc) < LowUsePerc),
           HighLUP2 = as.factor(quantile(LowUsePerc, 0.75)< LowUsePerc)) #Reference Variable
  
  
}