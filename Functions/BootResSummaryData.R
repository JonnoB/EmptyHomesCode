BootResSummaryData <- function(BootStrapRES){
  
  
  WardData <- ls(pattern = "DATA", envir = globalenv()) %>%
    map_df(~eval(parse(text=.x)) %>%
             group_by(LAD11CD) %>%
             mutate( WardLowUsePerc = ifelse(is.na(WardLowUsePerc),0, WardLowUsePerc),
                     LAD11CDCounts = n()) %>%
             ungroup %>%
             filter(LAD11CDCounts == max(LAD11CDCounts)) %>% #removes values that are classed as another borough
             summarise( maxPerc = max(WardLowUsePerc, na.rm=TRUE),
                        minPerc = min(WardLowUsePerc, na.rm=TRUE),
                        LADPerc = sum(WardLowUsePerc, na.rm=TRUE)/sum(Homes, na.rm=TRUE),
                        diffPerc = maxPerc-minPerc,
                        max = max(WardLowUse, na.rm=TRUE),
                        min = min(WardLowUse, na.rm=TRUE),
                        diff = max-min,
                        mean = mean(WardLowUse, na.rm=TRUE),
                        median = median(WardLowUse, na.rm=TRUE),
                        Pop = sum(Pop, na.rm = TRUE),
                        LUPS = sum(LowUse, na.rm = TRUE),
                        Homes = sum(Homes, na.rm = TRUE),
                        LAD = sub("DATA", "", .x),
                        TurnoverPerc = sum(PercTurnover*Homes, na.rm = TRUE)/sum(Homes, na.rm = TRUE),
                        LUPTurnoverPerc = sum(PercTurnover*LowUse, na.rm = TRUE)/sum(LowUse, na.rm = TRUE),
                        LAD11CD = first(LAD11CD)
                        )
           )
  
  
  BootDeets <- BootStrapRES %>% map_df(~.x %>% group_by(ID) %>%
                                    summarise(MedianHomes = first(MedianHomes),
                                              MeanHomes = first(MeanHomes),
                                              MedianLUPs = first(MedianLUPs),
                                              MeanLUPs = first(MeanLUPs),
                                              LAD11CD= first(LAD11CD),
                                              HighVal = sum(HighVal)) %>%
                                    group_by(LAD11CD) %>%
                                    summarise_all(., mean, na.rm = TRUE) %>%
                                      select(-ID)
  )
  
  #Perform the t-test on the resulting lists
  TtestRESgreater <-BootStrapRES %>% map(~ValueTtest(.x))
  #Tidy results of thejust the ttest
  TtestRES2 <- TtestRESgreater %>% map_df(~.x$t.test %>% tidy) %>%
    select(p.value) %>%
    mutate(HomePrice = TtestRESgreater %>% map_dbl(~.x$HomePrice),
           LowUsePrice = TtestRESgreater %>% map_dbl(~.x$LowUsePrice),
           absdiff = TtestRESgreater %>% map_dbl(~.x$absdiff),
           Percdiff = TtestRESgreater %>% map_dbl(~.x$Percdiff)) %>%
    rename(Ttest.p.value = p.value)
  

  
  Out <-   bind_cols(BootDeets, TtestRES2) %>% #BootDeets and TtestRES2 are both in the same order so can be column bound
    left_join(., WardData, by = "LAD11CD" )

  return(Out)
  
}