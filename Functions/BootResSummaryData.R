BootResSummaryData <- function(BootStrapRES){
  
  
  #gets the list of LADs we currently have data for in alphabetical order so they can be joined to the combine
  CurrentLADS <- ls(pattern = "DATA", envir = globalenv()) %>%
    map_chr(~{df<- get(.x)
    df <- df %>%
      group_by(LAD11CD) %>%
      summarise(count = n())
    df$LAD11CD[which.max(df$count)]}
    )
  
  #Perform the t-test on the resulting lists
  TtestRESgreater <-BootStrapRES %>% map(~ValueTtest(.x))
  #TtestREStwo.sided <-BootStrapRES %>% map(~ValueTtest(.x, alternative = "two.sided"))
  #Tidy results of thejust the ttest
  TtestRES2 <- TtestRESgreater %>% map_df(~.x$t.test %>% tidy) %>%
    select(p.value) %>%
    mutate(LAD = elementnames,
           LAD11CD = CurrentLADS,
           HomePrice = TtestRESgreater %>% map_dbl(~.x$HomePrice),
           LowUsePrice = TtestRESgreater %>% map_dbl(~.x$LowUsePrice),
           absdiff = TtestRESgreater %>% map_dbl(~.x$absdiff),
           Percdiff = TtestRESgreater %>% map_dbl(~.x$Percdiff)) 
  
  #Removing ANOVA as it causes probs when only tyoe a are included
  #Perform ANOVA on the difference between the ratios of the groups on the bootrapped list
  # ANOVARES <- BootStrapRES %>% 
  #   map_df(~aov(.x$RatioExvsAct~.x$class) %>% tidy) %>% 
  #   filter(term != "Residuals") %>%
  #   select(-term) %>%
  #   mutate(LAD = elementnames)
  
  WardData <- ls(pattern = "DATA", envir = globalenv()) %>%
    map_df(~eval(parse(text=.x)) %>%
             #select(WardLowUsePerc, WardLowUse) %>%
             mutate( WardLowUsePerc = ifelse(is.na(WardLowUsePerc),0, WardLowUsePerc)) %>%
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
                        LUPTurnoverPerc = sum(PercTurnover*LowUse, na.rm = TRUE)/sum(LowUse, na.rm = TRUE)
                        )
           )
  
  
  skew <- BootStrapRES %>% map_df(~.x %>% group_by(class) %>%
                                    summarise(skew = mean(RatioExvsAct, na.rm = TRUE)) %>%
                                    summarise(skew = skewness(skew))
  )
  
  
  Out <- left_join(WardData, TtestRES2, by = "LAD" ) %>% bind_cols(skew) %>%
    rename(Ttest.p.value = p.value)

    # Out <- left_join(ANOVARES, TtestRES2, by = "LAD" ) %>% left_join(WardData,by = "LAD" ) %>% bind_cols(skew) %>%
  #   select(-sumsq,-meansq,-statistic.x, -estimate, -estimate1, -estimate2, -statistic.y) %>%
  #   rename(ANOVA = p.value.x,
  #          Ttest.p.value = p.value.y)
  
  return(Out)
  
}