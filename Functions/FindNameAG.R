FindNameAG <- function(name){grep(name, AG$LAD11NM, ignore.case = T, value = T) %>% unique}
#matches a partial string to all the relevenat LAD names in the AG dataframe