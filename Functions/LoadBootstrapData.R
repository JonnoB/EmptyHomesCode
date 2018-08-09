LoadBootstrapData <-function(path= getwd()){
  #Loads all the Bootrapped files into a named list
  #Once all Bootratps complete load them all into a list
  BootStrapRES <- list.files(path) %>%
    map(~readRDS(.x))
  
  #Name the list by the LAD code
  names(BootStrapRES) <- list.files(path) %>% gsub(".rds", "",.)
  
  return(BootStrapRES)
}