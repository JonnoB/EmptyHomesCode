LoadAddDistribCompare<-function(FileName = "BootStrapRES.rds", type = NULL){
# This function smooths the process of loading the BootStrapRES file and adding new LADs to it
#   It Also allows for type A and B purchases only to be bootstrapped
#   This gives a more nuanced picture of the purchases and some analysis suggests
#   that when only Type A are inlcuded certain LADs swing from negative to 
#   positive difference. This function easier investigation of such phenomena

  #FileName: the name of the file you want to load or create
  #type: the purchases you want included, the default is NULL meaning all
        #specifying A or B and a character string will subset the bootstrapping


#Boostrap each LAD
elementnames <- sub("DATA", "", ls(pattern = "DATA", envir = globalenv())) #has to check the global envir not the function envir

if(file.exists(FileName)){
  #this sectio of code loads the old list and appends the new LADs
  
  BootStrapRES<- readRDS(FileName)
  newLADsnames <- sub("DATA", "", ls(pattern = "DATA", envir = globalenv())) %>% 
    .[!(. %in% names(BootStrapRES))] 
  newLADs <- paste0(newLADsnames, "DATA") 
  
  #only bootstrap if there is something to add.
  if(length(newLADsnames)>0){
    newLADsBoot <- newLADs  %>% map(~{print(.x)
      DistribCompareBootstrapper(get(.x), 1652, 1000, type = type)})  
    names(newLADsBoot) <- newLADsnames
    BootStrapRES <- c(BootStrapRES, newLADsBoot)
  }
  
  #re-order the whole lot to be alphabetical
  BootStrapRES <- BootStrapRES [order(names(BootStrapRES))]
  
}else{
  #If the list is not found it bootstrapps all the LADs at once, beware this can take a long time
  
  BootStrapRES <- ls(pattern = "DATA",envir = globalenv()) %>% map(~{print(.x)
    DistribCompareBootstrapper(get(.x), 1652, 1000, type = type)})
  #Give the resulting list the name of each LAD
  names(BootStrapRES)<- elementnames
  
}
#Save the file or updated file with the additional elements
saveRDS(BootStrapRES, file = FileName)

return(BootStrapRES)

}