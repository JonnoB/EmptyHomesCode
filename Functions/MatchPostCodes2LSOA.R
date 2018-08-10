MatchPostCode2LSOA<-function(ONSpostcodes, ONSpostcodeMeta){
  #Matches all postcodes to the relevant LSOA
  #The function uses the ONS code to point to postcode data which can be found at
  #https://www.ordnancesurvey.co.uk/business-and-government/products/code-point-open.html
  #This data set is updated regularly
  #ONSpostcodes: the path of the ONS postcodes CSVs
  #ONSpostcodeMeta: the path to the meta data

  setwd(ONSpostcodes)

postcodes <- lapply(list.files(), function(n){
  read.csv(n, header = FALSE, stringsAsFactors = FALSE)}
) %>% bind_rows()

setwd(ONSpostcodeMeta)
names(postcodes) <- read.csv("Code-Point_Open_Column_Headers.csv")[1,] %>%t

#set the Northings an eastings columns to be the coordinates
coordinates(postcodes) <- ~Eastings + Northings
#defines the coordinate system
proj4string(postcodes) <-CRS("+init=epsg:27700") # CRS("+proj=tmerc") 
#converts the coordinate system from the one defined to the coordinate system of the shape file, this step is not neccesary in this case as both are using the Uk grid referencing system.
postcodes <- spTransform(postcodes, proj4string(shape))

#You can plot great britain if you want to check it has worked but it takes a while!
#plot(postcodes)

PostInLSOA <- over(postcodes, shape)

#Check which LSOA each postcode falls within and bind it to the postcode data
PstCdLSOA.raw <- bind_cols(tidy(postcodes), PostInLSOA)

#remove any NA values
PstCdLSOA.raw <- PstCdLSOA.raw %>% filter(!is.na(PstCdLSOA.raw$lsoa11cd))

#Save the file as it is large and making it multiple times is annoying
#setwd(DataFolder)
#savedRDS(PstCdLSOA.raw, "PstCdLSOA.rds")

return(PstCdLSOA.raw)

}