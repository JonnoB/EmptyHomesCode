##This code is used to setup the project it is common to both the empty homes doc and also the template creation doc.


packages <- c("tidyverse", "spdep", "igraph", "maptools", "openxlsx", "readxl", "sp","rgdal", "rgeos", "raster", "broom", "leaflet",
              "forcats", "e1071", "htmlwidgets", "stringr", "modelr", "caret", "xtable", "rpart", "broom", "yardstick", "rsample")

#, "mapview"
new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


lapply(packages, library, character.only = TRUE)



#Set up file system to read the correct folders this switches between aws and windows mode
basewd <-"~/Dropbox/SSE/Empty Homes"

DataFolder <- file.path(basewd, "Data")
ShapeFiles <- file.path(basewd,"ShapeFiles/statistical-gis-boundaries-london/ESRI")
PostcodeLookups <- file.path(DataFolder, "PostcodeLookups")
ExcelLookups <- file.path(basewd, "ExcelLookups")
ONSpostcodes <- file.path(basewd, "ONS postcodes May17", "Data", "CSV")
ONSpostcodeMeta <- file.path(basewd, "ONS postcodes May17", "Doc")
LSOAshapedata <- file.path(basewd,"ShapeFiles",
                           "Lower_Layer_Super_Output_Areas_December_2011_Generalised_Clipped__Boundaries_in_England_and_Wales")
AddGeog <- file.path(basewd, "AdditionalGeographies")
#The regions folder is for the excel templates of each region of the UK
Regions <- file.path(basewd, "Regions")
RegionsComp <- file.path(basewd, "Regions-Complete")
CommonCode <- file.path(basewd, "EmptyHomesCode", "SubCode")
Functions <- file.path(basewd, "EmptyHomesCode", "Functions")
SemiLondon <- file.path(basewd, "semi-data-london")
Figures <- "/home/jonno/Dropbox/Apps/ShareLaTeX/Empty Homes Write up/Figures" #file.path(basewd, "Figures")
TexTables <- "/home/jonno/Dropbox/Apps/ShareLaTeX/Empty Homes Write up/Tables"

select <-dplyr::select


