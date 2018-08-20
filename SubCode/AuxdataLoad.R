#VOA data

#Council tax bands from VOA
#https://www.gov.uk/government/statistics/council-tax-stock-of-properties-2016

#Load the LSOA shape data for EW 



# Load Post code data

#needed for the LAD names
if(file.exists(file.path(DataFolder, "CorePstCd.rds"))){
  
  CorePstCd <- readRDS(file.path(DataFolder, "CorePstCd.rds"))
  
} else {
  
  
  print("Loading Postcode data")
  LADconv <- read_csv(file.path(AddGeog, 
                                "Local_Authority_Districts_December_2016_Names_and_Codes_in_the_United_Kingdom.csv")) %>%
    select(LAD11CD=LAD16CD, LAD11NM = LAD16NM)
  
  #This is used purely for westminster
  LSOANM <- read_csv(file.path(PostcodeLookups,"PCD11_OA11_LSOA11_MSOA11_LAD11_EW_LU_aligned_v2.csv")) %>%
    select(LSOA11CD, LSOA11NM) %>%
    distinct(LSOA11CD, .keep_all = TRUE)
  
  #uses the ONSPD dataset, this contains all postocodes including terminated ones and links them to the LSOA system.
  CorePstCd  <- read_csv(file.path(ONSpostcodes, 
                              "ONS_Postcode_Directory_Latest_Centroids.csv")) %>%
    select(Postcode = pcd, LSOA11CD = lsoa11, MSOA11CD = msoa11, LAD11CD = oslaua, OA11CD = oa11,Country_code = ctry, Region =  rgn) 
  
  CorePstCd <- CorePstCd %>%
    filter(grepl("(E92000001)|(W92000004)", Country_code)) %>%
    left_join(LADconv) %>%
    left_join(LSOANM) %>%
    filter(!is.na(LSOA11CD)) %>% 
    mutate(Postcode = gsub(" ", "", Postcode)) %>%
    select(Postcode,LSOA11CD, LAD11CD, LAD11NM, MSOA11CD, Country_code, LSOA11NM, Region)
    
  
  saveRDS(CorePstCd, file.path(DataFolder, "CorePstCd.rds"))
  
  rm(EW); rm(LSOANM);rm(LADconv)
}


setwd(DataFolder)
EW <- inner_join(read_csv("Table_CTSOP1.1_2016.csv") %>%
                   select( ECODE, ALL_PROPERTIES),
                 read_csv("SAPE2.csv") %>% select( `Area Codes`, `All Ages`),
                 by=c("ECODE" = "Area Codes")) %>% setNames(c("LSOA11CD", "Homes", "Pop"))

EW2 <- CorePstCd %>% 
  select(-Postcode) %>%
  distinct(LSOA11CD, .keep_all = TRUE) %>%
  left_join(EW, by= "LSOA11CD") 

rm(EW)

#Convert shape file to dataframe and join in the LSOA data
#
#
#
####Not necessary for paper only used for making leaflets
# if(file.exists(file.path(DataFolder,"shapeframe.rds"))){
# AG<- readRDS(file.path(DataFolder,"shapeframe.rds"))
# } else{
# setwd(LSOAshapedata)
# 
# shape <- readOGR(dsn = list.files(pattern = "shp"))
# 
# AG <- fortify(shape, region = "objectid")
# 
# AG <- AG %>% mutate(id = as.integer(id)) %>% 
# left_join(., shape@data, by = c("id" = "objectid")) %>% 
# left_join(., EW2, by = c("lsoa11cd" = "ECODE"))
# 
# saveRDS(AG, file.path(DataFolder,"shapeframe.rds"))
# }


#get prices
#
#
#
setwd(DataFolder)

#Need to replace the prices file with the new larger prices file

if(file.exists("prices.rds")){
  
  prices <- readRDS("prices.rds")
} else {
  #if you don't have enough ram this might crash your computer.
  prices <- read_csv("pp-complete.csv", col_names = FALSE )
  
  prices <- prices %>%
    filter(X5 %in% c("D", "S", "T", "F")) %>% #The property types, filtering hear greatly reduces the size of the vector
    filter(year(X3)>2012,year(X3)<2018) %>%
    mutate(X4 = gsub(" ", "", X4)) 
  
  prices <- prices %>%
    left_join(., CorePstCd,
              by = c("X4"="Postcode")) %>%
    filter(!is.na(LSOA11CD)) %>%
    select(X2, X5, X3, X15, X16, LSOA11CD, MSOA11CD, LAD11CD) 

  saveRDS(prices, "prices.rds")
  
}

if(!file.exists("pricesOld.rds")){
  
  pricesOld <- read_csv("pp-complete.csv", col_names = FALSE )
  
  pricesOld <- pricesOld %>%
    filter(X5 %in% c("D", "S", "T", "F")) %>% #The property types, filtering hear greatly reduces the size of the vector
    filter(year(X3)>2002,year(X3)<2008) %>%
    mutate(X4 = gsub(" ", "", X4)) 
  
  pricesOld <- pricesOld %>%
    left_join(., CorePstCd,
              by = c("X4"="Postcode")) %>%
    filter(!is.na(LSOA11CD)) %>%
    select(X2, X5, X3, X15, X16, LSOA11CD, MSOA11CD, LAD11CD) 
  
  saveRDS(pricesOld, "pricesOld.rds")
  rm(pricesOld)
} 

#Was previously ward until I increased the number of years to allow the use of LSOA


if(file.exists("MeanWardPrice.rds")){
  
  MeanWardPrice <- readRDS("MeanWardPrice.rds")
} else {
  #if you don't have enough ram this might crash your computer.
  MeanWardPrice <- prices %>%
    group_by(LSOA11CD) %>%
    summarise( MeanPrice = mean(X2),
               MedianPrice = median(X2),
               counts =n())

  saveRDS(MeanWardPrice, "MeanWardPrice.rds")
  
}


#MeanWardPrice %>% ggplot(., aes(x= counts)) + geom_density()

# WardDat2 <- WardDat %>% 
#   left_join(., MeanWardPrice) %>% 
#   mutate(vallow = LowUse*MeanPrice) %>%
# filter(!is.na(MeanPrice)) 


setwd(DataFolder)

#is mean income estimates
IncomeEst <- read_excel("1smallareaincomeestimatesdataupdate.xls", sheet = 4, skip = 4) %>%
setNames(make.names(names(.)) %>% 
gsub("\\.(?=\\.*$)", "", ., perl=TRUE)) %>% #removes trailing full stop
  rename(LAD11CD = Local.authority.code,
         LAD11NM = Local.authority.name,
         MSOA11CD = MSOA.code,
         MSOA11NM = MSOA.name) %>%
  mutate(Yearly.income= 52 * Total.weekly.income)

#Remoe stuff which isn't used again.


# IncomeEst <- prices %>% 
#   filter(X5 %in% c("D", "S", "T", "F")) %>%
#   #left_join(., select(EW2, ECODE, MSOA11CD),
#   #          by =c("lsoa11cd"="ECODE")) %>%
#   group_by(MSOA11CD) %>%
#   summarise(MedianPrice = median(X2),
#             MeanPrice = mean(X2),
#             counts = n()) %>%
#   left_join(., IncomeEst, by=c("MSOA11CD"= "MSOA.code")) %>%
#   mutate(Yearly.income= 52 * Total.weekly.income, 
#          ratio = MeanPrice/Yearly.income)

#No longer necessary kept just in case
# 
# #Load Deprivation Data
# setwd(file.path(basewd, "Deprivation"))
# list.files()
# LADDep<-read_excel("File_10_ID2015_Local_Authority_District_Summaries.xlsx", sheet = 2) %>%
#   setNames(make.names(names(.)) %>% gsub("IMD...", "",.) %>% trimws ) %>%
#   rename(LAD11CD = Local.Authority.District.code..2013.)
# 
# LSOADep<-read_excel("File_1_ID_2015_Index_of_Multiple_Deprivation.xlsx", sheet = 2) %>%
#   setNames(make.names(names(.))) %>%
#   setNames(make.names(names(.)) %>% gsub("Index.of.Multiple.Deprivation..IMD..", "",.) %>% trimws ) %>%
#   rename(LAD11CD = Local.Authority.District.code..2013.,
#          LSOA_CODE = LSOA.code..2011.)
# 
# 
# #Load Vacants Data
# setwd(DataFolder)
# Vacants <- read_excel("LT_615.xls", sheet = 2, skip = 5 ) %>%
#   set_names(make.names(names(.))) %>%
#   rename(LAD11CD = New.ONS.code, Vacants = X2016) %>%
#   filter(!is.na(LAD11CD)) %>%
#   select(LAD11CD, Vacants)
# 
# LTV <- read_excel("LT_615.xls", sheet = 3, skip = 5 ) %>%
#   set_names(make.names(names(.))) %>%
#   rename(LAD11CD = New.ONS.code, LTV = X2016) %>%
#   filter(!is.na(LAD11CD)) %>%
#   select(LAD11CD, LTV)
# 
# Vacants <- left_join(Vacants, LTV, by = "LAD11CD") %>%
#   #replace the st albarns and Welwyn codes
#   mutate(LAD11CD = case_when(
#     .$LAD11CD == "E07000100" ~ "E07000240",
#     .$LAD11CD == "E07000104" ~ "E07000241",
#     TRUE ~ .$LAD11CD
#   ))
# 
# rm(LTV)
# 
