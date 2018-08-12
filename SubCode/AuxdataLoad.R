#VOA data

#Council tax bands from VOA
#https://www.gov.uk/government/statistics/council-tax-stock-of-properties-2016

setwd(DataFolder)

CTtaxstock<- read_csv("Table_CTSOP1.1_2016.csv")

pop <- read_csv("SAPE2.csv")

EW <- inner_join(select(CTtaxstock, ECODE, ALL_PROPERTIES),select(pop, `Area Codes`, `All Ages`),
                 by=c("ECODE" = "Area Codes")) %>% setNames(c("ECODE", "Homes", "Pop"))


#Load the LSOA shape data for EW 

setwd(LSOAshapedata)

shape <- readOGR(dsn = list.files(pattern = "shp"))




# Load Post code data

#This chunk checks the exiatane of the postcode to lsoa lookup if it doesn't find it, the second part of the code is run that creates the lookup. Creating the lookup takes quite a lot of time so it is good to only have to do it once.


if(file.exists(file.path(DataFolder,"PstCdLSOA.rds"))){
PstCdLSOA.raw <- readRDS(file.path(DataFolder,"PstCdLSOA.rds"))
} else {
  PstCdLSOA.raw  <- MatchPostCode2LSOA(file.path(basewd, "ONS postcodes May17", "Data", "CSV"),
                        file.path(basewd, "ONS postcodes May17", "Doc") )
  setwd(DataFolder)
  saveRDS(PstCdLSOA.raw, "PstCdLSOA.rds")
}

#This and PstCdLSOA.raw should probably be merged at some point but it is only to make things cleaner
#Homes Could also be added in here
CorePstCd <- PstCdLSOA.raw %>%
  left_join(EW2, by = c("lsoa11cd" = "ECODE")) %>%
  select(Postcode,LSOA11CD= lsoa11cd, LAD11CD, LAD11NM, MSOA11CD, Pop)

#Bind the ward names and LAD names to the lsoa data
#
#
#
setwd(PostcodeLookups)
#It turned out  that the LAD11CD is out of date and so some of the LAD's aren't matching. I am replacing the LAD11CD with an
#Upt to date version, it is a bit of a hack but hopefully it will work an mean that areas like 
#St Albarns and Northumberland are not missed out
lsoa <- read_csv("PCD11_OA11_LSOA11_MSOA11_LAD11_EW_LU_aligned_v2.csv") %>%
select(LSOA11CD:LAD11NMW) %>% distinct(., LSOA11CD, .keep_all = TRUE) 

LSOAfix <- PstCdLSOA.raw %>% 
  select(Admin_district_code, LSOA11CD = lsoa11cd) %>%
  distinct(LSOA11CD, .keep_all = TRUE)

lsoa <- lsoa %>% 
  left_join(LSOAfix) %>%
  select(-LAD11CD) %>%
  rename(LAD11CD = Admin_district_code)


setwd(AddGeog)
wardnames <- read_csv("Ward_to_Local_Authority_District_to_County_to_Region_to_Country_December_2016_Lookup_in_United_Kingdom_V2.csv") %>% setNames(c("Admin_ward_code", names(.)[-1]))

#uses the dataframe from the CreateExceltemplates to get the ward names and the lsoa together
wardlsoa <- PstCdLSOA.raw %>% distinct(lsoa11cd, .keep_all = TRUE) %>% 
select(lsoa11cd, Admin_ward_code)

EW2 <- inner_join(EW, lsoa, by = c("ECODE" = "LSOA11CD") ) %>% #inner join gets rid of all non-LSOA from EW
  left_join(., wardlsoa, by = c("ECODE"="lsoa11cd") ) %>% 
  left_join(., select(wardnames, Admin_ward_code, WD16NM, Region = GOR10NM), by = "Admin_ward_code") %>%
  group_by(Admin_ward_code) %>%
  mutate(WardHomes = sum(Homes),  WardPop = sum(Pop)) %>% 
  ungroup #%>%
 # filter(grepl("E", ECODE)) don't filter wales


#Convert shape file to dataframe and join in the LSOA data
#
#
#
if(file.exists(file.path(DataFolder,"shapeframe.rds"))){
AG<- readRDS(file.path(DataFolder,"shapeframe.rds"))
} else{
setwd(LSOAshapedata)

shape <- readOGR(dsn = list.files(pattern = "shp"))

AG <- fortify(shape, region = "objectid")

AG <- AG %>% mutate(id = as.integer(id)) %>% 
left_join(., shape@data, by = c("id" = "objectid")) %>% 
left_join(., EW2, by = c("lsoa11cd" = "ECODE"))

saveRDS(AG, file.path(DataFolder,"shapeframe.rds"))
}


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
    mutate(X4 = gsub(" ", "", X4)) %>%
    left_join(., PstCdLSOA.raw %>% mutate(Postcode = gsub(" ", "", Postcode)),
              by = c("X4"="Postcode")) %>%
    filter(!is.na(lsoa11cd)) %>%
    left_join(select(EW2, ECODE, MSOA11CD, LAD11CD), by =c("lsoa11cd"="ECODE")) %>% #Add in the MSOA code
    #filter(Country_code == "E92000001") %>% keep in wales
    select(X2, X5, X3, X15, X16,lsoa11cd, MSOA11CD, LAD11CD) %>%
    rename(LSOA11CD = lsoa11cd)
    

  saveRDS(prices, "prices.rds")
  
}



#Was previously ward until I increased the number of years to allow the use of LSOA
MeanWardPrice <- prices %>%
  filter(X5 %in% c("D", "S", "T", "F")) %>%
  group_by(LSOA11CD) %>%
  summarise( MeanPrice = mean(X2),
             MedianPrice = median(X2),
             counts =n())

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
rm(pop)
rm(CTtaxstock)
rm(EW)
rm(wardlsoa)
rm(wardnames)
rm(lsoa)
rm(LSOAfix)

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
