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



#Bind the ward names and LAD names to the lsoa data
#
#
#
setwd(PostcodeLookups)
lsoa <- read_csv("PCD11_OA11_LSOA11_MSOA11_LAD11_EW_LU_aligned_v2.csv") %>%
select(LSOA11CD:LAD11NMW) %>% distinct(., LSOA11CD, .keep_all = TRUE)

setwd(AddGeog)
wardnames <- read_csv("Ward_to_Local_Authority_District_to_County_to_Region_to_Country_December_2016_Lookup_in_United_Kingdom_V2.csv") %>% setNames(c("Admin_ward_code", names(.)[-1]))

#uses the dataframe from the CreateExceltemplates to get the ward names and the lsoa together
wardlsoa <- PstCdLSOA.raw %>% distinct(lsoa11cd, .keep_all = TRUE) %>% 
select(lsoa11cd, Admin_ward_code)

EW2 <- inner_join(EW, lsoa, by = c("ECODE" = "LSOA11CD") ) %>% 
left_join(., wardlsoa, by = c("ECODE"="lsoa11cd") ) %>% 
left_join(., select(wardnames, Admin_ward_code, WD16NM), by = "Admin_ward_code") %>%
group_by(Admin_ward_code) %>%
mutate(WardHomes = sum(Homes),  WardPop = sum(Pop)) %>% ungroup
rm(wardlsoa)
rm(wardnames)



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

prices <-read_csv("pp-2016.csv", col_names = FALSE ) %>% 
mutate(X4 = gsub(" ", "", X4)) %>% 
left_join(., PstCdLSOA.raw %>% mutate(Postcode = gsub(" ", "", Postcode)), 
by = c("X4"="Postcode")) %>% 
filter(!is.na(lsoa11cd)) %>% mutate(Prime = X2>1e6, SuperPrime = X2>1e7)

MeanWardPrice <- prices %>% 
group_by(Admin_ward_code) %>% 
summarise( MeanPrice = mean(X2), 
MedianPrice = median(X2),counts =n(),  
Prime = sum(Prime), 
SuperPrime = sum(SuperPrime)) %>% 
mutate(TotPrime = Prime/sum(Prime), TotSupPrime = SuperPrime/sum(SuperPrime),
PrimePerc = Prime/counts, SuperPrimePerc = SuperPrime/counts)

MeanWardPrice %>% ggplot(., aes(x= counts)) + geom_density()

WardDat2 <- WardDat %>% left_join(., MeanWardPrice) %>% mutate(vallow = LowUse*MeanPrice) %>%
filter(!is.na(MeanPrice)) 


setwd(DataFolder)
list.files()

#Unnfordability at EW level salery 2014-2015
mean(prices$X2)/31920

#is mean income estimates
IncomeEst <- read_excel("1smallareaincomeestimatesdataupdate.xls", sheet = 4, skip = 4) %>%
setNames(make.names(names(.)) %>% 
gsub("\\.(?=\\.*$)", "", ., perl=TRUE)) #removes trailing full stop.

test2 <- prices %>% left_join(., select(EW2, ECODE, MSOA11CD), 
by =c("lsoa11cd"="ECODE")) %>%
group_by(MSOA11CD) %>%
summarise(MedianPrice = median(X2), counts = n()) %>%
left_join(., IncomeEst, by=c("MSOA11CD"= "MSOA.code")) %>%
mutate(Yearly.income= 52 * Total.weekly.income, ratio = MedianPrice/Yearly.income)

test2 %>% ggplot(.,aes(x=ratio)) +geom_density()

sum(test2$ratio>6)/nrow(test2)  #about 50% EW
test2 %>% filter(Region.name == "London") %>% mutate(HighRatio = ratio>6) %>%
summarise(total = sum(HighRatio)/n())


