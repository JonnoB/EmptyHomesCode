#This file loads all the data that has been sent in using the form, and so can be processed quite efficiently

#Lambeth

setwd(file.path(RegionsComp, "Region London"))

LambethLONDATA <- read_excel("LambethExemptionsLSOA.xlsx")[,1:4] %>%  
  StructureData(., lowuse = c(2:5,24,25), empty =  c(4:5,24,25))


test <- LambethLONDATA %>% select(LowUsePerc, WardLowUsePerc, LowuseClass, WardLowuseClass)

#Islington is exemptions only

IslingtonLONDATA <- read_csv("IslingtonExemptionsLSOA as at 2017 03 24.csv"  )[1:4]%>%  
  StructureData(., lowuse = c(2:11,14:18))

#Greenwhich

GreenwichLONDATA <- read_excel( "Royal Borough Of Greenwich.xlsx")[1:4] %>%  
  StructureData(., c(2,3,4,21,22), c(3,4,21,22))

#Camden

CamdenLONDATA <- read.xlsx("Camden Discounts LSOA - 20891480.xlsx" )[,1:4] %>%
  StructureData()

#Bromley

BromleyLONDATA <- read.xlsx("Copy of BromleyExemptionsLSOA.xlsx")[,c(1,4:6)] %>%
  StructureData(c(6:8,29:31))

#Haringey



#Haringey is funny and has to be worked over a bit first
HaringeyLONDATA <- read.xlsx("Haringey_Exemptions_Discounts_LSOA.xlsx")[c(1,3,5)] 

HaringeyLONDATA <- data_frame(Discount = rep(HaringeyLONDATA $`Exemption./.Discount.type`, HaringeyLONDATA$Count.of.Exemptions.by.Post.Code),
                           LSOA = rep(HaringeyLONDATA$LSOA_CODE, HaringeyLONDATA$Count.of.Exemptions.by.Post.Code)) %>%
  mutate( a= NA, b = NA) %>%
  StructureData(21:25)


#Sutton

SuttonLONDATA <- read_excel("SuttonExemptionsLSOADiscount.xlsx"  )[,c(1,4:6)] %>%
  StructureData()

#Richmond
RichmondLONDATA <- read_csv("Richmond_2.csv" )[1:4]%>%
  StructureData()

#Hillingdon

HillingdonLONDATA <- read.xlsx("HillingdonDiscountsLSOA.XLSX" )[1:4]%>% 
  StructureData()

#Bexley

BexleyLONDATA <- read.xlsx( "Bexley Exemptions.xlsx" , colNames = FALSE) %>%
  StructureData

#RBKC

ChelseaLONDATA <- read_excel( "Kensington and ChelseaDiscountsLSOA.xlsx" )[,1:4] %>%
  StructureData()

#Barking
BarkingLONDATA <- read.xlsx( "Barking and DagenhamExemptionsLSOA v3.xlsx" , colNames = FALSE)[,1:4] %>%
  StructureData(c(2,3,6,7))


#Barnet

BarnetLONDATA <- read.xlsx( "3386297 Attachment BarnetDiscounts.xlsx"  , colNames = TRUE) %>%
  StructureData(2:8)


#Newham
# NewhamLONDATA <- read.xlsx("NewhamExemptionsLSOA.XLSX"  , colNames = TRUE)[,1:4]%>%
#   StructureData(full = F)
# NewhamLONDATA <- read.xlsx( "NewhamDiscountsLSOA.xlsx", colNames = TRUE)[,1:4] %>%
#   StructureData(full = F)

#City of London

CoLLONDATA <- read_excel("City of LondonExemptionsLSOAApril2017.xlsx" )[,1:4] %>%
  StructureData(c(2,3,4,13))

#Southwark

SouthwarkLONDATA <- read_excel("Southwark discounts.xlsx")[1:4] %>%
  StructureData(empty = -c(1,2,5))


#Redbridge
#using all redbridge, may need to change when they give a clearer explanation of what the codes mean.

RedbridgeLONDATA <- read_excel("RedbridgeDiscountsLSOA.XLSX" )[1:4]  %>%
  StructureData

#Brent
#Brent data only inlcudes 2nd homes, but they struggled with understanding the request will try again when data consistancy becomes more important or I get help from f.eks mayors office.

BrentLONDATA <- read_excel("Brent Second homes and post codes.xlsx", col_names = FALSE) %>% 
  left_join(., PstCdLSOA.raw,  by=c("X__2"="Postcode") ) %>%
  rename(Exemption.type=X__1, LSOA_CODE = lsoa11cd) %>%
  select(Exemption.type, LSOA_CODE, X__2, Country_code) %>%
  StructureData()

#Hammersmith and fulham

HammersmithLONDATA <- read_excel("Hammersmith and FulhamExemptionsLSOA.xls"  )[1:4] %>%
  StructureData(lowuse = 19:23)
