#This file loads all the data that has been sent in using the form, and so can be processed quite efficiently

#Lambeth

setwd(file.path(RegionsComp, "Region London"))

LambethLONDATA <- read_excel("LambethExemptionsLSOA.xlsx")[,1:4] %>%  
  StructureData(., lowuse = c(2:5,24,25), empty =  c(4:5,24,25))

#Islington is exemptions only
#data is exemptions not discounts
 IslingtonLONDATA <- read_csv("Islington 492497 p.1.csv" )[1:4]%>%  
   StructureData(c(8:14))

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

BarnetLONDATA <- read.xlsx( "3386297 Attachment BarnetDiscounts.xlsx"  , colNames = TRUE)[1:4] %>%
  StructureData(2:8)


#Newham
NewhamLONDATA <- read.xlsx("NewhamDiscountsLSOA - updated.xlsx"  , colNames = TRUE)[,1:4]%>%
  StructureData(., lowuse = c(2,4:8))

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

BrentLONDATA <- read_excel("FOI  6900196 Brent empty domestic properties.xlsx", col_names = TRUE)[,c(1,5)] %>%
  setNames(c("X__1", "Postcode")) %>%
  mutate(Postcode = gsub(" ", "", Postcode)) %>%
  left_join(., CorePstCd,  by="Postcode" ) %>%
  rename(Exemption.type=X__1, LSOA_CODE = LSOA11CD) %>%
  select(Exemption.type, LSOA_CODE, LAD11CD, Country_code) %>%
  StructureData(c(2,15:19))

#Hammersmith and fulham

HammersmithLONDATA <- read_excel("Hammersmith and FulhamExemptionsLSOA.xls"  )[1:4] %>%
  StructureData(lowuse = 19:23)

#Waltham Forest
WalthamforestLONDATA <- read_excel("J Bourne Waltham ForestDiscountsLSOA 8.8.17.xlsx")[1:4] %>%
  StructureData(c(3:7))


#Westminster
#Holy SHIT!

#They sent it in the wrong format becuase they are annoying.
LSOA2LSOA <- CorePstCd %>% group_by(LSOA11CD) %>%
  summarise(LSOA = first(LSOA11NM)) %>%
  ungroup %>%
  rename(LSOA_CODE = LSOA11CD)

#They have missed something out
WestminsterLONDATA <- read_excel("Westminster 3123133  data Aug 2017.xlsx")[2:5] %>%
  left_join(., LSOA2LSOA, by = "LSOA") %>%
  select(1,5,3,4) %>%
  StructureData(2:7)

rm(LSOA2LSOA)
  
#Hackney

HackneyLONDATA <- read_csv("18632856-Hackney Discounts SOA FOI17-0301-09113.CSV" )[1:4] %>%
  StructureData(c(12:13,15:16,23))

#Ealing

EalingLONDATA <- read_excel("Ealing FOI 17 159.xlsx" )[c(1,4:6)] %>%
  StructureData(22:25)

#Lewisham
#I removed rows first
LewishamLONDDATA <-read_excel("FOI 411271 LewishamDiscountsLSOA.xlsx" )[c(1,4:6)] %>%
  StructureData(c(18:23))

#Enfield

EnfieldLONDATA <- read_excel("EnfieldDiscountsLSOA.xlsx")[c(1,4:6)] %>%
  StructureData(2:3)


#Hounslow

 HounslowLONDATA <- read_excel("Hounslow - Bourne Info -210817.xls", sheet = 2)[c(1,4:5)] %>%
   setNames(c("Postcode","X__1","X__2")) %>%
   mutate(Postcode = gsub(" ", "", Postcode)) %>%
   left_join(., CorePstCd,  by="Postcode" ) %>%
   rename(Exemption.type=X__2, LSOA_CODE = LSOA11CD) %>%
   select(Exemption.type, LSOA_CODE, LAD11CD, Country_code) %>%
   StructureData(c(2:4,6))

 
 MertonLONDATA <- read_excel("Merton Discounts for FoI Jonathan Bourne.xlsx" )[1:4] %>%
   StructureData()
 
 #Croydon
 CroydonLONDATA <- read_excel("Croydon8020_Final Attachment.xlsx" )[1:4] %>%
   StructureData(lowuse =c(2,3,6,7), empty = c(2,3,6))
 
 #Wandsworth
 
 WandsworthLONDATA <- read_excel("FOI 2017-15405  WandsworthDiscountsLSOA.xlsx" )[1:4] %>%
   StructureData()

 #Kingston
 
 KingstonLONDATA <- read_excel("RB Kingston Discounts by LSOA September2017.xlsx" )[1:4] %>%
   StructureData(c(2:5,7))
 
 
 