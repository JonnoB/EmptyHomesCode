#This file loads all the data that has been sent in using the form, and so can be processed quite efficiently

#Lambeth

setwd(file.path(RegionsComp, "Region London"))

Lambethinfo <- read_excel("LambethExemptionsLSOA.xlsx")[,1:4] %>%  
  StructureData(., lowuse = c(2:5,24,25), empty =  c(4:5,24,25))


test <- Lambethinfo %>% select(LowUsePerc, WardLowUsePerc, LowuseClass, WardLowuseClass)

#Islington is exemptions only

Islingtoninfo <- read_csv("IslingtonExemptionsLSOA as at 2017 03 24.csv"  )[1:4]%>%  
  StructureData(., lowuse = c(2:11,14:18))

#Greenwhich

Greenwichinfo <- read_excel( "Royal Borough Of Greenwich.xlsx")[1:4] %>%  
  StructureData(., c(2,3,4,21,22), c(3,4,21,22))

#Camden

Camdeninfo <- read.xlsx("Camden Discounts LSOA - 20891480.xlsx" )[,1:4] %>%
  StructureData()

#Bromley

Bromleyinfo <- read.xlsx("Copy of BromleyExemptionsLSOA.xlsx")[,c(1,4:6)] %>%
  StructureData(c(6:8,29:31))

#Haringey



#Haringey is funny and has to be worked over a bit first
Haringeyinfo <- read.xlsx("Haringey_Exemptions_Discounts_LSOA.xlsx")[c(1,3,5)] 

Haringeyinfo <- data_frame(Discount = rep(Haringey$`Exemption./.Discount.type`, Haringey$Count.of.Exemptions.by.Post.Code),
                           LSOA = rep(Haringey$LSOA_CODE, Haringey$Count.of.Exemptions.by.Post.Code)) %>%
  mutate( a= NA, b = NA) %>%
  StructureData(21:25)


#Sutton

Suttoninfo <- read_excel("SuttonExemptionsLSOADiscount.xlsx"  )[,c(1,4:6)] %>%
  StructureData()

#Richmond
Richmondinfo <- read_csv("Richmond_2.csv" )[1:4]%>%
  StructureData()

#Hillingdon

Hillingdoninfo <- read.xlsx("HillingdonDiscountsLSOA.XLSX" )[1:4]%>% 
  StructureData()

#Bexley

Bexleyinfo <- read.xlsx( "Bexley Exemptions.xlsx" , colNames = FALSE) %>%
  StructureData

#RBKC

Chelseainfo <- read_excel( "Kensington and ChelseaDiscountsLSOA.xlsx" )[,1:4] %>%
  StructureData()

Chelseainfo2 <- read_excel( "Kensington and ChelseaDiscountsLSOA.xlsx" )[,1:4] %>%
  StructureData(full= FALSE)
colSums(Chelseainfo2[,-1])

#Barking
Barkinginfo <- read.xlsx( "Barking and DagenhamExemptionsLSOA v3.xlsx" , colNames = FALSE)[,1:4] %>%
  StructureData(c(2,3,6,7))


#Barnet

Barnetinfo <- read.xlsx( "3386297 Attachment BarnetDiscounts.xlsx"  , colNames = TRUE) %>%
  StructureData(2:8)


#Newham
Newhaminfo <- read.xlsx("NewhamExemptionsLSOA.XLSX"  , colNames = TRUE)[,1:4]%>%
  StructureData(full = F)
Newhaminfo <- read.xlsx( "NewhamDiscountsLSOA.xlsx", colNames = TRUE)[,1:4] %>%
  StructureData(full = F)

#City of London

CoLinfo <- read_excel("City of LondonExemptionsLSOAApril2017.xlsx" )[,1:4] %>%
  StructureData(c(2,3,4,13))

#Southwark

Southwarkinfo <- read_excel("Southwark discounts.xlsx")[1:4] %>%
  StructureData(empty = -c(1,2,5))


#Redbridge
#using all redbridge, may need to change when they give a clearer explanation of what the codes mean.

Redbridgeinfo <- read_excel("RedbridgeDiscountsLSOA.XLSX" )[1:4]  %>%
  StructureData

#Brent
#Brent data only inlcudes 2nd homes, but they struggled with understanding the request will try again when data consistancy becomes more important or I get help from f.eks mayors office.

Brentinfo <- read_excel("Brent Second homes and post codes.xlsx", col_names = FALSE) %>% 
  left_join(., PstCdLSOA.raw,  by=c("X__2"="Postcode") ) %>%
  rename(Exemption.type=X__1, LSOA_CODE = lsoa11cd) %>%
  select(Exemption.type, LSOA_CODE, X__2, Country_code) %>%
  StructureData()
