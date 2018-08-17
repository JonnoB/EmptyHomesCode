
#This script cleans and processes all the Local authorities into a single data frame
#loading is by region and county but they are not in alphabetical order... sorry


setwd(file.path(RegionsComp, "Region North West"))

#Cumbria

print("Region North-west")

BarrowCUMBRIADATA <- read_excel("Barrow-in-FurnessExemptionsLSOA.xlsx", sheet=1)[1:4]%>%  
  StructureData(.,lowuse = c(2:6,8))

SLakelandCUMBRIADATA <- read_excel("South LakelandExemptionsLSOA.XLSX", sheet=1)[,1:4] %>%
  StructureData(., lowuse=2:9)

#only inlcudes postcodes so the mergeing needs to be done manually
CopelandCUMBRIADATA <- read_excel("CopelandFOI 5778 data 100517 CBC.XLSX", sheet=1) %>% 
  mutate(Postcode = sub(" ", "", Postcode)) %>%
  left_join(.,CorePstCd,  by=c("Postcode") ) %>%
  rename(Exemption.type=`Disc Type Ind`, LSOA_CODE = LSOA11CD) %>%
  select(Exemption.type, LSOA_CODE, Country_code, Admin_district_code=LAD11CD) %>%
  StructureData()


EdenCUMBRIADATA <- read_excel("EdenDiscountsLSOA.xlsx"  , sheet=1)[1:4] %>%  
  StructureData(.,lowuse = c(2:6,8))

CarliseCUMBRIADATA <- read_excel("FOI 5846 5983-CarlisleDiscountsLSOA June 2017.xlsx"  , sheet=1)[1:4] %>%  
  StructureData(.)

AllerdaleCUMBRIADATA <- read_excel("F17.132_Copy of Allerdale discounts LSOA_170815.xlsx"  , sheet=1)[1:4] %>%  
  StructureData(c(4:10))


#Manchester

OldhamMANCDATA <- read_excel("FOI 10635 OldhamDiscounts LSOA.xlsx", sheet = 1 )[c(1,4:6)] %>%  
  StructureData(., c(5:6,8:11))

ManchesterMANCDATA <- read_excel("ManchesterDiscountsLSOA.xlsx", sheet = 1 )[1:4] %>%  
  StructureData(. )

RochdaleMANCDATA <- read_excel("Rochdale Both.xlsx" , sheet = 1 )[1:4] %>%  
  StructureData(., c(5,6,14,15,20:22))

TamesideMANCDATA <- read_excel("Tameside FOI 6257A2.xlsx", sheet = 1 )[1:4] %>%  
  StructureData(. )

TraffordMANCDATA <- read_excel("Trafford Exemption and Levy.xlsx" , sheet = 1 )[1:4] %>%
  filter(Exemption_type =="LEVY") %>% 
  bind_rows(.,read_excel("TraffordDiscountsLSOA.XLSX", sheet = 1 )[1:4] %>% rename(Exemption_type = Class)) %>%
  StructureData()

WiganMANCDATA <- read_excel("Wigan 5528 DISCOUNTS.xlsx", sheet = 1 )[1:4] %>%  
  StructureData(., lowuse = c(2:3, 5:9) )



setwd(file.path(RegionsComp, "Region West Midlands"))
print("Region Midlands")

#West-midlands and Brum

SandWellDATA <- read_excel("FOI - SandwellDiscountsLSOA - FS57000079 Disc.xlsx" )[1:4] %>%  
  StructureData(2:6)

WolverhamptonDATA <- read_excel("WolverhamptonDiscountsLSOA.XLSX"  )[1:4] %>%  
  StructureData(c(2:5,7))

WalsallDATA <- read_excel("WalsallDiscountsLSOA.XLSX" )[1:4] %>%  
  StructureData(c(4:5,7:8))

BirminghamDATA <-  read_excel("Birmingham CT EMPTIES.xlsx" )%>%
  mutate(Postcode = sub(" ", "", `Post Code`)) %>%
  left_join(.,CorePstCd,  by=c("Postcode") ) %>%
  rename(Exemption.type=`Current Discount Type Description`, LSOA_CODE = LSOA11CD) %>%
  select(Exemption.type, LSOA_CODE, Country_code, Admin_district_code=LAD11CD) %>%
  StructureData(c(2,4:9))


DudleyDATA <- read_excel("Completed DudleyDiscountsLSOA.xlsx"  )[1:4] %>%  
  StructureData()



#South-West

#Cornwall

setwd(file.path(RegionsComp, "Region South West"))
print("Region South-West")

CornwallDATA <- read_excel("Cornwall.xlsx", sheet=1)%>% setNames(make.names(names(.))) %>% .[,1:4] %>%
  StructureData(lowuse = c(2,23:28))



#Devon


NorthDevonDATA <- read_excel("North DevonDiscountsLSOA.XLSX", sheet=1)[1:4] %>%
  StructureData  

PlymouthDATA <- read_excel("PlymouthDiscountsLSOA.XLSX" , sheet=1)[1:4] %>%
  StructureData(lowuse = 4:8)

TorbayDATA <- read_excel("Torbay_discounts 1718551.xlsx"  , sheet=1)[1:4] %>%
  StructureData()

MidDevonDATA <- read_excel("MidDevon FOI04942Information 3.xlsx" , sheet=1)[1:4] %>%
  StructureData(c(7,10:11,20:22,26,28))

TorridgeDATA <- read_excel("FOI_TorridgeDiscountsLSOA.XLSX" , sheet=1)[1:4] %>%
  StructureData(c(4:5,7:10,13))


#Somerset

NorthSomsertDATA <- read_excel("1501796 North SomersetDiscountsLSOA.xlsx" , sheet=1)[1:4] %>%
  StructureData(lowuse= 2:6)  

MendipDATA <- read_excel("Mendip 356 20170706 Response.xlsx" , sheet=1)[1:4] %>%
  StructureData(lowuse= c(37:39,52:54,65:66,69)) 


#Dorset

BournemouthDATA <- read_excel("BournemouthDiscountsLSOA_R.xlsx" )[1:4] %>%
  StructureData(c(6,8,9:13))

PooleDATA <- read_excel("Copy of FOI - 265 - Bourne - PooleDiscountsLSOA.xlsx" , sheet=1)[1:4] %>%
  StructureData(lowuse = c(5,11:14,18))

ChristchurchDATA <- read_excel("Christchurch FOI17 - 0312 - Bourne - Ctax Discounts and Exemptions.xls",
                               sheet=1)[1:4] %>%
  StructureData(lowuse = c(6,12:17,20))

WeymouthDATA <-  read_excel("63687 Weymouth and PortlandDiscountsLSOA - 010817.xlsx"    ,
                            sheet=1)[1:4] %>%
  StructureData()

WestDorsetDATA <-  read_excel("47019 West DorsetDiscountsLSOA - 010817.xlsx"     ,
                              sheet=1)[1:4] %>%
  StructureData()



PurbeckDATA <-  read_excel("PurbeckDiscountsLSOA - 010817.xlsx"      ,
                           sheet=1)[1:4] %>%
  StructureData()


#Wiltshire

WiltshireDATA <- read_excel("Wiltshire ENQ07536 - Council tax discounts 2.xlsx",
                            sheet=1)[1:4] %>%
  StructureData(lowuse = 3:8)

SwindonDATA <- read_excel("Swindon - 7th July 2017 - Part 2 (Discounts).xlsx" ,
                          sheet=1)[1:4] %>%
  StructureData(2:6)


#East of England
setwd(file.path(RegionsComp, "Region East of England"))

print("Region East")

#Suffolk

CoastSUFFDATA <- read.xlsx("FOI IMT178324 SuffolkCoastal.xlsx")[1:4] %>% setNames(make.names(names(.))) %>%
  mutate(Exemption.type = Exemption.type %>% trimws %>%
           make.names %>% 
           gsub("\\.{2,}" , ".", .))  %>%
  StructureData(.,c(5,6,11,12,14:16))

WaveneySUFFDATA <- read.xlsx("FOI IMT178324 Waveney.xlsx")[1:4] %>% setNames(make.names(names(.))) %>%
  mutate(Exemption.type = Exemption.type %>% trimws %>%
           make.names %>% 
           gsub("\\.{2,}" , ".", .))  %>%
  StructureData(., lowuse = c(5,6,13,14,16:18))

StEdmundsDATA <- read.xlsx("se FOI 146  3948502_Copy+of+St+EdmundsburyDiscountsLSOA.xlsx")[1:4] %>%
  StructureData()

ForestHeathDATA <- read.xlsx("FH FOI 146 Discounts.xlsx")[1:4] %>%
  StructureData()

IpswichDATA <- read_xlsx("IpswichDiscountsLSOA.xlsx")[1:4] %>%
  StructureData(c(2:4,6:11,13:15))

#Mid Suffolk and Babergh
MidSuffolkDATA <- read_excel("MidSuffolk FOI MF379 1718 Discount.xlsx") %>%
  mutate(Postcode = gsub(" ", "", `Post Code`)) %>%
  left_join(.,CorePstCd,  by=c("Postcode") ) %>%
  rename(Exemption.type=`Discount Type Description`, LSOA_CODE = LSOA11CD) %>%
  select(Exemption.type, LSOA_CODE, Country_code, Admin_district_code=LAD11CD) %>%
  StructureData(c(2:12,14:15))


BaberghDATA <- read_excel("Babergh FOI BF378 1718 Discount.xlsx")%>%
 mutate(Postcode = gsub(" ", "", `Post Code`)) %>%
  left_join(.,CorePstCd,  by=c("Postcode") ) %>%
  rename(Exemption.type=`Discount Type Description`, LSOA_CODE = LSOA11CD) %>%
  select(Exemption.type, LSOA_CODE, Country_code, Admin_district_code=LAD11CD) %>%
  StructureData(c(2:6,8:11,13:15))


#NorFolk
BroadlandsDATA <- read_excel("BroadlandDiscountsLSOA.xlsx", sheet=1)[1:4] %>%
  StructureData(4:9)

KingsLynnDATA <- read_excel("197782 - King s Lynn and West NorfolkDiscountsLSOA (2).xlsx" , sheet=1)[1:4] %>%
  StructureData(2:11)

NorthNorfolkDATA <- read_excel("North NorfolkDiscountsLSOA.XLSX" , sheet=1)[1:4] %>%
  StructureData()

BrecklandsDATA <- read_excel("BFOI-003031 BrecklandDiscountsLSOA.xlsx" , sheet=1)[1:4] %>%
  StructureData()

SNorfolkDATA <- read_excel("SNorfolk 17-307 Discounts.xlsx"  , sheet=1)[1:4] %>%
  StructureData()


NorwichDATA <- read_excel("Norwich Discounts FINAL.xlsx"  , sheet=1)[1:4] %>%
  StructureData(2:8)


#Cambridge

EastCambsDATA <- read_excel("EC FOI 130 - DISC_FOI_0_3944413b.xlsx", sheet=1)[1:4] %>%
  StructureData()


FenlandDATA <- read_excel("FL FOI 4544 DISCOUNTS.XLSX" , sheet=1)[1:4] %>%
  StructureData()

HuntingdonDATA <- read_excel("Huntingdonshire  FOI 568discounts.xlsx" , sheet=1)[1:4] %>%
  StructureData(lowuse = 2:8)

PeterboroughDATA <- read_excel("Peterborough 1707267585 completed.xlsx" , sheet=1)[1:4] %>%
  StructureData(lowuse = c(4,6:7,11))

CambridgeDATA <- read_excel("566 - Copy of Copy of CambridgeDiscountsLSOA .xlsx" , sheet=1)[1:4] %>%
  StructureData(2:5)


#South East

setwd(file.path(RegionsComp, "Region South East"))

print("Region South-East")

BrightonDATA <- read_excel("Copy of Brighton and HoveDiscountsLSOA (4).xlsx", sheet=1)%>% .[,c(2,3:5)] %>%
  StructureData(lowuse = 2:8)
#MakeLeaflet(BournemouthDATA)


ValeWhiteDATA <- read_excel("Vale of White HorseDiscountsLSOA (00000002).xlsx" , sheet=1)[1:4] %>%
  StructureData(c(5,7,8,9,10,14))

SouthOxfordDATA <- read_excel("South OxfordshireDiscountsLSOA.xlsx" , sheet=1)[1:4] %>%
  StructureData(c(5,7,8,9,10,14))




#Yorkshire and the Humber



setwd(file.path(RegionsComp, "Region Yorkshire and The Humber"))

print("Region Yorkshire and the Humber")

#Hull

HullDATA <-read_excel("KINGSTON UPON HULL%2c CITY OFDISCOUNTSLSOA.XLSX")[-c(c(1:6)),1:4] %>%
  StructureData(.)

#Harrowgate

HarrogateDATA <-read_excel("Harrogate DiscountsExemptionsLSOA.xlsx" )[,1:4] %>%
  StructureData(.,3:10)

#Selby
SelbyDATA <-read_excel("SelbyExemptionsLSOA.xlsx" )[,1:4]%>%
  StructureData(20:27)


# HambletonDATA <- read_excel("HDC1088.xlsx")[,1:4] %>%
#  StructureData()

ScarboroughDATA <- read_excel("FOIA 5156 Scarborough Discounts LSOA.xlsx"  )[,1:4]%>%
  StructureData(2:8)



# Middlesbrough sent the wrong data
# MiddlesbroughDATA <- read_excel( "Middlesbrough FOI RES - 011397 Discounts.xlsx"   )[,1:4]%>%
#   StructureData()

#Ryedale
RyedaleDATA <-  read_excel("Ryedale FOI 4597.xlsx")[,1:4]%>%
  StructureData(2:6)

#Crave they sent full addresses by mistake I'll do a temporary fix
CravenDATA <- read_excel("Craven Discounts.xlsx" ) %>%
  setNames(make.names(names(.))) %>%
  mutate(Postcode = str_split(Full.Property.Address, "(,)(?!.*,)", simplify = T)[,2] %>% 
           gsub(" ", "", .)) %>%
  left_join(., CorePstCd,  by=c("Postcode") ) %>%
  rename(Exemption.type=Discount.Type.Description, LSOA_CODE = LSOA11CD) %>%
  select(Exemption.type, LSOA_CODE, Postcode, Country_code) %>%
  StructureData(c(2:5,7))

#Redcar and cleveland

RedcarDATA <-  read_excel("Redcar 0614 attachment 1.xlsx")[,1:4]%>%
  StructureData(2:10)


CalderdaleDATA <- read_excel("Copy of CalderdaleDiscountsLSOA.xlsx" )[,1:4]%>%
  StructureData(c(7,10,22:25))

BradfordDATA <- read_excel("Copy of BradfordDiscountsLSOA.xlsx")[1:4] %>%
  StructureData(c(2:7))


RichmondshireDATA <- read_excel("Richmondshire FOI 4960.xls", col_names = FALSE)[1:2] %>%
  setNames(c("Exemption.type", "Postcode")) %>%
  mutate(Postcode = sub(" ", "", Postcode)) %>%
  left_join(., CorePstCd,  by=c("Postcode") ) %>%
  select(Exemption.type, LSOA_CODE= LSOA11CD, Postcode, Country_code) %>%
  StructureData(lowuse = c(2:7))


KirkleesDATA <- read_excel("Jonathan Bourne KirkleesDiscountsLSOA Complete.xlsx" )[1:4] %>%
  StructureData(2:5)

WakefieldDATA <-  read_excel("WakefieldDiscountsLSOA.xlsx" )[1:4] %>%
  StructureData(2:7)



#North-East

print("Region North-East")

setwd(file.path(RegionsComp, "Region North East"))

#Tyneside

#Not sure all are there ask for confirmation
SunderlandDATA <- read_excel("Sunderland Freedom of Informaton Request from Jonathan Bourne- 17-07-21-Discounts.xlsx"  , sheet=1)%>% .[,1:4] %>%
  StructureData(lowuse = c(2:4,6,8:10))


NewcastleDATA <- read_excel("Newcastle upon TyneDiscountsLSOA.xlsx", sheet=1)%>% .[,c(1,3:5)] %>%
  StructureData(lowuse = 2:7)


#FOr some reason Gateshead has the wrong LAD11CD I don't know why. I have changed it to the right one but feel that perhaps I should check them all, but how?
GatesheadDATA <- read_excel("Copy of GatesheadExemptionsLSOA.XLSX"   , sheet=1)%>% .[,1:4] %>%
  StructureData(lowuse = c(2:3, 20,22:25)) %>%
  mutate(LAD11CD = ifelse(is.na(LAD11CD), NA, "E08000037"))

STynesideDATA <- read_excel("FOI 17.18456 - South Tyneside Discounts.xlsx", sheet=1)[,c(1,4:6)] %>%
  StructureData(c(32:35,38))



#Northumberland
NorthumberlandDATA <- read_excel("FOI 2762 data NorthumberlandDiscounts.xlsx", sheet=1)%>% .[,1:4] %>%
  StructureData(2:7) %>%
  #The LAD code is wrong for some reason
  mutate(LAD11CD = "E06000057")


#Durham
DurhamDATA <- read_excel("County DurhamDiscountsLSOA.XLSX" , sheet=1) %>% .[,c(1,4:6)] %>%
  StructureData()

DarlingtonDATA <- read_excel("Darlington DBC-0809-17.xlsx" , sheet=1) %>% .[,1:4] %>%
  StructureData()

HartlepoolDATA <- read_csv("FOI 7942 HartlepoolDiscountsLSOA.csv") %>% .[,1:4] %>%
  StructureData(2:7)



#Wales

setwd(file.path(RegionsComp, "Region Wales"))
print("Region Wales")

#Ceredigion completed some major Student Halls which were transferred to the University, this created some very large B type transfers which artificially push the cost of second homes lower than the price of regular homes.

CeredigionDATA <- read_excel("CTAX - Ceredigion Discounts LSOA.xlsx", sheet=1)%>% .[,1:4] %>%
  StructureData(lowuse =c(8:13,15))

MonmouthDATA <- read_excel("MCC Discounts Jul 17.xlsx", sheet=1)%>% .[,1:4] %>%
  StructureData(lowuse = c(4:8))


#includes the zero class which is descibeed as
#ZERO – tis is a 50% reduction which applies after exemptions that are time limited, have expired. They are awarded on the basis that the property remains unoccupied and unfurnished.
CarmarthenDATA <- read_excel("CarmarthenshireDiscountsLSOA.XLSX" , sheet=1)%>% .[,1:4] %>%
  StructureData()

#Strangely low numbers checking
# PowysDATA <- read_excel("PowysDiscountsLSOA.XLSX"    , sheet=1)%>% .[,1:4] %>%
#   StructureData(lowuse = c(3,5:6))

#It seems Conwy really is this short on the discounts
ConwyDATA <- read_excel("Conwy FOI 0260-17.xlsx" , sheet=1) %>%
  rename(Postcode = geo_postcode) %>%
  mutate(Postcode = sub(" ", "", Postcode)) %>%
  left_join(., CorePstCd,  by=c("Postcode") ) %>%
  rename(Exemption.type=disc_type_desc, LSOA_CODE = LSOA11CD) %>%
  select(Exemption.type, LSOA_CODE, Postcode, Country_code) %>%
  StructureData(lowuse = c(2,4))

DenbigshireDATA <- read_excel("FOI 2503 Denbighshirev2 .xlsx" , sheet=1)%>% .[,1:4] %>%
  StructureData(c(8:9,11,13))

WrexhamDATA <- read_csv("WrexhamDiscountsLSOA.csv" )%>% .[,1:4] %>%
  StructureData(2:5)

#Pembrokeshire didn't send all the data
PembrokeshireDATA <- read_excel("Copy of PembrokeshireDiscountsLSOA.xlsx"  , sheet=1)%>% .[,1:4] %>%
  StructureData(3)

FlintshireDATA <- read_excel("FOI R014329 FlintshireDiscounts LSOA.xlsx"  , sheet=1)%>% .[,1:4] %>%
  StructureData()


#Source the code from another file as it is very long

print("Region London")
#Tower-Hamlets and Havering are not included as they didn't understand how to paste into the excel-spread sheet
source(file.path(CommonCode, "LondonFullProcesss.R"))




#
#
# Scrub the data to remove Cross overs with other LADs
#


#
#
# Combine Lads into a single df
#
#

DATAdf <- ls(pattern = "DATA$") %>%
  map_df(~{
    get(.x) %>%
      select(LSOA11CD, LowUse:PercTurnover) %>%
      group_by(LAD11CD) %>%
      mutate(LAD11CDCounts = n()) %>%
      arrange(-LAD11CDCounts) %>%
      ungroup %>%
      mutate(LAD11CD = first(LAD11CD),
             LAD11NM = first(LAD11NM)) #This allows me to keep tabs on the missing empty homes. this was caused by postcode problems
  
      }) %>%
  select(-LAD11CDCounts)

rm(list = ls(pattern = "DATA$"))

#print("Scrubbing loose ends")


#This is no longer necessary, keeping it here just in case
# DATAdf <-DATAdf %>% 
#     group_by(LSOA11CD) %>%  #Remove double LSOA that may have sneaked in
#     summarise_all(funs(first)) %>%
#     group_by(LAD11CD) %>%
#     mutate(LSOACounts = n()) %>%
#     filter(LSOACounts>5)  %>% #remove any odd bits that have mixed up LAD code and name
#     ungroup %>%
#     select(-LSOACounts)






