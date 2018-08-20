setwd(file.path(basewd, "VOA data"))

#Creates Tourism dataframe at LSOA level
Tourism <- read_delim("uk-englandwales-ndr-2017-listentries-compiled-epoch-0002-baseline-csv.csv", 
                      delim = "*", col_names = F) %>%
  mutate(X15 = gsub(" ", "", X15)) %>%
  left_join(., CorePstCd, by = c("X15" = "Postcode")) %>%
  #filter(!is.na(LAD11CD)) %>%
  filter(grepl("(SELF CATER)|(HOLIDAY)|(HOTEL)|(HOSTEL)|(GUEST)",X6))  %>%
  mutate(Guest = grepl("(SELF CATER)|(HOLIDAY)|(GUEST)",X6),
         Hotel = grepl("(HOTEL)|(HOSTEL)",X6)) %>%
  group_by(MSOA11CD) %>% #Have X6 as a grouping variable to 
  summarise(Tourism = n(),
            Guest = sum(Guest),
            Hotel = sum(Hotel))  %>%
  full_join(MSOAtoLAD,., by="MSOA11CD") %>%  #ensures all MSOA
  mutate(Tourism = ifelse(is.na(Tourism), 0, Tourism),
         Hotel = ifelse(is.na(Hotel), 0, Hotel),
         Guest = ifelse(is.na(Guest), 0, Guest))

MSOAAdjacency <- readShapeSpatial(file.path(basewd,"/ShapeFiles/Middle_Layer_Super_Output_Areas_December_2011_Super_Generalised_Clipped_Boundaries_in_England_and_Wales",
                                            "Middle_Layer_Super_Output_Areas_December_2011_Super_Generalised_Clipped_Boundaries_in_England_and_Wales.shp"))

test <- poly2nb(MSOAAdjacency)

MSOAAdjMat = nb2mat(test, style="B",zero.policy=T)

#Add MSOA codes so that they are added as the vertex names
colnames(MSOAAdjMat) <- MSOAAdjacency$msoa11cd

MSOAgraph <- graph.adjacency(MSOAAdjMat, mode = "undirected")

MSOAEgoList1 <- ego(MSOAgraph, 1) %>%map(~names(.x))
names(MSOAEgoList1) <- get.vertex.attribute(MSOAgraph, "name")

MSOAEgoList2 <- ego(MSOAgraph, 2) %>%map(~names(.x))
names(MSOAEgoList2) <- get.vertex.attribute(MSOAgraph, "name")


TourismMSOAEgo2 <- MSOAEgoList2 %>% map_df(~{
  Tourism %>%
    filter(MSOA11CD %in% .x) %>%
    summarise(Ego2Tourism = sum(Tourism),
              Ego2Guest = sum(Guest),
              Ego2Hotel = sum(Hotel),
              Ego2Homes = sum(Homes))
}) %>% mutate(Ego2TourismDens = Ego2Tourism/Ego2Homes,
              Ego2GuestDens = Ego2Guest/Ego2Homes,
              Ego2HotelDens = Ego2Hotel/Ego2Homes,
              MSOA11CD = names(MSOAEgoList1))



TourismMSOAEgo1 <- MSOAEgoList1 %>% map_df(~{
  Tourism %>%
    filter(MSOA11CD %in% .x) %>%
    summarise(Ego1Tourism = sum(Tourism),
              Ego1Guest = sum(Guest),
              Ego1Hotel = sum(Hotel),
              Ego1Homes = sum(Homes))
}) %>% mutate(Ego1TourismDens = Ego1Tourism/Ego1Homes,
              Ego1GuestDens = Ego1Guest/Ego1Homes,
              Ego1HotelDens = Ego1Hotel/Ego1Homes,
              MSOA11CD = names(MSOAEgoList1))

Tourism <- Tourism %>%
  left_join(., TourismMSOAEgo1, by = "MSOA11CD") %>%
  left_join(., TourismMSOAEgo2, by = "MSOA11CD")

saveRDS(Tourism, file.path(DataFolder, "Tourism.rds"))

rm(MSOAAdjacency);rm(test);rm(MSOAAdjMat); rm(MSOAgraph); rm(MSOAEgoList1);rm(MSOAEgoList2); rm(TourismMSOAEgo1);rm(TourismMSOAEgo2)
