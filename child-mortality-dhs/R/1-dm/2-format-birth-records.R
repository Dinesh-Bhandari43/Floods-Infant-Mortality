#-------------------------------------------------------------------------------

# @Organization - UCSF
# @Project - Effects of floods on child mortality in Bangladesh
# @Author - Francois Rerolle, rerollefrancois@gmail.com
# @Description - This file formats the birth record history out of the DHS data
#                and characterize flood exposure

#-------------------------------------------------------------------------------

# Source configuration code
library(here)
source(here("child-mortality-dhs/R", "0-config.R"))

#-------------------------------------------------------------------------------

## Load data
# Flood area
flood_area_percent <- readRDS(here("data/final", "flood_area_percent"))
flood_frequency <- readRDS(here("data/final", "flood_frequency"))
flood_year <- readRDS(here("data/final", "flood_year"))

# DHS birth records
BDBR_2017 <- read_dta(here("data/untouched/dhs",
                           "BD_2017-18_DHS_02082022_855_172978/BDBR7RDT",
                           "BDBR7RFL.DTA"))
BDBR_2014 <- read_dta(here("data/untouched/dhs",
                           "BD_2014_DHS_02082022_855_172978/BDBR72DT",
                           "BDBR72FL.DTA"))
BDBR_2011 <- read_dta(here("data/untouched/dhs",
                           "BD_2011_DHS_02032022_1032_172978/BDBR61DT",
                           "BDBR61FL.DTA"))
BDBR_2007 <- read_dta(here("data/untouched/dhs",
                           "BD_2007_DHS_02032022_1033_172978/BDBR51DT",
                           "BDBR51FL.DTA"))
BDBR_2004 <- read_dta(here("data/untouched/dhs",
                           "BD_2004_DHS_02032022_1033_172978/BDBR4JDT",
                           "BDBR4JFL.DTA"))
BDBR_1999 <- read_dta(here("data/untouched/dhs",
                           "BD_1999-00_DHS_03072022_1129_172978/BDBR41DT",
                           "BDBR41FL.DTA"))

# Cluster's GPS
BDBR_2017_GPS <- st_read(here("data/untouched/dhs",
                              "BD_2017-18_DHS_02082022_855_172978/BDGE7SFL",
                              "BDGE7SFL.shp"))
BDBR_2014_GPS <- st_read(here("data/untouched/dhs",
                              "BD_2014_DHS_02082022_855_172978/BDGE71FL",
                              "BDGE71FL.shp"))
BDBR_2011_GPS <- st_read(here("data/untouched/dhs",
                              "BD_2011_DHS_02032022_1032_172978/BDGE61FL",
                              "BDGE61FL.shp"))
BDBR_2007_GPS <- st_read(here("data/untouched/dhs",
                              "BD_2007_DHS_02032022_1033_172978/BDGE52FL",
                              "BDGE52FL.shp"))
BDBR_2004_GPS <- st_read(here("data/untouched/dhs",
                              "BD_2004_DHS_02032022_1033_172978/BDGE4JFL",
                              "BDGE4JFL.shp"))
BDBR_1999_GPS <- st_read(here("data/untouched/dhs",
                              "BD_1999-00_DHS_03072022_1129_172978/BDGE42FL",
                              "BDGE42FL.shp"))

#-------------------------------------------------------------------------------

## Process data
# Collate birth record across DHS datasets replacing values by their labels with to_factor
BDBR <- rbind_labelled(BDBR_2017 %>%
                mutate(v106 = as.character(to_factor(v106)),
                       v190 = as.character(to_factor(v190)),
                       v025 = as.character(to_factor(v025)),
                       v113 = as.character(to_factor(v113)),
                       v116 = as.character(to_factor(v116)),
                       v127 = as.character(to_factor(v127)),
                       v128 = as.character(to_factor(v128)),
                       v129 = as.character(to_factor(v129)),
                       v160 = as.character(to_factor(v160))) %>%
                  dplyr::select(b3, b7, DHSCLUST = v001, v005, v104, b0,
                              caseid, v011, v106, v190, v201, v212,
                              v025, v113, v116, v127, v128, v129, v160) %>%
                mutate(DHSYEAR = 2018),
              BDBR_2014 %>%
                mutate(v106 = as.character(to_factor(v106)),
                       v190 = as.character(to_factor(v190)),
                       v025 = as.character(to_factor(v025)),
                       v113 = as.character(to_factor(v113)),
                       v116 = as.character(to_factor(v116)),
                       v127 = as.character(to_factor(v127)),
                       v128 = as.character(to_factor(v128)),
                       v129 = as.character(to_factor(v129)),
                       v160 = as.character(to_factor(v160))) %>%
                dplyr::select(b3, b7, DHSCLUST = v001, v005, v104, b0,
                              caseid, v011, v106, v190, v201, v212,
                              v025, v113, v116, v127, v128, v129, v160) %>%
                mutate(DHSYEAR = 2014),
              BDBR_2011 %>%
                mutate(v106 = as.character(to_factor(v106)),
                       v190 = as.character(to_factor(v190)),
                       v025 = as.character(to_factor(v025)),
                       v113 = as.character(to_factor(v113)),
                       v116 = as.character(to_factor(v116)),
                       v127 = as.character(to_factor(v127)),
                       v128 = as.character(to_factor(v128)),
                       v129 = as.character(to_factor(v129)),
                       v160 = as.character(to_factor(v160))) %>%
                dplyr::select(b3, b7, DHSCLUST = v001, v005, v104, b0,
                              caseid, v011, v106, v190, v201, v212,
                              v025, v113, v116, v127, v128, v129, v160) %>%
                mutate(DHSYEAR = 2011),
              BDBR_2007 %>%
                mutate(v106 = as.character(to_factor(v106)),
                       v190 = as.character(to_factor(v190)),
                       v025 = as.character(to_factor(v025)),
                       v113 = as.character(to_factor(v113)),
                       v116 = as.character(to_factor(v116)),
                       v127 = as.character(to_factor(v127)),
                       v128 = as.character(to_factor(v128)),
                       v129 = as.character(to_factor(v129)),
                       v160 = as.character(to_factor(v160))) %>%
                dplyr::select(b3, b7, DHSCLUST = v001, v005, v104, b0,
                              caseid, v011, v106, v190, v201, v212,
                              v025, v113, v116, v127, v128, v129, v160) %>%
                mutate(DHSYEAR = 2007),
              BDBR_2004 %>%
                mutate(v160 = 2004) %>%  #Input toilets shared. Actual value doesn't matter as matching happens within survey wave
                mutate(v106 = as.character(to_factor(v106)),
                       v190 = as.character(to_factor(v190)),
                       v025 = as.character(to_factor(v025)),
                       v113 = as.character(to_factor(v113)),
                       v116 = as.character(to_factor(v116)),
                       v127 = as.character(to_factor(v127)),
                       v128 = as.character(to_factor(v128)),
                       v129 = as.character(to_factor(v129)),
                       v160 = as.character(to_factor(v160))) %>%
                dplyr::select(b3, b7, DHSCLUST = v001, v005, v104, b0,
                              caseid, v011, v106, v190, v201, v212,
                              v025, v113, v116, v127, v128, v129, v160) %>%
                mutate(DHSYEAR = 2004),
              BDBR_1999 %>%
                mutate(v160 = 1999, #Input toilets shared. Actual value doesn't matter as matching happens within survey wave
                       v190 = 1999) %>% #Input wealth index for 1999 survey wave. Actual value doesn't matter as matching happens within survey wave
                mutate(v106 = as.character(to_factor(v106)),
                       v190 = as.character(to_factor(v190)),
                       v025 = as.character(to_factor(v025)),
                       v113 = as.character(to_factor(v113)),
                       v116 = as.character(to_factor(v116)),
                       v127 = as.character(to_factor(v127)),
                       v128 = as.character(to_factor(v128)),
                       v129 = as.character(to_factor(v129)),
                       v160 = as.character(to_factor(v160))) %>%
                dplyr::select(b3, b7, DHSCLUST = v001, v005, v104, b0,
                              caseid, v011, v106, v190, v201, v212,
                              v025, v113, v116, v127, v128, v129, v160) %>%
                mutate(DHSYEAR = 2000)
              )

# Collate GPS DHS datasets
BDBR_GPS <- rbind(BDBR_2017_GPS,
                  BDBR_2014_GPS,
                  BDBR_2011_GPS,
                  BDBR_2007_GPS,
                  BDBR_2004_GPS,
                  BDBR_1999_GPS)

## Extract flood exposure
# Extract flood exposure at DHS clusters
BDBR_GPS$Flood_Prone_Percent <- raster::extract(x = flood_area_percent,
                                                y = BDBR_GPS)

BDBR_GPS$Flood_Frequency <- raster::extract(x = flood_frequency,
                                                y = BDBR_GPS)

#PNAS revison 2 most recent flood event before survey
BDBR_GPS$Flood_Year <- c(raster::extract(x = flood_year$flooded.7,
                                             y = BDBR_2017_GPS),
                             raster::extract(x = flood_year$flooded.6,
                                             y = BDBR_2014_GPS),
                             raster::extract(x = flood_year$flooded.6,
                                             y = BDBR_2011_GPS),
                             raster::extract(x = flood_year$flooded.4,
                                             y = BDBR_2007_GPS),
                             raster::extract(x = flood_year$flooded.3,
                                             y = BDBR_2004_GPS),
                             raster::extract(x = flood_year$flooded.1,
                                             y = BDBR_1999_GPS))

# Merge
BDBR_Flood <- (BDBR %>% left_join(BDBR_GPS))

#-------------------------------------------------------------------------------

# Finalize dataset
birth_records_formated <- 
  (BDBR_Flood %>%
     mutate(Age_At_Death_Months = b7,
            Birth_Date_Month_CMC = b3,
            Twin_Birth = b0,
            Region = toupper(ADM1NAME),
            Region = ifelse(test = Region == "MYMENSINGH", # New division not present in earlier survey
                            yes = "DHAKA",
                            no = Region),
            Region = ifelse(test = Region == "RAJASHAHI", # Typo
                            yes = "RAJSHAHI",
                            no = Region),
            LATNUM = replace(LATNUM, LATNUM == 0, NA),
            LONGNUM = replace(LONGNUM, LONGNUM == 0, NA),
            Years_Lived_In_Place_Of_Residence = v104,
            Women_Sampling_Weight = v005/1000000,
            Birth_Date_Mother_Month_CMC = v011,
            Highest_Level_Education = v106,
            Wealth_Index = v190,
            Total_Children = v201,
            Age_Mother_First_Birth_Years = v212,
            Type_Of_Place_Of_Residence = v025,
            Source_Of_Drinking_Water = v113,
            Type_Of_Toilet_Facility = v116,
            Main_Floor_Material = v127,
            Main_Wall_Material = v128,
            Main_Roof_Material = v129,
            Toilets_Facilities_Shared_Other_HH = v160) %>%
     dplyr::select(Birth_Date_Month_CMC,
                   Age_At_Death_Months,
                   Twin_Birth,
                   Region,
                   DHSCLUST,
                   DHSYEAR,
                   URBAN_RURA,
                   LATNUM,
                   LONGNUM,
                   caseid,
                   Years_Lived_In_Place_Of_Residence,
                   Women_Sampling_Weight,
                   Birth_Date_Mother_Month_CMC,
                   Highest_Level_Education,
                   Wealth_Index,
                   Total_Children,
                   Age_Mother_First_Birth_Years,
                   # Type_Of_Place_Of_Residence, # Redundant with URBAN_RURA
                   Source_Of_Drinking_Water,
                   Type_Of_Toilet_Facility,
                   Main_Floor_Material,
                   Main_Wall_Material,
                   Main_Roof_Material,
                   Toilets_Facilities_Shared_Other_HH,
                   Flood_Prone_Percent,
                   Flood_Frequency,
                   Flood_Year))

#-------------------------------------------------------------------------------

# Save dataset
saveRDS(birth_records_formated,
        file = here("data/temp", "birth_records_formated"))
readr::write_csv(birth_records_formated,
                 file = here("data/temp", "birth_records_formated.csv"))

#-------------------------------------------------------------------------------
