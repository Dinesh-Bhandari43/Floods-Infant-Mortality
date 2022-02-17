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

# Floods
# flood_jul_2004 <- stack(here("data/untouched/floods",
#                              "DFO_2507_From_20040620_to_20041007",
#                              "DFO_2507_From_20040620_to_20041007.tif")) # For later
flood_jul_2007 <- stack(here("data/untouched/floods",
                             "DFO_3136_From_20070721_to_20071015",
                             "DFO_3136_From_20070721_to_20071015.tif"))

# Bangladesh admin
BG_Adm <- getData("GADM",
                  country = "BGD",
                  level = 1,
                  path = here("data/untouched/country-admin"))
#-------------------------------------------------------------------------------



#-------------------------------------------------------------------------------
## Process data
# Collate birth record across DHS datasets
BDBR <- rbind(BDBR_2017 %>%
                dplyr::select(b3, b7, DHSCLUST = v001) %>%
                mutate(DHSYEAR = 2018),
              BDBR_2014 %>%
                dplyr::select(b3, b7, DHSCLUST = v001) %>%
                mutate(DHSYEAR = 2014),
              BDBR_2011 %>%
                dplyr::select(b3, b7, DHSCLUST = v001) %>%
                mutate(DHSYEAR = 2011),
              BDBR_2007 %>%
                dplyr::select(b3, b7, DHSCLUST = v001) %>%
                mutate(DHSYEAR = 2007),
              BDBR_2004 %>%
                dplyr::select(b3, b7, DHSCLUST = v001) %>%
                mutate(DHSYEAR = 2004))

# Collate GPS DHS datasets
BDBR_GPS <- rbind(BDBR_2017_GPS,
                  BDBR_2014_GPS,
                  BDBR_2011_GPS,
                  BDBR_2007_GPS,
                  BDBR_2004_GPS)

## Extract flood exposure
# Crop flood map to Bangladesh's extent (adding some margins)
# flood_jul_2004_cropped <- crop(flood_jul_2004, 1.2 * extent(BG_Adm))
flood_jul_2007_cropped <- crop(flood_jul_2007, 1.2 * extent(BG_Adm))

# Extract
BDBR_GPS$Flooded <- raster::extract(x = flood_jul_2007_cropped[[1]],
                                    y = BDBR_GPS)

# Merge
BDBR_Flood <- (BDBR %>% left_join(BDBR_GPS))
#-------------------------------------------------------------------------------



#-------------------------------------------------------------------------------
# Finalize dataset
birth_records <- (BDBR_Flood %>%
                    mutate(Age_At_Death_Months = b7,
                           Birth_Date_Month_CMC = b3,
                           Region = toupper(ADM1NAME),
                           Region = ifelse(test = Region == "MYMENSINGH",
                                           yes = "DHAKA",
                                           no = Region),
                           LATNUM = replace(LATNUM, LATNUM == 0, NA),
                           LONGNUM = replace(LONGNUM, LONGNUM == 0, NA)) %>%
                    dplyr::select(Birth_Date_Month_CMC,
                                  Age_At_Death_Months,
                                  Region,
                                  DHSCLUST,
                                  DHSYEAR,
                                  URBAN_RURA,
                                  LATNUM,
                                  LONGNUM,
                                  Flooded))

#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# Save dataset
saveRDS(birth_records, file = here("data/final", "birth_records"))
readr::write_csv(birth_records, file = here("data/final", "birth_records.csv"))
#-------------------------------------------------------------------------------
