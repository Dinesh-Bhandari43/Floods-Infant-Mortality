#-------------------------------------------------------------------------------

# @Organization - UCSF
# @Project - Effects of floods on child mortality in Bangladesh
# @Author - Francois Rerolle, rerollefrancois@gmail.com
# @Description - This file cleans the birth record history

#-------------------------------------------------------------------------------

# Source configuration code
library(here)
source(here("child-mortality-dhs/R", "0-config.R"))

#-------------------------------------------------------------------------------

## Load data
birth_records_formated <- readRDS(file = here("data/temp",
                                              "birth_records_formated"))

#-------------------------------------------------------------------------------

## Process data
# Filter out data with missing flood exposure (15 out of 2935 clusters)
# 3 clusters with wrong GPS coordinates (0,0) from MIS source (not GPS)
# 12 clusters with missing flood exposure but apparent ok GPS coordinates

birth_records_0 <- (birth_records_formated %>%
                      filter(!is.na(Flood_Prone_Percent)))

# Restrict dataset to before 2017 survey wave started and back 30 years
birth_records_1 <- (birth_records_0 %>%
                      filter(Birth_Date_Month_CMC <= 1413) %>% # September 2017
                      filter(Birth_Date_Month_CMC >= 1053) # September 1987
                  )

# Restrict to birth occurring when mother lived at current place of residence
birth_records_2 <- (birth_records_1 %>%
                    mutate(Year_Of_Birth = 1900 + floor((Birth_Date_Month_CMC-1)/12)) %>%
                    filter(!(Years_Lived_In_Place_Of_Residence %in% c(95, 96))) %>% # Removes visitors to the community (95 and 96)
                      mutate(Years_Lived_In_Place_Of_Residence = replace_na(Years_Lived_In_Place_Of_Residence, 150)) %>% # When missing assuming lived there at time of birth
                    filter(!(DHSYEAR - (Years_Lived_In_Place_Of_Residence + 1) > Year_Of_Birth)) %>%
                    dplyr::select(-Year_Of_Birth, -Years_Lived_In_Place_Of_Residence)
                  )

# Restrict to singleton births
birth_records_3 <- (birth_records_2 %>%
                    filter(Twin_Birth == 0) %>%
                    dplyr::select(-Twin_Birth))

# Recode categorical variables using DHS guide to statistics and UN definitions or categories within dataset
birth_records_4 <- (birth_records_3 %>%
                      filter(!Source_Of_Drinking_Water %in% c("not a dejure resident", "not dejure resident")) %>%
                      mutate(
                        Source_Of_Drinking_Water = replace(Source_Of_Drinking_Water,
                                                           Source_Of_Drinking_Water %in% c("bottled water",
                                                                                           "cart with small tank",
                                                                                           "deet tubewell",
                                                                                           "piped inside dwel.",
                                                                                           "piped inside dwelling",
                                                                                           "piped into dwelling",
                                                                                           "piped outside dwel.",
                                                                                           "piped outside dwelling",
                                                                                           "piped to neighbor",
                                                                                           "piped to yard/plot",
                                                                                           "protected spring",
                                                                                           "protected well",
                                                                                           "public tap/standpipe",
                                                                                           "rainwater",
                                                                                           "shallow tubewell",
                                                                                           "tanker truck",
                                                                                           "tube well or borehole",
                                                                                           "tubewell"),
                                                           "Improved"),
                        Source_Of_Drinking_Water = replace(Source_Of_Drinking_Water,
                                                           Source_Of_Drinking_Water %in% c("other",
                                                                                           "pond/tank/lake",
                                                                                           "river/dam/lake/ponds/stream/canal/irrigation channel",
                                                                                           "river/stream",
                                                                                           "surface well/other well",
                                                                                           "surface/other well",
                                                                                           "unprotected spring",
                                                                                           "unprotected well"),
                                                           "Unimproved")) %>%
                          mutate(
                            Type_Of_Toilet_Facility = replace(Type_Of_Toilet_Facility,
                                                              Type_Of_Toilet_Facility %in% c("composting toilet",
                                                                                             "flush - don't know where",
                                                                                             "flush - to piped sewer system",
                                                                                             "flush - to pit latrine",
                                                                                             "flush - to septic tank",
                                                                                             "flush - to piped sewer system",
                                                                                             "flush to piped sewer system",
                                                                                             "flush to pit latrine",
                                                                                             "flush to septic tank",
                                                                                             "flush, don't know where",
                                                                                             "pit latrine - with slab",
                                                                                             "pit latrine with slab",
                                                                                             "septic tank/modern toilet",
                                                                                             "septic tank/toilet",
                                                                                             "ventilated improved pit latrine (vip)",
                                                                                             "water sealed/slab la",
                                                                                             "water sealed/slab latrine"),
                                                               "Improved"),
                            Type_Of_Toilet_Facility = replace(Type_Of_Toilet_Facility,
                                                              Type_Of_Toilet_Facility %in% c("bucket toilet",
                                                                                             "flush - to somewhere else",
                                                                                             "flush to somewhere else",
                                                                                             "hanging latrine",
                                                                                             "hanging toilet/latrine",
                                                                                             "open latrine",
                                                                                             "other",
                                                                                             "pit latrine",
                                                                                             "pit latrine - without slab / open pit",
                                                                                             "pit latrine without slab/open pit"),
                                                               "Unimproved"),
                            Type_Of_Toilet_Facility = replace(Type_Of_Toilet_Facility,
                                                              Type_Of_Toilet_Facility %in% c("no facility, bush",
                                                                                             "no facility/bush/field"),
                                                              "Unimproved") # Used to be "Open defecation". I decided to reduce categories to 2
                            ) %>%
                      mutate(
                        Main_Floor_Material = replace(Main_Floor_Material,
                                                      Main_Floor_Material %in% c("dung",
                                                                                 "earth, sand",
                                                                                 "earth/sand"),
                                                      "Rudimentary"), # Used to be "Natural"
                        Main_Floor_Material = replace(Main_Floor_Material,
                                                      Main_Floor_Material %in% c("earth/bamboo",
                                                                                 "earth/bamboo (katcha)",
                                                                                 "palm, bamboo",
                                                                                 "palm/bamboo",
                                                                                 "other"),
                                                          "Rudimentary"),
                        Main_Floor_Material = replace(Main_Floor_Material,
                                                      Main_Floor_Material %in% c("carpet",
                                                                                 "cement",
                                                                                 "cement/concrete",
                                                                                 "ceramic tiles",
                                                                                 "parquet or polished wood",
                                                                                 "parquet, polished wood",
                                                                                 "wood",
                                                                                 "wood planks"),
                                                          "Finished")
                      ) %>%
                      mutate(
                        Main_Roof_Material = replace(Main_Roof_Material,
                                                      Main_Roof_Material %in% c("no roof",
                                                                                "sod",
                                                                                "thatch / palm leaf",
                                                                                "thatch/palm leaf"),
                                                     "Rudimentary"), # Used to be "Natural"
                        Main_Roof_Material = replace(Main_Roof_Material,
                                                      Main_Roof_Material %in% c("bamboo",
                                                                                "cardboard",
                                                                                "earth/bamboo",
                                                                                "katcha (bamboo/thatch)",
                                                                                "other",
                                                                                "palm / bamboo",
                                                                                "palm/bamboo",
                                                                                "rustic mat",
                                                                                "wood planks"),
                                                      "Rudimentary"),
                        Main_Roof_Material = replace(Main_Roof_Material,
                                                      Main_Roof_Material %in% c("calamine/cement fiber",
                                                                                "cement",
                                                                                "cement/concrete",
                                                                                "cement/concrete/tiled",
                                                                                "ceramic tiles",
                                                                                "metal",
                                                                                "roofing shingles",
                                                                                "tin",
                                                                                "wood"),
                                                      "Finished")
                      ) %>%
                      mutate(
                        Main_Wall_Material = replace(Main_Wall_Material,
                                                     Main_Wall_Material %in% c("dirt",
                                                                               "natural walls",
                                                                               "no walls"),
                                                     "Rudimentary"), # Used to be "Natural"
                        Main_Wall_Material = replace(Main_Wall_Material,
                                                     Main_Wall_Material %in% c("bamboo with mud",
                                                                               "cane / palm / trunks",
                                                                               "cane/palm/trunks",
                                                                               "cardboard",
                                                                               "jute/bamboo/mud (katcha)",
                                                                               "other",
                                                                               "plywood",
                                                                               "reused wood",
                                                                               "rudimentary walls",
                                                                               "stone with mud",
                                                                               "uncovered adobe"),
                                                     "Rudimentary"),
                        Main_Wall_Material = replace(Main_Wall_Material,
                                                     Main_Wall_Material %in% c("brick/cement",
                                                                               "bricks",
                                                                               "cement",
                                                                               "cement blocks",
                                                                               "covered adobe",
                                                                               "stone with lime / cement",
                                                                               "stone with lime/cement",
                                                                               "tin",
                                                                               "wood",
                                                                               "wood planks / shingles",
                                                                               "wood planks/shingles"),
                                                     "Finished")
                      )
                    )

# Recode missing values
birth_records <- (birth_records_4 %>%
                    mutate(Highest_Level_Education = as.factor(replace(Highest_Level_Education, Highest_Level_Education == 9, NA)),
                           Source_Of_Drinking_Water = as.factor(Source_Of_Drinking_Water),
                           Type_Of_Toilet_Facility = as.factor(Type_Of_Toilet_Facility),
                           Main_Floor_Material = as.factor(replace(Main_Floor_Material, Main_Floor_Material %in% c(99), NA)),
                           Main_Wall_Material = as.factor(replace(Main_Wall_Material, Main_Wall_Material %in% c(99), NA)),
                           Main_Roof_Material = as.factor(replace(Main_Roof_Material, Main_Roof_Material %in% c(99), NA)),
                           Toilets_Facilities_Shared_Other_HH = as.factor(Toilets_Facilities_Shared_Other_HH),
                           Wealth_Index = as.factor(Wealth_Index)
                           # , Type_Of_Place_Of_Residence = as.factor(Type_Of_Place_Of_Residence)
                           )
                  )

#-------------------------------------------------------------------------------

# Save dataset
saveRDS(birth_records, file = here("data/final", "birth_records"))
readr::write_csv(birth_records, file = here("data/final", "birth_records.csv"))

#-------------------------------------------------------------------------------