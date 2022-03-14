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
# 12 clusters with missing flood exposure but apparent ok GPS coordinates (To sort out later)

birth_records_0 <- (birth_records_formated %>%
                      filter(!is.na(Flooded)))

# Restrict dataset to 30 years before start of 2017 DHS survey wave
birth_records <- (birth_records_0 %>%
                    filter(Birth_Date_Month_CMC <= 1414) %>% # October 2017 (start of DHS2018 survey)
                    filter(Birth_Date_Month_CMC >= 1055) # November 1987 (30 years before)
                  )


#-------------------------------------------------------------------------------

# Save dataset
saveRDS(birth_records, file = here("data/final", "birth_records"))
readr::write_csv(birth_records, file = here("data/final", "birth_records.csv"))

#-------------------------------------------------------------------------------