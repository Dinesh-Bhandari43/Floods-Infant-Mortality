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
                      filter(!is.na(Flooded)))

# Restrict dataset to before 2017 survey wave started and back 30 years
birth_records_1 <- (birth_records_0 %>%
                      filter(Birth_Date_Month_CMC <= 1413) %>% # September 2017
                      filter(Birth_Date_Month_CMC >= 1053) # September 1987
                  )

# Restrict to birth occuring when mother lived at current place of residence
birth_records <- (birth_records_1 %>%
                    mutate(Year_Of_Birth = 1900 + floor((Birth_Date_Month_CMC-1)/12)) %>%
                    filter(Years_Lived_In_Place_Of_Residence < 95) %>% # Removes visitors to the community (95 and 96)
                    filter(DHSYEAR - (Years_Lived_In_Place_Of_Residence + 1) <= Year_Of_Birth)
                  )


#-------------------------------------------------------------------------------

# Save dataset
saveRDS(birth_records, file = here("data/final", "birth_records"))
readr::write_csv(birth_records, file = here("data/final", "birth_records.csv"))

#-------------------------------------------------------------------------------