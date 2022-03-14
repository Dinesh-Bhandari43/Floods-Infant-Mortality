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
# Filter out data with missing flood exposure (13 out of 2594 clusters)
# 3 clusters with wrong GPS coordinates (0,0) from MIS source (not GPS)
# 10 clusters with missing flood exposure but apparent ok GPS coordinates

birth_records_0 <- (birth_records_formated %>%
                      filter(!is.na(Flooded)))

# Restrict dataset to 5 years prior 2004 flood and 5 years after 2007 flood
birth_records <- (birth_records_0 %>%
                    filter(Birth_Date_Month_CMC >= 1189) %>% # January 1999
                    filter(Birth_Date_Month_CMC <= 1356) # December 2012
                  )


#-------------------------------------------------------------------------------

# Save dataset
saveRDS(birth_records, file = here("data/final", "birth_records"))
readr::write_csv(birth_records, file = here("data/final", "birth_records.csv"))

#-------------------------------------------------------------------------------