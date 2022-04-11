#-------------------------------------------------------------------------------

# @Organization - UCSF
# @Project - Effects of floods on child mortality in Bangladesh
# @Author - Francois Rerolle, rerollefrancois@gmail.com
# @Description - This code defines multiple flooded exposure based on
# distribution of flooded_prone_percent

#-------------------------------------------------------------------------------

# Source configuration code
library(here)
source(here("child-mortality-dhs/R", "0-config.R"))

#-------------------------------------------------------------------------------

## Load data
birth_records <- readRDS(file = here("data/final", "birth_records"))

#-------------------------------------------------------------------------------

## Process data
# Restrict dataset to unique flooded mothers
mother_records <- 
  (birth_records %>%
     dplyr::select(-c(Birth_Date_Month_CMC, Age_At_Death_Months)) %>%
     distinct(DHSCLUST, DHSYEAR, caseid, .keep_all = T) %>%
     filter(Flood_Prone_Percent > 0)
  )

# Assess distribution of flooded_prone_percent among flooded mothers
quartiles_flood_percent <- quantile(mother_records$Flood_Prone_Percent)

# Define flooded exposure
birth_records_flooded <- 
  (birth_records %>%
     mutate(
       Flooded_0_quartile = Flood_Prone_Percent >= quartiles_flood_percent["0%"],
       Flooded_1_quartile = Flood_Prone_Percent >= quartiles_flood_percent["25%"],
       Flooded_2_quartile = Flood_Prone_Percent >= quartiles_flood_percent["50%"],
       Flooded_3_quartile = Flood_Prone_Percent >= quartiles_flood_percent["75%"]
       )
   )

#-------------------------------------------------------------------------------

# Save dataset
saveRDS(birth_records_flooded, file = here("data/final", "birth_records_flooded"))
readr::write_csv(birth_records_flooded, file = here("data/final", "birth_records_flooded.csv"))

#-------------------------------------------------------------------------------