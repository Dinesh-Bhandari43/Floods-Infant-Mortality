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

# Bangladesh admin
BGD_Adm <- raster::getData("GADM",
                           country = "BGD",
                           level = 0,
                           path = here("data/untouched/country-admin"))

# Flood
flood_area_percent <- readRDS(here("data/final", "flood_area_percent"))
flood_area_percent_mask <- raster::mask(flood_area_percent, BGD_Adm)

#-------------------------------------------------------------------------------

## Process data
# Identify quartiles of distribution among flooded pixels
mean(flood_area_percent_mask[]>0, na.rm = T) # Percent of country classified in flood prone area
flood_area_percent_among_flooded_area <- flood_area_percent_mask[which(flood_area_percent_mask[] > 0)]
quartiles_flood_percent <- quantile(flood_area_percent_among_flooded_area)

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