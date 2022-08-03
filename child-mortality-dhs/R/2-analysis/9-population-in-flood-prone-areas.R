#-------------------------------------------------------------------------------

# @Organization - UCSF
# @Project - Effects of floods on child mortality in Bangladesh
# @Author - Francois Rerolle, rerollefrancois@gmail.com
# @Description - This code computes the totla population living in flood prone
# areas of Bangladesh

#-------------------------------------------------------------------------------

library(here)

source(here("child-mortality-dhs/R", "0-config.R"))

#-------------------------------------------------------------------------------

## Load data
# Bangladesh admin
BGD_Adm <- raster::getData("GADM",
                           country = "BGD",
                           level = 0,
                           path = here("data/untouched/country-admin"))

# Flood
flood_area_percent <- readRDS(here("data/final", "flood_area_percent"))


# 2020 World pop
worldpop2020 <- raster(here("data/untouched/worldpop/bgd_ppp_2020_1km_Aggregated_UNadj.tif"))


### Process data
# Aggregate flood map to population raster resolution
flood_area_percent_agg <- raster::resample(x = flood_area_percent > 0 ,
                                           y = worldpop2020,
                                           method = "ngb")

# Mask to country boundaries
flood_area_percent_mask <- raster::mask(flood_area_percent, BGD_Adm)
flood_area_percent_agg_mask <- raster::mask(flood_area_percent_agg, BGD_Adm)
worldpop2020_mask <- raster::mask(worldpop2020, BGD_Adm)

# Compute total population within flood prone areas
sum(worldpop2020_mask[]*(flood_area_percent_agg_mask[] > 0), na.rm = T)

# Check proportion of country flooded is similar after resampling
mean((flood_area_percent_agg_mask[] > 0), na.rm = T) 
mean((flood_area_percent_mask[] > 0), na.rm = T) 



