#-------------------------------------------------------------------------------

# @Organization - UCSF
# @Project - Effects of floods on child mortality in Bangladesh
# @Author - Francois Rerolle, rerollefrancois@gmail.com
# @Description - This code extracts precipitation data over Bangladesh country

#-------------------------------------------------------------------------------

library(here)

source(here("child-mortality-dhs/R", "0-config.R"))

## Load data
# Bangladesh admin
BGD_Adm <- raster::getData("GADM", country = "BGD", level = 0, path = here("data/untouched/country-admin"))

flood_area_percent <- readRDS(here("data/final", "flood_area_percent"))

#######################
### Data extraction ###
#######################
#### Randomly sample a couple points over Bangladesh
set.seed(12345)
number_of_points_to_sample <- 100
random_sample_points_BGD <- st_sample(x = st_as_sf(BGD_Adm), size = number_of_points_to_sample)
random_sample_points_BGD_sf <- st_as_sf(random_sample_points_BGD) # convert to sf for chirps extraction

##### Environmental covariates 
##################################################################################################################################
#### Flood prone area status
random_sample_points_BGD_sf_flood_prone <- raster::extract(x = flood_area_percent, y = random_sample_points_BGD_sf)
#### CHIRPS
BGD_chirps <- NULL
for(i in 1988:2017){
  print(i)
  start_date_chirps <- paste0(i, "-01-01") # January first of year i
  end_date_chirps <- paste0(i, "-12-31")  # December 31st of year i
  
  # Extract daily time series at all points
  daily_chirps <- get_chirps(object = random_sample_points_BGD_sf,
                             dates = as.character(c(start_date_chirps,
                                                    end_date_chirps)),
                             server = "CHC",
                             resolution = 0.25)
  
  # Process product
  daily_chirps$ID <- 1:number_of_points_to_sample # Correct ID
  daily_chirps$Year <- i
  daily_chirps$Month <- as.numeric(substring(daily_chirps$date, first = 6, last = 7))
  daily_chirps$Flooded <- random_sample_points_BGD_sf_flood_prone > 0
  
  # Monthly aggregate
  monthly_chirps <- (daily_chirps %>%
                       filter(!is.na(Flooded)) %>%
                       group_by(Year, Month, Flooded) %>%
                       summarise(Mean_Precipitation_mm = mean(chirps, na.rm = T)))
  
  # Append
  BGD_chirps <- rbind(BGD_chirps,
                      monthly_chirps)
}


#-------------------------------------------------------------------------------

# Save dataset
saveRDS(BGD_chirps, file = here("data/final", "BGD_chirps"))
readr::write_csv(BGD_chirps, file = here("data/final", "BGD_chirps.csv"))

