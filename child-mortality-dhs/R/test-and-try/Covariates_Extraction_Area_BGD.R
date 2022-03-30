#-------------------------------------------------------------------------------

# @Organization - UCSF
# @Project - Effects of floods on child mortality in Bangladesh
# @Author - Francois Rerolle, rerollefrancois@gmail.com
# @Description - This code extracts environmental covariates over Bangladesh country

#-------------------------------------------------------------------------------

# # Import packages
# library(readxl)
# library(sf)
# library(tidyverse)
# library(raster)
# # library(MODIS)
# library(MODISTools)
# library(chirps)
# library(rmapshaper)


library(here)

source(here("child-mortality-dhs/R", "0-config.R"))

## Load data
# Bangladesh admin
BGD_Adm <- raster::getData("GADM", country = "BGD", level = 0, path = here("data/untouched/country-admin"))


#######################
### Data extraction ###
#######################
# For altitude and Modis covariates, idea is to download and rasterize data then perform extraction.
# For chirps we can download and extract in 1 step
library(rasterVis)
param_meta$terraclim
aa <- getTerraClim(AOI = st_as_sf(BGD_Adm), param = "aet", startDate = "2004-01-01", endDate = "2004-12-31")
bb <- mask(aa[[1]], BGD_Adm)

gplot(bb) + 
  geom_tile(aes(fill = value)) +
  facet_wrap(~ variable) +
  scale_fill_gradientn(colours = rev(terrain.colors(225))) +
  coord_equal()

#### Randomly sample a couple points over Bangladesh
set.seed(12345)
number_of_points_to_sample <- 100
random_sample_points_BGD <- st_sample(x = st_as_sf(BGD_Adm), size = number_of_points_to_sample)
random_sample_points_BGD_sf <- st_as_sf(random_sample_points_BGD) # convert to sf for chirps extraction

##### Environmental covariates 
##################################################################################################################################
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
  
  # Monthly aggregate
  monthly_chirps <- (daily_chirps %>%
                       group_by(Year, Month) %>%
                       summarise(Mean_Precipitation_mm = mean(chirps, na.rm = T)))
  
  # Append
  BGD_chirps <- rbind(BGD_chirps,
                      monthly_chirps)
}


#-------------------------------------------------------------------------------

# Save dataset
saveRDS(BGD_chirps, file = here("data/final", "BGD_chirps"))
readr::write_csv(BGD_chirps, file = here("data/final", "BGD_chirps.csv"))

#-------------------------------------------------------------------------------


##################################################################################################################################
#### MODIS
mt_products()
mt_bands(product = "MOD11A2")
mt_bands(product = "VNP13A1")

### Define function for Modis data download
modis_local_rasterization <- function(product, band,
                                      shapefile, horizontal_km_range, vertical_km_range,
                                      start_date_to_download, end_date_to_download,
                                      fill_value){
  extent <- extent(shapefile)
  center_point_lat <- (extent@ymin + extent@ymax)/2
  center_point_lon <- (extent@xmin + extent@xmax)/2
  
  modis_download <- mt_subset(product = product,
                              band = band,
                              lat = center_point_lat,
                              lon = center_point_lon,
                              km_lr = horizontal_km_range,
                              km_ab = vertical_km_range,
                              start = start_date_to_download,
                              end = end_date_to_download,
                              progress = FALSE)
  
  modis_download_raster <- mt_to_raster(df = modis_download %>% mutate(value = replace(value, value == fill_value, NA)),
                                        reproject = T)
  
  return(modis_download_raster)
}

### Download
start_date_to_download <- "2017-01-15" 
end_date_to_download <- "2017-04-15" 

## LST (~1 min each downloading time)
# Day 
LST_Day_1km_Raster_BG <- modis_local_rasterization(product = "MOD11A2", #LST 1km resolution, 8 day average
                                           shapefile = BG_Adm, # Bangladesh
                                           horizontal_km_range = 100,
                                           vertical_km_range = 100,
                                           band = "LST_Day_1km",  # Day temperature
                                           start_date_to_download = start_date_to_download,
                                           end_date_to_download = end_date_to_download,
                                           fill_value = 0)
names(LST_Day_1km_Raster_BG) <- paste0(names(LST_Day_1km_Raster_BG), "_LST.Day.1km")

# Night 
LST_Night_1km_Raster_Z <- modis_local_rasterization(product = "MOD11A2", #LST 1km resolution, 8 Night average
                                                  shapefile = BG_Adm, # Bangladesh
                                                  horizontal_km_range = 100,
                                                  vertical_km_range = 75,
                                                  band = "LST_Night_1km",  # Night temperature
                                                  start_date_to_download = start_date_to_download,
                                                  end_date_to_download = end_date_to_download,
                                                  fill_value = 0)
names(LST_Night_1km_Raster_Z) <- paste0(names(LST_Night_1km_Raster_Z), "_LST.Night.1km")

## EVI (~4 min each downloading time)
EVI_500m_Raster_Z <- modis_local_rasterization(product = "VNP13A1", #EVI 500m resolution, 16 day average
                                                    shapefile = HCCA_shapefiles_Z, # Zambezi
                                                    horizontal_km_range = 100,
                                                    vertical_km_range = 75,
                                                    band = "500_m_16_days_EVI",  # EVI
                                                    start_date_to_download = start_date_to_download,
                                                    end_date_to_download = end_date_to_download,
                                                    fill_value = -15000)
names(EVI_500m_Raster_Z) <- paste0(names(EVI_500m_Raster_Z), "_EVI.500m")

EVI_500m_Raster_O <- modis_local_rasterization(product = "VNP13A1", #EVI 500m resolution, 16 day average
                                                    shapefile = HCCA_shapefiles_O, # Ohangwena
                                                    horizontal_km_range = 100,
                                                    vertical_km_range = 75,
                                                    band = "500_m_16_days_EVI",  # EVI
                                                    start_date_to_download = start_date_to_download,
                                                    end_date_to_download = end_date_to_download,
                                                    fill_value = -15000)
names(EVI_500m_Raster_O) <- paste0(names(EVI_500m_Raster_O), "_EVI.500m")



#### Extract
### Worksite
## Zambezi
Site_Z_LST_Day_1km_K_wide <- raster::extract(x = LST_Day_1km_Raster_Z, y = Site_Z %>% dplyr::select(lon, lat))
Site_Z_LST_Night_1km_K_wide <- raster::extract(x = LST_Night_1km_Raster_Z, y = Site_Z %>% dplyr::select(lon, lat))
Site_Z_EVI_500m_wide <- raster::extract(x = EVI_500m_Raster_Z, y = Site_Z %>% dplyr::select(lon, lat))

# Reshape
Site_Z_wide <- cbind(Site_Z_LST_Day_1km_K_wide, Site_Z_LST_Night_1km_K_wide, Site_Z_EVI_500m_wide)
Site_Z_long <- (Site_Z_wide
                %>% as_tibble()
                %>% mutate(site_name = Site_Z$site_name)
                %>% pivot_longer(cols = -site_name,
                                 names_to = c("Date", ".value"),
                                 names_sep = "_"
                                 )
                )

## Ohangwena
Site_O_LST_Day_1km_K_wide <- raster::extract(x = LST_Day_1km_Raster_O, y = Site_O %>% dplyr::select(lon, lat))
Site_O_LST_Night_1km_K_wide <- raster::extract(x = LST_Night_1km_Raster_O, y = Site_O %>% dplyr::select(lon, lat))
Site_O_EVI_500m_wide <- raster::extract(x = EVI_500m_Raster_O, y = Site_O %>% dplyr::select(lon, lat))

# Reshape
Site_O_wide <- cbind(Site_O_LST_Day_1km_K_wide, Site_O_LST_Night_1km_K_wide, Site_O_EVI_500m_wide)
Site_O_long <- (Site_O_wide
                %>% as_tibble()
                %>% mutate(site_name = Site_O$site_name)
                %>% pivot_longer(cols = -site_name,
                                 names_to = c("Date", ".value"),
                                 names_sep = "_"
                )
)

## Append together
Site_long <- rbind(Site_Z_long, Site_O_long)



### HCCA
## Zambezi
HCCA_Z_LST_Day_1km_Mean_K_wide <- raster::extract(x = LST_Day_1km_Raster_Z, y = HCCA_shapefiles_Z, fun = mean, na.rm = T, df = T)
HCCA_Z_LST_Day_1km_Min_K_wide <- raster::extract(x = LST_Day_1km_Raster_Z, y = HCCA_shapefiles_Z, fun = min, na.rm = T, df = T)
HCCA_Z_LST_Day_1km_Max_K_wide <- raster::extract(x = LST_Day_1km_Raster_Z, y = HCCA_shapefiles_Z, fun = max, na.rm = T, df = T)
HCCA_Z_LST_Night_1km_Mean_K_wide <- raster::extract(x = LST_Night_1km_Raster_Z, y = HCCA_shapefiles_Z, fun = mean, na.rm = T, df = T)
HCCA_Z_LST_Night_1km_Min_K_wide <- raster::extract(x = LST_Night_1km_Raster_Z, y = HCCA_shapefiles_Z, fun = min, na.rm = T, df = T)
HCCA_Z_LST_Night_1km_Max_K_wide <- raster::extract(x = LST_Night_1km_Raster_Z, y = HCCA_shapefiles_Z, fun = max, na.rm = T, df = T)
HCCA_Z_EVI_500m_Mean_K_wide <- raster::extract(x = EVI_500m_Raster_Z, y = HCCA_shapefiles_Z, fun = mean, na.rm = T, df = T)
HCCA_Z_EVI_500m_Min_K_wide <- raster::extract(x = EVI_500m_Raster_Z, y = HCCA_shapefiles_Z, fun = min, na.rm = T, df = T)
HCCA_Z_EVI_500m_Max_K_wide <- raster::extract(x = EVI_500m_Raster_Z, y = HCCA_shapefiles_Z, fun = max, na.rm = T, df = T)


# Reshape
HCCA_Z_wide <- cbind(HCCA_Z_LST_Day_1km_Mean_K_wide %>% dplyr::select(-ID) %>% rename_with(.fn = function(x){return(paste0(x,".Mean"))}), HCCA_Z_LST_Day_1km_Min_K_wide %>% dplyr::select(-ID) %>% rename_with(.fn = function(x){return(paste0(x,".Min"))}), HCCA_Z_LST_Day_1km_Max_K_wide %>% dplyr::select(-ID) %>% rename_with(.fn = function(x){return(paste0(x,".Max"))}),
                     HCCA_Z_LST_Night_1km_Mean_K_wide %>% dplyr::select(-ID) %>% rename_with(.fn = function(x){return(paste0(x,".Mean"))}), HCCA_Z_LST_Night_1km_Min_K_wide %>% dplyr::select(-ID) %>% rename_with(.fn = function(x){return(paste0(x,".Min"))}), HCCA_Z_LST_Night_1km_Max_K_wide %>% dplyr::select(-ID) %>% rename_with(.fn = function(x){return(paste0(x,".Max"))}),
                     HCCA_Z_EVI_500m_Mean_K_wide %>% dplyr::select(-ID) %>% rename_with(.fn = function(x){return(paste0(x,".Mean"))}), HCCA_Z_EVI_500m_Min_K_wide %>% dplyr::select(-ID) %>% rename_with(.fn = function(x){return(paste0(x,".Min"))}), HCCA_Z_EVI_500m_Max_K_wide %>% dplyr::select(-ID) %>% rename_with(.fn = function(x){return(paste0(x,".Max"))}))
HCCA_Z_long <- (HCCA_Z_wide
                %>% as_tibble()
                %>% mutate(HCCA = HCCA_shapefiles_Z$HCCA)
                %>% pivot_longer(cols = -HCCA,
                                 names_to = c("Date", ".value"),
                                 names_sep = "_"
                )
)

## Ohangwena
HCCA_O_LST_Day_1km_Mean_K_wide <- raster::extract(x = LST_Day_1km_Raster_O, y = HCCA_shapefiles_O, fun = mean, na.rm = T, df = T)
HCCA_O_LST_Day_1km_Min_K_wide <- raster::extract(x = LST_Day_1km_Raster_O, y = HCCA_shapefiles_O, fun = min, na.rm = T, df = T)
HCCA_O_LST_Day_1km_Max_K_wide <- raster::extract(x = LST_Day_1km_Raster_O, y = HCCA_shapefiles_O, fun = max, na.rm = T, df = T)
HCCA_O_LST_Night_1km_Mean_K_wide <- raster::extract(x = LST_Night_1km_Raster_O, y = HCCA_shapefiles_O, fun = mean, na.rm = T, df = T)
HCCA_O_LST_Night_1km_Min_K_wide <- raster::extract(x = LST_Night_1km_Raster_O, y = HCCA_shapefiles_O, fun = min, na.rm = T, df = T)
HCCA_O_LST_Night_1km_Max_K_wide <- raster::extract(x = LST_Night_1km_Raster_O, y = HCCA_shapefiles_O, fun = max, na.rm = T, df = T)
HCCA_O_EVI_500m_Mean_K_wide <- raster::extract(x = EVI_500m_Raster_O, y = HCCA_shapefiles_O, fun = mean, na.rm = T, df = T)
HCCA_O_EVI_500m_Min_K_wide <- raster::extract(x = EVI_500m_Raster_O, y = HCCA_shapefiles_O, fun = min, na.rm = T, df = T)
HCCA_O_EVI_500m_Max_K_wide <- raster::extract(x = EVI_500m_Raster_O, y = HCCA_shapefiles_O, fun = max, na.rm = T, df = T)


# Reshape
HCCA_O_wide <- cbind(HCCA_O_LST_Day_1km_Mean_K_wide %>% dplyr::select(-ID) %>% rename_with(.fn = function(x){return(paste0(x,".Mean"))}), HCCA_O_LST_Day_1km_Min_K_wide %>% dplyr::select(-ID) %>% rename_with(.fn = function(x){return(paste0(x,".Min"))}), HCCA_O_LST_Day_1km_Max_K_wide %>% dplyr::select(-ID) %>% rename_with(.fn = function(x){return(paste0(x,".Max"))}),
                     HCCA_O_LST_Night_1km_Mean_K_wide %>% dplyr::select(-ID) %>% rename_with(.fn = function(x){return(paste0(x,".Mean"))}), HCCA_O_LST_Night_1km_Min_K_wide %>% dplyr::select(-ID) %>% rename_with(.fn = function(x){return(paste0(x,".Min"))}), HCCA_O_LST_Night_1km_Max_K_wide %>% dplyr::select(-ID) %>% rename_with(.fn = function(x){return(paste0(x,".Max"))}),
                     HCCA_O_EVI_500m_Mean_K_wide %>% dplyr::select(-ID) %>% rename_with(.fn = function(x){return(paste0(x,".Mean"))}), HCCA_O_EVI_500m_Min_K_wide %>% dplyr::select(-ID) %>% rename_with(.fn = function(x){return(paste0(x,".Min"))}), HCCA_O_EVI_500m_Max_K_wide %>% dplyr::select(-ID) %>% rename_with(.fn = function(x){return(paste0(x,".Max"))}))
HCCA_O_long <- (HCCA_O_wide
                %>% as_tibble()
                %>% mutate(HCCA = HCCA_shapefiles_O$HCCA)
                %>% pivot_longer(cols = -HCCA,
                                 names_to = c("Date", ".value"),
                                 names_sep = "_"
                )
)


## Append together
HCCA_long <- rbind(HCCA_Z_long, HCCA_O_long)



##################################################################################################################################
### Clean different datasets before final merge
# Anomalies
HCCA_long[HCCA_long == "Inf" | HCCA_long == "-Inf" | HCCA_long == "NaN"] <- NA

# Clean date
HCCA_Modis <- (HCCA_long
               %>% mutate(Date = gsub(x = Date, pattern = "X", replacement = ""),
                          Date = gsub(x = Date, pattern = "\\.", replacement = "-"),
                          Date = as.POSIXct(x = Date, format = "%Y-%m-%d"))
               )

Site_Modis <- (Site_long
               %>% mutate(Date = gsub(x = Date, pattern = "X", replacement = ""),
                          Date = gsub(x = Date, pattern = "\\.", replacement = "-"),
                          Date = as.POSIXct(x = Date, format = "%Y-%m-%d"))
)

Incident_Cases_all_Clean <- (Incident_Cases_all
                             %>% mutate(Week_start = replace(Week_start, (Week_start == "3/2/2020") & (lag(Week_start, 1) == "20/01/2020"), "27/01/2020"),
                                        Date = Week_start,
                                        Date = replace(Date, Date == "13/01/2030", "13/01/2020"),
                                        Date = replace(Date, Date == "30/12/19", "30/12/2019"),
                                        Date = gsub(pattern = "/20$", x = Date, replacement = "/2020"),
                                        Date = as.POSIXct(x = Date, format = "%d/%m/%Y"))
                             )

CrossSectional_all_Clean <- (CrossSectional_all 
                             %>% mutate(Date = as.POSIXct(x = s2s2_datetxt,
                                                          format = "%Y-%m-%d"))
                             )


##################################################################################################################################
### Append Modis covariates to incidence and survey datasets and include CHIRPS variables
#### Incidence data
## Append altitude
Incident_Cases_all_Alt <- (Incident_Cases_all_Clean
                         %>% left_join(HCCA_Alt
                                       %>% as_tibble()
                                       %>% dplyr::select(HCCA, Altitude_Mean_m, Altitude_Max_m, Altitude_Min_m)
                                       %>% rename(HFCA = HCCA)
                                       )
                         )

## For Modis covariates, find Modis 8 days averages in 30 days prior survey date and average them
Incident_Modis <- NULL
for (i in 1:nrow(Incident_Cases_all_Alt)){
  HCCA_Modis_i <- (HCCA_Modis
                   %>% filter(HCCA == Incident_Cases_all_Alt$HFCA[i])
                   %>% filter(((Incident_Cases_all_Alt$Date[i] - Date) <= 30) & ((Incident_Cases_all_Alt$Date[i] - Date) >= 0))
                   %>% dplyr::select(-HCCA, -Date)
                   )
  
  Incident_Modis_i <- apply(X = HCCA_Modis_i, MARGIN = 2, FUN = mean, na.rm = T)
  
  Incident_Modis <- rbind(Incident_Modis, Incident_Modis_i)
}


Incident_Cases_all_Alt_MODIS <- cbind(Incident_Cases_all_Alt,
                                      Incident_Modis %>% as_tibble() %>% remove_rownames())

## For Chirps, download, extract and append all at once
# Simplify Sibinda shapefile
HCCA_shapefiles_Z6 <- rmapshaper::ms_simplify(input = as(HCCA_shapefiles[6,], 'Spatial')) %>% st_as_sf()
HCCA_shapefiles[6,] <- HCCA_shapefiles_Z6

Incident_Chirps <- NULL
for (i in 165:nrow(Incident_Cases_all_Alt_MODIS)){ # Takes some time and needs good internet as it is extracting over large HCCA. If loop breaks, just restart it at the last i. (or we can insert the for loop inside a while loop but that's usually mot recommended)
  print(i)
  HCCA_shapefiles_i <- HCCA_shapefiles[which(HCCA_shapefiles$HCCA == Incident_Cases_all_Alt_MODIS$HFCA[i]),]
  start_date_chirps <- round(Incident_Cases_all_Alt_MODIS$Date[i] - 30*24*3600, "days") # Beware of change of time zone with summer times: time becomes included and messes up the rest. So better to round to days
  end_date_chirps <- Incident_Cases_all_Alt_MODIS$Date[i]
  
  mean <- get_chirps(object = HCCA_shapefiles_i, dates = as.character(c(start_date_chirps, end_date_chirps)), operation = 5)
  Incident_Chirps_Mean_i <- mean(mean$chirps, na.rm = T)
  min <- get_chirps(object = HCCA_shapefiles_i, dates = as.character(c(start_date_chirps, end_date_chirps)), operation = 0)
  Incident_Chirps_Min_i <- min(min$chirps, na.rm = T)
  max <- get_chirps(object = HCCA_shapefiles_i, dates = as.character(c(start_date_chirps, end_date_chirps)), operation = 1)
  Incident_Chirps_Max_i <- max(max$chirps, na.rm = T)
  
  Incident_Chirps_i <- cbind("Chirps.Mean" = Incident_Chirps_Mean_i,
                             "Chirps.Min" = Incident_Chirps_Min_i,
                             "Chirps.Max" = Incident_Chirps_Max_i)
  
  Incident_Chirps <- rbind(Incident_Chirps, Incident_Chirps_i)
}

Incident_Cases_all_Alt_MODIS_Chirps <- cbind(Incident_Cases_all_Alt_MODIS,
                                             Incident_Chirps %>% as_tibble() %>% remove_rownames())







#### Cross-sectional data
## Append altitude
CrossSectional_all_Alt <- (CrossSectional_all_Clean
                           %>% left_join(Site %>% dplyr::select(site_name, Altitude_m),
                                         by = c("site_id" = "site_name"))
                           )

## For Modis covariates, find Modis 8 days averages in 30 days prior survey date and average them
CrossSectional_Modis <- NULL
for (i in 1:nrow(CrossSectional_all_Alt)){
  Site_Modis_i <- (Site_Modis
                   %>% filter(site_name == CrossSectional_all_Alt$site_id[i])
                   %>% filter(((CrossSectional_all_Alt$Date[i] - Date) <= 30) & ((CrossSectional_all_Alt$Date[i] - Date) >= 0))
                   %>% dplyr::select(-site_name, -Date)
  )
  
  CrossSectional_Modis_i <- apply(X = Site_Modis_i, MARGIN = 2, FUN = mean, na.rm = T)
  
  CrossSectional_Modis <- rbind(CrossSectional_Modis, CrossSectional_Modis_i)
}


CrossSectional_all_Alt_MODIS <- cbind(CrossSectional_all_Alt,
                                      CrossSectional_Modis %>% as_tibble() %>% remove_rownames())

## For Chirps, download, extract and append all at once
Date_Site <- (Date_Site
              %>% filter(!is.na(lat) & !is.na(lon))
              )
Date_Site_Chirps <- NULL
for (i in 1398:nrow(Date_Site)){ # Takes some time (~) and needs good internet as it is extracting at many worksite. If loop breaks, just restart it at the last i (or we can insert the for loop inside a while loop but that's usually mot recommended)
  print(i)
  aa <- get_chirps(object = data.frame(lat = Site$lat[which(Site$site_name == Date_Site$site_name[i])], lon = Site$lon[which(Site$site_name == Date_Site$site_name[i])]), dates = as.character(c(round(Date_Site$Date[i] - 30*24*3600, "days"), Date_Site$Date[i])), dist = 100)
  Date_Site_Chirps <- rbind(Date_Site_Chirps, mean(aa$chirps, na.rm = T))
}

CrossSectional_Cases_all_Alt_MODIS_Chirps <- (CrossSectional_all_Alt_MODIS
                                              %>% left_join(cbind(Date_Site %>% dplyr::select(site_name, Date) %>% rename(site_id = site_name), "Chirps" = Date_Site_Chirps))
                                              )





####################
####### Save #######
####################
CrossSectional_all_Covariates <- CrossSectional_Cases_all_Alt_MODIS_Chirps
Incident_Cases_all_Covariates <- Incident_Cases_all_Alt_MODIS_Chirps

setwd("/Users/francoisrerolle/Desktop/AutoEntrepreneur/Jenny/Covariates extraction main analysis/Data With Covariates")
write.csv(x = CrossSectional_all_Covariates, file = "CrossSectional_all_Covariates.csv")
write.csv(x = Incident_Cases_all_Covariates, file = "Incident_Cases_all_Covariates.csv")
