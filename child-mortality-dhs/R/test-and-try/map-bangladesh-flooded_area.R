# DHS Bangladesh exploration

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
flood_area_percent_mask <- mask(flood_area_percent, BGD_Adm)
percent_flooded_area <- mean(flood_area_percent_mask[] > 0, na.rm = T)

quantile(flood_area_percent_mask[which(flood_area_percent_mask[] > 0)])
hist(100*flood_area_percent_mask[which(flood_area_percent_mask[] > 0)],
     freq = F,
     main = "Distribution of the percent number of flooded days in areas flooded at least one day",
     xlab = "Percent number of flooded days %")
abline(v = quantile(100*flood_area_percent_mask[which(flood_area_percent_mask[] > 0)])[2], col = "red")
abline(v = quantile(100*flood_area_percent_mask[which(flood_area_percent_mask[] > 0)])[3], col = "red")
abline(v = quantile(100*flood_area_percent_mask[which(flood_area_percent_mask[] > 0)])[4], col = "red")
# Cluster' s GPS
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

#-------------------------------------------------------------------------------

## Map data
# mapping
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") # Color blind friendly palette
scales::show_col(cbbPalette)

pal <- colorNumeric(c("#FFFFCC", "#41B6C4", "#0C2C84"), values(flood_area_percent),
                    na.color = "transparent")


map <- leaflet() %>% 
  # addProviderTiles(providers$Esri.WorldGrayCanvas,
  #                  "Default") %>%
  # addProviderTiles(providers$Esri.NatGeoWorldMap,
  #                  group = "National Geographic") %>%
  addPolygons(data = BGD_Adm,
              color = "black",
              weight = 3,
              fillOpacity = 0) %>%
  # #Exposure floods
  # addRasterImage(flood_area_percent_mask, colors = pal, opacity = 0.8, project = FALSE, group = "Flood prone area") %>%
  # addLegend(pal = pal, values = values(flood_area_percent), title = "Percent flooded") %>%
  
  #DHS clusters
  # addCircles(data = BDBR_2004_GPS, color = "#E69F00", opacity = 0.7, weight = 2, group = "DHS clusters 2004") %>%
  # addCircles(data = BDBR_2007_GPS, color = "#56B4E9", opacity = 0.7, weight = 2, group = "DHS clusters 2007") %>%
  # addCircles(data = BDBR_2011_GPS, color = "#009E73", opacity = 0.7, weight = 2, group = "DHS clusters 2011") %>%
  # addCircles(data = BDBR_2014_GPS, color = "#0072B2", opacity = 0.7, weight = 2, group = "DHS clusters 2014") %>%
  # addCircles(data = BDBR_2017_GPS, color = "#D55E00", opacity = 0.7, weight = 2, group = "DHS clusters 2017") %>%
  # addLegend(colors = c("#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00"),
  #           labels = c("DHS clusters 2004", "DHS clusters 2007", "DHS clusters 2011", "DHS clusters 2014", "DHS clusters 2017")) %>%
  
  addCircles(data = BDBR_2004_GPS, color = "black", opacity = 0.7, weight = 2, group = "DHS clusters") %>%
  addCircles(data = BDBR_2007_GPS, color = "black", opacity = 0.7, weight = 2, group = "DHS clusters 2007") %>%
  addCircles(data = BDBR_2011_GPS, color = "black", opacity = 0.7, weight = 2, group = "DHS clusters 2011") %>%
  addCircles(data = BDBR_2014_GPS, color = "black", opacity = 0.7, weight = 2, group = "DHS clusters 2014") %>%
  addCircles(data = BDBR_2017_GPS, color = "black", opacity = 0.7, weight = 2, group = "DHS clusters 2017") %>%
  addLegend(colors = c("black"),
            labels = c("DHS clusters")) %>%
  # # Layers control
  # addLayersControl(
  #   baseGroups = c("Default", "National Geographic"),
  #   overlayGroups = c("DHS clusters 2004", "DHS clusters 2007", "DHS clusters 2011", "DHS clusters 2014", "DHS clusters 2017", "Flood prone area"),
  #   options = layersControlOptions(collapsed = FALSE)
  # ) %>%
  # hideGroup(c("DHS clusters 2004", "DHS clusters 2007", "DHS clusters 2011", "DHS clusters 2014", "DHS clusters 2017")) %>%
  addScaleBar()

map


