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
IND_Adm <- raster::getData("GADM",
                           country = "IND",
                           level = 0,
                           path = here("data/untouched/country-admin"))
NPL_Adm <- raster::getData("GADM",
                           country = "NPL",
                           level = 0,
                           path = here("data/untouched/country-admin"))
BTN_Adm <- raster::getData("GADM",
                           country = "BTN",
                           level = 0,
                           path = here("data/untouched/country-admin"))
MMR_Adm <- raster::getData("GADM",
                           country = "MMR",
                           level = 0,
                           path = here("data/untouched/country-admin"))
CHI_Adm <- raster::getData("GADM",
                           country = "CHN",
                           level = 0,
                           path = here("data/untouched/country-admin"))

#-------------------------------------------------------------------------------

## Map data
# Create Spatial polygon outlining contour of indent of interest
x_coord <- c(((extent(BGD_Adm)[1]-4)*0.5 + (extent(BGD_Adm)[2]+4)*0.5), extent(BGD_Adm)[1]-4, extent(BGD_Adm)[1]-4, extent(BGD_Adm)[2]+4, extent(BGD_Adm)[2]+4)
y_coord <- c(extent(BGD_Adm)[3]-1.5, ((extent(BGD_Adm)[3]-1.5)*0.4 + (extent(BGD_Adm)[4]+2)*0.6), extent(BGD_Adm)[4]+2, extent(BGD_Adm)[4]+2, extent(BGD_Adm)[3]-1.5)
xym <- cbind(x_coord, y_coord)
p = Polygon(xym)
ps = Polygons(list(p),1)
sps = SpatialPolygons(list(ps))
proj4string(sps) = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

# Create Spatial polygon outlining cview boundaries
x_coord <- c(extent(BGD_Adm)[1]-8, extent(BGD_Adm)[1]-8, extent(BGD_Adm)[2]+8, extent(BGD_Adm)[2]+8)
y_coord <- c(extent(BGD_Adm)[3]-8, extent(BGD_Adm)[4]+8, extent(BGD_Adm)[4]+8, extent(BGD_Adm)[3]-8)
xym2 <- cbind(x_coord, y_coord)
p2 = Polygon(xym2)
ps2 = Polygons(list(p2),1)
sps2 = SpatialPolygons(list(ps2))
proj4string(sps2) = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")


## Crop to desired extent
IND_Adm <- crop(IND_Adm, sps)
NPL_Adm <- crop(NPL_Adm, sps)
BTN_Adm <- crop(BTN_Adm, sps)
CHI_Adm <- crop(CHI_Adm, sps)
MMR_Adm <- crop(MMR_Adm, sps)


### map
# Palette
factpal <- colorFactor(palette = "Dark2", 1:8, reverse = T)
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") # Color blind friendly palette
scales::show_col(cbbPalette)


### Less colourful


map <- leaflet() %>%
  addProviderTiles(providers$Thunderforest.Transport) %>%
  addPolygons(data = sps2,
              color = "green",
              weight = 2,
              fillOpacity = 1,
              opacity = 1)  %>%
  addPolygons(data = sps,
              color = "#41B6C4",
              weight = 0,
              fillOpacity = 1,
              opacity = 1)  %>%
  addPolygons(data = IND_Adm,
              color = "grey", fillColor = "white",
              weight = 2,
              fillOpacity = 1,
              opacity = 1) %>%
  addPolygons(data = NPL_Adm,
              color = "grey", fillColor = "white",
              weight = 2,
              fillOpacity = 1,
              opacity = 1) %>%
  addPolygons(data = BTN_Adm,
              color = "grey", fillColor = "white",
              weight = 2,
              fillOpacity = 1,
              opacity = 1) %>%
  addPolygons(data = MMR_Adm,
              color = "grey", fillColor = "white",
              weight = 2,
              fillOpacity = 1,
              opacity = 1) %>%
  addPolygons(data = CHI_Adm,
              color = "grey", fillColor = "white",
              weight = 2,
              fillOpacity = 1,
              opacity = 1) %>%
  addPolygons(data = sps,
              color = "black",
              weight = 8,
              fillOpacity = 0,
              opacity = 1)  %>%
  addPolygons(data = BGD_Adm,
              color = factpal(1), fillColor = "grey",
              weight = 2,
              fillOpacity = 1,
              opacity = 1) %>%
  addLabelOnlyMarkers(
    lng = 88.4,
    lat = 24.75,
    label = htmltools::HTML(paste0('<strong>',"Bangladesh",'<strong>')),
    labelOptions = labelOptions(noHide = T,
                                textOnly = T,
                                textsize = "30px")) %>%
  addLabelOnlyMarkers(
    lng = 85.5,
    lat = 24.3,
    label = htmltools::HTML(paste0('<strong>',"India",'<strong>')),
    labelOptions = labelOptions(noHide = T,
                                textOnly = T,
                                textsize = "25px"))  %>%
  addLabelOnlyMarkers(
    lng = 85.6,
    lat = 27.3,
    label = htmltools::HTML(paste0('<strong>',"Nepal",'<strong>')),
    labelOptions = labelOptions(noHide = T,
                                textOnly = T,
                                textsize = "25px"))   %>%
  addLabelOnlyMarkers(
    lng = 89.5,
    lat = 27.5,
    label = htmltools::HTML(paste0('<strong>',"Bhutan",'<strong>')),
    labelOptions = labelOptions(noHide = T,
                                textOnly = T,
                                textsize = "25px"))   %>%
  addLabelOnlyMarkers(
    lng = 87.8,
    lat = 28.35,
    label = htmltools::HTML(paste0('<strong>',"China",'<strong>')),
    labelOptions = labelOptions(noHide = T,
                                textOnly = T,
                                textsize = "25px"))   %>%
  addLabelOnlyMarkers(
    lng = 96,
    lat = 21,
    label = htmltools::HTML(paste0('<strong>',"Myanmar",'<strong>')),
    labelOptions = labelOptions(noHide = T,
                                textOnly = T,
                                textsize = "25px")) %>%
  addLabelOnlyMarkers(
    lng = 89.5,
    lat = 20.8,
    label = htmltools::HTML(paste0('<em>',"Bay of Bengal",'<em>')),
    labelOptions = labelOptions(noHide = T,
                                textOnly = T,
                                textsize = "15px")) 


map
