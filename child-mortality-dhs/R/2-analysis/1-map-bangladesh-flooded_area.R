#-------------------------------------------------------------------------------

# @Organization - UCSF
# @Project - Effects of floods on child mortality in Bangladesh
# @Author - Francois Rerolle, rerollefrancois@gmail.com
# @Description - This code maps flood prone areas

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
flood_area_percent_mask <- mask(flood_area_percent, BGD_Adm)
flood_area_percent_mask_2 <- 100*flood_area_percent_mask

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
BDBR_2000_GPS <- st_read(here("data/untouched/dhs",
                              "BD_1999-00_DHS_03072022_1129_172978/BDGE42FL",
                              "BDGE42FL.shp"))

BDBR_GPS <- rbind(BDBR_2017_GPS,
                  BDBR_2014_GPS,
                  BDBR_2011_GPS,
                  BDBR_2007_GPS,
                  BDBR_2004_GPS,
                  BDBR_2000_GPS)

BDBR_GPS$Flooded <- extract(flood_area_percent_mask_2, BDBR_GPS)
BDBR_GPS_Flooded = BDBR_GPS %>% filter(Flooded > 0)

#-------------------------------------------------------------------------------
pal2 <- colorNumeric(c("#FFFFCC", "#41B6C4", "#0C2C84"), values(flood_area_percent_mask_2),
                    na.color = "transparent")

pal <- colorRampPalette(c("#FFFFCC", "#41B6C4", "#0C2C84"))

## Map data
flood_area_percentPlot <- rasterVis::levelplot(flood_area_percent_mask_2, 
                                                       margin = FALSE,                       
                                                       colorkey = NULL,
                                                       main = NULL,
                                                       par.settings = list(
                                                         axis.line = list(col = 'transparent') 
                                                       ),
                                                       xlab = NULL,
                                                       ylab = NULL,
                                                       scales = list(draw = FALSE),            
                                                       col.regions = pal,
                                                       at = seq(from = 0, to = 80, by = 1)
) +
  latticeExtra::layer(sp.polygons(BGD_Adm, lwd = 2))  
# +   latticeExtra::layer(panel.text(90.3, 26.7, 'Percent number of flooded days (%)'))



pdf(here("child-mortality-dhs/output/figures", "flood-area-percent-map.pdf"))
gridExtra::grid.arrange(flood_area_percentPlot)
dev.off()


### DHS cluster locations
dhs_cluster_plot <- rasterVis::levelplot(flood_area_percent_mask, 
                                         margin = FALSE,                       
                                         colorkey = FALSE,
                                         main = NULL,
                                         par.settings = list(
                                           axis.line = list(col = 'transparent') 
                                         ),
                                         xlab = NULL,
                                         ylab = NULL,
                                         scales = list(draw = FALSE),            
                                         col.regions = pal2,
                                         at = seq(from = 0, to = 80, by = 1)
) +
  latticeExtra::layer(sp.polygons(BGD_Adm, lwd = 2, fill = "grey")) +
  latticeExtra::layer(sp.points(as_Spatial(BDBR_GPS), pch = 19, cex = 0.3, col = "#FFFFCC")) +
  latticeExtra::layer(sp.points(as_Spatial(BDBR_GPS_Flooded), pch = 19, cex = 0.3, col = "#41B6C4"))

pdf(here("child-mortality-dhs/output/figures", "dhs-clusters-map.pdf"))
dhs_cluster_plot
dev.off() 

# Arranged together
pdf(here("child-mortality-dhs/output/figures", "figure-1-top.pdf"))
gridExtra::grid.arrange(flood_area_percentPlot, dhs_cluster_plot,
                        nrow = 1)
dev.off()
