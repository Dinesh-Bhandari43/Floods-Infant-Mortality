#-------------------------------------------------------------------------------

# @Organization - UCSF
# @Project - Effects of floods on child mortality in Bangladesh
# @Author - Francois Rerolle, rerollefrancois@gmail.com
# @Description - This file plots the 7 country wide flood events

#-------------------------------------------------------------------------------

# Source configuration code
library(here)
source(here("child-mortality-dhs/R", "0-config.R"))

#-------------------------------------------------------------------------------

# Bangladesh admin
BGD_Adm <- raster::getData("GADM",
                           country = "BGD",
                           level = 1,
                           path = here("data/untouched/country-admin"))

# Floods
flood_jun_2002 <- stack(here("data/untouched/floods",
                             "DFO_1974_From_20020621_to_20020828",
                             "DFO_1974_From_20020621_to_20020828.tif"))
flood_jun_2003 <- stack(here("data/untouched/floods",
                             "DFO_2248_From_20030611_to_20031010",
                             "DFO_2248_From_20030611_to_20031010.tif"))
flood_jul_2004 <- stack(here("data/untouched/floods",
                             "DFO_2507_From_20040620_to_20041007",
                             "DFO_2507_From_20040620_to_20041007.tif"))
flood_jul_2007 <- stack(here("data/untouched/floods",
                             "DFO_3136_From_20070721_to_20071015",
                             "DFO_3136_From_20070721_to_20071015.tif"))
flood_jul_2010 <- stack(here("data/untouched/floods",
                             "DFO_3679_From_20100701_to_20100713",
                             "DFO_3679_From_20100701_to_20100713.tif"))
flood_oct_2010 <- stack(here("data/untouched/floods",
                             "DFO_3732_From_20101001_to_20101012",
                             "DFO_3732_From_20101001_to_20101012.tif"))
flood_aug_2017 <- stack(here("data/untouched/floods",
                             "DFO_4508_From_20170810_to_20170826",
                             "DFO_4508_From_20170810_to_20170826.tif"))

#-------------------------------------------------------------------------------

## Process data
# Crop to Bangladesh extent
flood_jun_2002_cropped <- crop(flood_jun_2002$flooded, BGD_Adm)
flood_jun_2003_cropped <- crop(flood_jun_2003$flooded, BGD_Adm)
flood_jul_2004_cropped <- crop(flood_jul_2004$flooded, BGD_Adm)
flood_jul_2007_cropped <- crop(flood_jul_2007$flooded, BGD_Adm)
flood_jul_2010_cropped <- crop(flood_jul_2010$flooded, BGD_Adm)
flood_oct_2010_cropped <- crop(flood_oct_2010$flooded, BGD_Adm)
flood_aug_2017_cropped <- crop(flood_aug_2017$flooded, BGD_Adm)

# mask to Bangladesh extent
flood_jun_2002_masked <- raster::mask(flood_jun_2002_cropped, BGD_Adm)
flood_jun_2003_masked <- raster::mask(flood_jun_2003_cropped, BGD_Adm)
flood_jul_2004_masked <- raster::mask(flood_jul_2004_cropped, BGD_Adm)
flood_jul_2007_masked <- raster::mask(flood_jul_2007_cropped, BGD_Adm)
flood_jul_2010_masked <- raster::mask(flood_jul_2010_cropped, BGD_Adm)
flood_oct_2010_masked <- raster::mask(flood_oct_2010_cropped, BGD_Adm)
flood_aug_2017_masked <- raster::mask(flood_aug_2017_cropped, BGD_Adm)

#-------------------------------------------------------------------------------

## Plot data
pal <- colorRampPalette(c("#FFFFCC", "#41B6C4"))
## Map data
flood_jun_2002Plot <- rasterVis::levelplot(flood_jun_2002_masked, 
                                           margin = FALSE,                       
                                           colorkey = NULL,
                                           main = NULL,
                                           par.settings = list(
                                             axis.line = list(col = 'transparent') 
                                           ),
                                           xlab = NULL,
                                           ylab = NULL,
                                           scales = list(draw = FALSE),            
                                           col.regions = pal
) +
  latticeExtra::layer(sp.polygons(BGD_Adm, lwd = 1.3))  +
  latticeExtra::layer(panel.text(90.1, 21.1, 'June 2002'))

flood_jun_2003Plot <- rasterVis::levelplot(flood_jun_2003_masked, 
                                           margin = FALSE,                       
                                           colorkey = NULL,
                                           main = NULL,
                                           par.settings = list(
                                             axis.line = list(col = 'transparent') 
                                           ),
                                           xlab = NULL,
                                           ylab = NULL,
                                           scales = list(draw = FALSE),            
                                           col.regions = pal
) +
  latticeExtra::layer(sp.polygons(BGD_Adm, lwd = 1.3))  +
  latticeExtra::layer(panel.text(90.1, 21.1, 'June 2003'))

flood_jul_2004Plot <- rasterVis::levelplot(flood_jul_2004_masked, 
                                           margin = FALSE,                       
                                           colorkey = NULL,
                                           main = NULL,
                                           par.settings = list(
                                             axis.line = list(col = 'transparent') 
                                           ),
                                           xlab = NULL,
                                           ylab = NULL,
                                           scales = list(draw = FALSE),            
                                           col.regions = pal
) +
  latticeExtra::layer(sp.polygons(BGD_Adm, lwd = 1.3))  +
  latticeExtra::layer(panel.text(90.1, 21.1, 'July 2004'))

flood_jul_2007Plot <- rasterVis::levelplot(flood_jul_2007_masked, 
                                           margin = FALSE,                       
                                           colorkey = NULL,
                                           main = NULL,
                                           par.settings = list(
                                             axis.line = list(col = 'transparent') 
                                           ),
                                           xlab = NULL,
                                           ylab = NULL,
                                           scales = list(draw = FALSE),            
                                           col.regions = pal
) +
  latticeExtra::layer(sp.polygons(BGD_Adm, lwd = 1.3))  +
  latticeExtra::layer(panel.text(90.1, 21.1, 'July 2007'))

flood_jul_2010Plot <- rasterVis::levelplot(flood_jul_2010_masked, 
                                           margin = FALSE,                       
                                           colorkey = NULL,
                                           main = NULL,
                                           par.settings = list(
                                             axis.line = list(col = 'transparent') 
                                           ),
                                           xlab = NULL,
                                           ylab = NULL,
                                           scales = list(draw = FALSE),            
                                           col.regions = pal
) +
  latticeExtra::layer(sp.polygons(BGD_Adm, lwd = 1.3))  +
  latticeExtra::layer(panel.text(90.1, 21.1, 'July 2010'))

flood_oct_2010Plot <- rasterVis::levelplot(flood_oct_2010_masked, 
                                           margin = FALSE,                       
                                           colorkey = NULL,
                                           main = NULL,
                                           par.settings = list(
                                             axis.line = list(col = 'transparent') 
                                           ),
                                           xlab = NULL,
                                           ylab = NULL,
                                           scales = list(draw = FALSE),            
                                           col.regions = pal
) +
  latticeExtra::layer(sp.polygons(BGD_Adm, lwd = 1.3))  +
  latticeExtra::layer(panel.text(90.1, 21.1, 'October 2010'))

flood_aug_2017Plot <- rasterVis::levelplot(flood_aug_2017_masked, 
                                           margin = FALSE,                       
                                           colorkey = NULL,
                                           main = NULL,
                                           par.settings = list(
                                             axis.line = list(col = 'transparent') 
                                           ),
                                           xlab = NULL,
                                           ylab = NULL,
                                           scales = list(draw = FALSE),            
                                           col.regions = pal
) +
  latticeExtra::layer(sp.polygons(BGD_Adm, lwd = 1.3))  +
  latticeExtra::layer(panel.text(90.1, 21.1, 'August 2017'))



pdf(here("child-mortality-dhs/output/figures", "flood-events-map.pdf"))
gridExtra::grid.arrange(flood_jun_2002Plot,
                        flood_jun_2003Plot,
                        flood_jul_2004Plot,
                        flood_jul_2007Plot,
                        flood_jul_2010Plot,
                        flood_oct_2010Plot,
                        flood_aug_2017Plot)
dev.off()



