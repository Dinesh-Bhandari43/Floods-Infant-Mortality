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

## Plot floods
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
  # latticeExtra::layer(panel.text(88.1, 26.5, "a", cex = 1.5, fontface="bold"))

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
  # latticeExtra::layer(panel.text(88.1, 26.5, "b", cex = 1.5, fontface="bold"))

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
  # latticeExtra::layer(panel.text(88.1, 26.5, "c", cex = 1.5, fontface="bold"))

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
  # latticeExtra::layer(panel.text(88.1, 26.5, "d", cex = 1.5, fontface="bold"))

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
  # latticeExtra::layer(panel.text(88.1, 26.5, "e", cex = 1.5, fontface="bold"))

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
  # latticeExtra::layer(panel.text(88.1, 26.5, "f", cex = 1.5, fontface="bold"))

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
  # latticeExtra::layer(panel.text(88.1, 26.5, "g", cex = 1.5, fontface="bold"))





#-------------------------------------------------------------------------------
# Estimate pixel-wise correlation between the different flood events

#-------------------------------
# extract values from each raster
#-------------------------------
flood_jun_2002_vals <- raster::values(flood_jun_2002_masked)
flood_jun_2003_vals <- raster::values(flood_jun_2003_masked)
flood_jul_2004_vals <- raster::values(flood_jul_2004_masked)
flood_jul_2007_vals <- raster::values(flood_jul_2007_masked)
flood_jul_2010_vals <- raster::values(flood_jul_2010_masked)
flood_oct_2010_vals <- raster::values(flood_oct_2010_masked)
flood_aug_2017_vals <- raster::values(flood_aug_2017_masked)

#-------------------------------
# combine into a single matrix
#-------------------------------
flood_vals <- data.frame(
  Jun_2002 = flood_jun_2002_vals,
  Jun_2003 = flood_jun_2003_vals,
  Jul_2004 = flood_jul_2004_vals,
  Jul_2007 = flood_jul_2007_vals,
  Jul_2010 = flood_jul_2010_vals,
  Oct_2010 = flood_oct_2010_vals,
  Aug_2017 = flood_aug_2017_vals
  ) %>%
  # drop the masked pixels that are all NA
  filter(!is.na(Jun_2002))

#-------------------------------
# correlation for all pixels
#-------------------------------
# color palette
pcols <- viridis(n=9)

( flood_corrs <- cor(flood_vals, use = "pairwise.complete.obs") )
plot_flood_cors <- ggcorrplot(corr=flood_corrs,
                             legend.title = "Correlation",
                             type = "lower",
                             lab = TRUE,
                             lab_size = 3,
                             outline.color="white"
                             ) +
  scale_fill_gradient2(breaks=c(0, 1), limit=c(0, 1), 
                       low="white",high=pcols[5], 
                       guide = guide_colorbar(title="Pixel-level correlation\nbetween flood events")) +
  theme(
    legend.position = "top",
    legend.title = element_text(vjust=1),
  )
plot_flood_cors


#-------------------------------
# calculate the number of times
# a pixel was in the flood zone
# identify pixels that were 
# ever flooded over the 7 floods
#-------------------------------
flood_vals2 <- flood_vals %>%
  mutate(nfloods = Jun_2002+Jun_2003+Jul_2004+Jul_2007+Jul_2010+Oct_2010+Aug_2017,
         flooded = ifelse(nfloods>0,1,0))

#-------------------------------
# in locations that ever flooded
# estimate the percentage of pixels
# by number of times they flooded (1 to 7)
#-------------------------------
nflood_pct <- flood_vals2 %>%
  filter(flooded==1) %>% 
  dplyr::select(nfloods) %>%
  group_by(nfloods) %>%
  mutate(nobs = sum(!is.na(nfloods))) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(totobs = sum(nobs),
         nfloodpct = nobs/totobs*100,
         cumfloodpct = cumsum(nfloodpct), 
         nfloodscum = factor(nfloods, levels=1:7,labels=paste0("≥ ",1:7))
  ) %>%
  # create the reverse cumulative distribution
  arrange(desc(nfloods)) %>%
  mutate(cumfloodpct2 = cumsum(nfloodpct))

#-------------------------------
# plot the % of pixels by the
# number of times they flooded
#-------------------------------
plot_nflood_cumdist <- ggplot(nflood_pct, aes(x=nfloodscum, y = cumfloodpct2)) +
  geom_bar(stat = "identity", width = 0.5, fill=pcols[5], alpha=0.7,color=NA) + 
  geom_text(aes(label = paste0(sprintf("%1.0f",cumfloodpct2),"%")), 
            position = position_nudge(y=2), size=3) +
  labs(x = "Number of times flooded during the 7 flood events", y = "Percentage of flooded pixels (%)") +
  # scale_x_continuous(breaks=1:7) +
  scale_y_continuous(breaks=seq(0,100,by=20)) +
  coord_cartesian(ylim=c(0,100)) +
  theme_minimal() +
  theme(
    axis.text = element_text(size=12),
    axis.title = element_text(size=13),
    panel.grid.minor.x = element_blank()
  )
plot_nflood_cumdist


#-------------------------------
# create a composite figure
# with 7 small replicates plus
# the correlation and cumdist 
# panels
#-------------------------------
plot_flood_composite <- cowplot::plot_grid(
  flood_jun_2002Plot,
  flood_jun_2003Plot,
  flood_jul_2004Plot,
  flood_jul_2007Plot,
  flood_jul_2010Plot,
  flood_oct_2010Plot,
  flood_aug_2017Plot,
  plot_flood_cors,
  plot_nflood_cumdist,
  ncol = 3, 
  labels = "auto", label_size = 18
  )

ggsave(filename = here("child-mortality-dhs/output/figures","flood-event-summary.png"),
       plot_flood_composite,
       device="png",
       width=360,height=360, units = "mm")

