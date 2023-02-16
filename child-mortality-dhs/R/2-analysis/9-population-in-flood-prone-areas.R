#-------------------------------------------------------------------------------

# @Organization - UCSF
# @Project - Effects of floods on child mortality in Bangladesh
# @Author - Francois Rerolle, rerollefrancois@gmail.com
# @Description - This code translates estimates of excess risk into estimates
# of excess mortality combining population data and average birth rates

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

BGD_Adm_2 <- raster::getData("GADM",
                             country = "BGD",
                             level = 2,
                             path = here("data/untouched/country-admin"))

# Flood
flood_area_percent <- readRDS(here("data/final", "flood_area_percent"))


# World pop
worldpop2020 <- raster(here("data/untouched/worldpop/bgd_ppp_2020_1km_Aggregated_UNadj.tif"))
worldpop2019 <- raster(here("data/untouched/worldpop/bgd_ppp_2019_1km_Aggregated_UNadj.tif"))
worldpop2018 <- raster(here("data/untouched/worldpop/bgd_ppp_2018_1km_Aggregated_UNadj.tif"))
worldpop2017 <- raster(here("data/untouched/worldpop/bgd_ppp_2017_1km_Aggregated_UNadj.tif"))
worldpop2016 <- raster(here("data/untouched/worldpop/bgd_ppp_2016_1km_Aggregated_UNadj.tif"))
worldpop2015 <- raster(here("data/untouched/worldpop/bgd_ppp_2015_1km_Aggregated_UNadj.tif"))
worldpop2014 <- raster(here("data/untouched/worldpop/bgd_ppp_2014_1km_Aggregated_UNadj.tif"))
worldpop2013 <- raster(here("data/untouched/worldpop/bgd_ppp_2013_1km_Aggregated_UNadj.tif"))
worldpop2012 <- raster(here("data/untouched/worldpop/bgd_ppp_2012_1km_Aggregated_UNadj.tif"))
worldpop2011 <- raster(here("data/untouched/worldpop/bgd_ppp_2011_1km_Aggregated_UNadj.tif"))
worldpop2010 <- raster(here("data/untouched/worldpop/bgd_ppp_2010_1km_Aggregated_UNadj.tif"))
worldpop2009 <- raster(here("data/untouched/worldpop/bgd_ppp_2009_1km_Aggregated_UNadj.tif"))
worldpop2008 <- raster(here("data/untouched/worldpop/bgd_ppp_2008_1km_Aggregated_UNadj.tif"))
worldpop2007 <- raster(here("data/untouched/worldpop/bgd_ppp_2007_1km_Aggregated_UNadj.tif"))
worldpop2006 <- raster(here("data/untouched/worldpop/bgd_ppp_2006_1km_Aggregated_UNadj.tif"))
worldpop2005 <- raster(here("data/untouched/worldpop/bgd_ppp_2005_1km_Aggregated_UNadj.tif"))
worldpop2004 <- raster(here("data/untouched/worldpop/bgd_ppp_2004_1km_Aggregated_UNadj.tif"))
worldpop2003 <- raster(here("data/untouched/worldpop/bgd_ppp_2003_1km_Aggregated_UNadj.tif"))
worldpop2002 <- raster(here("data/untouched/worldpop/bgd_ppp_2002_1km_Aggregated_UNadj.tif"))
worldpop2001 <- raster(here("data/untouched/worldpop/bgd_ppp_2001_1km_Aggregated_UNadj.tif"))
worldpop2000 <- raster(here("data/untouched/worldpop/bgd_ppp_2000_1km_Aggregated_UNadj.tif"))

#-------------------------------------------------------------------------------

## Process data
# Aggregate and resample flood map to population raster resolution
flood_area_agg <- raster::resample(x = flood_area_percent > 0 ,
                                           y = worldpop2020,
                                           method = "ngb")

# Mask to country boundaries
flood_area_percent_mask <- raster::mask(flood_area_percent, BGD_Adm)
flood_area_agg_mask <- raster::mask(flood_area_agg, BGD_Adm)
worldpop2020_mask <- raster::mask(worldpop2020, BGD_Adm)
worldpop2019_mask <- raster::mask(worldpop2019, BGD_Adm)
worldpop2018_mask <- raster::mask(worldpop2018, BGD_Adm)
worldpop2017_mask <- raster::mask(worldpop2017, BGD_Adm)
worldpop2016_mask <- raster::mask(worldpop2016, BGD_Adm)
worldpop2015_mask <- raster::mask(worldpop2015, BGD_Adm)
worldpop2014_mask <- raster::mask(worldpop2014, BGD_Adm)
worldpop2013_mask <- raster::mask(worldpop2013, BGD_Adm)
worldpop2012_mask <- raster::mask(worldpop2012, BGD_Adm)
worldpop2011_mask <- raster::mask(worldpop2011, BGD_Adm)
worldpop2010_mask <- raster::mask(worldpop2010, BGD_Adm)
worldpop2009_mask <- raster::mask(worldpop2009, BGD_Adm)
worldpop2008_mask <- raster::mask(worldpop2008, BGD_Adm)
worldpop2007_mask <- raster::mask(worldpop2007, BGD_Adm)
worldpop2006_mask <- raster::mask(worldpop2006, BGD_Adm)
worldpop2005_mask <- raster::mask(worldpop2005, BGD_Adm)
worldpop2004_mask <- raster::mask(worldpop2004, BGD_Adm)
worldpop2003_mask <- raster::mask(worldpop2003, BGD_Adm)
worldpop2002_mask <- raster::mask(worldpop2002, BGD_Adm)
worldpop2001_mask <- raster::mask(worldpop2001, BGD_Adm)
worldpop2000_mask <- raster::mask(worldpop2000, BGD_Adm)

# Check proportion of country flooded is similar after resampling
mean((flood_area_agg_mask[] > 0), na.rm = T) 
mean((flood_area_percent_mask[] > 0), na.rm = T) 

## Combine population and flooded areas in excess birth deaths  layer
# parameters
birth_rate <- 25/1000 # per 1000 person (WorldBank data, in 2003, median of time period)
number_of_years <- 30 # 30 years between 1988 and 2017
risk_difference <- 5.9/1000 # per 1000 births (from results)
risk_difference_lb <- 2.8/1000 # per 1000 births (from results)
risk_difference_ub <- 8.9/1000 # per 1000 births (from results)

# Population exposed 
worldpop2003_exposed_mask <- worldpop2003_mask
worldpop2003_exposed_mask[flood_area_agg_mask == 0] <- 0 # Set population exposed to 0 in non flood prone areas

# Excess
excess_birth_death_mask <- worldpop2003_exposed_mask * birth_rate * risk_difference * number_of_years
excess_birth_death_lb_mask <- worldpop2003_exposed_mask * birth_rate * risk_difference_lb * number_of_years
excess_birth_death_ub_mask <- worldpop2003_exposed_mask * birth_rate * risk_difference_ub * number_of_years

## Aggregate by admin region
# Subregion (e.g, zilas)
BGD_Adm_2$excess_birth_death <- raster::extract(x = excess_birth_death_mask, y = BGD_Adm_2, fun = sum, na.rm = T)
BGD_Adm_2$excess_birth_death_lb <- raster::extract(x = excess_birth_death_lb_mask, y = BGD_Adm_2, fun = sum, na.rm = T)
BGD_Adm_2$excess_birth_death_ub <- raster::extract(x = excess_birth_death_ub_mask, y = BGD_Adm_2, fun = sum, na.rm = T)
BGD_Adm_2$total_population <- raster::extract(x = worldpop2003_mask, y = BGD_Adm_2, fun = sum, na.rm = T)
BGD_Adm_2$excess_birth_death_per_1000_birth <- 1000 * BGD_Adm_2$excess_birth_death / (BGD_Adm_2$total_population * birth_rate * 30)
BGD_Adm_2$excess_birth_death_per_1000_birth_lb <- 1000 * BGD_Adm_2$excess_birth_death_lb / (BGD_Adm_2$total_population * birth_rate * 30)
BGD_Adm_2$excess_birth_death_per_1000_birth_ub <- 1000 * BGD_Adm_2$excess_birth_death_ub / (BGD_Adm_2$total_population * birth_rate * 30)
excess_birth_death_per_1000_birth_upzilla <- rasterize(BGD_Adm_2, excess_birth_death_mask, field = "excess_birth_death_per_1000_birth") # Rasterize for levelplot below

# Entire country
BGD_Adm$excess_birth_death <- raster::extract(x = excess_birth_death_mask, y = BGD_Adm, fun = sum, na.rm = T)
BGD_Adm$excess_birth_death_lb <- raster::extract(x = excess_birth_death_lb_mask, y = BGD_Adm, fun = sum, na.rm = T)
BGD_Adm$excess_birth_death_ub <- raster::extract(x = excess_birth_death_ub_mask, y = BGD_Adm, fun = sum, na.rm = T)

#-------------------------------------------------------------------------------

# Compute total population within flood prone areas
sum(worldpop2020_mask[]*(flood_area_agg_mask[] > 0), na.rm = T)
sum(worldpop2003_mask[]*(flood_area_agg_mask[] > 0), na.rm = T)

#-------------------------------------------------------------------------------
## Map data
excess_birth_death_mask[excess_birth_death_mask > 10] <- 10 # Floor values above 10 for appropriate coloring in map below

colorlength <- 100
excess_birth_deathPlot <- rasterVis::levelplot(excess_birth_death_mask, 
                                               margin = FALSE,                       
                                               colorkey = list(
                                                 space = 'bottom',
                                                 labels=list(cex=1.5,
                                                             at = c(0, 2, 4, 6, 8, 10),
                                                             labels = c(0, 2, 4, 6, 8, "10+"))
                                               ),
                                               main = NULL,
                                               par.settings = list(
                                                 axis.line = list(col = 'transparent') 
                                               ),
                                               xlab = NULL,
                                               ylab = NULL,
                                               scales = list(draw = FALSE),            
                                               col.regions = magma(n = colorlength, direction = -1),
                                               at = seq(from = 0, to = 10, length.out = colorlength),
                                               between = list(x=0, y=0)
                                               )  + 
  latticeExtra::layer(sp.polygons(BGD_Adm_2, lwd = 0.5)) +
  latticeExtra::layer(sp.polygons(BGD_Adm, lwd = 2)) +
  latticeExtra::layer(panel.text(90.3, 21, "Excess infant deaths", cex = 1.5))


pdf(here("child-mortality-dhs/output/figures", "excess-birth-death-map.pdf"))
gridExtra::grid.arrange(excess_birth_deathPlot)
dev.off()


## Upzilla aggregation
excess_birth_death_per_1000_birth_upzillaPlot <- rasterVis::levelplot(excess_birth_death_per_1000_birth_upzilla, 
                                                                      margin = FALSE,                       
                                                                      colorkey = list(
                                                                        space = 'bottom',
                                                                        labels=list(cex=1.5,
                                                                                    at = c(0, 1, 2, 3, 4, 5, 6),
                                                                                    labels = c(0, 1, 2, 3, 4, 5, 6))
                                                                      ),
                                                                      main = NULL,
                                                                      par.settings = list(
                                                                        axis.line = list(col = 'transparent') 
                                                                      ),
                                                                      xlab = NULL,
                                                                      ylab = NULL,
                                                                      scales = list(draw = FALSE),            
                                                                      col.regions = viridis,
                                                                      at = c(seq(from = 0, to = 5.25, length.out = colorlength)),
                                                                      between = list(x=0, y=0)
) +
  latticeExtra::layer(sp.polygons(BGD_Adm_2, lwd = 0.5)) + latticeExtra::layer(sp.polygons(BGD_Adm, lwd = 2)) +
  latticeExtra::layer(panel.text(90.3, 21, "Excess infant mortality\nper 1000 births", cex = 1.5))


pdf(here("child-mortality-dhs/output/figures", "excess-birth-death-per-1000-birth-upzilla-map.pdf"))
gridExtra::grid.arrange(excess_birth_death_per_1000_birth_upzillaPlot)
dev.off()


# # Arranged together
# pdf(here("child-mortality-dhs/output/figures", "figure-3-left.pdf"))
# gridExtra::grid.arrange(excess_birth_deathPlot,
#                         excess_birth_death_per_1000_birth_upzillaPlot,
#                         ncol = 1)
# dev.off()


### Upzilla bar plot
upzilla_bar_plot <- (tibble(Upzilla = BGD_Adm_2$NAME_2,
                            Excess = BGD_Adm_2$excess_birth_death_per_1000_birth,
                            Excess_lb = BGD_Adm_2$excess_birth_death_per_1000_birth_lb,
                            Excess_ub = BGD_Adm_2$excess_birth_death_per_1000_birth_ub) %>%
                       ggplot() +
                       geom_bar(aes(x = reorder(Upzilla, -Excess), y = Excess, fill = Excess), stat = "identity") +
                       geom_errorbar(aes(x = reorder(Upzilla, -Excess), ymin = Excess_lb, ymax = Excess_ub), width=0.4, alpha=0.9, size = 0.5) +
                       scale_fill_continuous(type = "viridis", direction = 1) +
                       scale_y_continuous("Excess infant mortality per 1000 births",
                                          position = "left", guide = guide_axis(angle=90),
                                          # trans = "reverse",
                                          breaks = seq(from = 0, to = 8, by = 1)) +
                       scale_x_discrete(position = "bottom") +
                       # ylab() +
                       theme(panel.grid = element_blank(),
                             panel.background = element_blank(),
                             axis.ticks.x = element_blank(),
                             axis.text = element_text(angle = 90, size = 18),
                             axis.text.x = element_text(angle = 90, size = 18, hjust = 1),
                             legend.position = "none",
                             legend.title = element_blank(),
                             legend.text = element_text(angle = 90, size = 16, hjust = 0.5),
                             legend.key.size = unit(3.28, 'cm'),
                             axis.title = element_blank())
                     )


upzilla_bar_plot

### Population propotion in flood prone area over time
data_population_prop_flood_prone <- tibble(Year = 2000:2020,
                                           `Percent population living in flood prone area` = 100*c(sum(worldpop2000_mask[]*(flood_area_agg_mask[] > 0), na.rm = T)/sum(worldpop2000_mask[], na.rm = T),
                                                                                                   sum(worldpop2001_mask[]*(flood_area_agg_mask[] > 0), na.rm = T)/sum(worldpop2001_mask[], na.rm = T),
                                                                                                   sum(worldpop2002_mask[]*(flood_area_agg_mask[] > 0), na.rm = T)/sum(worldpop2002_mask[], na.rm = T),
                                                                                                   sum(worldpop2003_mask[]*(flood_area_agg_mask[] > 0), na.rm = T)/sum(worldpop2003_mask[], na.rm = T),
                                                                                                   sum(worldpop2004_mask[]*(flood_area_agg_mask[] > 0), na.rm = T)/sum(worldpop2004_mask[], na.rm = T),
                                                                                                   sum(worldpop2005_mask[]*(flood_area_agg_mask[] > 0), na.rm = T)/sum(worldpop2005_mask[], na.rm = T),
                                                                                                   sum(worldpop2006_mask[]*(flood_area_agg_mask[] > 0), na.rm = T)/sum(worldpop2006_mask[], na.rm = T),
                                                                                                   sum(worldpop2007_mask[]*(flood_area_agg_mask[] > 0), na.rm = T)/sum(worldpop2007_mask[], na.rm = T),
                                                                                                   sum(worldpop2008_mask[]*(flood_area_agg_mask[] > 0), na.rm = T)/sum(worldpop2008_mask[], na.rm = T),
                                                                                                   sum(worldpop2009_mask[]*(flood_area_agg_mask[] > 0), na.rm = T)/sum(worldpop2009_mask[], na.rm = T),
                                                                                                   sum(worldpop2010_mask[]*(flood_area_agg_mask[] > 0), na.rm = T)/sum(worldpop2010_mask[], na.rm = T),
                                                                                                   sum(worldpop2011_mask[]*(flood_area_agg_mask[] > 0), na.rm = T)/sum(worldpop2011_mask[], na.rm = T),
                                                                                                   sum(worldpop2012_mask[]*(flood_area_agg_mask[] > 0), na.rm = T)/sum(worldpop2012_mask[], na.rm = T),
                                                                                                   sum(worldpop2013_mask[]*(flood_area_agg_mask[] > 0), na.rm = T)/sum(worldpop2013_mask[], na.rm = T),
                                                                                                   sum(worldpop2014_mask[]*(flood_area_agg_mask[] > 0), na.rm = T)/sum(worldpop2014_mask[], na.rm = T),
                                                                                                   sum(worldpop2015_mask[]*(flood_area_agg_mask[] > 0), na.rm = T)/sum(worldpop2015_mask[], na.rm = T),
                                                                                                   sum(worldpop2016_mask[]*(flood_area_agg_mask[] > 0), na.rm = T)/sum(worldpop2016_mask[], na.rm = T),
                                                                                                   sum(worldpop2017_mask[]*(flood_area_agg_mask[] > 0), na.rm = T)/sum(worldpop2017_mask[], na.rm = T),
                                                                                                   sum(worldpop2018_mask[]*(flood_area_agg_mask[] > 0), na.rm = T)/sum(worldpop2018_mask[], na.rm = T),
                                                                                                   sum(worldpop2019_mask[]*(flood_area_agg_mask[] > 0), na.rm = T)/sum(worldpop2019_mask[], na.rm = T),
                                                                                                   sum(worldpop2020_mask[]*(flood_area_agg_mask[] > 0), na.rm = T)/sum(worldpop2020_mask[], na.rm = T))
                                           )

data_population_prop_flood_prone_plot <- (data_population_prop_flood_prone
                                          %>% ggplot(aes(x = Year, y = `Percent population living in flood prone area`))
                                          + geom_ribbon(aes(ymin = 0, ymax = `Percent population living in flood prone area`), fill = "#41B6C4")
                                          + geom_ribbon(aes(ymin = `Percent population living in flood prone area`, ymax = 100), fill = "#FFFFCC")
                                          + geom_text(aes(label = paste(round(`Percent population living in flood prone area`, digits = 1), "%")), angle = 45)
                                          + ggtitle("Percent population living in flood prone area\nover time")
                                          + scale_x_continuous(breaks = 2000:2020)
                                          + theme(legend.position = "right",
                                                  panel.grid.major.x = element_blank(),
                                                  strip.background = element_blank(),
                                                  strip.text.x = element_blank(),
                                                  axis.text = element_text(size = 12),
                                                  axis.text.x = element_text(angle = 45),
                                                  legend.title = element_text(size = 12),
                                                  legend.text = element_text(size = 10),
                                                  axis.title = element_blank(),
                                                  plot.title = element_text(hjust = 0.5,
                                                                            size = 20))
                                          )

data_population_prop_flood_prone_plot

### Save
pdf(here("child-mortality-dhs/output/figures", "population-percent-in-flood-prone-area.pdf"))
data_population_prop_flood_prone_plot
dev.off()
