#-------------------------------------------------------------------------------

# @Organization - UCSF
# @Project - Effects of floods on child mortality in Bangladesh
# @Author - Francois Rerolle, rerollefrancois@gmail.com
# @Description - This code computes the total population living in flood prone
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

BGD_Adm_2 <- raster::getData("GADM",
                             country = "BGD",
                             level = 2,
                             path = here("data/untouched/country-admin"))

# Flood
flood_area_percent <- readRDS(here("data/final", "flood_area_percent"))


# World pop
worldpop2020 <- raster(here("data/untouched/worldpop/bgd_ppp_2020_1km_Aggregated_UNadj.tif"))
worldpop2003 <- raster(here("data/untouched/worldpop/bgd_ppp_2003_1km_Aggregated_UNadj.tif"))
worldpop2003_100m <- raster(here("data/untouched/worldpop/bgd_ppp_2003_UNadj.tif"))
worldpop2020_100m <- raster(here("data/untouched/worldpop/bgd_ppp_2020_UNadj.tif"))

#-------------------------------------------------------------------------------

## Process data
# Aggregate flood map to population raster resolution
flood_area_percent_agg <- raster::resample(x = flood_area_percent > 0 ,
                                           y = worldpop2020,
                                           method = "ngb")

flood_area_percent_agg <- raster::resample(x = flood_area_percent > 0 ,
                                           y = worldpop2020_100m,
                                           method = "ngb")

# Aggregate population map to flood raster resolution
worldpop2003_300m <- aggregate(x = worldpop2003_100m,
                               fact = 3,
                               fun = sum)

worldpop2020_300m <- aggregate(x = worldpop2020_100m,
                               fact = 3,
                               fun = sum)

flood_area_percent_agg <- raster::resample(x = flood_area_percent > 0 ,
                                           y = worldpop2003_300m,
                                           method = "ngb")

# Mask to country boundaries
flood_area_percent_mask <- raster::mask(flood_area_percent, BGD_Adm)
flood_area_percent_agg_mask <- raster::mask(flood_area_percent_agg, BGD_Adm)
worldpop2020_mask <- raster::mask(worldpop2020_100m, BGD_Adm)
worldpop2003_mask <- raster::mask(worldpop2003_100m, BGD_Adm)

# Combine population and flooded areas in excess birth deaths  layer
birth_rate <- 25/1000 # per 1000 person (WorldBank data, in 2003, median of time period)
risk_difference <- 5.9/1000 # per 1000 births
risk_difference_lb <- 2.8/1000 # per 1000 births
risk_difference_ub <- 8.9/1000 # per 1000 births
worldpop2003_exposed_mask <- worldpop2003_mask
worldpop2003_exposed_mask[flood_area_percent_agg_mask == 0] <- 0
excess_birth_death_mask <- worldpop2003_exposed_mask * birth_rate * risk_difference * 30 # (number of years)
excess_birth_death_lb_mask <- worldpop2003_exposed_mask * birth_rate * risk_difference_lb * 30 # (number of years)
excess_birth_death_ub_mask <- worldpop2003_exposed_mask * birth_rate * risk_difference_ub * 30 # (number of years)

# Aggregate by admin region
BGD_Adm_2$excess_birth_death <- raster::extract(x = excess_birth_death_mask, y = BGD_Adm_2, fun = sum, na.rm = T)
BGD_Adm_2$excess_birth_death_lb <- raster::extract(x = excess_birth_death_lb_mask, y = BGD_Adm_2, fun = sum, na.rm = T)
BGD_Adm_2$excess_birth_death_ub <- raster::extract(x = excess_birth_death_ub_mask, y = BGD_Adm_2, fun = sum, na.rm = T)
BGD_Adm_2$total_population <- raster::extract(x = worldpop2003_mask, y = BGD_Adm_2, fun = sum, na.rm = T)
BGD_Adm_2$excess_birth_death_per_1000_birth <- 1000 * BGD_Adm_2$excess_birth_death / (BGD_Adm_2$total_population * birth_rate * 30)
BGD_Adm_2$excess_birth_death_per_1000_birth_lb <- 1000 * BGD_Adm_2$excess_birth_death_lb / (BGD_Adm_2$total_population * birth_rate * 30)
BGD_Adm_2$excess_birth_death_per_1000_birth_ub <- 1000 * BGD_Adm_2$excess_birth_death_ub / (BGD_Adm_2$total_population * birth_rate * 30)
excess_birth_death_per_1000_birth_upzilla <- rasterize(BGD_Adm_2, excess_birth_death_mask, field = "excess_birth_death_per_1000_birth") # Rasterize for levelplot below
#-------------------------------------------------------------------------------

# Compute total population within flood prone areas
sum(worldpop2020_mask[]*(flood_area_percent_agg_mask[] > 0), na.rm = T)
sum(worldpop2003_mask[]*(flood_area_percent_agg_mask[] > 0), na.rm = T)

# Check proportion of country flooded is similar after resampling
mean((flood_area_percent_agg_mask[] > 0), na.rm = T) 
mean((flood_area_percent_mask[] > 0), na.rm = T) 

#-------------------------------------------------------------------------------
## Map data
pal <- colorRampPalette(c("#FFFFCC", "#E69F00", "#D55E00")) # Define color palette
excess_birth_death_mask[excess_birth_death_mask > 1] <- 1 # Floor values above 3 for appropriate coloring in map below
excess_birth_deathPlot <- rasterVis::levelplot(excess_birth_death_mask, 
                                               margin = FALSE,                       
                                               colorkey = list(
                                                 space = 'bottom',
                                                 labels=list(cex=1.5,
                                                             at = c(0, 1),
                                                             labels = c(0, "1+"))
                                               ),
                                               main = NULL,
                                               par.settings = list(
                                                 axis.line = list(col = 'transparent') 
                                               ),
                                               xlab = NULL,
                                               ylab = NULL,
                                               scales = list(draw = FALSE),            
                                               col.regions = viridis,
                                               at = c(seq(from = 0, to = 1, by = 0.05))
# )  + latticeExtra::layer(sp.polygons(BGD_Adm_2, lwd = 2, col = "lightgrey")
                         ) +
  latticeExtra::layer(sp.polygons(BGD_Adm, lwd = 2))
# +   latticeExtra::layer(panel.text(90.3, 26.7, 'Percent number of flooded days (%)'))


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
                                               main = "Excess infant mortality\nper 1000 births",
                                               par.settings = list(
                                                 axis.line = list(col = 'transparent') 
                                               ),
                                               xlab = NULL,
                                               ylab = NULL,
                                               scales = list(draw = FALSE),            
                                               col.regions = viridis,
                                               at = c(seq(from = 0, to = 6, by = 0.5))
) +
  latticeExtra::layer(sp.polygons(BGD_Adm, lwd = 2)) + latticeExtra::layer(sp.polygons(BGD_Adm_2, lwd = 2))  
# +   latticeExtra::layer(panel.text(90.3, 26.7, 'Percent number of flooded days (%)'))



pdf(here("child-mortality-dhs/output/figures", "excess-birth-death-per-1000-birth-upzilla-map.pdf"))
gridExtra::grid.arrange(excess_birth_death_per_1000_birth_upzillaPlot)
dev.off()


### Upzilla bar plot
upzilla_bar_plot <- (tibble(Upzilla = BGD_Adm_2$NAME_2,
                            Excess = BGD_Adm_2$excess_birth_death_per_1000_birth,
                            Excess_lb = BGD_Adm_2$excess_birth_death_per_1000_birth_lb,
                            Excess_ub = BGD_Adm_2$excess_birth_death_per_1000_birth_ub) %>%
                       ggplot() +
                       geom_bar(aes(x = reorder(Upzilla, -Excess), y = Excess, fill = Excess), stat = "identity") +
                       geom_errorbar(aes(x = reorder(Upzilla, -Excess), ymin = Excess_lb, ymax = Excess_ub), width=0.4, alpha=0.9, size = 0.5) +
                       scale_fill_continuous(type = "viridis") +
                       ylab("Excess infant mortality\nper 1000 births") +
                       theme(panel.grid = element_blank(),
                             panel.background = element_blank(),
                             axis.ticks.x = element_blank(),
                             axis.text = element_text(angle = 90, size = 18),
                             legend.position = "none",
                             legend.title = element_blank(),
                             legend.text = element_text(angle = 90),
                             axis.title = element_blank())
                     )


upzilla_bar_plot
