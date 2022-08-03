#-------------------------------------------------------------------------------

# @Organization - UCSF
# @Project - Effects of floods on child mortality in Bangladesh
# @Author - Francois Rerolle, rerollefrancois@gmail.com
# @Description - This code plots the distribution of percent-flooded

#-------------------------------------------------------------------------------

# Source configuration code
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
flood_area_percent_mask <- raster::mask(flood_area_percent, BGD_Adm)

mean(flood_area_percent_mask[]>0, na.rm = T)
x <- 100*flood_area_percent_mask[which(flood_area_percent_mask[] > 0)]
y <- density(x, n = 2^12)

data <- data.frame(percent = y$x, Density = y$y)
quartiles <- quantile(x)

# Prepare
data.label.quartile <- data.frame(quartile = c(quartiles[2:4]),
                                  density_at_quartile = c(y$y[which.min((y$x-quartiles[2])^2)],
                                                          y$y[which.min((y$x-quartiles[3])^2)],
                                                          y$y[which.min((y$x-quartiles[4])^2)]),
                                  label = c(paste("First quartile:",
                                                  round(quartiles[2],
                                                        digits = 1),
                                                  "%"),
                                            paste("Median:",
                                                  round(quartiles[3],
                                                        digits = 1),
                                                  "%"),
                                            paste("Third quartile:",
                                                  round(quartiles[4],
                                                        digits = 1),
                                                  "%")),
                                  xend = c(quartiles[2:4]),
                                  yend = c(0, 0, 0)
                                  )

# plot
percent_area_flooded_distribution <- ggplot() + 
  geom_segment(data = data %>% filter(percent>0),
               aes(x = percent,
                   y = Density,
                   xend = percent,
                   yend = 0,
                   colour = percent)) + 
  geom_segment(data = data.label.quartile,
               aes(x = quartile,
                   y = density_at_quartile,
                   xend = xend,
                   yend = yend),
               colour = "black",
               linetype = "dotted") + 
  geom_text(data = data.label.quartile,
            aes(label = label,
                x = xend,
                y = density_at_quartile),
            hjust = 0,
            vjust = 0,
            angle = 15,
            size = 5) + 
  scale_x_continuous(breaks = seq(0, 80, by = 10)) +
  scale_color_gradient2(low = "#FFFFCC", mid="#41B6C4", high = "#0C2C84",
                        midpoint=40) +
  xlab("Percent number of flooded days (%)") +
  coord_cartesian(clip = "off") +
  theme(legend.position = "none",
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 16, margin = ggplot2::margin(t = -20)),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 16, margin = ggplot2::margin(r = -20)))


### Save
pdf(here("child-mortality-dhs/output/figures", "percent-area-flooded-distribution.pdf"))
percent_area_flooded_distribution
dev.off()
