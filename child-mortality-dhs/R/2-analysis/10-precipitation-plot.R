#-------------------------------------------------------------------------------

# @Organization - UCSF
# @Project - Effects of floods on child mortality in Bangladesh
# @Author - Francois Rerolle, rerollefrancois@gmail.com
# @Description - This code plots precipitation data over time to highlight
# dry vs rainy season

#-------------------------------------------------------------------------------

# Source configuration code
library(here)
source(here("child-mortality-dhs/R", "0-config.R"))

#-------------------------------------------------------------------------------

### Load data
# Precipitation
BGD_chirps <- readRDS(file = here("data/final", "BGD_chirps"))

#-------------------------------------------------------------------------------

## Plot
precipitation_plot <- (BGD_chirps %>%
                         group_by(Flooded, Month) %>%
                         summarise(Mean_Precipitation_mm = mean(Mean_Precipitation_mm, na.rm = T)) %>%
                         ggplot(aes(x = Month)) +
                         geom_rect(aes(xmin = 5, xmax = 10, ymin = 5.5, ymax = 7.5), alpha = 0.02) +
                         geom_smooth(aes(y = Mean_Precipitation_mm, col = Flooded), se = F, size = 2) +
                         ylab("Average daily precipitation (mm)\nover 1988-2017 period") +
                         scale_x_continuous(breaks = 1:12, labels = c("January",
                                                                      " February",
                                                                      "March",
                                                                      "April",
                                                                      "May",
                                                                      "June",
                                                                      "July",
                                                                      "August",
                                                                      "September",
                                                                      "October",
                                                                      "November",
                                                                      "December")) +
                         scale_color_manual(labels = c("Non-flood prone area","Flood prone area"),
                                            values = c("#FFFFCC", "#41B6C4")) +
                         theme(legend.title = element_blank(),
                               legend.text = element_text(size = 16),
                               legend.position = "top",
                               axis.text.x = element_text(angle = 45,
                                                          size = 10),
                               axis.title.x = element_text(size = 20),
                               axis.text.y = element_text(size = 10),
                               axis.title.y = element_text(size = 18))
                       )

precipitation_plot

### Save
pdf(here("child-mortality-dhs/output/figures", "precipitation_plot.pdf"))
precipitation_plot
dev.off()





