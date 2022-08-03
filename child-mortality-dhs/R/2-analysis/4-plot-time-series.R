#-------------------------------------------------------------------------------

# @Organization - UCSF
# @Project - Effects of floods on child mortality in Bangladesh
# @Author - Francois Rerolle, rerollefrancois@gmail.com
# @Description - This code plots the infant mortality time series

#-------------------------------------------------------------------------------

# Source configuration code
library(here)
source(here("child-mortality-dhs/R", "0-config.R"))

#-------------------------------------------------------------------------------

## Load data
monthly_time_series_0_quartile <- readRDS(file = here("data/final",
                                                      "monthly_time_series_0_quartile"))
monthly_time_series_control <- readRDS(file = here("data/final",
                                                      "monthly_time_series_control"))

RD_flooded_vs_non_flooded_tb_empiric <- readRDS(file = here("data/final",
                                                            "RD_flooded_vs_non_flooded_tb_empiric"))

monthly_time_series <- (rbind(monthly_time_series_0_quartile,
                             monthly_time_series_control))

## Aggregate flooded vs non flooded
monthly_time_series_grouping_season <- (monthly_time_series
                                                  %>% ungroup()
                                                  %>% mutate(Year = 1900 + floor((Birth_Date_Month_CMC-1)/12),
                                                             Month = Birth_Date_Month_CMC - 12*(Year - 1900),
                                                             Season = ifelse(Month %in% c(5:10), "Rainy", "Dry"),
                                                             Season_Change = (Season != lag(Season)),
                                                             Season_Change = replace(Season_Change, is.na(Season_Change), FALSE),
                                                             Grouping_Season = cumsum(Season_Change))
                                                  %>% group_by(Grouping_Season, Flooded)
                                                  %>% mutate(Number_Of_Birth_Season = sum(Number_Of_Birth, na.rm = T),
                                                             Number_Of_Dead_Birth_Season = sum(Number_Of_Dead_Birth, na.rm = T),
                                                             Infant_Mortality_Season = Number_Of_Dead_Birth_Season / Number_Of_Birth_Season)
                                                  )

monthly_time_series_grouping_season_processed <- (monthly_time_series_grouping_season
                                                  %>% mutate(Infant_Mortality = Infant_Mortality_Season)
                                                  %>% filter(Month %in% c(1,7))  
                                                  %>% left_join(RD_flooded_vs_non_flooded_tb_empiric %>%
                                                                   filter(Exposure == "0 quartile") %>%
                                                                   dplyr::select(Grouping_Season, RD_empiric, RD_SE_empiric)))

### Plot
monthly_time_series_plot <- (ggplot()
                             + geom_rect(aes(ymin = 0, ymax = 100,
                                             xmin = seq(1061-0.5, 1409-0.5, by = 12), xmax = seq(1066+0.5, 1414+0.5, by = 12)),
                                         fill = "darkgrey", alpha = 0.5)
                             + geom_rect(aes(ymin = 0, ymax = 100,
                                             xmin = seq(1055-0.5, 1403-0.5, by = 12), xmax = seq(1060+0.5, 1408+0.5, by = 12)),
                                         fill = "grey", alpha = 0.5)

                             + geom_segment(aes(x = 1054, xend = 1414, y = 25, yend = 25), colour = "lightgrey", linetype = "dashed")
                             + geom_segment(aes(x = 1054, xend = 1414, y = 50, yend = 50), colour = "lightgrey", linetype = "dashed")
                             + geom_segment(aes(x = 1054, xend = 1414, y = 75, yend = 75), colour = "lightgrey", linetype = "dashed")
                             + geom_point(data = monthly_time_series_grouping_season_processed %>% filter(Flooded != FALSE),
                                          aes(x = Birth_Date_Month_CMC,
                                              y = 1000*Infant_Mortality,
                                              group = Flooded, col = Flooded))
                             # + geom_smooth(method = "lm", se = F)
                             # + geom_line()
                             + geom_smooth(data = monthly_time_series_grouping_season_processed %>% filter(Flooded != FALSE),
                                           aes(x = Birth_Date_Month_CMC,
                                               y = 1000*Infant_Mortality,
                                               group = Flooded, col = Flooded),
                                           se = F, span = 0.1, size = 2)
                             # + scale_x_continuous(breaks = seq(1057, 1411, by = 6), labels = paste(c("Dry", "Rainy"), rep(1988:2017, each = 2)))
                             + scale_x_continuous(breaks = seq(1057, 1411, by = 6), labels = c(rbind(rep("Dry", 30),
                                                                                                     paste("Rainy", 1988:2017))))
                             # + xlim(c(1135, 1291))
                             + ylim(c(0,100)) 
                             + scale_color_manual(labels = c("Non-flood prone area","Flood prone area"),
                                                  values = c("#FFFFCC", "#41B6C4"))
                             # + geom_vline(xintercept = 1291) # July 2007
                             # + geom_vline(xintercept = 1255) # July 2004
                             + xlab("Seasons 1988-2017")
                             + ylab("Infant mortality rate (per 1000 birth)")
                             + theme(legend.title = element_blank(),
                                     legend.text = element_text(size = 16),
                                     panel.background = element_blank(),
                                     panel.grid = element_blank(),
                                     legend.position = "top",
                                     axis.text.x = element_text(vjust = 0.5,
                                                                hjust = 0,
                                                                angle = 90,
                                                                margin = ggplot2::margin(t = -45),
                                                                size = 7),
                                     axis.title.x = element_text(margin = ggplot2::margin(t = 10),
                                                                 size = 20),
                                     axis.text.y = element_text(vjust = 0,
                                                                hjust = 0,
                                                                margin = ggplot2::margin(r = -20),
                                                                size = 10),
                                     axis.title.y = element_text(margin = ggplot2::margin(r = 10),
                                                                 size = 18),
                                     axis.ticks = element_blank(),
                                     legend.margin = ggplot2::margin(b = -20))
)



monthly_time_series_plot

### Save
pdf(here("child-mortality-dhs/output/figures", "infant_mortality_time_series.pdf"))
monthly_time_series_plot
dev.off()
