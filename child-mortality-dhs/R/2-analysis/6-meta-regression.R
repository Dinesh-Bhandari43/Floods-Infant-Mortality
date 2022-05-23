#-------------------------------------------------------------------------------

# @Organization - UCSF
# @Project - Effects of floods on child mortality in Bangladesh
# @Author - Francois Rerolle, rerollefrancois@gmail.com
# @Description - This code runs the meta-regression between RR effect estimates
# for living in flood prone area on infant mortality and environmental covariates

#-------------------------------------------------------------------------------

# Source configuration code
library(here)
source(here("child-mortality-dhs/R", "0-config.R"))

#-------------------------------------------------------------------------------

### Load data
## RR effect estimates
RR_flooded_vs_non_flooded_tb <- readRDS(file = here("data/final",
                                                    "RR_flooded_vs_non_flooded_tb"))

RR_flooded_vs_non_flooded_tb_empiric <- readRDS(file = here("data/final",
                                                    "RR_flooded_vs_non_flooded_tb_empiric"))

# Combine
data_RR <- (RR_flooded_vs_non_flooded_tb %>%
           left_join(RR_flooded_vs_non_flooded_tb_empiric) %>%
           mutate(Decade = floor(Grouping_Season/20),
                  Subgroup = paste(Season, Decade),
                  Year = rep(rep(1988:2017, 2, each = T), 4))
         )

## Environmental covariates
# Precipitation
BGD_chirps <- readRDS(file = here("data/final", "BGD_chirps"))

BGD_chirps_Grouping_Season <- (BGD_chirps %>%
                                 mutate(Season = factor(ifelse(test = Month %in% c(5:10),
                                                               yes = "Rainy",
                                                               no = "Dry"))) %>%
                                 group_by(Year, Season, Flooded) %>%
                                 summarise(Mean_Precipitation_mm = mean(Mean_Precipitation_mm))
                               )

## Merge
data <- (data_RR %>%
           left_join(BGD_chirps_Grouping_Season))

#-------------------------------------------------------------------------------

## Plot
data_plot <- (data %>%
                filter(Exposure == "0 quartile") %>%
                ggplot(aes(x = Grouping_Season)) +
                geom_smooth(aes(y = Mean_Precipitation_mm, col = Flooded), span = 0.1, se = F) +
                geom_smooth(aes(y = RR_log_empiric), span = 0.1, se = F) +
                ylab("Precipitation (top)\nRR (log empiric bottom)")
              )

data_plot

BGD_chirps_plot <- (BGD_chirps_Grouping_Season %>%
                      ggplot())




