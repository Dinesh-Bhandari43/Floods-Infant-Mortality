# DHS Bangladesh exploration

library(here)

source(here("child-mortality-dhs/R", "0-config.R"))


## Load data
monthly_time_series <- readRDS(file = here("data/final", "monthly_time_series"))
BGD_chirps <- readRDS(file = here("data/final", "BGD_chirps"))

## Aggregate flooded vs non flooded
monthly_time_series_flooded_vs_non_flooded <- (monthly_time_series
                                                  %>% group_by(Birth_Date_Month_CMC, Flooded)
                                                  %>% summarise(Number_Of_Birth = sum(Number_Of_Birth, na.rm = T),
                                                                Number_Of_Dead_Birth = sum(Number_Of_Dead_Birth, na.rm = T),
                                                                Number_Of_Birth_QS = sum(Number_Of_Birth_QS, na.rm = T),
                                                                Number_Of_Dead_Birth_QS = sum(Number_Of_Dead_Birth_QS, na.rm = T))
                                                  %>% ungroup()
                                                  %>% mutate(Year = 1900 + floor((Birth_Date_Month_CMC-1)/12),
                                                             Month = Birth_Date_Month_CMC - 12*(Year - 1900),
                                                             Season = factor(ifelse(Month %in% c(5:10), "Rainy", "Dry")),
                                                             Season_Change = (Season != lag(Season)),
                                                             Season_Change = replace(Season_Change, is.na(Season_Change), FALSE),
                                                             Grouping_Season = cumsum(Season_Change))
                                                  %>% group_by(Grouping_Season, Flooded)
                                                  %>% mutate(Number_Of_Birth_Season = sum(Number_Of_Birth, na.rm = T),
                                                             Number_Of_Dead_Birth_Season = sum(Number_Of_Dead_Birth, na.rm = T))
                                                  %>% ungroup()
                                               %>% mutate(Decade = factor(floor((Year - 1988)/15)),
                                                          Year_Standardized = (Year - 2003))
                                                  )

monthly_time_series_flooded_vs_non_flooded_chirps <- (monthly_time_series_flooded_vs_non_flooded
                                                      %>% left_join(BGD_chirps)
                                                      %>% filter(Year!=1987)
                                                      %>% filter(Birth_Date_Month_CMC != 1413))

## Model
model_gam <- gam(Number_Of_Dead_Birth ~ Flooded*relevel(Season, ref = "Rainy")*relevel(Decade, ref = "1") + s(Birth_Date_Month_CMC) + s(Mean_Precipitation_mm) + offset(log(Number_Of_Birth)),
                 data = monthly_time_series_flooded_vs_non_flooded_chirps,
                 family = "nb")

model_gam <- gam(Number_Of_Dead_Birth ~ Flooded + s(Birth_Date_Month_CMC) + s(Mean_Precipitation_mm) + offset(log(Number_Of_Birth)),
                 data = monthly_time_series_flooded_vs_non_flooded_chirps,
                 family = "nb")

summary(model_gam)

## Extract effect estimates
number_decade <- 2
RR_flooded_vs_non_flooded_tb <- tibble(RR = rep(NA, number_decade*2),
                                       RR_SE = rep(NA, number_decade*2),
                                       Season = rep(c("Dry", "Rainy"), number_decade),
                                       Decade = rep(c("0","1"), each = 2))

for (i in 1:(number_decade*2)){
  model_gam <- gam(Number_Of_Dead_Birth ~ Flooded*relevel(Season, ref = RR_flooded_vs_non_flooded_tb$Season[i])*relevel(Decade, ref = RR_flooded_vs_non_flooded_tb$Decade[i]) + s(Birth_Date_Month_CMC) + s(Mean_Precipitation_mm) + offset(log(Number_Of_Birth)),
                   data = monthly_time_series_flooded_vs_non_flooded_chirps,
                   family = "nb")
  
  RR_flooded_vs_non_flooded_tb$RR[i] <- summary(model_gam)$p.coeff[2]
  RR_flooded_vs_non_flooded_tb$RR_SE[i] <- summary(model_gam)$se[2]
}

RR_plot <- (RR_flooded_vs_non_flooded_tb %>%
              mutate(Decade = replace(Decade, Decade == "0", "1988-2002"),
                     Decade = replace(Decade, Decade == "1", "2003-2017")) %>%
              ggplot(aes(x = Season, y = exp(RR), col = Season)) +
              geom_point(size = 4) +
              geom_errorbar(aes(ymin = exp(RR - 1.96*RR_SE),
                                ymax = exp(RR + 1.96*RR_SE)),
                            size = 2) +
              geom_hline(aes(yintercept = 1)) +
              facet_wrap(~Decade) +
              ylab("Risk ratio for living in\nflood-prone areas vs non-flood prone areas") +
              coord_flip()
            )

RR_plot

# plot(model_gam)
# margins(model_gam)
# vcov.gam(model_gam)
# 
# monthly_time_series_flooded_vs_non_flooded_chirps$Predictions <- predict(model_gam)
# 
# modelled_plot <- (monthly_time_series_flooded_vs_non_flooded_chirps %>%
#                     mutate(Infant_Mortality_Predictions = exp(Predictions)/Number_Of_Birth,
#                            Group = paste(Flooded, Season, Decade),
#                            Color = paste(Flooded, Season)) %>%
#                     ggplot(aes(x = Birth_Date_Month_CMC, y = Infant_Mortality_Predictions, group = Color, col = Color)) +
#                     geom_smooth() +
#                     geom_point(aes(y = Number_Of_Dead_Birth_QS/Number_Of_Birth_QS))
#                   )
# 
# modelled_plot
# 
