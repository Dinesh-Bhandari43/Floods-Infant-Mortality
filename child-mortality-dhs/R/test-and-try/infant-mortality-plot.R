# DHS Bangladesh exploration

library(here)

source(here("child-mortality-dhs/R", "0-config.R"))


## Load data
monthly_time_series <- readRDS(file = here("data/final", "monthly_time_series_0_quartile"))

## Aggregate flooded vs non flooded
monthly_time_series_qs_flooded_vs_non_flooded <- (monthly_time_series
                                                  %>% group_by(Birth_Date_Month_CMC, Flooded)
                                                  %>% summarise(Number_Of_Birth = sum(Number_Of_Birth, na.rm = T),
                                                                Number_Of_Dead_Birth = sum(Number_Of_Dead_Birth, na.rm = T),
                                                                Infant_Mortality = Number_Of_Dead_Birth/Number_Of_Birth)
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
                                                             Infant_Mortality_Season = Number_Of_Dead_Birth_Season/Number_Of_Birth_Season)
                                                  )

monthly_birth_plot <- (monthly_time_series_qs_flooded_vs_non_flooded
                       %>% mutate(Infant_Mortality = Infant_Mortality_Season)
                       %>% filter(Month %in% c(1,7))
                       %>% ggplot(aes(x = Birth_Date_Month_CMC, y = 100*Infant_Mortality, group = Flooded, col = Flooded, shape = Season))
                       + geom_point()
                       # + geom_smooth(method = "lm", se = F)
                       # + geom_line()
                       + geom_smooth(se = F, span = 0.1)
                       + scale_x_continuous(breaks = seq(1063, 1411, by = 12), labels = paste("July", 1988:2017))
                       # + xlim(c(1135, 1291))
                       + ylim(c(0,10))
                       # + geom_vline(xintercept = 1291) # July 2007
                       # + geom_vline(xintercept = 1255) # July 2004
                       + xlab("Date")
                       + ylab("Infant mortality (%)")
                       + theme(axis.text.x = element_text(angle = 30))
                       )

monthly_birth_plot


monthly_time_series_qs_flooded_vs_non_flooded_RR <- (monthly_time_series_qs_flooded_vs_non_flooded
                                                     %>% filter(Flooded == 1)
                                                     %>% left_join(monthly_time_series_qs_flooded_vs_non_flooded
                                                                   %>% filter(Flooded == 0),
                                                                   by = "Birth_Date_Month_CMC")
                                                     %>% mutate(Infant_Mortality_RR = Infant_Mortality.x / Infant_Mortality.y,
                                                                Infant_Mortality_SE_ln_RR = sqrt((1/Number_Of_Dead_Birth_QS.x) - (1/Number_Of_Birth_QS.x)
                                                                                                 + (1/Number_Of_Dead_Birth_QS.y) - (1/Number_Of_Birth_QS.y)),
                                                                Infant_Mortality_RR_lb = exp(log(Infant_Mortality_RR) - 1.96 * Infant_Mortality_SE_ln_RR),
                                                                Infant_Mortality_RR_ub = exp(log(Infant_Mortality_RR) + 1.96 * Infant_Mortality_SE_ln_RR)
                                                                )
                                                     %>% mutate(Infant_Mortality_Season_RR = Infant_Mortality_Season.x / Infant_Mortality_Season.y,
                                                                Infant_Mortality_Season_SE_ln_RR = sqrt((1/Number_Of_Dead_Birth_Season.x) - (1/Number_Of_Birth_Season.x)
                                                                                                 + (1/Number_Of_Dead_Birth_Season.y) - (1/Number_Of_Birth_Season.y)),
                                                                Infant_Mortality_Season_RR_lb = exp(log(Infant_Mortality_Season_RR) - 1.96 * Infant_Mortality_Season_SE_ln_RR),
                                                                Infant_Mortality_Season_RR_ub = exp(log(Infant_Mortality_Season_RR) + 1.96 * Infant_Mortality_Season_SE_ln_RR)
                                                     )
                                                     )

monthly_birth_plot_RR <- (monthly_time_series_qs_flooded_vs_non_flooded_RR
                          %>% mutate(Infant_Mortality_RR = Infant_Mortality_Season_RR,
                                     Infant_Mortality_RR_lb = Infant_Mortality_Season_RR_lb,
                                     Infant_Mortality_RR_ub = Infant_Mortality_Season_RR_ub)
                       %>% ggplot(aes(x = Birth_Date_Month_CMC, y = Infant_Mortality_RR, group = 1, col = Season.x))
                       + geom_point()
                       # + geom_smooth(method = "lm", se = F)
                       + geom_smooth(se = T, span = 0.08)
                       # + geom_errorbar(aes(ymin = Infant_Mortality_RR_lb, ymax = Infant_Mortality_RR_ub))
                       + scale_x_continuous(breaks = seq(1063, 1411, by = 12), labels = paste("July", 1988:2017))
                       # + scale_x_continuous(breaks = seq(1063, 1411-6*12, by = 12), labels = paste("July", 1988:(2017-6)))
                       # + xlim(c(1063, 1411 - 6*12))
                       # + ylim(c(0,7.5))
                       # + geom_vline(xintercept = 1291, col = "orange") # July 2007
                       # + geom_vline(xintercept = 1255, col = "orange") # July 2004
                       + geom_hline(yintercept = 1) # RR = 1
                       + xlab("Date")
                       # + facet_wrap(~Season.x)
                       + ylab("Infant mortality Risk Ratio\nFlooded vs non flooded")
                       + theme(axis.text.x = element_text(angle = 30))
)

monthly_birth_plot_RR
