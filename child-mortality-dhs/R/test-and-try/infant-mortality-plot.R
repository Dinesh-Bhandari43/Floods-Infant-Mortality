# DHS Bangladesh exploration

library(here)

source(here("child-mortality-dhs/R", "0-config.R"))


## Load data
monthly_time_series <- readRDS(file = here("data/final", "monthly_time_series"))

## Aggregate flooded vs non flooded
monthly_time_series_qs_flooded_vs_non_flooded <- (monthly_time_series
                                                  %>% group_by(Birth_Date_Month_CMC, Flooded)
                                                  %>% summarise(Number_Of_Birth_QS = sum(Number_Of_Birth_QS, na.rm = T),
                                                                Number_Of_Dead_Birth_QS = sum(Number_Of_Dead_Birth_QS, na.rm = T))
                                                  %>% mutate(Infant_Mortality_Quarterly_Smoothed = Number_Of_Dead_Birth_QS/Number_Of_Birth_QS,
                                                             Infant_Mortality = Infant_Mortality_Quarterly_Smoothed)
                                                  %>% ungroup()
                                                  )

monthly_birth_plot <- (monthly_time_series_qs_flooded_vs_non_flooded
                       %>% ggplot(aes(x = Birth_Date_Month_CMC, y = 100*Infant_Mortality, group = Flooded, col = Flooded))
                       + geom_point()
                       # + geom_smooth(method = "lm", se = F)
                       + geom_smooth(se = T, span = 0.1)
                       + scale_x_continuous(breaks = seq(1063, 1411, by = 12), labels = paste("July", 1988:2017))
                       # + xlim(c(1135, 1291))
                       + ylim(c(0,15))
                       + geom_vline(xintercept = 1291) # July 2007
                       + geom_vline(xintercept = 1255) # July 2004
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
                                                     %>% mutate(Infant_Mortality_RR = Infant_Mortality.x / Infant_Mortality.y))

monthly_birth_plot_RR <- (monthly_time_series_qs_flooded_vs_non_flooded_RR
                       %>% ggplot(aes(x = Birth_Date_Month_CMC, y = Infant_Mortality_RR))
                       + geom_point()
                       # + geom_smooth(method = "lm", se = F)
                       + geom_smooth(se = T, span = 0.08)
                       + scale_x_continuous(breaks = seq(1063, 1411, by = 12), labels = paste("July", 1988:2017))
                       # + xlim(c(1135, 1291))
                       + ylim(c(0,5))
                       + geom_vline(xintercept = 1291) # July 2007
                       + geom_vline(xintercept = 1255) # July 2004
                       + geom_hline(yintercept = 1) # RR = 1
                       + xlab("Date")
                       + ylab("Infant mortality Rate Ratio\nFlooded vs non flooded")
                       + theme(axis.text.x = element_text(angle = 30))
)

monthly_birth_plot_RR
