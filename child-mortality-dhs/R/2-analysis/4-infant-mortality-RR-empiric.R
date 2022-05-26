#-------------------------------------------------------------------------------

# @Organization - UCSF
# @Project - Effects of floods on child mortality in Bangladesh
# @Author - Francois Rerolle, rerollefrancois@gmail.com
# @Description - This code computes empirically the risk ratio for
# living in flood prone area on infant mortality over time

#-------------------------------------------------------------------------------

# Source configuration code
library(here)
source(here("child-mortality-dhs/R", "0-config.R"))

#-------------------------------------------------------------------------------

## Load data
monthly_time_series_0_quartile <- readRDS(file = here("data/final", "monthly_time_series_0_quartile"))
monthly_time_series_1_quartile <- readRDS(file = here("data/final", "monthly_time_series_1_quartile"))
monthly_time_series_2_quartile <- readRDS(file = here("data/final", "monthly_time_series_2_quartile"))
monthly_time_series_3_quartile <- readRDS(file = here("data/final", "monthly_time_series_3_quartile"))
BGD_chirps <- readRDS(file = here("data/final", "BGD_chirps"))

monthly_time_series_list <- list(monthly_time_series_0_quartile,
                                 monthly_time_series_1_quartile,
                                 monthly_time_series_2_quartile,
                                 monthly_time_series_3_quartile)

## Merge to precipitation data
monthly_time_series_list_2 <-
  lapply(X = monthly_time_series_list,
         FUN = function(x){
           (x %>%
              mutate(Year = 1900 + floor((Birth_Date_Month_CMC-1)/12),
                     Month = Birth_Date_Month_CMC - 12*(Year - 1900),
                     Season = factor(ifelse(Month %in% c(5:10), "Rainy", "Dry")),
                     Half = factor(floor((Year - 1988)/15)),
                     Decade = factor(floor((Year - 1988)/10)),
                     Quinquenat = factor(floor((Year - 1988)/5)))
            %>% group_by(Flooded)
            %>% mutate(New_Season = Season != lag(Season),
                       New_Season = replace(New_Season, is.na(New_Season), FALSE),
                       Grouping_Season = cumsum(New_Season))
            %>% group_by(Flooded, Grouping_Season)
            # %>% group_by(Flooded, Season, Decade)  # Variant to aggregate
            %>% summarise(Number_Of_Dead_Birth = sum(Number_Of_Dead_Birth),
                       Number_Of_Birth = sum(Number_Of_Birth))
            )
           }
         )


## Extract effect estimates Grouping Season
RR_flooded_vs_non_flooded_tb_quartile <- tibble(RR_log_empiric = rep(NA, 60),
                                                RR_log_SE_empiric = rep(NA, 60),
                                                RD_empiric = rep(NA, 60),
                                                RD_SE_empiric = rep(NA, 60),
                                                Grouping_Season = rep(1:60))

RR_flooded_vs_non_flooded_tb <- rbind(RR_flooded_vs_non_flooded_tb_quartile %>%
                                        mutate(Exposure = "0 quartile"),
                                      RR_flooded_vs_non_flooded_tb_quartile %>%
                                        mutate(Exposure = "1 quartile"),
                                      RR_flooded_vs_non_flooded_tb_quartile %>%
                                        mutate(Exposure = "2 quartile"),
                                      RR_flooded_vs_non_flooded_tb_quartile %>%
                                        mutate(Exposure = "3 quartile"))

for (j in 1:4){
  for (i in 1:60){
    data <- monthly_time_series_list_2[[j]]
    RR_flooded_vs_non_flooded_tb$RR_log_empiric[i + (j-1)*60] <- log((data$Number_Of_Dead_Birth[which(data$Flooded == TRUE & data$Grouping_Season == RR_flooded_vs_non_flooded_tb$Grouping_Season[i])]/data$Number_Of_Birth[which(data$Flooded == TRUE & data$Grouping_Season == RR_flooded_vs_non_flooded_tb$Grouping_Season[i])]) /
                                                             (data$Number_Of_Dead_Birth[which(data$Flooded == FALSE & data$Grouping_Season == RR_flooded_vs_non_flooded_tb$Grouping_Season[i])]/data$Number_Of_Birth[which(data$Flooded == FALSE & data$Grouping_Season == RR_flooded_vs_non_flooded_tb$Grouping_Season[i])]))
    RR_flooded_vs_non_flooded_tb$RR_log_SE_empiric[i + (j-1)*60] <- sqrt((1/data$Number_Of_Dead_Birth[which(data$Flooded == TRUE & data$Grouping_Season == RR_flooded_vs_non_flooded_tb$Grouping_Season[i])]) - (1/(data$Number_Of_Birth[which(data$Flooded == TRUE & data$Grouping_Season == RR_flooded_vs_non_flooded_tb$Grouping_Season[i])])) +
                                                                           (1/data$Number_Of_Dead_Birth[which(data$Flooded == FALSE & data$Grouping_Season == RR_flooded_vs_non_flooded_tb$Grouping_Season[i])]) - (1/(data$Number_Of_Birth[which(data$Flooded == FALSE & data$Grouping_Season == RR_flooded_vs_non_flooded_tb$Grouping_Season[i])])))
    RR_flooded_vs_non_flooded_tb$RD_empiric[i + (j-1)*60] <- (data$Number_Of_Dead_Birth[which(data$Flooded == TRUE & data$Grouping_Season == RR_flooded_vs_non_flooded_tb$Grouping_Season[i])]/data$Number_Of_Birth[which(data$Flooded == TRUE & data$Grouping_Season == RR_flooded_vs_non_flooded_tb$Grouping_Season[i])]) -
                                                                       (data$Number_Of_Dead_Birth[which(data$Flooded == FALSE & data$Grouping_Season == RR_flooded_vs_non_flooded_tb$Grouping_Season[i])]/data$Number_Of_Birth[which(data$Flooded == FALSE & data$Grouping_Season == RR_flooded_vs_non_flooded_tb$Grouping_Season[i])])
    RR_flooded_vs_non_flooded_tb$RD_SE_empiric[i + (j-1)*60] <- sqrt((data$Number_Of_Dead_Birth[which(data$Flooded == TRUE & data$Grouping_Season == RR_flooded_vs_non_flooded_tb$Grouping_Season[i])]*((data$Number_Of_Birth[which(data$Flooded == TRUE & data$Grouping_Season == RR_flooded_vs_non_flooded_tb$Grouping_Season[i])]) - data$Number_Of_Dead_Birth[which(data$Flooded == TRUE & data$Grouping_Season == RR_flooded_vs_non_flooded_tb$Grouping_Season[i])])/(data$Number_Of_Birth[which(data$Flooded == TRUE & data$Grouping_Season == RR_flooded_vs_non_flooded_tb$Grouping_Season[i])])^3) +
                                                                       (data$Number_Of_Dead_Birth[which(data$Flooded == FALSE & data$Grouping_Season == RR_flooded_vs_non_flooded_tb$Grouping_Season[i])]*((data$Number_Of_Birth[which(data$Flooded == FALSE & data$Grouping_Season == RR_flooded_vs_non_flooded_tb$Grouping_Season[i])]) - data$Number_Of_Dead_Birth[which(data$Flooded == FALSE & data$Grouping_Season == RR_flooded_vs_non_flooded_tb$Grouping_Season[i])])/(data$Number_Of_Birth[which(data$Flooded == FALSE & data$Grouping_Season == RR_flooded_vs_non_flooded_tb$Grouping_Season[i])])^3))
  }
}


# ## Extract effect estimates Decade Season
# RR_flooded_vs_non_flooded_tb_quartile <- tibble(RR_log_empiric = rep(NA, 6),
#                                                 RR_log_SE_empiric = rep(NA, 6),
#                                                 RD_empiric = rep(NA, 6),
#                                                 RD_SE_empiric = rep(NA, 6),
#                                                 Season = rep(c("Dry", "Rainy"), 3),
#                                                 Decade = rep(c("0","1","2"), each = 2))
# 
# RR_flooded_vs_non_flooded_tb <- rbind(RR_flooded_vs_non_flooded_tb_quartile %>%
#                                         mutate(Exposure = "0 quartile"),
#                                       RR_flooded_vs_non_flooded_tb_quartile %>%
#                                         mutate(Exposure = "1 quartile"),
#                                       RR_flooded_vs_non_flooded_tb_quartile %>%
#                                         mutate(Exposure = "2 quartile"),
#                                       RR_flooded_vs_non_flooded_tb_quartile %>%
#                                         mutate(Exposure = "3 quartile"))
# 
# for (j in 1:4){
#   for (i in 1:6){
#     data <- monthly_time_series_list_2[[j]]
#     RR_flooded_vs_non_flooded_tb$RR_log_empiric[i + (j-1)*6] <- log((data$Number_Of_Dead_Birth[which(data$Flooded == TRUE & data$Decade == RR_flooded_vs_non_flooded_tb$Decade[i] & data$Season == RR_flooded_vs_non_flooded_tb$Season[i])]/data$Number_Of_Birth[which(data$Flooded == TRUE & data$Decade == RR_flooded_vs_non_flooded_tb$Decade[i] & data$Season == RR_flooded_vs_non_flooded_tb$Season[i])]) /
#                                                                        (data$Number_Of_Dead_Birth[which(data$Flooded == FALSE & data$Decade == RR_flooded_vs_non_flooded_tb$Decade[i] & data$Season == RR_flooded_vs_non_flooded_tb$Season[i])]/data$Number_Of_Birth[which(data$Flooded == FALSE & data$Decade == RR_flooded_vs_non_flooded_tb$Decade[i] & data$Season == RR_flooded_vs_non_flooded_tb$Season[i])]))
#     RR_flooded_vs_non_flooded_tb$RR_log_SE_empiric[i + (j-1)*6] <- sqrt((1/data$Number_Of_Dead_Birth[which(data$Flooded == TRUE & data$Decade == RR_flooded_vs_non_flooded_tb$Decade[i] & data$Season == RR_flooded_vs_non_flooded_tb$Season[i])]) - (1/(data$Number_Of_Birth[which(data$Flooded == TRUE & data$Decade == RR_flooded_vs_non_flooded_tb$Decade[i] & data$Season == RR_flooded_vs_non_flooded_tb$Season[i])])) +
#                                                                            (1/data$Number_Of_Dead_Birth[which(data$Flooded == FALSE & data$Decade == RR_flooded_vs_non_flooded_tb$Decade[i] & data$Season == RR_flooded_vs_non_flooded_tb$Season[i])]) - (1/(data$Number_Of_Birth[which(data$Flooded == FALSE & data$Decade == RR_flooded_vs_non_flooded_tb$Decade[i] & data$Season == RR_flooded_vs_non_flooded_tb$Season[i])])))
#     RR_flooded_vs_non_flooded_tb$RD_empiric[i + (j-1)*6] <- (data$Number_Of_Dead_Birth[which(data$Flooded == TRUE & data$Decade == RR_flooded_vs_non_flooded_tb$Decade[i] & data$Season == RR_flooded_vs_non_flooded_tb$Season[i])]/data$Number_Of_Birth[which(data$Flooded == TRUE & data$Decade == RR_flooded_vs_non_flooded_tb$Decade[i] & data$Season == RR_flooded_vs_non_flooded_tb$Season[i])]) -
#       (data$Number_Of_Dead_Birth[which(data$Flooded == FALSE & data$Decade == RR_flooded_vs_non_flooded_tb$Decade[i] & data$Season == RR_flooded_vs_non_flooded_tb$Season[i])]/data$Number_Of_Birth[which(data$Flooded == FALSE & data$Decade == RR_flooded_vs_non_flooded_tb$Decade[i] & data$Season == RR_flooded_vs_non_flooded_tb$Season[i])])
#     RR_flooded_vs_non_flooded_tb$RD_SE_empiric[i + (j-1)*6] <- sqrt((data$Number_Of_Dead_Birth[which(data$Flooded == TRUE & data$Decade == RR_flooded_vs_non_flooded_tb$Decade[i] & data$Season == RR_flooded_vs_non_flooded_tb$Season[i])]*((data$Number_Of_Birth[which(data$Flooded == TRUE & data$Decade == RR_flooded_vs_non_flooded_tb$Decade[i] & data$Season == RR_flooded_vs_non_flooded_tb$Season[i])]) - data$Number_Of_Dead_Birth[which(data$Flooded == TRUE & data$Decade == RR_flooded_vs_non_flooded_tb$Decade[i] & data$Season == RR_flooded_vs_non_flooded_tb$Season[i])])/(data$Number_Of_Birth[which(data$Flooded == TRUE & data$Decade == RR_flooded_vs_non_flooded_tb$Decade[i] & data$Season == RR_flooded_vs_non_flooded_tb$Season[i])])^3) +
#                                                                        (data$Number_Of_Dead_Birth[which(data$Flooded == FALSE & data$Decade == RR_flooded_vs_non_flooded_tb$Decade[i] & data$Season == RR_flooded_vs_non_flooded_tb$Season[i])]*((data$Number_Of_Birth[which(data$Flooded == FALSE & data$Decade == RR_flooded_vs_non_flooded_tb$Decade[i] & data$Season == RR_flooded_vs_non_flooded_tb$Season[i])]) - data$Number_Of_Dead_Birth[which(data$Flooded == FALSE & data$Decade == RR_flooded_vs_non_flooded_tb$Decade[i] & data$Season == RR_flooded_vs_non_flooded_tb$Season[i])])/(data$Number_Of_Birth[which(data$Flooded == FALSE & data$Decade == RR_flooded_vs_non_flooded_tb$Decade[i] & data$Season == RR_flooded_vs_non_flooded_tb$Season[i])])^3))
#   }
# }
# 
# 
# # Plot
# RR_plot <- (RR_flooded_vs_non_flooded_tb %>%
#               # filter(Exposure != "3 quartile") %>%
#               # filter(Exposure != "1 quartile") %>%
#               mutate(Decade = replace(Decade, Decade == "0", "1988-1997"),
#                      Decade = replace(Decade, Decade == "1", "1998-2007"),
#                      Decade = replace(Decade, Decade == "2", "2008-2017")) %>%
#               ggplot(aes(x = Season, y = exp(RR_log_empiric), col = Season)) +
#               geom_point(size = 4) +
#               geom_errorbar(aes(ymin = exp(RR_log_empiric - 1.96*RR_log_SE_empiric),
#                                 ymax = exp(RR_log_empiric + 1.96*RR_log_SE_empiric)),
#                             size = 2) +
#               geom_hline(aes(yintercept = 1)) +
#               facet_wrap(Exposure~Decade, ncol = 3) +
#               ylab("Risk ratio for living in\nflood-prone areas vs non-flood prone areas (empiric)") +
#               coord_flip()
# )
# 
# RR_plot
# 


#-------------------------------------------------------------------------------
RR_flooded_vs_non_flooded_tb_empiric <- RR_flooded_vs_non_flooded_tb
# Save dataset
saveRDS(RR_flooded_vs_non_flooded_tb_empiric,
        file = here("data/final", "RR_flooded_vs_non_flooded_tb_empiric"))

#-------------------------------------------------------------------------------
