#-------------------------------------------------------------------------------

# @Organization - UCSF
# @Project - Effects of floods on child mortality in Bangladesh
# @Author - Francois Rerolle, rerollefrancois@gmail.com
# @Description - This code formats the runs the models to estimate effects of
# living in flood prone area on infant mortality over time and per season

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
monthly_time_series_chirps_list <-
  lapply(X = monthly_time_series_list,
         FUN = function(x){
           (x %>%
              mutate(Year = 1900 + floor((Birth_Date_Month_CMC-1)/12),
                     Month = Birth_Date_Month_CMC - 12*(Year - 1900),
                     Season = factor(ifelse(Month %in% c(5:10), "Rainy", "Dry")),
                     Half = factor(floor((Year - 1988)/15)),
                     Decade = factor(floor((Year - 1988)/10)),
                     Quinquenat = factor(floor((Year - 1988)/5)))
            %>% left_join(BGD_chirps)
            %>% filter(Year!=1987)
            %>% filter(Birth_Date_Month_CMC != 1413)
            )
           }
         )

## Model
model_gam <- gam(Number_Of_Dead_Birth ~ Flooded*relevel(Season, ref = "Rainy")*relevel(Decade, ref = "0") + s(Birth_Date_Month_CMC) + s(Mean_Precipitation_mm) + offset(log(Number_Of_Birth)),
                 data = monthly_time_series_chirps_list[[1]],
                 select = T,
                 family = "nb")

summary(model_gam)
plot(model_gam)
# 
# ## Extract effect estimates Halves
# RR_flooded_vs_non_flooded_tb_quartile <- tibble(RR = rep(NA, 4),
#                                                 RR_SE = rep(NA, 4),
#                                                 Season = rep(c("Dry", "Rainy"), 2),
#                                                 Half = rep(c("0","1"), each = 2))
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
#   for (i in 1:4){
#     model_gam <- gam(Number_Of_Dead_Birth ~ Flooded*relevel(Season, ref = RR_flooded_vs_non_flooded_tb$Season[i])*relevel(Half, ref = RR_flooded_vs_non_flooded_tb$Half[i]) + s(Birth_Date_Month_CMC) + s(Mean_Precipitation_mm) + offset(log(Number_Of_Birth)),
#                      data = monthly_time_series_chirps_list[[j]],
#                      family = "nb")
#     
#     RR_flooded_vs_non_flooded_tb$RR[i + (j-1)*4] <- summary(model_gam)$p.coeff[2]
#     RR_flooded_vs_non_flooded_tb$RR_SE[i + (j-1)*4] <- summary(model_gam)$se[2]
#   }
# }
# 
# RR_plot <- (RR_flooded_vs_non_flooded_tb %>%
#               # filter(Exposure != "3 quartile") %>%
#               # filter(Exposure != "1 quartile") %>%
#               mutate(Half = replace(Half, Half == "0", "1988-2002"),
#                      Half = replace(Half, Half == "1", "2003-2017")) %>%
#               ggplot(aes(x = Season, y = exp(RR), col = Season)) +
#               geom_point(size = 4) +
#               geom_errorbar(aes(ymin = exp(RR - 1.96*RR_SE),
#                                 ymax = exp(RR + 1.96*RR_SE)),
#                             size = 2) +
#               geom_hline(aes(yintercept = 1)) +
#               facet_wrap(Exposure~Half, ncol = 2) +
#               ylab("Risk ratio for living in\nflood-prone areas vs non-flood prone areas") +
#               coord_flip()
#             )
# 
# RR_plot

## Extract effect estimates Decade
RR_flooded_vs_non_flooded_tb_quartile <- tibble(RR = rep(NA, 6),
                                                RR_SE = rep(NA, 6),
                                                Season = rep(c("Dry", "Rainy"), 3),
                                                Decade = rep(c("0","1","2"), each = 2))

RR_flooded_vs_non_flooded_tb <- rbind(RR_flooded_vs_non_flooded_tb_quartile %>%
                                        mutate(Exposure = "0 quartile"),
                                      RR_flooded_vs_non_flooded_tb_quartile %>%
                                        mutate(Exposure = "1 quartile"),
                                      RR_flooded_vs_non_flooded_tb_quartile %>%
                                        mutate(Exposure = "2 quartile"),
                                      RR_flooded_vs_non_flooded_tb_quartile %>%
                                        mutate(Exposure = "3 quartile"))

for (j in 1:4){
  for (i in 1:6){
    model_gam <- gam(Number_Of_Dead_Birth ~ Flooded*relevel(Season, ref = RR_flooded_vs_non_flooded_tb$Season[i])*relevel(Decade, ref = RR_flooded_vs_non_flooded_tb$Decade[i]) + s(Birth_Date_Month_CMC) + s(Mean_Precipitation_mm) + offset(log(Number_Of_Birth)),
                     data = monthly_time_series_chirps_list[[j]],
                     family = "nb")
    
    RR_flooded_vs_non_flooded_tb$RR[i + (j-1)*6] <- summary(model_gam)$p.coeff[2]
    RR_flooded_vs_non_flooded_tb$RR_SE[i + (j-1)*6] <- summary(model_gam)$se[2]
  }
}

RR_plot <- (RR_flooded_vs_non_flooded_tb %>%
              filter(Exposure != "3 quartile") %>%
              # filter(Exposure != "1 quartile") %>%
              mutate(Decade = replace(Decade, Decade == "0", "1988-1997"),
                     Decade = replace(Decade, Decade == "1", "1998-2007"),
                     Decade = replace(Decade, Decade == "2", "2008-2017")) %>%
              ggplot(aes(x = Season, y = exp(RR), col = Season)) +
              geom_point(size = 4) +
              geom_errorbar(aes(ymin = exp(RR - 1.96*RR_SE),
                                ymax = exp(RR + 1.96*RR_SE)),
                            size = 2) +
              geom_hline(aes(yintercept = 1)) +
              facet_wrap(Exposure~Decade, ncol = 3) +
              ylab("Risk ratio for living in\nflood-prone areas vs non-flood prone areas") +
              coord_flip()
)

RR_plot


# ## Extract effect estimates Quinquenat
# RR_flooded_vs_non_flooded_tb_quartile <- tibble(RR = rep(NA, 12),
#                                                 RR_SE = rep(NA, 12),
#                                                 Season = rep(c("Dry", "Rainy"), 6),
#                                                 Quinquenat = rep(c("0","1", "2", "3", "4", "5"), each = 2))
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
#   for (i in 1:12){
#     model_gam <- gam(Number_Of_Dead_Birth ~ Flooded*relevel(Season, ref = RR_flooded_vs_non_flooded_tb$Season[i])*relevel(Quinquenat, ref = RR_flooded_vs_non_flooded_tb$Quinquenat[i]) + s(Birth_Date_Month_CMC) + s(Mean_Precipitation_mm) + offset(log(Number_Of_Birth)),
#                      data = monthly_time_series_chirps_list[[j]],
#                      family = "nb")
#     
#     RR_flooded_vs_non_flooded_tb$RR[i + (j-1)*12] <- summary(model_gam)$p.coeff[2]
#     RR_flooded_vs_non_flooded_tb$RR_SE[i + (j-1)*12] <- summary(model_gam)$se[2]
#   }
# }
# 
# RR_plot <- (RR_flooded_vs_non_flooded_tb %>%
#               # filter(Exposure != "3 quartile") %>%
#               # filter(Exposure != "1 quartile") %>%
#               mutate(Quinquenat = replace(Quinquenat, Quinquenat == "0", "1988-1992"),
#                      Quinquenat = replace(Quinquenat, Quinquenat == "1", "1993-1997"),
#                      Quinquenat = replace(Quinquenat, Quinquenat == "2", "1998-2002"),
#                      Quinquenat = replace(Quinquenat, Quinquenat == "3", "2003-2007"),
#                      Quinquenat = replace(Quinquenat, Quinquenat == "4", "2008-2012"),
#                      Quinquenat = replace(Quinquenat, Quinquenat == "5", "2013-2017")) %>%
#               ggplot(aes(x = Season, y = exp(RR), col = Season)) +
#               geom_point(size = 4) +
#               geom_errorbar(aes(ymin = exp(RR - 1.96*RR_SE),
#                                 ymax = exp(RR + 1.96*RR_SE)),
#                             size = 2) +
#               geom_hline(aes(yintercept = 1)) +
#               facet_wrap(Exposure~Quinquenat, ncol = 6) +
#               ylab("Risk ratio for living in\nflood-prone areas vs non-flood prone areas") +
#               coord_flip()
# )
# 
# RR_plot
