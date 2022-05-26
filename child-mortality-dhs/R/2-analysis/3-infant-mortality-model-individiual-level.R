#-------------------------------------------------------------------------------

# @Organization - UCSF
# @Project - Effects of floods on child mortality in Bangladesh
# @Author - Francois Rerolle, rerollefrancois@gmail.com
# @Description - This code runs the models to estimate effects (RR) of
# living in flood prone area on infant mortality at the individual level

#-------------------------------------------------------------------------------

# Source configuration code
library(here)
source(here("child-mortality-dhs/R", "0-config.R"))

#-------------------------------------------------------------------------------

## Load data
birth_records_flooded_0_quartile <- readRDS(file = here("data/final", "birth_records_flooded_0_quartile"))
birth_records_flooded_1_quartile <- readRDS(file = here("data/final", "birth_records_flooded_1_quartile"))
birth_records_flooded_2_quartile <- readRDS(file = here("data/final", "birth_records_flooded_2_quartile"))
birth_records_flooded_3_quartile <- readRDS(file = here("data/final", "birth_records_flooded_3_quartile"))

birth_records_list <- list(birth_records_flooded_0_quartile,
                           birth_records_flooded_1_quartile,
                           birth_records_flooded_2_quartile,
                           birth_records_flooded_3_quartile)

## Add season variables
birth_records_list <-
  lapply(X = birth_records_list,
         FUN = function(x){
           (x %>%
              mutate(Year = 1900 + floor((Birth_Date_Month_CMC-1)/12),
                     Month = Birth_Date_Month_CMC - 12*(Year - 1900),
                     Season = factor(ifelse(Month %in% c(5:10), "Rainy", "Dry")),
                     Decade = factor(floor((Year - 1988)/10)))
            %>% group_by(Flooded)
            %>% arrange(Birth_Date_Month_CMC)
            %>% mutate(New_Season = Season != lag(Season),
                       New_Season = replace(New_Season, is.na(New_Season), FALSE),
                       Grouping_Season = cumsum(New_Season))
            %>% ungroup()
            %>% filter(Year!=1987)
            %>% filter(Birth_Date_Month_CMC != 1413)
           )
         }
  )


## Model
# model_glm <- gam(Infant_Death ~ Flooded*factor(Grouping_Season),
#                  data = birth_records_list[[4]],
#                  family = binomial(link = "log"))
# 
# model_glm <- survival::clogit(Infant_Death ~ Flooded*Decade*Season + strata(subclass),
#                               data = birth_records_list[[4]],
#                               method = "approximate")
# 
# model_glm <- survival::clogit(Infant_Death ~ Flooded*factor(Grouping_Season) + strata(subclass),
#                               data = birth_records_list[[1]],
#                               method = "approximate")
# 
# summary(model_glm)
# summary(model_glm)$coefficients
# 
# 
# coeftest(model_glm, vcov. = vcovCL, cluster = ~subclass)
# exp(coef(model_glm))


## Extract effect estimates Decade
OR_flooded_vs_non_flooded_tb_quartile <- tibble(OR_log = rep(NA, 6),
                                                OR_log_SE = rep(NA, 6),
                                                Season = rep(c("Dry", "Rainy"), 3),
                                                Decade = rep(c("0","1","2"), each = 2))

OR_flooded_vs_non_flooded_tb <- rbind(OR_flooded_vs_non_flooded_tb_quartile %>%
                                        mutate(Exposure = "0 quartile"),
                                      OR_flooded_vs_non_flooded_tb_quartile %>%
                                        mutate(Exposure = "1 quartile"),
                                      OR_flooded_vs_non_flooded_tb_quartile %>%
                                        mutate(Exposure = "2 quartile"),
                                      OR_flooded_vs_non_flooded_tb_quartile %>%
                                        mutate(Exposure = "3 quartile"))

for (j in 1:4){
  for (i in 1:6){
    model_clogit <- survival::clogit(Infant_Death ~ Flooded*relevel(Season, ref = OR_flooded_vs_non_flooded_tb$Season[i])*relevel(Decade, ref = OR_flooded_vs_non_flooded_tb$Decade[i]) + strata(subclass),
                     data = birth_records_list[[j]],
                     method = "approximate")
    
    OR_flooded_vs_non_flooded_tb$OR_log[i + (j-1)*6] <- summary(model_clogit)$coefficients[1,1]
    OR_flooded_vs_non_flooded_tb$OR_log_SE[i + (j-1)*6] <- summary(model_clogit)$coefficients[1,3]
  }
}

OR_plot <- (OR_flooded_vs_non_flooded_tb %>%
              filter(Exposure != "3 quartile") %>%
              # filter(Exposure != "1 quartile") %>%
              mutate(Decade = replace(Decade, Decade == "0", "1988-1997"),
                     Decade = replace(Decade, Decade == "1", "1998-2007"),
                     Decade = replace(Decade, Decade == "2", "2008-2017")) %>%
              ggplot(aes(x = Season, y = exp(OR_log), col = Season)) +
              geom_point(size = 4) +
              geom_errorbar(aes(ymin = exp(OR_log - 1.96*OR_log_SE),
                                ymax = exp(OR_log + 1.96*OR_log_SE)),
                            size = 2) +
              geom_hline(aes(yintercept = 1)) +
              facet_wrap(Exposure~Decade, ncol = 3) +
              ylab("Odds ratio for living in\nflood-prone areas vs non-flood prone areas") +
              coord_flip()
)

OR_plot


## Extract effect estimates Grouping Season
OR_flooded_vs_non_flooded_tb_quartile <- tibble(OR_log = rep(NA, 60),
                                                OR_log_SE = rep(NA, 60),
                                                Season = rep(c("Dry", "Rainy"), 30),
                                                Grouping_Season = rep(1:60))

OR_flooded_vs_non_flooded_tb <- rbind(OR_flooded_vs_non_flooded_tb_quartile %>%
                                        mutate(Exposure = "0 quartile"),
                                      OR_flooded_vs_non_flooded_tb_quartile %>%
                                        mutate(Exposure = "1 quartile"),
                                      OR_flooded_vs_non_flooded_tb_quartile %>%
                                        mutate(Exposure = "2 quartile"),
                                      OR_flooded_vs_non_flooded_tb_quartile %>%
                                        mutate(Exposure = "3 quartile"))

for (j in 1:3){
  for (i in 1:60){
    model_clogit <- survival::clogit(Infant_Death ~ Flooded*relevel(factor(Grouping_Season), ref = OR_flooded_vs_non_flooded_tb$Grouping_Season[i]) + strata(subclass),
                                     data = birth_records_list[[j]],
                                     method = "approximate")
    
    OR_flooded_vs_non_flooded_tb$OR_log[i + (j-1)*60] <- summary(model_clogit)$coefficients[1,1]
    OR_flooded_vs_non_flooded_tb$OR_log_SE[i + (j-1)*60] <- summary(model_clogit)$coefficients[1,3]
  }
}

OR_plot <- (OR_flooded_vs_non_flooded_tb %>%
              filter(Exposure == "0 quartile") %>%
              # filter(Exposure != "1 quartile") %>%
              # mutate(Decade = replace(Decade, Decade == "0", "1988-1997"),
              #        Decade = replace(Decade, Decade == "1", "1998-2007"),
              #        Decade = replace(Decade, Decade == "2", "2008-2017")) %>%
              ggplot(aes(x = Season, y = exp(OR_log), col = Season)) +
              geom_point(size = 4) +
              geom_errorbar(aes(ymin = exp(OR_log - 1.96*OR_log_SE),
                                ymax = exp(OR_log + 1.96*OR_log_SE)),
                            size = 2) +
              ylim(0,5) +
              geom_hline(aes(yintercept = 1)) +
              facet_wrap(~Grouping_Season, ncol = 5) +
              ylab("Odds ratio for living in\nflood-prone areas vs non-flood prone areas") +
              coord_flip()
)

OR_plot


#-------------------------------------------------------------------------------

# Save dataset
saveRDS(OR_flooded_vs_non_flooded_tb,
        file = here("data/final", "OR_flooded_vs_non_flooded_tb_individual_level"))

#-------------------------------------------------------------------------------
