#-------------------------------------------------------------------------------

# @Organization - UCSF
# @Project - Effects of floods on child mortality in Bangladesh
# @Author - Francois Rerolle, rerollefrancois@gmail.com
# @Description - This code runs the meta-analysis of effect estimates for
# living in flood prone area on infant mortality over time and per season

#-------------------------------------------------------------------------------

# Source configuration code
library(here)
source(here("child-mortality-dhs/R", "0-config.R"))

#-------------------------------------------------------------------------------

## Load data
OR_flooded_vs_non_flooded_tb <- readRDS(file = here("data/final",
                                                    "OR_flooded_vs_non_flooded_tb_individual_level"))

RR_flooded_vs_non_flooded_tb <- readRDS(file = here("data/final",
                                                    "RR_flooded_vs_non_flooded_tb"))

RR_flooded_vs_non_flooded_tb_empiric <- readRDS(file = here("data/final",
                                                    "RR_flooded_vs_non_flooded_tb_empiric"))


data <- (RR_flooded_vs_non_flooded_tb %>%
           left_join(RR_flooded_vs_non_flooded_tb_empiric) %>%
           mutate(Decade = floor(Grouping_Season/20),
                  Subgroup = paste(Season, Decade),
                  Year = rep(rep(1988:2017, each = 2), 4))
         )

data_OR <- (OR_flooded_vs_non_flooded_tb %>%
              mutate(Decade = floor(Grouping_Season/20),
                     Subgroup = paste(Season, Decade),
                     Year = rep(rep(1988:2017, each = 2), 4))
)
#-------------------------------------------------------------------------------

## Meta analysis
# meta.analysis <- metagen(data = data,
#                          subset = Exposure == "0 quartile" & Grouping_Season < 60 & Grouping_Season > 1,
#                          TE = RR,
#                          seTE = RR_SE,
#                          subgroup = Subgroup,
#                          studlab = Grouping_Season,
#                          fixed = F,
#                          overall = T)
# 
# forest.meta(x = meta.analysis,
#             subgroup = T,
#             xlab = paste("Infant mortality risk ratio (log) for living in\nflood-prone areas vs non-flood prone areas (model)"),
#             fontsize = 8,
#             spacing = 0.5,
#             hetstat = F)

## Meta analysis RR (empiric)
meta.analysis <- metagen(data = data,
                         subset = Exposure == "0 quartile" & Grouping_Season < 60 & Grouping_Season > 1,
                         TE = RR_log_empiric,
                         seTE = RR_log_SE_empiric,
                         subgroup = Subgroup,
                         studlab = Grouping_Season,
                         fixed = F,
                         overall = T)

forest.meta(x = meta.analysis,
            subgroup = T,
            xlab = paste("Infant mortality risk ratio (log) for living in\nflood-prone areas vs non-flood prone areas (empiric)"),
            fontsize = 8,
            spacing = 0.5,
            hetstat = T)


## Meta analysis RD (empiric)
meta.analysis <- metagen(data = data,
                         subset = Exposure == "0 quartile" & Grouping_Season < 60 & Grouping_Season > 1,
                         TE = 1000*RD_empiric,
                         seTE = 1000*RD_SE_empiric,
                         subgroup = Subgroup,
                         studlab = Grouping_Season,
                         fixed = F,
                         overall = T)

forest.meta(x = meta.analysis,
            subgroup = T,
            xlab = paste("Infant mortality risk difference for living in\nflood-prone areas vs non-flood prone areas (empiric)"),
            fontsize = 8,
            spacing = 0.5,
            hetstat = T)


## Meta analysis OR
meta.analysis <- metagen(data = data_OR,
                         subset = Exposure == "0 quartile" & Grouping_Season < 60 & Grouping_Season > 1,
                         TE = OR_log,
                         seTE = OR_log_SE,
                         subgroup = Subgroup,
                         studlab = Grouping_Season,
                         fixed = F,
                         overall = T)

forest.meta(x = meta.analysis,
            subgroup = T,
            xlab = paste("Infant mortality odds ratio for living in\nflood-prone areas vs non-flood prone areas (empiric)"),
            fontsize = 8,
            spacing = 0.5,
            hetstat = T)
