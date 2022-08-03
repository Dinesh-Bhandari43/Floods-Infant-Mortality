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

RD_flooded_vs_non_flooded_tb_empiric <- readRDS(file = here("data/final",
                                                    "RD_flooded_vs_non_flooded_tb_empiric"))


data <- (RD_flooded_vs_non_flooded_tb_empiric %>%
           left_join(OR_flooded_vs_non_flooded_tb) %>%
           mutate(Season = rep(c("Dry", "Rainy"), nrow(RD_flooded_vs_non_flooded_tb_empiric)/2),
                  Decade = as.character(floor(Grouping_Season/20)),
                  Decade = replace(Decade, Decade == 0, "1988-1997"),
                  Decade = replace(Decade, Decade == 1, "1998-2007"),
                  Decade = replace(Decade, Decade == 2, "2008-2017"),
                  Subgroup = paste(Season, Decade),
                  Year = rep(rep(1988:2017, each = 2), 4))
         )

#-------------------------------------------------------------------------------

estimate.te <- c("RR_log_empiric", "RD_empiric",
                 "OR_log_empiric", "OR_log")

estimate.se <- c("RR_log_SE_empiric", "RD_SE_empiric",
                 "OR_log_SE_empiric", "OR_log_SE")

subgroup.j <- c("Subgroup", "Decade", "Season")

season.data <- c("Rainy", "Dry",
                 "Rainy", "Dry",
                 "Rainy", "Dry",
                 "Overall", "Overall", "Overall",
                 "Rainy", "Dry",
                 "Overall")

decade.data <- c("1988-1997", "1988-1997",
                 "1998-2007", "1998-2007",
                 "2008-2017", "2008-2017",
                 "1988-1997",
                 "1998-2007",
                 "2008-2017",
                 "1988-2017", "1988-2017",
                 "1988-2017")

exposure.k <- c("0 quartile", "1 quartile", "2 quartile", "3 quartile")

TE <- NULL
TE.lower <- NULL
TE.upper <- NULL

for (k in 1:length(exposure.k)){
  for (i in 1:length(estimate.te)){
    for (j in 1:length(subgroup.j)){
      meta.analysis <- metagen(data = data,
                           subset = Exposure == exposure.k[k] & Grouping_Season < 60 & Grouping_Season > 1,
                           TE = get(estimate.te[i]),
                           seTE = get(estimate.se[i]),
                           subgroup = get(subgroup.j[j]),
                           studlab = Grouping_Season,
                           fixed = F,
                           overall = T)
      # Store stratified results
      TE <- c(TE, meta.analysis$TE.random.w)
      TE.lower <- c(TE.lower, meta.analysis$lower.random.w)
      TE.upper <- c(TE.upper, meta.analysis$upper.random.w)
      }
    # Store overall results
    TE <- c(TE, meta.analysis$TE.random)
    TE.lower <- c(TE.lower, meta.analysis$lower.random)
    TE.upper <- c(TE.upper, meta.analysis$upper.random)
  }
}

## Compile data
meta.analyses.results <- data.frame(TE, TE.lower, TE.upper,
                                    Estimate = rep(rep(estimate.te, each = 12), length(exposure.k)),
                                    Season = rep(season.data, length(exposure.k)),
                                    Decade = rep(decade.data, length(exposure.k)),
                                    Exposure = rep(exposure.k, each = length(season.data)*length(estimate.te)))

# Save dataset
saveRDS(meta.analyses.results, file = here("data/final", "meta.analyses.results"))
readr::write_csv(meta.analyses.results, file = here("data/final", "meta.analyses.results.csv"))

# Plot for appendix
# Decade*Season stratified
meta.analysis <- metagen(data = data %>% mutate(Study = paste(Season, rep(1988:2017, each = 2))),
                         subset = Exposure == "0 quartile" & Grouping_Season < 60 & Grouping_Season > 1,
                         TE = 1000*RD_empiric,
                         seTE = 1000*RD_SE_empiric,
                         subgroup = Subgroup,
                         studlab = Study,
                         fixed = F,
                         overall = T)

forest.meta(x = meta.analysis,
            subgroup = T,
            xlab = paste("Infant mortality risk difference (per 1000 births) for living in\nflood-prone areas vs non-flood prone areas (empiric)"),
            fontsize = 8,
            spacing = 0.5,
            hetstat = T)

### Save
pdf(here("child-mortality-dhs/output/figures", "meta-analyses-RD.pdf"))
forest.meta(x = meta.analysis,
            subgroup = T,
            xlab = paste("Infant mortality risk difference (per 1000 births) for living in\nflood-prone areas vs non-flood prone areas (empiric)"),
            fontsize = 6,
            fs.axis = 4,
            fs.xlab = 8,
            spacing = 0.35,
            hetstat = T)
dev.off()


# Decade stratified
meta.analysis.d <- metagen(data = data %>% mutate(Study = paste(Season, rep(1988:2017, each = 2))),
                         subset = Exposure == "0 quartile" & Grouping_Season < 60 & Grouping_Season > 1,
                         TE = 1000*RD_empiric,
                         seTE = 1000*RD_SE_empiric,
                         subgroup = Decade,
                         studlab = Study,
                         fixed = F,
                         overall = T)

forest.meta(x = meta.analysis.d,
            subgroup = T,
            xlab = paste("Infant mortality risk difference (per 1000 births) for living in\nflood-prone areas vs non-flood prone areas (empiric)"),
            fontsize = 8,
            spacing = 0.5,
            hetstat = T)

### Save
pdf(here("child-mortality-dhs/output/figures", "meta-analyses-RD.d.pdf"))
forest.meta(x = meta.analysis.d,
            subgroup = T,
            xlab = paste("Infant mortality risk difference (per 1000 births) for living in\nflood-prone areas vs non-flood prone areas (empiric)"),
            fontsize = 6,
            fs.axis = 4,
            fs.xlab = 8,
            spacing = 0.35,
            hetstat = T)
dev.off()


# Season stratified
meta.analysis.s <- metagen(data = data %>% mutate(Study = paste(Season, rep(1988:2017, each = 2))),
                           subset = Exposure == "0 quartile" & Grouping_Season < 60 & Grouping_Season > 1,
                           TE = 1000*RD_empiric,
                           seTE = 1000*RD_SE_empiric,
                           subgroup = Season,
                           studlab = Study,
                           fixed = F,
                           overall = T)

forest.meta(x = meta.analysis.s,
            subgroup = T,
            xlab = paste("Infant mortality risk difference (per 1000 births) for living in\nflood-prone areas vs non-flood prone areas (empiric)"),
            fontsize = 8,
            spacing = 0.5,
            hetstat = T)

### Save
pdf(here("child-mortality-dhs/output/figures", "meta-analyses-RD.s.pdf"))
forest.meta(x = meta.analysis.s,
            subgroup = T,
            xlab = paste("Infant mortality risk difference (per 1000 births) for living in\nflood-prone areas vs non-flood prone areas (empiric)"),
            fontsize = 6,
            fs.axis = 4,
            fs.xlab = 8,
            spacing = 0.35,
            hetstat = T)
dev.off()


