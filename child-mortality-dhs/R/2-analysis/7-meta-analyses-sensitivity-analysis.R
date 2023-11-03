#-------------------------------------------------------------------------------

# @Organization - UCSF
# @Project - Effects of floods on child mortality in Bangladesh
# @Author - Francois Rerolle, rerollefrancois@gmail.com
# @Description - This code runs the meta-analysis of effect estimates for
# living in flood prone area on infant mortality over time and per season
# For PNAS revision 2

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
                  Decade = as.character(floor(Grouping_Season/10)),
                  Decade = replace(Decade, Decade == 0, "1988-1992"),
                  Decade = replace(Decade, Decade == 1, "1993-1997"),
                  Decade = replace(Decade, Decade == 2, "1998-2002"),
                  Decade = replace(Decade, Decade == 3, "2003-2007"),
                  Decade = replace(Decade, Decade == 4, "2008-2012"),
                  Decade = replace(Decade, Decade == 5, "2013-2017"),
                  Subgroup = paste(Season, Decade),
                  Year = rep(rep(1988:2017, each = 2), 12))
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

decade.data <- c("2003-2007", "2003-2007",
                 "2008-2012", "2008-2012",
                 "2013-2017", "2013-2017",
                 "2003-2007",
                 "2008-2012",
                 "2013-2017",
                 "2003-2017", "2003-2017",
                 "2003-2017")

# season.data <- c("Rainy", "Dry",
#                  "Rainy", "Dry",
#                  "Overall", "Overall",
#                  "Rainy", "Dry",
#                  "Overall")
# 
# decade.data <- c("2003-2007", "2003-2007",
#                  "2008-2017", "2008-2017",
#                  "2003-2007","2008-2017",
#                  "2003-2017", "2003-2017",
#                  "2003-2017")

exposure.k <- c("0 quartile", "Flood year")

TE <- NULL
TE.lower <- NULL
TE.upper <- NULL

for (k in 1:length(exposure.k)){
  for (i in 1:length(estimate.te)){
    for (j in 1:length(subgroup.j)){
      meta.analysis <- metagen(data = data,
                           subset = Exposure == exposure.k[k] & Grouping_Season < 60 & Grouping_Season > 29,
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
meta.analyses.results_se <- data.frame(TE, TE.lower, TE.upper,
                                    Estimate = rep(rep(estimate.te, each = 12), length(exposure.k)),
                                    Season = rep(season.data, length(exposure.k)),
                                    Decade = rep(decade.data, length(exposure.k)),
                                    Exposure = rep(exposure.k, each = length(season.data)*length(estimate.te)))

# Save dataset
saveRDS(meta.analyses.results_se, file = here("data/final", "meta.analyses.results_se"))
readr::write_csv(meta.analyses.results_se, file = here("data/final", "meta.analyses.results-se.csv"))


#-------------------------------------------------------------------------------
# examine individual year estimates for time-varying and composite exposures
# over the 2003 - 2017 period

# time varying
meta_analysis_time_varying <- metagen(data = data,
                           subset = Exposure == "Flood year" & Grouping_Season < 60 & Grouping_Season > 29,
                           TE = 1000*RD_empiric,
                           seTE = 1000*RD_SE_empiric,
                           subgroup = Season,
                           studlab = Year,
                           fixed = F,
                           overall = T)
forest.meta(x = meta_analysis_time_varying,
            subgroup = T,
            xlab = paste("Infant mortality risk difference (per 1000 births) for living in\nflood-prone areas vs non-flood prone areas (empiric)"),
            fontsize = 6,
            fs.axis = 4,
            fs.xlab = 5,
            spacing = 0.35,
            hetstat = T)

# composite exposure (as used in the main analysis, but over a unique time period here, 2003 - 2017)
meta_analysis_composite <- metagen(data = data,
                                      subset = Exposure == "0 quartile" & Grouping_Season < 60 & Grouping_Season > 29,
                                      TE = 1000*RD_empiric,
                                      seTE = 1000*RD_SE_empiric,
                                      subgroup = Season,
                                      studlab = Year,
                                      fixed = F,
                                      overall = T)
forest.meta(x = meta_analysis_composite,
            subgroup = T,
            xlab = paste("Infant mortality risk difference (per 1000 births) for living in\nflood-prone areas vs non-flood prone areas (empiric)"),
            fontsize = 6,
            fs.axis = 4,
            fs.xlab = 5,
            spacing = 0.35,
            hetstat = T)


#-------------------------------------------------------------------------------
# Summarize results from the time-varying sensitivity analysis in a single figure


# Panel A
# Love plot for balance in the time-varying analysis

# Load matched output
m_flood_year <- readRDS(file = here("child-mortality-dhs/output/matching", "matching_flood_year"))


# Rename covariates
v <- data.frame(old = c("distance",
                        "factor(Region)_BARISAL", "factor(Region)_CHITTAGONG", "factor(Region)_DHAKA", "factor(Region)_KHULNA", "factor(Region)_RAJSHAHI", "factor(Region)_RANGPUR", "factor(Region)_SYLHET",
                        "DHSYEAR_2000", "DHSYEAR_2004", "DHSYEAR_2007", "DHSYEAR_2011", "DHSYEAR_2014", "DHSYEAR_2018",
                        "factor(URBAN_RURA)_U",
                        "Birth_Date_Mother_Month_CMC",
                        "Highest_Level_Education_higher", "Highest_Level_Education_no education", "Highest_Level_Education_primary", "Highest_Level_Education_secondary",
                        "Wealth_Index_middle", "Wealth_Index_poorer", "Wealth_Index_poorest", "Wealth_Index_richer", "Wealth_Index_richest",
                        "Total_Children",
                        "Age_Mother_First_Birth_Years",
                        "Source_Of_Drinking_Water_Unimproved",
                        "Type_Of_Toilet_Facility_Unimproved",
                        "Main_Floor_Material_Rudimentary", "Main_Wall_Material_Rudimentary", "Main_Roof_Material_Rudimentary",
                        "Toilets_Facilities_Shared_Other_HH_no", "Toilets_Facilities_Shared_Other_HH_yes"
),
new = c("Propensity Score",
        "Region Barisal", "Region Chittagong", "Region Dakha", "Region Khulna", "Region Rajshahi", "Region Rangpur", "Region Sylhet",
        "1999 DHS wave", "2004 DHS wave", "2007 DHS wave", "2011 DHS wave", "2014 DHS wave", "2018 DHS wave",
        "Urban residence",
        "Mother's birth date",
        "Higher education", "No education", "Primary education", "Secondary education",
        "Middle wealth index quintile", "Poorer wealth index quintile", "Poorest wealth index quintile", "Richer wealth index quintile", "Richest wealth index quintile",
        "Total number of children",
        "Age of mother at first birth",
        "Unimproved source of drinking water",
        "Unimproved sanitation",
        "Rudimentray floor materials", "Rudimentray wall materials", "Rudimentray roof materials",
        "Sanitation facilities shared", "Sanitation facilities not shared"
))

love_plot_flood_year <- love.plot(m_flood_year,
                                  stats = c("mean.diffs"), 
                                  threshold = c(m = .1), 
                                  binary = "std",
                                  abs = TRUE,
                                  # var.order = "unadjusted",
                                  var.names = v,
                                  limits = c(0, 1),
                                  grid = FALSE,
                                  wrap = 20,
                                  sample.names = c("Unmatched", "Matched"),
                                  position = "top",
                                  shapes = c("circle", "circle"),
                                  colors = c(cbpal[8], "black")) +
  labs(tag = "a") +
  scale_x_continuous(breaks=seq(0,1,by=0.2)) +
  theme(plot.tag = element_text(face = "bold", size = 18))

#-----------------------------------------
# Supplemental figure that focuses on the 
# 15 year period, 2003-2017
# shorter 5 year periods x season
# are too fine of stratification, as there
# is insufficient information in 5-year
# periods to compare
#-----------------------------------------

# Panel b - risk difference
d2_rd <- meta.analyses.results_se %>%
  filter(Decade == "2003-2017", Estimate == "RD_empiric") %>%
  mutate(Season = factor(Season, levels = c("Overall", "Rainy", "Dry")),
         Exposure = factor(Exposure)
  )

plot_rd_flood_year <- ggplot(data=d2_rd, 
                             aes(x = Season, 
                                 y = 1000*TE, ymin = 1000*TE.lower, ymax = 1000*TE.upper, 
                                 col = Season, shape = Exposure)) +
  geom_hline(yintercept = 0, lty = "dashed") + 
  geom_pointrange(cex = 0.5,
                  position = position_dodge(0.5)) +
  scale_y_continuous(breaks = seq(from = -10, to = 40, by = 5)) + 
  scale_color_manual(labels = c("Overall", "Rainy", "Dry"),
                     values = c("black", "#56B4E9", "#E69F00"), 
                     guide = "none") +
  scale_shape_discrete(name = "Flood definition",
                       breaks = c("0 quartile", "Flood year"),
                       labels = c("Composite\n(main analysis)", "Time varying")) +
  labs(y="Risk difference (per 1000 births)", tag="b") +
  theme_minimal() + 
  theme(legend.position = "top",
        panel.grid.major.x = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12),
        legend.title = element_text(vjust=1),
        legend.text = element_text(vjust=1),
        axis.title.x = element_blank(), 
        plot.tag = element_text(face = "bold", size = 18)
  )


# panel c - risk ratio
d2_rr <- meta.analyses.results_se %>%
  filter(Decade == "2003-2017", Estimate == "RR_log_empiric") %>%
  mutate(Season = factor(Season, levels = c("Overall", "Rainy", "Dry")),
         Exposure = factor(Exposure)
  ) %>%
  mutate(rr = exp(TE),
         rr_lower = exp(TE.lower),
         rr_upper = exp(TE.upper))


plot_rr_flood_year <- ggplot(data=d2_rr, 
                             aes(x = Season, 
                                 y = rr, ymin = rr_lower, ymax = rr_upper, 
                                 col = Season, shape = Exposure)) +
  geom_hline(yintercept = 1, lty = "dashed") +
  geom_pointrange(cex = 0.5,
                  position = position_dodge(0.5)) +
   scale_y_continuous(breaks = seq(from = 0.8, to = 1.6, by = 0.2), trans = "log") + 
  scale_color_manual(labels = c("Overall", "Rainy", "Dry"),
                     values = c("black", "#56B4E9", "#E69F00"), 
                     guide = "none") +
  scale_shape_discrete(name = "Flood definition",
                       breaks = c("0 quartile", "Flood year"),
                       labels = c("Composite\n(main analysis)", "Time varying")) +
  labs(y="Risk ratio", tag="c") +
  coord_cartesian(ylim=c(0.8,1.6)) +
  theme_minimal() +
  theme(legend.position = "top",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12),
        legend.title = element_text(vjust=1),
        legend.text = element_text(vjust=1),
        axis.title.x = element_blank(), 
        plot.tag = element_text(face = "bold", size = 18)
  )


#-----------------------------------------
# composite figure, all 3 panels
#-----------------------------------------
library(gridExtra)
library(cowplot)

grid_flood_year_composite <- grid.arrange(
  love_plot_flood_year, 
  plot_rd_flood_year,
  plot_rr_flood_year, 
  ncol = 2, 
  layout_matrix = cbind(c(1,1),c(2,3)), 
  widths = c(0.6,0.4)
)

plot_flood_year_composite <- cowplot::ggdraw(grid_flood_year_composite) +
  theme(plot.background = element_rect(fill="white", color = NA))

# save the figure
ggsave(filename=here("child-mortality-dhs/output/figures", "sensitivity-analysis-time-varying-flood-exposure.pdf"), plot_flood_year_composite,
       device = "pdf",
        width=11,height=8
       )




