#-------------------------------------------------------------------------------

# @Organization - UCSF
# @Project - Effects of floods on child mortality in Bangladesh
# @Author - Francois Rerolle, rerollefrancois@gmail.com
# @Description - This code runs all scripts for the PNAS suggested leave one out cross-validation

#-------------------------------------------------------------------------------

# Source configuration code
library(here)
source(here("child-mortality-dhs/R", "0-config.R"))

#-------------------------------------------------------------------------------

# pick scenario and overwrite primary exposure layer (N.B: cpnvergence issues for 3rd quartile exposure def when leaving out 2007 flood. Not an issue as we focus on 0 quartile primary exposure def)

# 2002
flood_area_percent_loo_2002 <- readRDS(here("data/final", "flood_area_percent_loo_2002"))
saveRDS(flood_area_percent_loo_2002,
        file = here("data/final", "flood_area_percent"))
# 
# # 2003
# flood_area_percent_loo_2003 <- readRDS(here("data/final", "flood_area_percent_loo_2003"))
# saveRDS(flood_area_percent_loo_2003,
#         file = here("data/final", "flood_area_percent"))
# 
# # 2004
# flood_area_percent_loo_2004 <- readRDS(here("data/final", "flood_area_percent_loo_2004"))
# saveRDS(flood_area_percent_loo_2004,
#         file = here("data/final", "flood_area_percent"))
# 
# # 2007
# flood_area_percent_loo_2007 <- readRDS(here("data/final", "flood_area_percent_loo_2007"))
# saveRDS(flood_area_percent_loo_2007,
#         file = here("data/final", "flood_area_percent"))
# 
# # 2010
# flood_area_percent_loo_2010 <- readRDS(here("data/final", "flood_area_percent_loo_2010"))
# saveRDS(flood_area_percent_loo_2010,
#         file = here("data/final", "flood_area_percent"))
# 
# # 2010b
# flood_area_percent_loo_2010b <- readRDS(here("data/final", "flood_area_percent_loo_2010b"))
# saveRDS(flood_area_percent_loo_2010b,
#         file = here("data/final", "flood_area_percent"))
# 
# # 2017
# flood_area_percent_loo_2017 <- readRDS(here("data/final", "flood_area_percent_loo_2017"))
# saveRDS(flood_area_percent_loo_2017,
#         file = here("data/final", "flood_area_percent"))


#-------------------------------------------------------------------------------
# after picking a LOO scenario (above), re-run the data processing and analysis
# to overwrite the main workflow

source(here("child-mortality-dhs/R/1-dm", "2-format-birth-records.R"))
source(here("child-mortality-dhs/R/1-dm", "3-clean-birth-records.R"))
source(here("child-mortality-dhs/R/1-dm", "4-define-flooded-exposure.R"))
source(here("child-mortality-dhs/R/1-dm", "5-append-dhs-spatial-covariates.R"))
source(here("child-mortality-dhs/R/1-dm", "6-clean-mother-records.R"))
source(here("child-mortality-dhs/R/1-dm", "7-mother-matching.R"))
source(here("child-mortality-dhs/R/1-dm", "8-finalize-birth-records.R"))
source(here("child-mortality-dhs/R/1-dm", "9-format-time-series.R"))

source(here("child-mortality-dhs/R/2-analysis", "6a-infant-mortality-RR-empiric.R"))
source(here("child-mortality-dhs/R/2-analysis", "7-meta-analyses.R"))

#-------------------------------------------------------------------------------
# Save LOO results (uncomment for each scenario in turn)

# 2002
# saveRDS(meta.analyses.results, file = here("data/final", "meta.analyses.results.loo.2002"))

# # 2003
# saveRDS(meta.analyses.results, file = here("data/final", "meta.analyses.results.loo.2003"))
# 
# # 2004
# saveRDS(meta.analyses.results, file = here("data/final", "meta.analyses.results.loo.2004"))
# 
# # 2007
# saveRDS(meta.analyses.results, file = here("data/final", "meta.analyses.results.loo.2007"))
# 
# # 2010
# saveRDS(meta.analyses.results, file = here("data/final", "meta.analyses.results.loo.2010"))
# 
# # 2010b
# saveRDS(meta.analyses.results, file = here("data/final", "meta.analyses.results.loo.2010b"))
# 
# # 2017
# saveRDS(meta.analyses.results, file = here("data/final", "meta.analyses.results.loo.2017"))
# 


#-------------------------------------------------------------------------------
# after running each scenario above,  
#
# 1. RE-RUN the main analysis data processing, i.e., bash floods-bangladesh-run-all-dm.sh &
#
# 2. RE-RUN 6a-infant-mortality-RR-empiric.R and 7-meta-analyses.R
#
# 3. THEN create a summary plot by running code below

#-----------------------------------------
### Plot leave one out cross validation
# Load LOO results, created above
# and the main results, created by
# 7-meta-analyses.R
#-----------------------------------------
meta.analyses.results <- readRDS(file = here("data/final", "meta.analyses.results"))
meta.analyses.results.loo.2002 <- readRDS(file = here("data/final", "meta.analyses.results.loo.2002"))
meta.analyses.results.loo.2003 <- readRDS(file = here("data/final", "meta.analyses.results.loo.2003"))
meta.analyses.results.loo.2004 <- readRDS(file = here("data/final", "meta.analyses.results.loo.2004"))
meta.analyses.results.loo.2007 <- readRDS(file = here("data/final", "meta.analyses.results.loo.2007"))
meta.analyses.results.loo.2010 <- readRDS(file = here("data/final", "meta.analyses.results.loo.2010"))
meta.analyses.results.loo.2010b <- readRDS(file = here("data/final", "meta.analyses.results.loo.2010b"))
meta.analyses.results.loo.2017 <- readRDS(file = here("data/final", "meta.analyses.results.loo.2017"))

# Prepare data for plotting
data <- (rbind(meta.analyses.results %>% mutate(`Leave one out` = "None"),
               meta.analyses.results.loo.2002 %>% mutate(`Leave one out` = "2002"),
               meta.analyses.results.loo.2003 %>% mutate(`Leave one out` = "2003"),
               meta.analyses.results.loo.2004 %>% mutate(`Leave one out` = "2004"),
               meta.analyses.results.loo.2007 %>% mutate(`Leave one out` = "2007"),
               meta.analyses.results.loo.2010 %>% mutate(`Leave one out` = "2010 jul"),
               meta.analyses.results.loo.2010b %>% mutate(`Leave one out` = "2010 oct"),
               meta.analyses.results.loo.2017 %>% mutate(`Leave one out` = "2017"),
               meta.analyses.results.loo.2017 %>% mutate(`Leave one out` = "fake1"), # Add these 2 fakes for nicer grouping plot in position_dodge
               meta.analyses.results.loo.2017 %>% mutate(`Leave one out` = "fake2")
)
%>% mutate(Season = factor(Season, levels = c("Overall", "Rainy", "Dry")),
           Decade = factor(Decade, levels = c("1988-2017", "1988-1997", "1998-2007", "2008-2017"))
) 
%>% mutate(facet = (!Decade == "1988-2017"),
           `Leave one out` = factor(`Leave one out`,
                                    levels = c("fake1", "2002", "2003", "2004", "None", "2007", "2010 jul", "2010 oct", "2017", "fake2"))
)
)

## Risk difference
data_plot_RD_loo <- (data %>%
                       filter(Exposure == "0 quartile") %>%
                       filter(Estimate == "RD_empiric") %>%
                       ggplot(aes(x = Decade, y = 1000*TE, ymin = 1000*TE.lower, ymax = 1000*TE.upper,
                                  col = Season,
                                  shape = `Leave one out`,
                                  alpha = `Leave one out`)) +
                       geom_pointrange(cex = 0.3,
                                       position = position_dodge(1)) +
                       geom_hline(aes(yintercept = 0), lty = "dashed") +
                       scale_y_continuous(breaks = seq(from = -10, to = 30, by = 5)) + 
                       facet_grid( ~ facet, scales = "free_x", space = "free_x") +
                       scale_shape_manual(values = c("2002" = 2, "2003" = 2, "2004" = 2, "None" = 19, "2007" = 2, "2010 jul" = 2, "2010 oct" = 2, "2017" = 2),
                                          breaks = c("2002", "None"),
                                          labels = c("Yes", "No")) +
                       scale_alpha_manual(values = c("2002" = 0.5, "2003" = 0.5, "2004" = 0.5, "None" = 1, "2007" = 0.5, "2010 jul" = 0.5, "2010 oct" = 0.5, "2017" = 0.5),
                                          breaks = c("2002", "None"),
                                          labels = c("Yes", "No"),
                                          guide = "none") +
                       scale_color_manual(labels = c("Overall", "Rainy", "Dry"),
                                          values = c("black", "#56B4E9", "#E69F00")) +
                       ylab("Risk difference\n(per 1000 births)") +
                       theme(legend.position = "top",
                             legend.box = "vertical",
                             panel.grid.major.x = element_blank(),
                             strip.background = element_blank(),
                             strip.text.x = element_blank(),
                             axis.ticks = element_blank(),
                             axis.title = element_text(size = 16),
                             axis.text = element_text(size = 14),
                             legend.title = element_text(size = 14),
                             legend.text = element_text(size = 12),
                             axis.title.x = element_blank())
)


data_plot_RD_loo

### Save the figure
ggsave(here("child-mortality-dhs/output/figures", "forest-plot-RD-loo-cv.pdf"),
       data_plot_RD_loo,
       width = 360, height = 180, units = "mm"
       )


