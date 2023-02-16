#-------------------------------------------------------------------------------

# @Organization - UCSF
# @Project - Effects of floods on child mortality in Bangladesh
# @Author - Francois Rerolle, rerollefrancois@gmail.com
# @Description - This code plots the results from the meta-analysis

#-------------------------------------------------------------------------------

# Source configuration code
library(here)
source(here("child-mortality-dhs/R", "0-config.R"))

#-------------------------------------------------------------------------------
# Load data
meta.analyses.results <- readRDS(file = here("data/final", "meta.analyses.results"))

# Prepare data for plotting
data <- (meta.analyses.results %>%
           mutate(Season = factor(Season, levels = c("Overall", "Rainy", "Dry")),
                  Decade = factor(Decade, levels = c("1988-2017", "1988-1997", "1998-2007", "2008-2017"))
           ) %>%
           mutate(facet = (!Decade == "1988-2017"))
         )

results_RD <- (data %>%
                 filter(Exposure == "0 quartile") %>%
                 filter(Estimate == "RD_empiric") %>%
                 mutate(TE = 1000*TE,
                        TE.lower = 1000*TE.lower,
                        TE.upper = 1000*TE.upper))

results_RR <- (data %>%
                 filter(Exposure == "0 quartile") %>%
                 filter(Estimate == "RR_log_empiric") %>%
                 mutate(TE = exp(TE),
                        TE.lower = exp(TE.lower),
                        TE.upper = exp(TE.upper)))


## Risk difference
data_plot_RD <- (data %>%
                   filter(Exposure == "0 quartile") %>%
                filter(Estimate == "RD_empiric") %>%
                ggplot(aes(x = Decade, y = 1000*TE, ymin = 1000*TE.lower, ymax = 1000*TE.upper, col = Season)) +
                geom_pointrange(cex = 0.5,
                                position = position_dodge(0.3)) +
                geom_hline(aes(yintercept = 0), lty = "dashed") +
                scale_y_continuous(breaks = seq(from = -10, to = 20, by = 5)) + 
                facet_grid( ~ facet, scales = "free_x", space = "free_x") +
                scale_color_manual(labels = c("Overall", "Rainy", "Dry"),
                                   values = c("black", "#56B4E9", "#E69F00")) +
                ylab("Risk difference\n(per 1000 births)") +
                theme(legend.position = "top",
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


data_plot_RD

data_plot_RD_exposure <- (data %>%
                            filter(Exposure %in% c("0 quartile", "1 quartile", "2 quartile")) %>%
                   filter(Estimate == "RD_empiric") %>%
                   ggplot(aes(x = Decade, y = 1000*TE, ymin = 1000*TE.lower, ymax = 1000*TE.upper, col = Season, shape = Exposure)) +
                   geom_pointrange(cex = 0.5,
                                   position = position_dodge(0.5)) +
                   geom_hline(aes(yintercept = 0), lty = "dashed") +
                   scale_y_continuous(breaks = seq(from = -10, to = 40, by = 5)) + 
                   facet_grid( ~ facet, scales = "free_x", space = "free_x") +
                   scale_color_manual(labels = c("Overall", "Rainy", "Dry"),
                                      values = c("black", "#56B4E9", "#E69F00")) +
                     scale_shape_discrete(name = "Percent number flooded\ndays across 7 events\n(maximum of 206 days)",
                                          labels = c("> 0% (0 days)", "> 2.4% (5 days)", "> 9.2% (19 days)")) +
                     ylab("Risk difference\n(per 1000 births)") +
                   theme(legend.position = "right",
                         panel.grid.major.x = element_blank(),
                         strip.background = element_blank(),
                         strip.text.x = element_blank(),
                         axis.ticks = element_blank(),
                         axis.title = element_text(size = 16),
                         axis.text = element_text(size = 12),
                         legend.title = element_text(size = 12),
                         legend.text = element_text(size = 10),
                         axis.title.x = element_blank())
)


data_plot_RD_exposure

#frequency
data_plot_RD_frequency <- (data %>%
                            filter(Exposure %in% c("1 frequency",
                                                   "2 frequency",
                                                   "3 frequency",
                                                   "4 frequency"
                                                   # "5 frequency"
                                                   # ,"6 frequency"
                                                   # ,"7 frequency"
                                                   )) %>%
                            filter(Estimate == "RD_empiric") %>%
                            ggplot(aes(x = Decade, y = 1000*TE, ymin = 1000*TE.lower, ymax = 1000*TE.upper, col = Season, shape = Exposure)) +
                            geom_pointrange(cex = 0.5,
                                            position = position_dodge(0.5)) +
                            geom_hline(aes(yintercept = 0), lty = "dashed") +
                            scale_y_continuous(breaks = seq(from = -10, to = 40, by = 5)) + 
                            facet_grid( ~ facet, scales = "free_x", space = "free_x") +
                            scale_color_manual(labels = c("Overall", "Rainy", "Dry"),
                                               values = c("black", "#56B4E9", "#E69F00")) +
                             scale_shape_discrete(name = "Flooded during\nat least",
                                                  breaks = c("1 frequency", "2 frequency", "3 frequency", "4 frequency"),
                                                labels = c("1 event", "2 events",
                                                           "3 events", "4 events")) +
                            ylab("Risk difference\n(per 1000 births)") +
                            # scale_shape_discrete(name = "Exposure definition\nthreshold",
                            #                      labels = c("Minimun", "First quartile", "Median")) +
                            theme(legend.position = "right",
                                  panel.grid.major.x = element_blank(),
                                  strip.background = element_blank(),
                                  strip.text.x = element_blank(),
                                  axis.ticks = element_blank(),
                                  axis.title = element_text(size = 16),
                                  axis.text = element_text(size = 12),
                                  legend.title = element_text(size = 12),
                                  legend.text = element_text(size = 10),
                                  axis.title.x = element_blank())
)


data_plot_RD_frequency

# Risk ratio
data_plot_RR <- (data %>%
                   filter(Estimate == "RR_log_empiric") %>%
                   filter(Exposure == "0 quartile") %>%
                   ggplot(aes(x = Decade, y = exp(TE), ymin = exp(TE.lower), ymax = exp(TE.upper), col = Season)) +
                   geom_pointrange(cex = 0.5,
                                   position = position_dodge(0.3)) +
                   geom_hline(aes(yintercept = 1), lty = "dashed") +
                   # scale_y_continuous(breaks = seq(from = -10, to = 20, by = 5)) + 
                   facet_grid( ~ facet, scales = "free_x", space = "free_x") +
                   scale_color_manual(labels = c("Overall", "Rainy", "Dry"),
                                      values = c("black", "#56B4E9", "#E69F00")) +
                   ylab("Risk ratio") +
                   theme(legend.position = "right",
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


data_plot_RR

# Odds ratio
data_plot_OR <- (data %>%
                   filter(Estimate %in% c("OR_log_empiric", "OR_log")) %>%
                   filter(Exposure == "0 quartile") %>%
                   ggplot(aes(x = Decade, y = exp(TE), ymin = exp(TE.lower), ymax = exp(TE.upper), col = Season, shape = Estimate)) +
                   geom_pointrange(cex = 0.5,
                                   position = position_dodge(0.3)) +
                   geom_hline(aes(yintercept = 1), lty = "dashed") +
                   scale_y_continuous(breaks = seq(from = 0.8, to = 1.8, by = 0.2)) +
                   facet_grid( ~ facet, scales = "free_x", space = "free_x") +
                   scale_color_manual(labels = c("Overall", "Rainy", "Dry"),
                                      values = c("black", "#56B4E9", "#E69F00")) +
                   scale_shape_discrete(name = "Analytical method",
                                        labels = c("Conditional logistic\nregression",
                                                   "Cluster matched\ncohort")) +
                   ylab("Odds ratio") +
                   theme(legend.position = "right",
                         panel.grid.major.x = element_blank(),
                         strip.background = element_blank(),
                         strip.text.x = element_blank(),
                         axis.ticks = element_blank(),
                         axis.title = element_text(size = 16),
                         axis.text = element_text(size = 12),
                         legend.title = element_text(size = 12),
                         legend.text = element_text(size = 10),
                         axis.title.x = element_blank())
)


data_plot_OR

### Save
pdf(here("child-mortality-dhs/output/figures", "forest-plot-RD.pdf"))
data_plot_RD
dev.off()

pdf(here("child-mortality-dhs/output/figures", "forest-plot-RD_exposure.pdf"))
data_plot_RD_exposure
dev.off()

pdf(here("child-mortality-dhs/output/figures", "forest-plot-RD_frequency.pdf"))
data_plot_RD_frequency
dev.off()

pdf(here("child-mortality-dhs/output/figures", "forest-plot-RR.pdf"))
data_plot_RR
dev.off()

pdf(here("child-mortality-dhs/output/figures", "forest-plot-OR.pdf"))
data_plot_OR
dev.off()


### Plot for PNAS suggested leave one out cross validation
# Load data
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
data_plot_RD <- (data %>%
                   filter(Exposure == "0 quartile") %>%
                   filter(Estimate == "RD_empiric") %>%
                   ggplot(aes(x = Decade, y = 1000*TE, ymin = 1000*TE.lower, ymax = 1000*TE.upper,
                              col = Season,
                              shape = `Leave one out`,
                              alpha = `Leave one out`)) +
                   geom_pointrange(cex = 0.3,
                                   position = position_dodge(1)) +
                   geom_hline(aes(yintercept = 0), lty = "dashed") +
                   scale_y_continuous(breaks = seq(from = -10, to = 25, by = 5)) + 
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


data_plot_RD

### Save
pdf(here("child-mortality-dhs/output/figures", "forest-plot-RD-loo-cv.pdf"))
data_plot_RD
dev.off()

