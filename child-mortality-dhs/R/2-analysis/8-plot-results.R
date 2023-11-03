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
# Load saved results
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

#-----------------------------------------
# list results for point estimates and 
# appendices
#-----------------------------------------
print(results_RD %>% dplyr::select(Decade,Season,starts_with("TE")) %>% arrange(Decade,Season), digits=2)

print(results_RR %>% dplyr::select(Decade,Season,starts_with("TE")) %>% arrange(Decade,Season), digits=3)

#-----------------------------------------
# Plot Risk differences
#-----------------------------------------

# main analysis, ever flooded
data_plot_RD <- (data %>%
                   filter(Exposure == "0 quartile") %>%
                filter(Estimate == "RD_empiric") %>%
                ggplot(aes(x = Decade, y = 1000*TE, ymin = 1000*TE.lower, ymax = 1000*TE.upper, col = Season)) +
                geom_pointrange(cex = 0.5,
                                position = position_dodge(0.3)) +
                geom_hline(aes(yintercept = 0), lty = "dashed") +
                scale_y_continuous(breaks = seq(from = -10, to = 30, by = 5)) + 
                facet_grid( ~ facet, scales = "free_x", space = "free_x") +
                scale_color_manual(labels = c("Overall", "Rainy", "Dry"),
                                   values = c("black", "#56B4E9", "#E69F00")) +
                ylab("Risk difference\n(per 1000 births)") +
                  coord_cartesian(ylim=c(-10,28)) +
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

# quantiles of days flooded
data_plot_RD_exposure <- (data %>%
                            filter(Exposure %in% c("0 quartile", "1 quartile", "2 quartile")) %>%
                   filter(Estimate == "RD_empiric") %>%
                   ggplot(aes(x = Decade, y = 1000*TE, ymin = 1000*TE.lower, ymax = 1000*TE.upper, col = Season, shape = Exposure)) +
                   geom_pointrange(cex = 0.5,
                                   position = position_dodge(0.5)) +
                   geom_hline(aes(yintercept = 0), lty = "dashed") +
                   scale_y_continuous(breaks = seq(from = -20, to = 50, by = 5)) + 
                   facet_grid( ~ facet, scales = "free_x", space = "free_x") +
                   scale_color_manual(labels = c("Overall", "Rainy", "Dry"),
                                      values = c("black", "#56B4E9", "#E69F00")) +
                     scale_shape_discrete(name = "Percent of flooded\ndays across 7 events\n(maximum of 206 days)",
                                          labels = c("> 0% (0 days)", "> 2.4% (5 days)", "> 9.2% (19 days)")) +
                     ylab("Risk difference\n(per 1000 births)") +
                     coord_cartesian(ylim=c(-20,50)) +
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

#frequency of flooding
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
                            scale_y_continuous(breaks = seq(from = -20, to = 50, by = 5)) + 
                            facet_grid( ~ facet, scales = "free_x", space = "free_x") +
                            scale_color_manual(labels = c("Overall", "Rainy", "Dry"),
                                               values = c("black", "#56B4E9", "#E69F00")) +
                             scale_shape_discrete(name = "Flooded during\nat least",
                                                  breaks = c("1 frequency", "2 frequency", "3 frequency", "4 frequency"),
                                                labels = c("1 event", "2 events",
                                                           "3 events", "4 events")) +
                            ylab("Risk difference\n(per 1000 births)") +
                             coord_cartesian(ylim=c(-20,50)) +
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

#-----------------------------------------
# Plot Risk ratios
#-----------------------------------------

# main analysis, ever flooded
data_plot_RR <- (data %>%
                   filter(Estimate == "RR_log_empiric") %>%
                   filter(Exposure == "0 quartile") %>%
                   ggplot(aes(x = Decade, y = exp(TE), ymin = exp(TE.lower), ymax = exp(TE.upper), col = Season)) +
                   geom_hline(aes(yintercept = 1), lty = "dashed") +
                   geom_pointrange(cex = 0.5,
                                   position = position_dodge(0.3)) +
                   scale_y_continuous(breaks = seq(from = 0.8, to = 1.8, by = 0.1), trans="log") +
                   facet_grid( ~ facet, scales = "free_x", space = "free_x") +
                   scale_color_manual(labels = c("Overall", "Rainy", "Dry"),
                                      values = c("black", "#56B4E9", "#E69F00")) +
                   ylab("Risk ratio") +
                   theme(legend.position = "top",
                         panel.grid.major.x = element_blank(),
                         panel.grid.minor.y = element_blank(),
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

# Odds ratio comparing cluster level analysis with conditional logistic regression 
# conditioned on mother matched pairs
data_plot_OR <- (data %>%
                   filter(Estimate %in% c("OR_log_empiric", "OR_log")) %>%
                   filter(Exposure == "0 quartile") %>%
                   ggplot(aes(x = Decade, y = exp(TE), ymin = exp(TE.lower), ymax = exp(TE.upper), col = Season, shape = Estimate)) +
                   geom_pointrange(cex = 0.5,
                                   position = position_dodge(0.3)) +
                   geom_hline(aes(yintercept = 1), lty = "dashed") +
                   scale_y_continuous(breaks = seq(from = 0.8, to = 1.8, by = 0.1), trans="log") +
                   facet_grid( ~ facet, scales = "free_x", space = "free_x") +
                   scale_color_manual(labels = c("Overall", "Rainy", "Dry"),
                                      values = c("black", "#56B4E9", "#E69F00")) +
                   scale_shape_discrete(name = "Analytical method",
                                        labels = c("Conditional logistic\nregression",
                                                   "Cluster matched\ncohort")) +
                   ylab("Odds ratio") +
                   theme(legend.position = "right",
                         panel.grid.major.x = element_blank(),
                         panel.grid.minor.y = element_blank(),
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

#-----------------------------------------
### Save Plots as PDF
#-----------------------------------------

# Figure 2 - risk differences
ggsave(filename = here("child-mortality-dhs/output/figures", "figure-2-risk-differences.pdf"),
       data_plot_RD,
       device = "pdf",
       width = 180, height = 180, units = "mm"
       )

# Figure A3 - risk ratios
ggsave(filename = here("child-mortality-dhs/output/figures", "figure-A3-forest-plot-RR.pdf"),
       data_plot_RR,
       device = "pdf",
       width = 180, height = 180, units = "mm"
)

# Figure A8 - sensitivity analysis by % days flooded
ggsave(filename = here("child-mortality-dhs/output/figures", "figure-A8-forest-plot-RD_exposure.pdf"),
       data_plot_RD_exposure,
       device = "pdf",
       width = 360, height = 180, units = "mm"
)


# Figure A9 - sensitivity analysis by number of times location flooded
ggsave(filename = here("child-mortality-dhs/output/figures", "figure-A9-forest-plot-RD_frequency.pdf"),
       data_plot_RD_frequency,
       device = "pdf",
       width = 360, height = 180, units = "mm"
)



# Figure A11 - sensitivity analysis cohort and conditional logistic estimators
ggsave(filename = here("child-mortality-dhs/output/figures", "figure-A11-forest-plot-OR.pdf"),
       data_plot_OR,
       device = "pdf",
       width = 270, height = 180, units = "mm"
)





