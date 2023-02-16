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
flood_area_percent_loo_2007 <- readRDS(here("data/final", "flood_area_percent_loo_2007"))
saveRDS(flood_area_percent_loo_2007,
        file = here("data/final", "flood_area_percent"))

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

# Save main results
colnames(meta.analyses.results.loo.2007)[1:3] <- c("TE", "TE.lower", "TE.upper")
saveRDS(meta.analyses.results, file = here("data/final", "meta.analyses.results.loo.2007"))


