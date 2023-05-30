#-------------------------------------------------------------------------------

# @Organization - UCSF
# @Project - Effects of floods on child mortality in Bangladesh
# @Author - Francois Rerolle, rerollefrancois@gmail.com
# @Description - This code runs all data management scripts (except precipitation extraction which is kept seperated)

#-------------------------------------------------------------------------------

# Source configuration code
library(here)
source(here("child-mortality-dhs/R", "0-config.R"))

#-------------------------------------------------------------------------------

source(here("child-mortality-dhs/R/1-dm", "1-create-flood-prone-area-exposure.R"))
source(here("child-mortality-dhs/R/1-dm", "2-format-birth-records.R"))
source(here("child-mortality-dhs/R/1-dm", "3-clean-birth-records.R"))
source(here("child-mortality-dhs/R/1-dm", "4-define-flooded-exposure.R"))
source(here("child-mortality-dhs/R/1-dm", "5-append-dhs-spatial-covariates.R"))
source(here("child-mortality-dhs/R/1-dm", "6-clean-mother-records.R"))
source(here("child-mortality-dhs/R/1-dm", "7-mother-matching.R"))
source(here("child-mortality-dhs/R/1-dm", "8-finalize-birth-records.R"))
source(here("child-mortality-dhs/R/1-dm", "9-format-time-series.R"))