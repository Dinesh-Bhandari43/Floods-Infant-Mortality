#-------------------------------------------------------------------------------

# @Organization - UCSF
# @Project - Effects of floods on child mortality in Bangladesh
# @Author - Francois Rerolle, rerollefrancois@gmail.com
# @Description - This code asses covariates balance after matching

#-------------------------------------------------------------------------------

# Source configuration code
library(here)
source(here("child-mortality-dhs/R", "0-config.R"))

m.out0 <- readRDS(file = here("child-mortality-dhs/output/matching", "matching_0_quartile"))

plot(summary(m.out0))
summary(m.out0)
