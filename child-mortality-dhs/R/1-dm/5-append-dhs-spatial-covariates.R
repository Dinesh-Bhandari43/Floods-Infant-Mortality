#-------------------------------------------------------------------------------

# @Organization - UCSF
# @Project - Effects of floods on child mortality in Bangladesh
# @Author - Francois Rerolle, rerollefrancois@gmail.com
# @Description - This code appends spatial covariates made available by DHS

#-------------------------------------------------------------------------------

# Source configuration code
library(here)
source(here("child-mortality-dhs/R", "0-config.R"))

#-------------------------------------------------------------------------------

## Load data
birth_records_flooded <- readRDS(file = here("data/final",
                                             "birth_records_flooded"))

BDGC4JFL <- read.csv(here("data/untouched/dhs/BD_2004_DHS_02032022_1033_172978/BDGC4JFL/BDGC4JFL.csv"))
BDGC7RFL <- read.csv(here("data/untouched/dhs/BD_2017-18_DHS_02082022_855_172978/BDGC7RFL/BDGC7RFL.csv"))
BDGC42FL <- read.csv(here("data/untouched/dhs/BD_1999-00_DHS_03072022_1129_172978/BDGC42FL/BDGC42FL.csv"))
BDGC52FL <- read.csv(here("data/untouched/dhs/BD_2007_DHS_02032022_1033_172978/BDGC52FL/BDGC52FL.csv"))
BDGC62FL <- read.csv(here("data/untouched/dhs/BD_2011_DHS_02032022_1032_172978/BDGC62FL/BDGC62FL.csv"))
BDGC72FL <- read.csv(here("data/untouched/dhs/BD_2014_DHS_02082022_855_172978/BDGC72FL/BDGC72FL.csv"))

#-------------------------------------------------------------------------------

## Process data
# Append spatial covariates together
# common variables (2018 survey wave is missing a few variables and includes more recent ones as well)
BDGC7RFL <- (BDGC7RFL %>% mutate(UN_Population_Density_2015 = replace(UN_Population_Density_2015, UN_Population_Density_2015 == 0, NA)))
common_variables <- intersect(colnames(BDGC7RFL), colnames(BDGC4JFL))
BDGFL <- rbind(BDGC4JFL[,common_variables],
               BDGC7RFL[,common_variables],
               BDGC42FL[,common_variables],
               BDGC52FL[,common_variables],
               BDGC62FL[,common_variables],
               BDGC72FL[,common_variables])

# Handle missing values
BDGFL[BDGFL == -9999] <- NA

# Merge
birth_records_flooded_spatial <- (birth_records_flooded %>%
                                    left_join(BDGFL))

#-------------------------------------------------------------------------------

# Save dataset
saveRDS(birth_records_flooded_spatial,
        file = here("data/final", "birth_records_flooded_spatial"))
readr::write_csv(birth_records_flooded_spatial,
                 file = here("data/final", "birth_records_flooded_spatial.csv"))

#-------------------------------------------------------------------------------