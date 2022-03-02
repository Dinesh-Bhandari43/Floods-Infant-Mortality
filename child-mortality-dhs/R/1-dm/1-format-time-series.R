#-------------------------------------------------------------------------------

# @Organization - UCSF
# @Project - Effects of floods on child mortality in Bangladesh
# @Author - Francois Rerolle, rerollefrancois@gmail.com
# @Description - This formats the time series from the birth record history

#-------------------------------------------------------------------------------

# Source configuration code
library(here)
source(here("child-mortality-dhs/R", "0-config.R"))

#-------------------------------------------------------------------------------

## Load data
birth_records <- readRDS(file = here("data/final", "birth_records"))

#-------------------------------------------------------------------------------

## Process data
# Aggregate birth records data at monthly temporal resolution
# Monthly
monthly_time_series <- 
  (birth_records %>%
     group_by(Birth_Date_Month_CMC, Flooded) %>%
     summarise(Number_Of_Birth = n(),
               Number_Of_Dead_Birth = sum(Age_At_Death_Months <= 0,
                                          na.rm = T)) %>%
     group_by(Flooded) %>%
     mutate(Number_Of_Birth_QS = lag(Number_Of_Birth) + Number_Of_Birth + lead(Number_Of_Birth),
            Number_Of_Dead_Birth_QS = lag(Number_Of_Dead_Birth) + Number_Of_Dead_Birth + lead(Number_Of_Dead_Birth),
            Infant_Mortality = Number_Of_Dead_Birth/Number_Of_Birth,
            Infant_Mortality_Quarterly_Smoothed = Number_Of_Dead_Birth_QS/Number_Of_Birth_QS) %>%
     mutate(Post_2004 = ifelse(Birth_Date_Month_CMC >= 1256, 1, 0), # Aug 2004
            Post_2007 = ifelse(Birth_Date_Month_CMC >= 1292, 1, 0)) # Aug 2007
)

#-------------------------------------------------------------------------------

# Save dataset
saveRDS(monthly_time_series, file = here("data/final", "monthly_time_series"))
readr::write_csv(monthly_time_series, file = here("data/final", "monthly_time_series.csv"))

#-------------------------------------------------------------------------------