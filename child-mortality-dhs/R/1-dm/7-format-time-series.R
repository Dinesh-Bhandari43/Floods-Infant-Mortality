#-------------------------------------------------------------------------------

# @Organization - UCSF
# @Project - Effects of floods on child mortality in Bangladesh
# @Author - Francois Rerolle, rerollefrancois@gmail.com
# @Description - This code formats the time series from the birth record history

#-------------------------------------------------------------------------------

# Source configuration code
library(here)
source(here("child-mortality-dhs/R", "0-config.R"))

#-------------------------------------------------------------------------------

## Load data
birth_records_flooded_0_quartile <- readRDS(file = here("data/final", "birth_records_flooded_0_quartile"))
birth_records_flooded_1_quartile <- readRDS(file = here("data/final", "birth_records_flooded_1_quartile"))
birth_records_flooded_2_quartile <- readRDS(file = here("data/final", "birth_records_flooded_2_quartile"))
birth_records_flooded_3_quartile <- readRDS(file = here("data/final", "birth_records_flooded_3_quartile"))

#-------------------------------------------------------------------------------

## Process data
# Aggregate birth records data at monthly temporal resolution
monthly_time_series_0_quartile <- 
  (birth_records_flooded_0_quartile %>%
     group_by(Birth_Date_Month_CMC, Flooded) %>%
     summarise(Number_Of_Birth = n(),
               Number_Of_Dead_Birth = sum(Age_At_Death_Months <= 1,
                                          na.rm = T)) %>%
     mutate(Infant_Mortality = Number_Of_Dead_Birth/Number_Of_Birth) %>%
     ungroup()
)

monthly_time_series_1_quartile <- 
  (birth_records_flooded_1_quartile %>%
     group_by(Birth_Date_Month_CMC, Flooded) %>%
     summarise(Number_Of_Birth = n(),
               Number_Of_Dead_Birth = sum(Age_At_Death_Months <= 1,
                                          na.rm = T)) %>%
     mutate(Infant_Mortality = Number_Of_Dead_Birth/Number_Of_Birth) %>%
     ungroup()
  )

monthly_time_series_2_quartile <- 
  (birth_records_flooded_2_quartile %>%
     group_by(Birth_Date_Month_CMC, Flooded) %>%
     summarise(Number_Of_Birth = n(),
               Number_Of_Dead_Birth = sum(Age_At_Death_Months <= 1,
                                          na.rm = T)) %>%
     mutate(Infant_Mortality = Number_Of_Dead_Birth/Number_Of_Birth) %>%
     ungroup()
  )

monthly_time_series_3_quartile <- 
  (birth_records_flooded_3_quartile %>%
     group_by(Birth_Date_Month_CMC, Flooded) %>%
     summarise(Number_Of_Birth = n(),
               Number_Of_Dead_Birth = sum(Age_At_Death_Months <= 1,
                                          na.rm = T)) %>%
     mutate(Infant_Mortality = Number_Of_Dead_Birth/Number_Of_Birth) %>%
     ungroup()
  )

#-------------------------------------------------------------------------------

# Save dataset
saveRDS(monthly_time_series_0_quartile,
        file = here("data/final", "monthly_time_series_0_quartile"))

saveRDS(monthly_time_series_1_quartile,
        file = here("data/final", "monthly_time_series_1_quartile"))

saveRDS(monthly_time_series_2_quartile,
        file = here("data/final", "monthly_time_series_2_quartile"))

saveRDS(monthly_time_series_3_quartile,
        file = here("data/final", "monthly_time_series_3_quartile"))

#-------------------------------------------------------------------------------