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
birth_records_flooded_control <- readRDS(file = here("data/final", "birth_records_flooded_control"))

birth_records_flooded_1_frequency <- readRDS(file = here("data/final", "birth_records_flooded_1_frequency"))
birth_records_flooded_2_frequency <- readRDS(file = here("data/final", "birth_records_flooded_2_frequency"))
birth_records_flooded_3_frequency <- readRDS(file = here("data/final", "birth_records_flooded_3_frequency"))
birth_records_flooded_4_frequency <- readRDS(file = here("data/final", "birth_records_flooded_4_frequency"))
birth_records_flooded_5_frequency <- readRDS(file = here("data/final", "birth_records_flooded_5_frequency"))
birth_records_flooded_6_frequency <- readRDS(file = here("data/final", "birth_records_flooded_6_frequency"))
birth_records_flooded_7_frequency <- readRDS(file = here("data/final", "birth_records_flooded_7_frequency"))

birth_records_flooded_year <- readRDS(file = here("data/final", "birth_records_flooded_year"))

#-------------------------------------------------------------------------------

## Process data
# Aggregate birth records data at monthly temporal resolution
monthly_time_series_0_quartile <- 
  (birth_records_flooded_0_quartile %>%
     mutate(index = 1) %>%
     group_by(Birth_Date_Month_CMC, Flooded) %>%
     summarise(Number_Of_Birth = sum(index*Women_Sampling_Weight),
               Number_Of_Dead_Birth = sum((Age_At_Death_Months <= 11)*Women_Sampling_Weight,
                                          na.rm = T)) %>%
     mutate(Infant_Mortality = Number_Of_Dead_Birth/Number_Of_Birth) %>%
     ungroup()
)

monthly_time_series_1_quartile <- 
  (birth_records_flooded_1_quartile %>%
     mutate(index = 1) %>%
     group_by(Birth_Date_Month_CMC, Flooded) %>%
     summarise(Number_Of_Birth = sum(index*Women_Sampling_Weight),
               Number_Of_Dead_Birth = sum((Age_At_Death_Months <= 11)*Women_Sampling_Weight,
                                          na.rm = T)) %>%
     mutate(Infant_Mortality = Number_Of_Dead_Birth/Number_Of_Birth) %>%
     ungroup()
  )

monthly_time_series_2_quartile <- 
  (birth_records_flooded_2_quartile %>%
     mutate(index = 1) %>%
     group_by(Birth_Date_Month_CMC, Flooded) %>%
     summarise(Number_Of_Birth = sum(index*Women_Sampling_Weight),
               Number_Of_Dead_Birth = sum((Age_At_Death_Months <= 11)*Women_Sampling_Weight,
                                          na.rm = T)) %>%
     mutate(Infant_Mortality = Number_Of_Dead_Birth/Number_Of_Birth) %>%
     ungroup()
  )

monthly_time_series_3_quartile <- 
  (birth_records_flooded_3_quartile %>%
     mutate(index = 1) %>%
     group_by(Birth_Date_Month_CMC, Flooded) %>%
     summarise(Number_Of_Birth = sum(index*Women_Sampling_Weight),
               Number_Of_Dead_Birth = sum((Age_At_Death_Months <= 11)*Women_Sampling_Weight,
                                          na.rm = T)) %>%
     mutate(Infant_Mortality = Number_Of_Dead_Birth/Number_Of_Birth) %>%
     ungroup()
  )

monthly_time_series_control <- 
  (birth_records_flooded_control %>%
     mutate(index = 1) %>%
     group_by(Birth_Date_Month_CMC, Flooded) %>%
     summarise(Number_Of_Birth = sum(index*Women_Sampling_Weight),
               Number_Of_Dead_Birth = sum((Age_At_Death_Months <= 11)*Women_Sampling_Weight,
                                          na.rm = T)) %>%
     mutate(Infant_Mortality = Number_Of_Dead_Birth/Number_Of_Birth) %>%
     ungroup()
  )

monthly_time_series_1_frequency <- 
  (birth_records_flooded_1_frequency %>%
     mutate(index = 1) %>%
     group_by(Birth_Date_Month_CMC, Flooded) %>%
     summarise(Number_Of_Birth = sum(index*Women_Sampling_Weight),
               Number_Of_Dead_Birth = sum((Age_At_Death_Months <= 11)*Women_Sampling_Weight,
                                          na.rm = T)) %>%
     mutate(Infant_Mortality = Number_Of_Dead_Birth/Number_Of_Birth) %>%
     ungroup()
  )

monthly_time_series_2_frequency <- 
  (birth_records_flooded_2_frequency %>%
     mutate(index = 1) %>%
     group_by(Birth_Date_Month_CMC, Flooded) %>%
     summarise(Number_Of_Birth = sum(index*Women_Sampling_Weight),
               Number_Of_Dead_Birth = sum((Age_At_Death_Months <= 11)*Women_Sampling_Weight,
                                          na.rm = T)) %>%
     mutate(Infant_Mortality = Number_Of_Dead_Birth/Number_Of_Birth) %>%
     ungroup()
  )

monthly_time_series_3_frequency <- 
  (birth_records_flooded_3_frequency %>%
     mutate(index = 1) %>%
     group_by(Birth_Date_Month_CMC, Flooded) %>%
     summarise(Number_Of_Birth = sum(index*Women_Sampling_Weight),
               Number_Of_Dead_Birth = sum((Age_At_Death_Months <= 11)*Women_Sampling_Weight,
                                          na.rm = T)) %>%
     mutate(Infant_Mortality = Number_Of_Dead_Birth/Number_Of_Birth) %>%
     ungroup()
  )

monthly_time_series_4_frequency <- 
  (birth_records_flooded_4_frequency %>%
     mutate(index = 1) %>%
     group_by(Birth_Date_Month_CMC, Flooded) %>%
     summarise(Number_Of_Birth = sum(index*Women_Sampling_Weight),
               Number_Of_Dead_Birth = sum((Age_At_Death_Months <= 11)*Women_Sampling_Weight,
                                          na.rm = T)) %>%
     mutate(Infant_Mortality = Number_Of_Dead_Birth/Number_Of_Birth) %>%
     ungroup()
  )

monthly_time_series_5_frequency <- 
  (birth_records_flooded_5_frequency %>%
     mutate(index = 1) %>%
     group_by(Birth_Date_Month_CMC, Flooded) %>%
     summarise(Number_Of_Birth = sum(index*Women_Sampling_Weight),
               Number_Of_Dead_Birth = sum((Age_At_Death_Months <= 11)*Women_Sampling_Weight,
                                          na.rm = T)) %>%
     mutate(Infant_Mortality = Number_Of_Dead_Birth/Number_Of_Birth) %>%
     ungroup()
  )

monthly_time_series_6_frequency <- 
  (birth_records_flooded_6_frequency %>%
     mutate(index = 1) %>%
     group_by(Birth_Date_Month_CMC, Flooded) %>%
     summarise(Number_Of_Birth = sum(index*Women_Sampling_Weight),
               Number_Of_Dead_Birth = sum((Age_At_Death_Months <= 11)*Women_Sampling_Weight,
                                          na.rm = T)) %>%
     mutate(Infant_Mortality = Number_Of_Dead_Birth/Number_Of_Birth) %>%
     ungroup()
  )

monthly_time_series_7_frequency <- 
  (birth_records_flooded_7_frequency %>%
     mutate(index = 1) %>%
     group_by(Birth_Date_Month_CMC, Flooded) %>%
     summarise(Number_Of_Birth = sum(index*Women_Sampling_Weight),
               Number_Of_Dead_Birth = sum((Age_At_Death_Months <= 11)*Women_Sampling_Weight,
                                          na.rm = T)) %>%
     mutate(Infant_Mortality = Number_Of_Dead_Birth/Number_Of_Birth) %>%
     ungroup()
  )

#PNAS revision 2
monthly_time_series_flood_year <- 
  (birth_records_flooded_year %>%
     mutate(index = 1) %>%
     group_by(Birth_Date_Month_CMC, Flooded) %>%
     summarise(Number_Of_Birth = sum(index*Women_Sampling_Weight),
               Number_Of_Dead_Birth = sum((Age_At_Death_Months <= 11)*Women_Sampling_Weight,
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

saveRDS(monthly_time_series_control,
        file = here("data/final", "monthly_time_series_control"))

saveRDS(monthly_time_series_1_frequency,
        file = here("data/final", "monthly_time_series_1_frequency"))

saveRDS(monthly_time_series_2_frequency,
        file = here("data/final", "monthly_time_series_2_frequency"))

saveRDS(monthly_time_series_3_frequency,
        file = here("data/final", "monthly_time_series_3_frequency"))

saveRDS(monthly_time_series_4_frequency,
        file = here("data/final", "monthly_time_series_4_frequency"))

saveRDS(monthly_time_series_5_frequency,
        file = here("data/final", "monthly_time_series_5_frequency"))

saveRDS(monthly_time_series_6_frequency,
        file = here("data/final", "monthly_time_series_6_frequency"))

saveRDS(monthly_time_series_7_frequency,
        file = here("data/final", "monthly_time_series_7_frequency"))

saveRDS(monthly_time_series_flood_year,
        file = here("data/final", "monthly_time_series_flood_year"))



#-------------------------------------------------------------------------------