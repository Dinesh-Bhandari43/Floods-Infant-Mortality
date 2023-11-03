#-------------------------------------------------------------------------------

# @Organization - UCSF
# @Project - Effects of floods on child mortality in Bangladesh
# @Author - Francois Rerolle, rerollefrancois@gmail.com
# @Description - This code finalizes matched birth record data

#-------------------------------------------------------------------------------

# Source configuration code
library(here)
source(here("child-mortality-dhs/R", "0-config.R"))

#-------------------------------------------------------------------------------

## Load data
birth_records_flooded <- readRDS(file = here("data/final", "birth_records_flooded"))
matched_mothers_0_quartile <- readRDS(file = here("data/final", "matched_mothers_0_quartile"))
matched_mothers_1_quartile <- readRDS(file = here("data/final", "matched_mothers_1_quartile"))
matched_mothers_2_quartile <- readRDS(file = here("data/final", "matched_mothers_2_quartile"))
matched_mothers_3_quartile <- readRDS(file = here("data/final", "matched_mothers_3_quartile"))
matched_mothers_1_frequency <- readRDS(file = here("data/final", "matched_mothers_1_frequency"))
matched_mothers_2_frequency <- readRDS(file = here("data/final", "matched_mothers_2_frequency"))
matched_mothers_3_frequency <- readRDS(file = here("data/final", "matched_mothers_3_frequency"))
matched_mothers_4_frequency <- readRDS(file = here("data/final", "matched_mothers_4_frequency"))
matched_mothers_5_frequency <- readRDS(file = here("data/final", "matched_mothers_5_frequency"))
matched_mothers_6_frequency <- readRDS(file = here("data/final", "matched_mothers_6_frequency"))
matched_mothers_7_frequency <- readRDS(file = here("data/final", "matched_mothers_7_frequency"))
matched_mothers_flood_year <- readRDS(file = here("data/final", "matched_mothers_flood_year"))

#-------------------------------------
# summarize the overall number of
# mothers and births in the dataset
#-------------------------------------

# number of births
cat("\n The number of unique births in the dataset is:",paste(nrow(birth_records_flooded)))

# number of mothers
cat("\n The number of unique mothers in the dataset is:",paste(length(unique(birth_records_flooded$caseid))))

#-------------------------------------
## Filter to matched mothers only
#-------------------------------------

# primary exposure, any flooding
birth_records_flooded_0_quartile <- matched_mothers_0_quartile %>%
  dplyr::select(DHSYEAR, caseid, weights, subclass) %>%
  mutate(DHSYEAR = as.numeric(paste(DHSYEAR))) %>%
  left_join(birth_records_flooded, by = c("DHSYEAR","caseid")) %>%
  mutate(Flooded = Flooded_0_quartile,
         Infant_Death = !(Age_At_Death_Months > 11 | is.na(Age_At_Death_Months))
  )

# first quartile (25th percentile of flooded days)
birth_records_flooded_1_quartile <- matched_mothers_1_quartile %>%
  dplyr::select(DHSYEAR, caseid, weights, subclass) %>%
  mutate(DHSYEAR = as.numeric(paste(DHSYEAR))) %>%
  left_join(birth_records_flooded, by = c("DHSYEAR","caseid")) %>%
  mutate(Flooded = Flooded_1_quartile,
         Infant_Death = !(Age_At_Death_Months > 11 | is.na(Age_At_Death_Months))
  )

# second quartile (50th percentile of flooded days)
birth_records_flooded_2_quartile <-  matched_mothers_2_quartile %>%
  dplyr::select(DHSYEAR, caseid, weights, subclass) %>%
  mutate(DHSYEAR = as.numeric(paste(DHSYEAR))) %>%
  left_join(birth_records_flooded, by = c("DHSYEAR","caseid")) %>%
  mutate(Flooded = Flooded_2_quartile,
         Infant_Death = !(Age_At_Death_Months > 11 | is.na(Age_At_Death_Months))
  )
  
# third quartile (75th percentile of flooded days)
birth_records_flooded_3_quartile <- matched_mothers_3_quartile %>%
  dplyr::select(DHSYEAR, caseid, weights, subclass) %>%
  mutate(DHSYEAR = as.numeric(paste(DHSYEAR))) %>%
  left_join(birth_records_flooded, by = c("DHSYEAR","caseid")) %>%
  mutate(Flooded = Flooded_3_quartile,
         Infant_Death = !(Age_At_Death_Months > 11 | is.na(Age_At_Death_Months))
  )

# non-flood prone areas
birth_records_flooded_control <- (birth_records_flooded %>%
                                    filter(Flooded_0_quartile == FALSE) %>%
                                    mutate(Flooded = "Control",
                                           Infant_Death = !(Age_At_Death_Months > 11 | is.na(Age_At_Death_Months)))
                                  )

# located flooded 1 to 7 times
birth_records_flooded_1_frequency <- matched_mothers_1_frequency %>%
  dplyr::select(DHSYEAR, caseid, weights, subclass) %>%
  mutate(DHSYEAR = as.numeric(paste(DHSYEAR))) %>%
  left_join(birth_records_flooded, by = c("DHSYEAR","caseid")) %>%
  mutate(Flooded = Flooded_1_Frequency,
         Infant_Death = !(Age_At_Death_Months > 11 | is.na(Age_At_Death_Months))
  )
  

birth_records_flooded_2_frequency <- matched_mothers_2_frequency %>%
  dplyr::select(DHSYEAR, caseid, weights, subclass) %>%
  mutate(DHSYEAR = as.numeric(paste(DHSYEAR))) %>%
  left_join(birth_records_flooded, by = c("DHSYEAR","caseid")) %>%
  mutate(Flooded = Flooded_2_Frequency,
         Infant_Death = !(Age_At_Death_Months > 11 | is.na(Age_At_Death_Months))
  )
  

birth_records_flooded_3_frequency <- matched_mothers_3_frequency %>%
  dplyr::select(DHSYEAR, caseid, weights, subclass) %>%
  mutate(DHSYEAR = as.numeric(paste(DHSYEAR))) %>%
  left_join(birth_records_flooded, by = c("DHSYEAR","caseid")) %>%
  mutate(Flooded = Flooded_3_Frequency,
         Infant_Death = !(Age_At_Death_Months > 11 | is.na(Age_At_Death_Months))
  )


birth_records_flooded_4_frequency <- matched_mothers_4_frequency %>%
  dplyr::select(DHSYEAR, caseid, weights, subclass) %>%
  mutate(DHSYEAR = as.numeric(paste(DHSYEAR))) %>%
  left_join(birth_records_flooded, by = c("DHSYEAR","caseid")) %>%
  mutate(Flooded = Flooded_4_Frequency,
         Infant_Death = !(Age_At_Death_Months > 11 | is.na(Age_At_Death_Months))
  )


birth_records_flooded_5_frequency <- matched_mothers_5_frequency %>%
  dplyr::select(DHSYEAR, caseid, weights, subclass) %>%
  mutate(DHSYEAR = as.numeric(paste(DHSYEAR))) %>%
  left_join(birth_records_flooded, by = c("DHSYEAR","caseid")) %>%
  mutate(Flooded = Flooded_5_Frequency,
         Infant_Death = !(Age_At_Death_Months > 11 | is.na(Age_At_Death_Months))
  )


birth_records_flooded_6_frequency <- matched_mothers_6_frequency %>%
  dplyr::select(DHSYEAR, caseid, weights, subclass) %>%
  mutate(DHSYEAR = as.numeric(paste(DHSYEAR))) %>%
  left_join(birth_records_flooded, by = c("DHSYEAR","caseid")) %>%
  mutate(Flooded = Flooded_6_Frequency,
         Infant_Death = !(Age_At_Death_Months > 11 | is.na(Age_At_Death_Months))
  )


birth_records_flooded_7_frequency <- matched_mothers_7_frequency %>%
  dplyr::select(DHSYEAR, caseid, weights, subclass) %>%
  mutate(DHSYEAR = as.numeric(paste(DHSYEAR))) %>%
  left_join(birth_records_flooded, by = c("DHSYEAR","caseid")) %>%
  mutate(Flooded = Flooded_7_Frequency,
         Infant_Death = !(Age_At_Death_Months > 11 | is.na(Age_At_Death_Months))
  )


# Time-varying exposure for 2003 - 2017
birth_records_flooded_year <- matched_mothers_flood_year %>%
  dplyr::select(DHSYEAR, caseid, weights, subclass) %>%
  mutate(DHSYEAR = as.numeric(paste(DHSYEAR))) %>%
  left_join(birth_records_flooded, by = c("DHSYEAR","caseid")) %>%
  mutate(Flooded = Flood_Year,
         Infant_Death = !(Age_At_Death_Months > 11 | is.na(Age_At_Death_Months))
  )


#-------------------------------------------------------------------------------
# print number of births and mothers included in the main analysis and in
# the time-varying analysis

#------------------------------------
# main analysis
#------------------------------------
# number of mothers
cat("\n The number of unique mothers in the matched dataset is:",paste(nrow(matched_mothers_0_quartile)))

# number of births
cat("\n The number of unique births in the matched dataset is:",paste(nrow(birth_records_flooded_0_quartile)))

# number of deaths
cat("\n The number of deaths in the matched dataset is:",paste(sum(birth_records_flooded_0_quartile$Infant_Death)))


#------------------------------------
# time-varying exposure analysis
#------------------------------------
# number of mothers
cat("\n The number of unique mothers in the matched dataset is:",paste(nrow(matched_mothers_flood_year)))

# number of births
cat("\n The number of unique births in the matched dataset is:",paste(nrow(birth_records_flooded_year)))

# number of deaths
cat("\n The number of deaths in the matched dataset is:",paste(sum(birth_records_flooded_year$Infant_Death)))


#-------------------------------------------------------------------------------

# Save dataset
saveRDS(birth_records_flooded_0_quartile,
        file = here("data/final", "birth_records_flooded_0_quartile"))

saveRDS(birth_records_flooded_1_quartile,
        file = here("data/final", "birth_records_flooded_1_quartile"))

saveRDS(birth_records_flooded_2_quartile,
        file = here("data/final", "birth_records_flooded_2_quartile"))

saveRDS(birth_records_flooded_3_quartile,
        file = here("data/final", "birth_records_flooded_3_quartile"))

saveRDS(birth_records_flooded_control,
        file = here("data/final", "birth_records_flooded_control"))

saveRDS(birth_records_flooded_1_frequency,
        file = here("data/final", "birth_records_flooded_1_frequency"))

saveRDS(birth_records_flooded_2_frequency,
        file = here("data/final", "birth_records_flooded_2_frequency"))

saveRDS(birth_records_flooded_3_frequency,
        file = here("data/final", "birth_records_flooded_3_frequency"))

saveRDS(birth_records_flooded_4_frequency,
        file = here("data/final", "birth_records_flooded_4_frequency"))

saveRDS(birth_records_flooded_5_frequency,
        file = here("data/final", "birth_records_flooded_5_frequency"))

saveRDS(birth_records_flooded_6_frequency,
        file = here("data/final", "birth_records_flooded_6_frequency"))

saveRDS(birth_records_flooded_7_frequency,
        file = here("data/final", "birth_records_flooded_7_frequency"))

saveRDS(birth_records_flooded_year,
        file = here("data/final", "birth_records_flooded_year"))

#-------------------------------------------------------------------------------