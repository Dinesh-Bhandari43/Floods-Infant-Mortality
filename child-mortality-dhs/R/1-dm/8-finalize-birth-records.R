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

## Filter to matched mothers only
birth_records_flooded_0_quartile <- (birth_records_flooded %>%
                                       right_join(matched_mothers_0_quartile %>%
                                                    dplyr::select(caseid, weights, subclass)) %>%
                                       mutate(Flooded = Flooded_0_quartile,
                                              Infant_Death = !(Age_At_Death_Months > 1 | is.na(Age_At_Death_Months)))
                                     )

birth_records_flooded_1_quartile <- (birth_records_flooded %>%
                                       right_join(matched_mothers_1_quartile %>%
                                                    dplyr::select(caseid, weights, subclass)) %>%
                                       mutate(Flooded = Flooded_1_quartile,
                                              Infant_Death = !(Age_At_Death_Months > 1 | is.na(Age_At_Death_Months)))
                                     )

birth_records_flooded_2_quartile <- (birth_records_flooded %>%
                                       right_join(matched_mothers_2_quartile %>%
                                                    dplyr::select(caseid, weights, subclass)) %>%
                                       mutate(Flooded = Flooded_2_quartile,
                                              Infant_Death = !(Age_At_Death_Months > 1 | is.na(Age_At_Death_Months)))
                                     )

birth_records_flooded_3_quartile <- (birth_records_flooded %>%
                                       right_join(matched_mothers_3_quartile %>%
                                                    dplyr::select(caseid, weights, subclass)) %>%
                                       mutate(Flooded = Flooded_3_quartile,
                                              Infant_Death = !(Age_At_Death_Months > 1 | is.na(Age_At_Death_Months)))
                                     )

birth_records_flooded_control <- (birth_records_flooded %>%
                                    filter(Flooded_0_quartile == FALSE) %>%
                                    mutate(Flooded = "Control",
                                           Infant_Death = !(Age_At_Death_Months > 1 | is.na(Age_At_Death_Months)))
)


birth_records_flooded_1_frequency <- (birth_records_flooded %>%
                                       right_join(matched_mothers_1_frequency %>%
                                                    dplyr::select(caseid, weights, subclass)) %>%
                                       mutate(Flooded = Flooded_1_Frequency,
                                              Infant_Death = !(Age_At_Death_Months > 1 | is.na(Age_At_Death_Months)))
)

birth_records_flooded_2_frequency <- (birth_records_flooded %>%
                                        right_join(matched_mothers_2_frequency %>%
                                                     dplyr::select(caseid, weights, subclass)) %>%
                                        mutate(Flooded = Flooded_2_Frequency,
                                               Infant_Death = !(Age_At_Death_Months > 1 | is.na(Age_At_Death_Months)))
)

birth_records_flooded_3_frequency <- (birth_records_flooded %>%
                                        right_join(matched_mothers_3_frequency %>%
                                                     dplyr::select(caseid, weights, subclass)) %>%
                                        mutate(Flooded = Flooded_3_Frequency,
                                               Infant_Death = !(Age_At_Death_Months > 1 | is.na(Age_At_Death_Months)))
)

birth_records_flooded_4_frequency <- (birth_records_flooded %>%
                                        right_join(matched_mothers_4_frequency %>%
                                                     dplyr::select(caseid, weights, subclass)) %>%
                                        mutate(Flooded = Flooded_4_Frequency,
                                               Infant_Death = !(Age_At_Death_Months > 1 | is.na(Age_At_Death_Months)))
)

birth_records_flooded_5_frequency <- (birth_records_flooded %>%
                                        right_join(matched_mothers_5_frequency %>%
                                                     dplyr::select(caseid, weights, subclass)) %>%
                                        mutate(Flooded = Flooded_5_Frequency,
                                               Infant_Death = !(Age_At_Death_Months > 1 | is.na(Age_At_Death_Months)))
)

birth_records_flooded_6_frequency <- (birth_records_flooded %>%
                                        right_join(matched_mothers_6_frequency %>%
                                                     dplyr::select(caseid, weights, subclass)) %>%
                                        mutate(Flooded = Flooded_6_Frequency,
                                               Infant_Death = !(Age_At_Death_Months > 1 | is.na(Age_At_Death_Months)))
)

birth_records_flooded_7_frequency <- (birth_records_flooded %>%
                                        right_join(matched_mothers_7_frequency %>%
                                                     dplyr::select(caseid, weights, subclass)) %>%
                                        mutate(Flooded = Flooded_7_Frequency,
                                               Infant_Death = !(Age_At_Death_Months > 1 | is.na(Age_At_Death_Months)))
)

# PNAS revision 2
birth_records_flooded_year <- (birth_records_flooded %>%
                                        right_join(matched_mothers_flood_year %>%
                                                     dplyr::select(caseid, weights, subclass)) %>%
                                        mutate(Flooded = Flood_Year,
                                               Infant_Death = !(Age_At_Death_Months > 1 | is.na(Age_At_Death_Months)))
)

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