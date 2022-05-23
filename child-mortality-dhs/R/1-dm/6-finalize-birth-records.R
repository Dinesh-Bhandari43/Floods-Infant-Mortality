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

#-------------------------------------------------------------------------------