#-------------------------------------------------------------------------------

# @Organization - UCSF
# @Project - Effects of floods on child mortality in Bangladesh
# @Author - Francois Rerolle, rerollefrancois@gmail.com
# @Description - This code extracts and cleans mother records from birth records 

#-------------------------------------------------------------------------------

# Source configuration code
library(here)
source(here("child-mortality-dhs/R", "0-config.R"))

#-------------------------------------------------------------------------------

## Load data
birth_records_flooded_spatial <- readRDS(file = here("data/final",
                                                     "birth_records_flooded_spatial"))

#-------------------------------------------------------------------------------

## Process data
# Restrict dataset to mothers only
mother_records_0 <- 
  (birth_records_flooded_spatial %>%
     dplyr::select(-c(Birth_Date_Month_CMC, Age_At_Death_Months)) %>%
     distinct(DHSCLUST, DHSYEAR, caseid, .keep_all = T) 
)

# select relevant variables
mother_records_1 <- 
  (mother_records_0 %>%
     mutate(DHSYEAR = factor(DHSYEAR)) %>%
     dplyr::select(c(Flooded_0_quartile, Flooded_1_quartile, Flooded_2_quartile, Flooded_3_quartile,
                     Flooded_1_Frequency, Flooded_2_Frequency, Flooded_3_Frequency, Flooded_4_Frequency, Flooded_5_Frequency, Flooded_6_Frequency, Flooded_7_Frequency,
                     Region, DHSYEAR, caseid,
                    URBAN_RURA, 
                     Birth_Date_Mother_Month_CMC, Highest_Level_Education,
                     Wealth_Index, Total_Children, Age_Mother_First_Birth_Years,
                    Source_Of_Drinking_Water, Type_Of_Toilet_Facility,
                     Main_Floor_Material, Main_Wall_Material, Main_Roof_Material,
                     Toilets_Facilities_Shared_Other_HH,
                    Land_Surface_Temperature_2015, Rainfall_2015, UN_Population_Density_2015,
                    Women_Sampling_Weight
                     ))
   )

# Handle missing values
# Complete case analysis
mother_records <- mother_records_1[complete.cases(mother_records_1),]

# Percent of flooded mothers discarded
100*sum(mother_records_1[!complete.cases(mother_records_1), "Flooded_0_quartile"]) / sum(mother_records_1[, "Flooded_0_quartile"])

#-------------------------------------------------------------------------------

# Save dataset
saveRDS(mother_records, file = here("data/final", "mother_records"))
readr::write_csv(mother_records, file = here("data/final", "mother_records.csv"))

#-------------------------------------------------------------------------------