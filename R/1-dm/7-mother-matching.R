#-------------------------------------------------------------------------------

# @Organization - UCSF
# @Project - Effects of floods on child mortality in Bangladesh
# @Author - Francois Rerolle, rerollefrancois@gmail.com
# @Description - This code samples mother using matching to balance covariates
# between flooded and non flooded mothers

#-------------------------------------------------------------------------------

# Source configuration code
library(here)
source(here("child-mortality-dhs/R", "0-config.R"))

#-------------------------------------------------------------------------------

## Load data
mother_records <- readRDS(file = here("data/final", "mother_records"))
#-------------------------------------------------------------------------------

## Process data
# Propensity score matching
# 0 quartile = min
m.out0 <- matchit(factor(Flooded_0_quartile) ~ Region + DHSYEAR + URBAN_RURA +
                    Birth_Date_Mother_Month_CMC + Highest_Level_Education +
                    Wealth_Index + Total_Children + Age_Mother_First_Birth_Years +
                    Source_Of_Drinking_Water + Type_Of_Toilet_Facility + Main_Floor_Material +
                    Main_Wall_Material + Main_Roof_Material + Toilets_Facilities_Shared_Other_HH +
                    Land_Surface_Temperature_2015 + Rainfall_2015 + UN_Population_Density_2015,
                  data = mother_records,
                  method = "nearest",
                  exact = "DHSYEAR",
                  distance = "glm",
                  link = "logit"
                  # , s.weights = ~Women_Sampling_Weight # Doesn't improve covariates balance
                  )

m.data0 <- match.data(m.out0)

# 1 quartile
m.out1 <- matchit(factor(Flooded_1_quartile) ~ Region + DHSYEAR + URBAN_RURA +
                    Birth_Date_Mother_Month_CMC + Highest_Level_Education +
                    Wealth_Index + Total_Children + Age_Mother_First_Birth_Years +
                    Source_Of_Drinking_Water + Type_Of_Toilet_Facility + Main_Floor_Material +
                    Main_Wall_Material + Main_Roof_Material + Toilets_Facilities_Shared_Other_HH +
                    Land_Surface_Temperature_2015 + Rainfall_2015 + UN_Population_Density_2015,
                  data = mother_records %>% filter(!(Flooded_0_quartile & !Flooded_1_quartile)),
                  method = "nearest",
                  exact = "DHSYEAR",
                  distance = "glm",
                  link = "logit")

m.data1 <- match.data(m.out1)

# 2 quartile
m.out2 <- matchit(factor(Flooded_2_quartile) ~ Region + DHSYEAR + URBAN_RURA +
                    Birth_Date_Mother_Month_CMC + Highest_Level_Education +
                    Wealth_Index + Total_Children + Age_Mother_First_Birth_Years +
                    Source_Of_Drinking_Water + Type_Of_Toilet_Facility + Main_Floor_Material +
                    Main_Wall_Material + Main_Roof_Material + Toilets_Facilities_Shared_Other_HH +
                    Land_Surface_Temperature_2015 + Rainfall_2015 + UN_Population_Density_2015,
                  data = mother_records %>% filter(!(Flooded_0_quartile & !Flooded_2_quartile)),
                  method = "nearest",
                  exact = "DHSYEAR",
                  distance = "glm",
                  link = "logit")

m.data2 <- match.data(m.out2)

# 3 quartile
m.out3 <- matchit(factor(Flooded_3_quartile) ~ Region + DHSYEAR + URBAN_RURA +
                    Birth_Date_Mother_Month_CMC + Highest_Level_Education +
                    Wealth_Index + Total_Children + Age_Mother_First_Birth_Years +
                    Source_Of_Drinking_Water + Type_Of_Toilet_Facility + Main_Floor_Material +
                    Main_Wall_Material + Main_Roof_Material + Toilets_Facilities_Shared_Other_HH +
                    Land_Surface_Temperature_2015 + Rainfall_2015 + UN_Population_Density_2015,
                  data = mother_records %>% filter(!(Flooded_0_quartile & !Flooded_3_quartile)),
                  method = "nearest",
                  exact = "DHSYEAR",
                  distance = "glm",
                  link = "logit")

m.data3 <- match.data(m.out3)


#-------------------------------------------------------------------------------

# Save dataset
saveRDS(m.data0, file = here("data/final", "matched_mothers_0_quartile"))
saveRDS(m.data1, file = here("data/final", "matched_mothers_1_quartile"))
saveRDS(m.data2, file = here("data/final", "matched_mothers_2_quartile"))
saveRDS(m.data3, file = here("data/final", "matched_mothers_3_quartile"))

# Save matching models for output
saveRDS(m.out0, file = here("child-mortality-dhs/output/matching", "matching_0_quartile"))
saveRDS(m.out1, file = here("child-mortality-dhs/output/matching", "matching_1_quartile"))
saveRDS(m.out2, file = here("child-mortality-dhs/output/matching", "matching_2_quartile"))
saveRDS(m.out3, file = here("child-mortality-dhs/output/matching", "matching_3_quartile"))

#-------------------------------------------------------------------------------