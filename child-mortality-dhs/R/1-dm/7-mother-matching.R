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
m.out0 <- matchit(factor(Flooded_0_quartile) ~ factor(Region) + DHSYEAR + factor(URBAN_RURA) +
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
m.out1 <- matchit(factor(Flooded_1_quartile) ~ factor(Region) + DHSYEAR + factor(URBAN_RURA) +
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
m.out2 <- matchit(factor(Flooded_2_quartile) ~ factor(Region) + DHSYEAR + factor(URBAN_RURA) +
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
m.out3 <- matchit(factor(Flooded_3_quartile) ~ factor(Region) + DHSYEAR + factor(URBAN_RURA) +
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

## Frequency
# 1
m.out_1freq <- matchit(factor(Flooded_1_Frequency) ~ factor(Region) + DHSYEAR + factor(URBAN_RURA) +
                    Birth_Date_Mother_Month_CMC + Highest_Level_Education +
                    Wealth_Index + Total_Children + Age_Mother_First_Birth_Years +
                    Source_Of_Drinking_Water + Type_Of_Toilet_Facility + Main_Floor_Material +
                    Main_Wall_Material + Main_Roof_Material + Toilets_Facilities_Shared_Other_HH +
                    Land_Surface_Temperature_2015 + Rainfall_2015 + UN_Population_Density_2015,
                  data = mother_records,
                  method = "nearest",
                  exact = "DHSYEAR",
                  distance = "glm",
                  link = "logit")

m.data_1freq <- match.data(m.out_1freq)

# 2
m.out_2freq <- matchit(factor(Flooded_2_Frequency) ~ factor(Region) + DHSYEAR + factor(URBAN_RURA) +
                         Birth_Date_Mother_Month_CMC + Highest_Level_Education +
                         Wealth_Index + Total_Children + Age_Mother_First_Birth_Years +
                         Source_Of_Drinking_Water + Type_Of_Toilet_Facility + Main_Floor_Material +
                         Main_Wall_Material + Main_Roof_Material + Toilets_Facilities_Shared_Other_HH +
                         Land_Surface_Temperature_2015 + Rainfall_2015 + UN_Population_Density_2015,
                       data = mother_records %>% filter(!(Flooded_1_Frequency & !Flooded_2_Frequency)),
                       method = "nearest",
                       exact = "DHSYEAR",
                       distance = "glm",
                       link = "logit")

m.data_2freq <- match.data(m.out_2freq)

# 3
m.out_3freq <- matchit(factor(Flooded_3_Frequency) ~ factor(Region) + DHSYEAR + factor(URBAN_RURA) +
                         Birth_Date_Mother_Month_CMC + Highest_Level_Education +
                         Wealth_Index + Total_Children + Age_Mother_First_Birth_Years +
                         Source_Of_Drinking_Water + Type_Of_Toilet_Facility + Main_Floor_Material +
                         Main_Wall_Material + Main_Roof_Material + Toilets_Facilities_Shared_Other_HH +
                         Land_Surface_Temperature_2015 + Rainfall_2015 + UN_Population_Density_2015,
                       data = mother_records %>% filter(!(Flooded_1_Frequency & !Flooded_3_Frequency)),
                       method = "nearest",
                       exact = "DHSYEAR",
                       distance = "glm",
                       link = "logit")

m.data_3freq <- match.data(m.out_3freq)

# 4
m.out_4freq <- matchit(factor(Flooded_4_Frequency) ~ factor(Region) + DHSYEAR + factor(URBAN_RURA) +
                         Birth_Date_Mother_Month_CMC + Highest_Level_Education +
                         Wealth_Index + Total_Children + Age_Mother_First_Birth_Years +
                         Source_Of_Drinking_Water + Type_Of_Toilet_Facility + Main_Floor_Material +
                         Main_Wall_Material + Main_Roof_Material + Toilets_Facilities_Shared_Other_HH +
                         Land_Surface_Temperature_2015 + Rainfall_2015 + UN_Population_Density_2015,
                       data = mother_records %>% filter(!(Flooded_1_Frequency & !Flooded_4_Frequency)),
                       method = "nearest",
                       exact = "DHSYEAR",
                       distance = "glm",
                       link = "logit")

m.data_4freq <- match.data(m.out_4freq)

# 5
m.out_5freq <- matchit(factor(Flooded_5_Frequency) ~ factor(Region) + DHSYEAR + factor(URBAN_RURA) +
                         Birth_Date_Mother_Month_CMC + Highest_Level_Education +
                         Wealth_Index + Total_Children + Age_Mother_First_Birth_Years +
                         Source_Of_Drinking_Water + Type_Of_Toilet_Facility + Main_Floor_Material +
                         Main_Wall_Material + Main_Roof_Material + Toilets_Facilities_Shared_Other_HH +
                         Land_Surface_Temperature_2015 + Rainfall_2015 + UN_Population_Density_2015,
                       data = mother_records %>% filter(!(Flooded_1_Frequency & !Flooded_5_Frequency)),
                       method = "nearest",
                       exact = "DHSYEAR",
                       distance = "glm",
                       link = "logit")

m.data_5freq <- match.data(m.out_5freq)

# 6
m.out_6freq <- matchit(factor(Flooded_6_Frequency) ~ factor(Region) + DHSYEAR + factor(URBAN_RURA) +
                         Birth_Date_Mother_Month_CMC + Highest_Level_Education +
                         Wealth_Index + Total_Children + Age_Mother_First_Birth_Years +
                         Source_Of_Drinking_Water + Type_Of_Toilet_Facility + Main_Floor_Material +
                         Main_Wall_Material + Main_Roof_Material + Toilets_Facilities_Shared_Other_HH +
                         Land_Surface_Temperature_2015 + Rainfall_2015 + UN_Population_Density_2015,
                       data = mother_records %>% filter(!(Flooded_1_Frequency & !Flooded_6_Frequency)),
                       method = "nearest",
                       exact = "DHSYEAR",
                       distance = "glm",
                       link = "logit")

m.data_6freq <- match.data(m.out_6freq)

# 2
m.out_7freq <- matchit(factor(Flooded_7_Frequency) ~ factor(Region) + DHSYEAR + factor(URBAN_RURA) +
                         Birth_Date_Mother_Month_CMC + Highest_Level_Education +
                         Wealth_Index + Total_Children + Age_Mother_First_Birth_Years +
                         Source_Of_Drinking_Water + Type_Of_Toilet_Facility + Main_Floor_Material +
                         Main_Wall_Material + Main_Roof_Material + Toilets_Facilities_Shared_Other_HH +
                         Land_Surface_Temperature_2015 + Rainfall_2015 + UN_Population_Density_2015,
                       data = mother_records %>% filter(!(Flooded_1_Frequency & !Flooded_7_Frequency)),
                       method = "nearest",
                       exact = "DHSYEAR",
                       distance = "glm",
                       link = "logit")

m.data_7freq <- match.data(m.out_7freq)

#PNAS revision 2 Flood_Year
m.out <- matchit(factor(Flood_Year) ~ factor(Region) + DHSYEAR + factor(URBAN_RURA) +
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

m.data <- match.data(m.out)


#-------------------------------------------------------------------------------

# Save dataset
saveRDS(m.data0, file = here("data/final", "matched_mothers_0_quartile"))
saveRDS(m.data1, file = here("data/final", "matched_mothers_1_quartile"))
saveRDS(m.data2, file = here("data/final", "matched_mothers_2_quartile"))
saveRDS(m.data3, file = here("data/final", "matched_mothers_3_quartile"))
saveRDS(m.data_1freq, file = here("data/final", "matched_mothers_1_frequency"))
saveRDS(m.data_2freq, file = here("data/final", "matched_mothers_2_frequency"))
saveRDS(m.data_3freq, file = here("data/final", "matched_mothers_3_frequency"))
saveRDS(m.data_4freq, file = here("data/final", "matched_mothers_4_frequency"))
saveRDS(m.data_5freq, file = here("data/final", "matched_mothers_5_frequency"))
saveRDS(m.data_6freq, file = here("data/final", "matched_mothers_6_frequency"))
saveRDS(m.data_7freq, file = here("data/final", "matched_mothers_7_frequency"))
saveRDS(m.data, file = here("data/final", "matched_mothers_flood_year"))

# Save matching models for output
saveRDS(m.out0, file = here("child-mortality-dhs/output/matching", "matching_0_quartile"))
saveRDS(m.out1, file = here("child-mortality-dhs/output/matching", "matching_1_quartile"))
saveRDS(m.out2, file = here("child-mortality-dhs/output/matching", "matching_2_quartile"))
saveRDS(m.out3, file = here("child-mortality-dhs/output/matching", "matching_3_quartile"))
saveRDS(m.out_1freq, file = here("child-mortality-dhs/output/matching", "matching_1_frequency"))
saveRDS(m.out_2freq, file = here("child-mortality-dhs/output/matching", "matching_2_frequency"))
saveRDS(m.out_3freq, file = here("child-mortality-dhs/output/matching", "matching_3_frequency"))
saveRDS(m.out_4freq, file = here("child-mortality-dhs/output/matching", "matching_4_frequency"))
saveRDS(m.out_5freq, file = here("child-mortality-dhs/output/matching", "matching_5_frequency"))
saveRDS(m.out_6freq, file = here("child-mortality-dhs/output/matching", "matching_6_frequency"))
saveRDS(m.out_7freq, file = here("child-mortality-dhs/output/matching", "matching_7_frequency"))
saveRDS(m.out, file = here("child-mortality-dhs/output/matching", "matching_flood_year"))

#-------------------------------------------------------------------------------