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
birth_records_flooded <- readRDS(file = here("data/final", "birth_records_flooded"))

#-------------------------------------------------------------------------------

## Process data
# Restrict dataset to mothers only
mother_records <- 
  (birth_records_flooded %>%
     dplyr::select(-c(Birth_Date_Month_CMC, Age_At_Death_Months)) %>%
     distinct(DHSCLUST, DHSYEAR, caseid, .keep_all = T) %>%
     replace_na(list(Source_Of_Drinking_Water = 99, # Input 99 for NA otherwise records needs to be discarded before being matched
                     Type_Of_Toilet_Facility = 99,
                     Main_Floor_Material = 99,
                     Main_Wall_Material = 99,
                     Main_Roof_Material = 99,
                     Toilets_Facilities_Shared_Other_HH = 99))
)

# 0 quartile = min
m.out0 <- matchit(factor(Flooded_0_quartile) ~ Region + factor(DHSYEAR) + URBAN_RURA +
                    Birth_Date_Mother_Month_CMC + factor(Highest_Level_Education) +
                    factor(Wealth_Index) + Total_Children + Age_Mother_First_Birth_Years +
                    Type_Of_Place_Of_Residence + Source_Of_Drinking_Water + Type_Of_Toilet_Facility +
                    Main_Floor_Material + Main_Wall_Material + Main_Roof_Material + Toilets_Facilities_Shared_Other_HH,
                  data = mother_records,
                  method = "nearest",
                  exact = "DHSYEAR",
                  distance = "glm",
                  link = "logit")

m.data0 <- match.data(m.out0)

# 1 quartile
m.out1 <- matchit(factor(Flooded_1_quartile) ~ Region + factor(DHSYEAR) + URBAN_RURA +
                    Birth_Date_Mother_Month_CMC + factor(Highest_Level_Education) +
                    factor(Wealth_Index) + Total_Children + Age_Mother_First_Birth_Years +
                    Type_Of_Place_Of_Residence + Source_Of_Drinking_Water + Type_Of_Toilet_Facility +
                    Main_Floor_Material + Main_Wall_Material + Main_Roof_Material + Toilets_Facilities_Shared_Other_HH,
                  data = mother_records,
                  method = "nearest",
                  exact = "DHSYEAR",
                  distance = "glm",
                  link = "logit")

m.data1 <- match.data(m.out1)

# 2 quartile
m.out2 <- matchit(factor(Flooded_2_quartile) ~ Region + factor(DHSYEAR) + URBAN_RURA +
                    Birth_Date_Mother_Month_CMC + factor(Highest_Level_Education) +
                    factor(Wealth_Index) + Total_Children + Age_Mother_First_Birth_Years +
                    Type_Of_Place_Of_Residence + Source_Of_Drinking_Water + Type_Of_Toilet_Facility +
                    Main_Floor_Material + Main_Wall_Material + Main_Roof_Material + Toilets_Facilities_Shared_Other_HH,
                  data = mother_records,
                  method = "nearest",
                  exact = "DHSYEAR",
                  distance = "glm",
                  link = "logit")

m.data2 <- match.data(m.out2)

# 3 quartile
m.out3 <- matchit(factor(Flooded_3_quartile) ~ Region + factor(DHSYEAR) + URBAN_RURA +
                    Birth_Date_Mother_Month_CMC + factor(Highest_Level_Education) +
                    factor(Wealth_Index) + Total_Children + Age_Mother_First_Birth_Years +
                    Type_Of_Place_Of_Residence + Source_Of_Drinking_Water + Type_Of_Toilet_Facility +
                    Main_Floor_Material + Main_Wall_Material + Main_Roof_Material + Toilets_Facilities_Shared_Other_HH,
                  data = mother_records,
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