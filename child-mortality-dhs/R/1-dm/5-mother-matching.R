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

m.out0 <- matchit(factor(Flooded) ~ Region + factor(DHSYEAR) + URBAN_RURA +
                    Birth_Date_Mother_Month_CMC + factor(Highest_Level_Education) +
                    factor(Wealth_Index) + Total_Children + Age_Mother_First_Birth_Years +
                    Type_Of_Place_Of_Residence + Source_Of_Drinking_Water + Type_Of_Toilet_Facility +
                    Main_Floor_Material + Main_Wall_Material + Main_Roof_Material + Toilets_Facilities_Shared_Other_HH,
                  data = mother_records,
                  method = NULL,
                  distance = "glm")

summary(m.out0)

m.out1 <- matchit(factor(Flooded) ~ Region + factor(DHSYEAR) + URBAN_RURA +
                    Birth_Date_Mother_Month_CMC + factor(Highest_Level_Education) +
                    factor(Wealth_Index) + Total_Children + Age_Mother_First_Birth_Years +
                    Type_Of_Place_Of_Residence + Source_Of_Drinking_Water + Type_Of_Toilet_Facility +
                    Main_Floor_Material + Main_Wall_Material + Main_Roof_Material + Toilets_Facilities_Shared_Other_HH,
                  data = mother_records,
                  method = "nearest",
                  exact = "DHSYEAR",
                  distance = "glm",
                  link = "logit")

summary(m.out1)

# plot(m.out1, type = "jitter", interactive = FALSE)
# 
# plot(m.out1, type = "qq", interactive = FALSE,
#      which.xs = c("Total_Children", "URBAN_RURA"))
# 
# plot(summary(m.out1))

m.data1 <- match.data(m.out1)


#-------------------------------------------------------------------------------

# Save dataset
saveRDS(m.data1, file = here("data/final", "matched_mothers"))
readr::write_csv(m.data1, file = here("data/final", "matched_mothers.csv"))

#-------------------------------------------------------------------------------