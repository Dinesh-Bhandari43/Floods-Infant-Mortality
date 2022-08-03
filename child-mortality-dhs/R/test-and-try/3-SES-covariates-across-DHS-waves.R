#-------------------------------------------------------------------------------

# @Organization - UCSF
# @Project - Effects of floods on child mortality in Bangladesh
# @Author - Francois Rerolle, rerollefrancois@gmail.com
# @Description - This code explores variations in SES variables across DHS waves

#-------------------------------------------------------------------------------

# Source configuration code
library(here)
source(here("child-mortality-dhs/R", "0-config.R"))

#-------------------------------------------------------------------------------

## Load data
birth_records_flooded_0_quartile <- readRDS(file = here("data/final", "birth_records_flooded_0_quartile"))

## Sumarise across DHS waves
SES_summary <- (birth_records_flooded_0_quartile %>%
                  group_by(DHSYEAR) %>%
                  summarise(Years_Lived_In_Place_Of_Residence_Mean = mean(Years_Lived_In_Place_Of_Residence, na.rm = T),
                            Highest_Level_Education_Mean = mean(Highest_Level_Education),
                            Wealth_Index_Mean = mean(Wealth_Index),
                            Total_Children_Mean = mean(Total_Children),
                            Age_Mother_First_Birth_Years_Mean = mean(Age_Mother_First_Birth_Years),
                            Type_Of_Place_Of_Residence_Mean = mean(Type_Of_Place_Of_Residence))
                )


