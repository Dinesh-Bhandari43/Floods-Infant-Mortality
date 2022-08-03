#-------------------------------------------------------------------------------

# @Organization - UCSF
# @Project - Effects of floods on child mortality in Bangladesh
# @Author - Francois Rerolle, rerollefrancois@gmail.com
# @Description - This code produces descriptive tables for manuscript

#-------------------------------------------------------------------------------

# Source configuration code
library(here)
source(here("child-mortality-dhs/R", "0-config.R"))
source(here("child-mortality-dhs/R", "0-modified-table1-package.R"))

#-------------------------------------------------------------------------------

## Load data
mother_records <- readRDS(file = here("data/final", "mother_records"))

#-------------------------------------------------------------------------------

## Prepare inouts for tables
# label variables' factors
mother_records <- (mother_records %>%
                     mutate(Flooded_0_quartile = factor(Flooded_0_quartile,
                                                        levels = c(FALSE, TRUE),
                                                        labels = c("Non-flood prone area", "Flood prone area")),
                            URBAN_RURA = factor(URBAN_RURA,
                                                        levels = c("R", "U"),
                                                        labels = c("Rural", "Urban")),
                            Highest_Level_Education = factor(Highest_Level_Education,
                                                levels = c("no education", "primary", "secondary", "higher"),
                                                labels = c("No education", "Primary", "Secondary", "Higher")),
                            Wealth_Index = factor(Wealth_Index,
                                                             levels = c("poorest", "poorer", "middle", "richer", "richest", "1999"),
                                                             labels = c("Poorest", "Poorer", "Middle", "Richer", "Richest", "Not asked in 1999 survey<sup>1</sup>")),
                            Toilets_Facilities_Shared_Other_HH = factor(Toilets_Facilities_Shared_Other_HH,
                                                  levels = c("no", "yes", "1999", "2004"),
                                                  labels = c("No", "Yes", "Not asked in 1999 and 2004 surveys<sup>1</sup>", "Not asked in 1999 and 2004 surveys<sup>1</sup>"))
                            )
                   )



# label variable
labels <- list(
  variables=list(URBAN_RURA = "Place of residence",
                 Highest_Level_Education = "Highest level of education",
                 Wealth_Index = "Wealth index quintile",
                 Total_Children = "Number of children",
                 Age_Mother_First_Birth_Years = "Age of mother at first birth (years)",
                 Source_Of_Drinking_Water = "Source of drinking water",
                 Type_Of_Toilet_Facility = "Sanitation",
                 Toilets_Facilities_Shared_Other_HH = "Sanitation facilities shared with other HH",
                 Main_Floor_Material = "Main floor material",
                 Main_Wall_Material = "Main wall material",
                 Main_Roof_Material = "Main roof material",
                 Land_Surface_Temperature_2015 = "Land surface temperature (\u00B0C)",
                 Rainfall_2015 = "Rainfall (mm)",
                 UN_Population_Density_2015 = "Population density (per km2)")
  )

# Specify tables column's stratification
strata.1 <- c(split(mother_records, ~ Flooded_0_quartile),
              list(Overall = mother_records))
strata.2 <- c(split(mother_records, ~ DHSYEAR))

# Specify render functions
my.render.cat <- function(x, weights, variable) {
  c("", sapply(stats.default(x, weights), function(y) with(y, sprintf("%d (%0.0f %%)",
                                                                   FREQ, PCT))))
}

my.render.cont <- function(x, weights, variable) {
  if(variable == "UN_Population_Density_2015"){
    with(stats.apply.rounding(stats.default(x, weights), digits=2), c("",
                                                                      "Median [IQR]"=sprintf("%s [%s - %s]", MEDIAN, q25w, q75w)))
  }else{
    with(stats.apply.rounding(stats.default(x, weights), digits=3), c("",
                                                                      "Mean (SD)"=sprintf("%s (&plusmn; %s)", MEAN, SD)))
  }

}


# Weighted Table 1 (stratified by flood prone area)
table1(x = strata.1,
       labels,
       render.continuous = my.render.cont,
       render.categorical = my.render.cat,
       footnote = "<sup>1</sup>Wealth index and sanitation sharing weren't reported in all DHS questionnaires. As matching was conducted within DHS wave, missingness here is not an issue."
       )


# Weighted Table 1 appendix (stratified by DHS waves)
table1(strata.2,
       labels,
       render.continuous = my.render.cont,
       render.categorical = my.render.cat,
       footnote = "<sup>1</sup>Wealth index and sanitation sharing weren't reported in all DHS questionnaires. As matching was conducted within DHS wave, missingness here is not an issue."
       )
