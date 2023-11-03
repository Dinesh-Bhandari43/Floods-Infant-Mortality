#-------------------------------------------------------------------------------

# @Organization - UCSF
# @Project - Effects of floods on child mortality in Bangladesh
# @Author - Francois Rerolle, rerollefrancois@gmail.com
# @Description - This code assesses covariates balance after matching

#-------------------------------------------------------------------------------

# Source configuration code
library(here)
source(here("child-mortality-dhs/R", "0-config.R"))

#-------------------------------------------------------------------------------
# Love plot for balance in the primary analysis

# Load matched output
m.out0 <- readRDS(file = here("child-mortality-dhs/output/matching", "matching_0_quartile"))

# Rename covariates
v <- data.frame(old = c("distance",
                        "factor(Region)_BARISAL", "factor(Region)_CHITTAGONG", "factor(Region)_DHAKA", "factor(Region)_KHULNA", "factor(Region)_RAJSHAHI", "factor(Region)_RANGPUR", "factor(Region)_SYLHET",
                        "DHSYEAR_2000", "DHSYEAR_2004", "DHSYEAR_2007", "DHSYEAR_2011", "DHSYEAR_2014", "DHSYEAR_2018",
                        "factor(URBAN_RURA)_U",
                        "Birth_Date_Mother_Month_CMC",
                        "Highest_Level_Education_higher", "Highest_Level_Education_no education", "Highest_Level_Education_primary", "Highest_Level_Education_secondary",
                        "Wealth_Index_middle", "Wealth_Index_poorer", "Wealth_Index_poorest", "Wealth_Index_richer", "Wealth_Index_richest",
                        "Total_Children",
                        "Age_Mother_First_Birth_Years",
                        "Source_Of_Drinking_Water_Unimproved",
                        "Type_Of_Toilet_Facility_Unimproved",
                        "Main_Floor_Material_Rudimentary", "Main_Wall_Material_Rudimentary", "Main_Roof_Material_Rudimentary",
                        "Toilets_Facilities_Shared_Other_HH_no", "Toilets_Facilities_Shared_Other_HH_yes",
                        "Land_Surface_Temperature_2015", "Rainfall_2015", "UN_Population_Density_2015"
                        ),
                new = c("Propensity Score",
                        "Region Barisal", "Region Chittagong", "Region Dakha", "Region Khulna", "Region Rajshahi", "Region Rangpur", "Region Sylhet",
                        "1999 DHS wave", "2004 DHS wave", "2007 DHS wave", "2011 DHS wave", "2014 DHS wave", "2018 DHS wave",
                        "Urban residence",
                        "Mother's birth date",
                        "Higher education", "No education", "Primary education", "Secondary education",
                        "Middle wealth index quintile", "Poorer wealth index quintile", "Poorest wealth index quintile", "Richer wealth index quintile", "Richest wealth index quintile",
                        "Total number of children",
                        "Age of mother at first birth",
                        "Unimproved source of drinking water",
                        "Unimproved sanitation",
                        "Rudimentray floor materials", "Rudimentray wall materials", "Rudimentray roof materials",
                        "Sanitation facilities shared", "Sanitation facilities not shared",
                        "Land surface temperature (2015)", "Rainfall (2015)", "Population Density (2015)"
                        ))

# Produce plot
love_plot <- love.plot(m.out0,
          stats = c("mean.diffs"), 
          threshold = c(m = .1), 
          binary = "std",
          abs = TRUE,
          # var.order = "unadjusted",
          var.names = v,
          limits = c(0, 1),
          grid = FALSE,
          wrap = 20,
          sample.names = c("Unmatched", "Matched"),
          position = "top",
          shapes = c("circle", "circle"),
          colors = c(cbpal[2], "black"))


### Save
pdf(here("child-mortality-dhs/output/figures", "love-plot.pdf"))
love_plot
dev.off()




