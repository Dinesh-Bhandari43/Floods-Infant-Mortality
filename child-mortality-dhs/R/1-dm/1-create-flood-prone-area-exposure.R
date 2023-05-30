#-------------------------------------------------------------------------------

# @Organization - UCSF
# @Project - Effects of floods on child mortality in Bangladesh
# @Author - Francois Rerolle, rerollefrancois@gmail.com
# @Description - This file appends data from multiple country-wide floods to
#               to create flood-prone area exposure layer

#-------------------------------------------------------------------------------

# Source configuration code
library(here)
source(here("child-mortality-dhs/R", "0-config.R"))

#-------------------------------------------------------------------------------

# Bangladesh admin
BGD_Adm <- raster::getData("GADM",
                           country = "BGD",
                           level = 1,
                           path = here("data/untouched/country-admin"))

# Floods
flood_jun_2002 <- stack(here("data/untouched/floods",
                             "DFO_1974_From_20020621_to_20020828",
                             "DFO_1974_From_20020621_to_20020828.tif"))
flood_jun_2003 <- stack(here("data/untouched/floods",
                             "DFO_2248_From_20030611_to_20031010",
                             "DFO_2248_From_20030611_to_20031010.tif"))
flood_jul_2004 <- stack(here("data/untouched/floods",
                             "DFO_2507_From_20040620_to_20041007",
                             "DFO_2507_From_20040620_to_20041007.tif"))
flood_jul_2007 <- stack(here("data/untouched/floods",
                             "DFO_3136_From_20070721_to_20071015",
                             "DFO_3136_From_20070721_to_20071015.tif"))
flood_jul_2010 <- stack(here("data/untouched/floods",
                             "DFO_3679_From_20100701_to_20100713",
                             "DFO_3679_From_20100701_to_20100713.tif"))
flood_oct_2010 <- stack(here("data/untouched/floods",
                             "DFO_3732_From_20101001_to_20101012",
                             "DFO_3732_From_20101001_to_20101012.tif"))
flood_jul_2016 <- stack(here("data/untouched/floods",
                             "DFO_4382_From_20160725_to_20160826",
                             "DFO_4382_From_20160725_to_20160826.tif"))
flood_aug_2017 <- stack(here("data/untouched/floods",
                             "DFO_4508_From_20170810_to_20170826",
                             "DFO_4508_From_20170810_to_20170826.tif"))

#-------------------------------------------------------------------------------

## Process data
# Crop to Bangladesh extent
flood_jun_2002_cropped <- crop(flood_jun_2002, BGD_Adm)
flood_jun_2003_cropped <- crop(flood_jun_2003, BGD_Adm)
flood_jul_2004_cropped <- crop(flood_jul_2004, BGD_Adm)
flood_jul_2007_cropped <- crop(flood_jul_2007, BGD_Adm)
flood_jul_2010_cropped <- crop(flood_jul_2010, BGD_Adm)
flood_oct_2010_cropped <- crop(flood_oct_2010, BGD_Adm)
flood_jul_2016_cropped <- crop(flood_jul_2016, BGD_Adm)
flood_aug_2017_cropped <- crop(flood_aug_2017, BGD_Adm)

# Append together to create layer of total number of days flooded
flood_area_duration <- (
  flood_jun_2002_cropped$duration +
  flood_jun_2003_cropped$duration +
  flood_jul_2004_cropped$duration +
  flood_jul_2007_cropped$duration +
  flood_jul_2010_cropped$duration +
  flood_oct_2010_cropped$duration +
  # flood_jul_2016_cropped$duration + # Missing data near confluence of bay
  flood_aug_2017_cropped$duration
  )


# Calculate maximum possible number of days flooded
max_flood_duration <- (
  max(flood_jun_2002_cropped$duration[], na.rm = T) +
  max(flood_jun_2003_cropped$duration[], na.rm = T) +
  max(flood_jul_2004_cropped$duration[], na.rm = T) +
  max(flood_jul_2007_cropped$duration[], na.rm = T) +
  max(flood_jul_2010_cropped$duration[], na.rm = T) +
  max(flood_oct_2010_cropped$duration[], na.rm = T) +
  # max(flood_jul_2016_cropped$duration[], na.rm = T) +
  max(flood_aug_2017_cropped$duration[], na.rm = T)
  )

# Extract percent of flooded days out of possible flooded days
flood_area_percent <- flood_area_duration / max_flood_duration

# Number of flooded event
flood_frequency <- (flood_jun_2002_cropped$flooded +
                      flood_jun_2003_cropped$flooded +
                      flood_jul_2004_cropped$flooded +
                      flood_jul_2007_cropped$flooded +
                      flood_jul_2010_cropped$flooded +
                      flood_oct_2010_cropped$flooded +
                      # flood_jul_2016_cropped$flooded + # Missing data near confluence of bay
                      flood_aug_2017_cropped$flooded
                    )

# Years flooded PNAS revision #2
flood_year<- stack(flood_jun_2002_cropped$flooded ,
                     flood_jun_2003_cropped$flooded ,
                     flood_jul_2004_cropped$flooded ,
                     flood_jul_2007_cropped$flooded ,
                     flood_jul_2010_cropped$flooded ,
                     flood_oct_2010_cropped$flooded ,
                     # flood_jul_2016_cropped$flooded , # Missing data near confluence of bay
                     flood_aug_2017_cropped$flooded
                   )


#-------------------------------------------------------------------------------

# Save dataset
saveRDS(flood_area_percent,
        file = here("data/final", "flood_area_percent"))

saveRDS(flood_frequency,
        file = here("data/final", "flood_frequency"))

saveRDS(flood_year,
        file = here("data/final", "flood_year"))

#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
## PNAS revision: leave one out analysis
# Append together to create layer of total number of days flooded
flood_area_duration_loo_2002 <- (
  # flood_jun_2002_cropped$duration +
    flood_jun_2003_cropped$duration +
    flood_jul_2004_cropped$duration +
    flood_jul_2007_cropped$duration +
    flood_jul_2010_cropped$duration +
    flood_oct_2010_cropped$duration +
    # flood_jul_2016_cropped$duration + # Missing data near confluence of bay
    flood_aug_2017_cropped$duration
)


# Calculate maximum possible number of days flooded
max_flood_duration_loo_2002 <- (
  # max(flood_jun_2002_cropped$duration[], na.rm = T) +
    max(flood_jun_2003_cropped$duration[], na.rm = T) +
    max(flood_jul_2004_cropped$duration[], na.rm = T) +
    max(flood_jul_2007_cropped$duration[], na.rm = T) +
    max(flood_jul_2010_cropped$duration[], na.rm = T) +
    max(flood_oct_2010_cropped$duration[], na.rm = T) +
    # max(flood_jul_2016_cropped$duration[], na.rm = T) +
    max(flood_aug_2017_cropped$duration[], na.rm = T)
)

# Extract percent of flooded days out of possible flooded days
flood_area_percent_loo_2002 <- flood_area_duration_loo_2002 / max_flood_duration_loo_2002

# Append together to create layer of total number of days flooded
flood_area_duration_loo_2003 <- (
  flood_jun_2002_cropped$duration +
  # flood_jun_2003_cropped$duration +
    flood_jul_2004_cropped$duration +
    flood_jul_2007_cropped$duration +
    flood_jul_2010_cropped$duration +
    flood_oct_2010_cropped$duration +
    # flood_jul_2016_cropped$duration + # Missing data near confluence of bay
    flood_aug_2017_cropped$duration
)


# Calculate maximum possible number of days flooded
max_flood_duration_loo_2003 <- (
  max(flood_jun_2002_cropped$duration[], na.rm = T) +
  # max(flood_jun_2003_cropped$duration[], na.rm = T) +
    max(flood_jul_2004_cropped$duration[], na.rm = T) +
    max(flood_jul_2007_cropped$duration[], na.rm = T) +
    max(flood_jul_2010_cropped$duration[], na.rm = T) +
    max(flood_oct_2010_cropped$duration[], na.rm = T) +
    # max(flood_jul_2016_cropped$duration[], na.rm = T) +
    max(flood_aug_2017_cropped$duration[], na.rm = T)
)

# Extract percent of flooded days out of possible flooded days
flood_area_percent_loo_2003 <- flood_area_duration_loo_2003 / max_flood_duration_loo_2003

# Append together to create layer of total number of days flooded
flood_area_duration_loo_2004 <- (
  flood_jun_2002_cropped$duration +
    flood_jun_2003_cropped$duration +
    # flood_jul_2004_cropped$duration +
    flood_jul_2007_cropped$duration +
    flood_jul_2010_cropped$duration +
    flood_oct_2010_cropped$duration +
    # flood_jul_2016_cropped$duration + # Missing data near confluence of bay
    flood_aug_2017_cropped$duration
)


# Calculate maximum possible number of days flooded
max_flood_duration_loo_2004 <- (
  max(flood_jun_2002_cropped$duration[], na.rm = T) +
    max(flood_jun_2003_cropped$duration[], na.rm = T) +
    # max(flood_jul_2004_cropped$duration[], na.rm = T) +
    max(flood_jul_2007_cropped$duration[], na.rm = T) +
    max(flood_jul_2010_cropped$duration[], na.rm = T) +
    max(flood_oct_2010_cropped$duration[], na.rm = T) +
    # max(flood_jul_2016_cropped$duration[], na.rm = T) +
    max(flood_aug_2017_cropped$duration[], na.rm = T)
)

# Extract percent of flooded days out of possible flooded days
flood_area_percent_loo_2004 <- flood_area_duration_loo_2004 / max_flood_duration_loo_2004

# Append together to create layer of total number of days flooded
flood_area_duration_loo_2007 <- (
  flood_jun_2002_cropped$duration +
    flood_jun_2003_cropped$duration +
    flood_jul_2004_cropped$duration +
    # flood_jul_2007_cropped$duration +
    flood_jul_2010_cropped$duration +
    flood_oct_2010_cropped$duration +
    # flood_jul_2016_cropped$duration + # Missing data near confluence of bay
    flood_aug_2017_cropped$duration
)


# Calculate maximum possible number of days flooded
max_flood_duration_loo_2007 <- (
  max(flood_jun_2002_cropped$duration[], na.rm = T) +
    max(flood_jun_2003_cropped$duration[], na.rm = T) +
    max(flood_jul_2004_cropped$duration[], na.rm = T) +
    # max(flood_jul_2007_cropped$duration[], na.rm = T) +
    max(flood_jul_2010_cropped$duration[], na.rm = T) +
    max(flood_oct_2010_cropped$duration[], na.rm = T) +
    # max(flood_jul_2016_cropped$duration[], na.rm = T) +
    max(flood_aug_2017_cropped$duration[], na.rm = T)
)

# Extract percent of flooded days out of possible flooded days
flood_area_percent_loo_2007 <- flood_area_duration_loo_2007 / max_flood_duration_loo_2007

# Append together to create layer of total number of days flooded
flood_area_duration_loo_2010 <- (
  flood_jun_2002_cropped$duration +
    flood_jun_2003_cropped$duration +
    flood_jul_2004_cropped$duration +
    flood_jul_2007_cropped$duration +
    # flood_jul_2010_cropped$duration +
    flood_oct_2010_cropped$duration +
    # flood_jul_2016_cropped$duration + # Missing data near confluence of bay
    flood_aug_2017_cropped$duration
)


# Calculate maximum possible number of days flooded
max_flood_duration_loo_2010 <- (
  max(flood_jun_2002_cropped$duration[], na.rm = T) +
    max(flood_jun_2003_cropped$duration[], na.rm = T) +
    max(flood_jul_2004_cropped$duration[], na.rm = T) +
    max(flood_jul_2007_cropped$duration[], na.rm = T) +
    # max(flood_jul_2010_cropped$duration[], na.rm = T) +
    max(flood_oct_2010_cropped$duration[], na.rm = T) +
    # max(flood_jul_2016_cropped$duration[], na.rm = T) +
    max(flood_aug_2017_cropped$duration[], na.rm = T)
)

# Extract percent of flooded days out of possible flooded days
flood_area_percent_loo_2010 <- flood_area_duration_loo_2010 / max_flood_duration_loo_2010

# Append together to create layer of total number of days flooded
flood_area_duration_loo_2010b <- (
  flood_jun_2002_cropped$duration +
    flood_jun_2003_cropped$duration +
    flood_jul_2004_cropped$duration +
    flood_jul_2007_cropped$duration +
    flood_jul_2010_cropped$duration +
    # flood_oct_2010_cropped$duration +
    # flood_jul_2016_cropped$duration + # Missing data near confluence of bay
    flood_aug_2017_cropped$duration
)


# Calculate maximum possible number of days flooded
max_flood_duration_loo_2010b <- (
  max(flood_jun_2002_cropped$duration[], na.rm = T) +
    max(flood_jun_2003_cropped$duration[], na.rm = T) +
    max(flood_jul_2004_cropped$duration[], na.rm = T) +
    max(flood_jul_2007_cropped$duration[], na.rm = T) +
    max(flood_jul_2010_cropped$duration[], na.rm = T) +
    # max(flood_oct_2010_cropped$duration[], na.rm = T) +
    # max(flood_jul_2016_cropped$duration[], na.rm = T) +
    max(flood_aug_2017_cropped$duration[], na.rm = T)
)

# Extract percent of flooded days out of possible flooded days
flood_area_percent_loo_2010b <- flood_area_duration_loo_2010b / max_flood_duration_loo_2010b

# Append together to create layer of total number of days flooded
flood_area_duration_loo_2017 <- (
  flood_jun_2002_cropped$duration +
    flood_jun_2003_cropped$duration +
    flood_jul_2004_cropped$duration +
    flood_jul_2007_cropped$duration +
    flood_jul_2010_cropped$duration +
    flood_oct_2010_cropped$duration
    # flood_jul_2016_cropped$duration + # Missing data near confluence of bay
    # flood_aug_2017_cropped$duration
)


# Calculate maximum possible number of days flooded
max_flood_duration_loo_2017 <- (
  max(flood_jun_2002_cropped$duration[], na.rm = T) +
    max(flood_jun_2003_cropped$duration[], na.rm = T) +
    max(flood_jul_2004_cropped$duration[], na.rm = T) +
    max(flood_jul_2007_cropped$duration[], na.rm = T) +
    max(flood_jul_2010_cropped$duration[], na.rm = T) +
    max(flood_oct_2010_cropped$duration[], na.rm = T)
    # max(flood_jul_2016_cropped$duration[], na.rm = T) +
    # max(flood_aug_2017_cropped$duration[], na.rm = T)
)

# Extract percent of flooded days out of possible flooded days
flood_area_percent_loo_2017 <- flood_area_duration_loo_2017 / max_flood_duration_loo_2017


#-------------------------------------------------------------------------------
# Save dataset
saveRDS(flood_area_percent_loo_2017,
        file = here("data/final", "flood_area_percent_loo_2017"))

saveRDS(flood_area_percent_loo_2010b,
        file = here("data/final", "flood_area_percent_loo_2010b"))

saveRDS(flood_area_percent_loo_2010,
        file = here("data/final", "flood_area_percent_loo_2010"))

saveRDS(flood_area_percent_loo_2007,
        file = here("data/final", "flood_area_percent_loo_2007"))

saveRDS(flood_area_percent_loo_2004,
        file = here("data/final", "flood_area_percent_loo_2004"))

saveRDS(flood_area_percent_loo_2003,
        file = here("data/final", "flood_area_percent_loo_2003"))

saveRDS(flood_area_percent_loo_2002,
        file = here("data/final", "flood_area_percent_loo_2002"))

#-------------------------------------------------------------------------------