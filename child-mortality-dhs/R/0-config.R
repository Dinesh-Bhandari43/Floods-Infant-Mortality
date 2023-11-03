#-------------------------------------------------------------------------------
# @Organization - UCSF
# @Project - Effects of floods on child mortality in Bangladesh
# @Author - Francois Rerolle, rerollefrancois@gmail.com
# @Description - This file is responsible for configuring the projects,
#               declaring global variables, path to data, loading packages, etc.
#-------------------------------------------------------------------------------

# Load packages
library(here)
library(haven)
library(tidyverse)
library(readr)
library(ggplot2)
library(sf)
library(raster)
library(leaflet)
library(Polychrome)
library(gsynth)
library(MatchIt)
library(MODISTools)
library(chirps)
library(mgcv)
library(rdhs)
library(randomForest)
library(cobalt)
library(meta)
library(lmtest)
library(sandwich)
library(boot)
library(survival)
library(labelled)
library(raster)
library(sp)
library(rgdal)
library(rasterVis)
library(RColorBrewer)
library(viridis)
library(maptools)
library(scales)
library(rgeos)
library(table1)
library(Hmisc)
library(spatstat)
library(lwgeom)
library(gridExtra)
library(cowplot)
library(ggcorrplot)


# safe color blind palette
# http://jfly.iam.u-tokyo.ac.jp/color/
# http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
# Reference: Bang Wong, Nature Methods 2011: https://www.nature.com/articles/nmeth.1618
cbpal <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")