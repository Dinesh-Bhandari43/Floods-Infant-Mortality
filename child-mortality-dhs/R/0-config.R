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
