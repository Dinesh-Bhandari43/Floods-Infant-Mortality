# floods-bangladesh
Research projects assessing impacts of flooding on health in Bangladesh

## Description

This repository includes R code to run all of the analysis for the paper:

"Excess risk in infant mortality among populations living in flood prone areas in Bangladesh: A cluster-matched cohort study over three decades, 1988-2017"

Rerolle, F., Arnold, B. F., Benmarhnia, T.

Should you have any questions about the files in this repository, please contact Francois Rerolle at UCSF (francois.rerolle@ucsf.edu).

## Additional Resources

### Open Science Framework 

All data used in the analysis is publicly available but access must be granted by the DHS program for the 6 population surveys conducted in Bangladesh (2017, 2014, 2011, 2007, 2004, 1999). When requesting data, include GPS coordinates and spatial covariates data. The other data are deposited on OSF: https://osf.io/vrfmz/

### System Requirements

All analyses were run using R software version 4.1.1 on Mac OSX Big Sur using the RStudio IDE (https://www.rstudio.com).

> sessionInfo()

R version 4.1.3 (2022-03-10)

Platform: x86_64-apple-darwin17.0 (64-bit)

Running under: macOS Monterey 12.4

### Installation Guide

You can download and install R from CRAN: https://cran.r-project.org

You can download and install RStudio from their website: https://www.rstudio.com

All R packages required to run the analyses are sourced in the file `0-config.R`.

The installation time should be < 10 minutes total on a typical desktop computer.

### Instructions for Use

To reproduce all analyses in the paper, we recommend that you: 

1. clone the GitHub repository

2. Create a `data` directory with 3 subdirectories: `untouched`, `temp` and `final`.

3. In the `data/untouched` directory copy and paste repository from OSF: https://osf.io/vrfmz/

4. In the `data/untouched/dhs` directory paste downloaded DHS data. You should have 6 subdirectories: `BD_1999-00_DHS_03072022_1129_172978`, `BD_2004_DHS_02032022_1033_172978`, `BD_2007_DHS_02032022_1033_172978`, `BD_2011_DHS_02032022_1032_172978`, `BD_2014_DHS_02082022_855_172978` and `BD_2017-18_DHS_02082022_855_172978`

5. Create `child-mortality-dhs\output` directory with 2 subdirectories: `figures` and `matching`

6. All of the analysis scripts should run smoothly. 

Running the all analyses on the above Mac laptop configuration required ~1h. 

Note that the only script that takes very long is `6b-infant-mortality-model-individiual-level.R` which pertains to the sensitivity analysis. 
