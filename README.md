# floods-bangladesh
Excess risk in infant mortality among populations living in flood prone areas in Bangladesh

## Description

This repository includes R code to run all of the analysis for the paper:

_Excess risk in infant mortality among populations living in flood prone areas in Bangladesh: A cluster-matched cohort study over three decades, 1988-2017_

Rerolle, F., Arnold, B. F., Benmarhnia, T.

_in press_ at PNAS.

Should you have any questions about the files in this repository, please contact Francois Rerolle at UCSF (francois.rerolle@ucsf.edu) or see the corresponding author contact information in the manuscript.

## Additional Resources

### Open Science Framework 

All data used in the analysis is publicly available but access must be granted by the DHS program for the 6 population surveys conducted in Bangladesh (2017, 2014, 2011, 2007, 2004, 1999). When requesting data, include GPS coordinates and spatial covariates data. The other data are deposited on OSF: https://osf.io/vrfmz/

### System Requirements

All analyses were run using R software version 4.3.0 on MacOS Monterey using the RStudio IDE (https://www.rstudio.com).

> sessionInfo()

R version 4.3.0 (2023-04-21)

Platform: aarch64-apple-darwin20 (64-bit)

Running under: macOS Monterey 12.6

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

4. In the `data/untouched/dhs` directory paste downloaded DHS data. You should have 6 subdirectories corresponding to each DHS survey (note these subdirectories include a download date, `05302023`, which was used in this study: `BD_1999-00_DHS_05302023_1740_172978`, `BD_2004_DHS_05302023_1740_172978`, `BD_2007_DHS_05302023_1740_172978`, `BD_2011_DHS_05302023_1739_172978`, `BD_2014_DHS_05302023_1739_172978` and `BD_2017-18_DHS_05302023_1738_172978`

5. Create `child-mortality-dhs\output` directory with 2 subdirectories: `figures` and `matching`

6. All of the data management and analysis scripts should run smoothly. 

Running the all analyses on the above Mac configuration required ~1h. 

Note that the only script that takes very long is `6b-infant-mortality-model-individiual-level.R` which pertains to the sensitivity analysis estimating effects using conditional logistic regression. 

We identified two additional wrinkles in our internal replication effort that users should keep in mind. In the data management workflow the script `10-extract-precipitation-data.R` can be very slow to download the CHIRPS data. Additionally, the script `11-leave-one-out-cross-validation.R` requires manual commenting and un-commenting blocks of code to repeat the LOO analysis, leaving out each event in turn. Unfortunately, our team simply didn't have time to code this in a more elegant way, but there are clear instructions in the script for doing that piece of the analysis. 
