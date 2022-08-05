## Load libraries
library(tidyverse)
library(naniar)
library(rjags)
library(R2jags)
library(devtools)

## Load all functions
load_all()

## Queensland is the catchment name here
catchments <- readRDS("data/catchments.rds")
catchments


  for(c in 1:4) # there are 4 climate variables
  {
    id <- get_id(catchment_num = 1,
                 climate_num = c,
                 catchment_scale = FALSE)
    run_mod(id, include_lag = TRUE)
  }# end c loop



