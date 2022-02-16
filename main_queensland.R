## Load libraries
library(tidyverse)
library(naniar)
library(rjags)
library(R2jags)
library(devtools)

## Load all functions
load_all()

## queensland is the catchment name here
catchments <- readRDS("data/catchments.rds")
catchments


  dir.create("output", showWarnings = FALSE)
  dir.create(paste0("output/",catchments[1]),showWarnings = F)
  for(c in 1:4) # there are 4 climate variables
  {
    id <- get_id(catchment_num = 1,
                climate_num = c,
                 catchment_scale = FALSE)
    run_mod(id)
  }# end c loop



