## Load libraries
library(tidyverse)
library(naniar)
library(rjags)
library(R2jags)
library(devtools)

## Load all functions
load_all()

## read in all the catchment names
catchments <- readRDS("data_all/catchments.rds")
catchments

## choose the catchments names you want to run
catchments_to_run <- c("brisbane")

## match the catchments to run with the full list to get the correct index
catch_index <- match(catchments_to_run,catchments)

## loop through the model run for all catchments you want to run
## An output folder will be created for each catchment
## get_id() will get proxy data ids based on the catchment and climate index choice
## run_mod() will run the model and the output will be saved in the output folder

for(j in catch_index)
{
  print(j)
  dir.create("output", showWarnings = FALSE)
  dir.create(paste0("output/",catchments[j]),showWarnings = F)
  for(c in 1:13) # there are 13 climate variables
  {
    id <- get_id(catchment_num = j,
                 climate_num = c)

    run_mod(id)

  }# end c loop
}# end j loop


