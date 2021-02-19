## Load libraries
library(tidyverse)
library(naniar)
library(rjags)
library(R2jags)
library(devtools)

## Load all functions
load_all()

dir.create("model_output") # create dir. to save output for each run

for(i in 1:2) # 18 catchments
{
  for(j in 1:2) # 13 climate indices
  {

## get proxy data ids based on the catchment and climate index choice
id <- get_id(catchment_num = i, # choose from no. 1 to 18
             climate_num = j) # choose from no. 1 to 13

## get_proxy_clim() will return the proxy and climate data needed for the model
## It will be based on the catchment and climate index choice
## It will also return some meta data related to Dataset IDs, recon years etc
dat <- get_proxy_clim_data(catchment = id$catchment,
                           climate_index = id$climate_index,
                           proxy_id = id$proxy_id,
                           lag_match = id$lag_match)

# Model -------------------------------------------------------------------
## run_jags_model() will run the model and return results
## if you want to apply the lag then set include_lag = FALSE
mod <- run_jags_model(dat,
                      include_lag = FALSE)

saveRDS(mod$res, paste0("model_output/res",id$catchment,"_",id$climate_index))
  } # end j loop
} # end i loop
