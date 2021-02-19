## Load libraries
library(tidyverse)
library(naniar)
library(rjags)
library(R2jags)
library(devtools)

## Load all functions
load_all()

## get proxy data ids based on the catchment and climate index choice
id <- get_id(catchment_num = 1, # choose from no. 1 to 18
                         climate_num = 1) # choose from no. 1 to 13
id


## get_proxy_clim() will return the proxy and climate data needed for the model
## It will be based on the catchment and climate index choice
## It will also return some meta data related to Dataset IDs, recon years etc
## This is currently set up to use all proxies associated with the catchment-climate combo.
## You can select from these proxies if you wish
## For example, to only use the first proxy use id$proxy_id[1] and id$lag_match[1] in get_proxy_clim()
dat <- get_proxy_clim_data(catchment = id$catchment,
                           climate_index = id$climate_index,
                           proxy_id = id$proxy_id,
                           lag_match = id$lag_match)
## check the meta data
dat$meta_data

## plot the data, incl missing values
ggplot(dat$proxy_clim_data %>% filter(!is.na(proxy_value)), aes(x = clim_value, y = proxy_value)) +
  geom_point() +
  geom_smooth(method = lm, formula = y ~ x + I(x^2)) +
  geom_miss_point(jitter = 0.01) +
  facet_wrap(~DatasetID, scales = "free")

# Model -------------------------------------------------------------------
## run_jags_model() will run the model and return results
## if you want to apply the lag then set include_lag = FALSE
mod <- run_jags_model(dat,
                      include_lag = FALSE)

mod$res

## Plot results
p_recon <- ggplot(mod$res, aes(x= year, y = climate_variable_recon))+
    geom_line(colour = "red",alpha = 0.7)+
    geom_ribbon(aes(ymin=lower,ymax=upper),alpha=0.5)+
    theme_bw()+
    xlab("Year")
p_recon


