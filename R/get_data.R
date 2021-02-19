get_id <- function(catchment_num = 1,
                         climate_num = 1)
{

  catchment_climate_list <- readRDS("data/catchment_climate_list.rds")
  lag_match_list <- readRDS("data/lag_match_list.rds")

  catchments <- readRDS("data/catchments.rds")
  climate_indices <- readRDS("data/climate_indices.rds")

  ## Get the list of proxies corresponding to the catchment, climate combo.
  proxy_id <- catchment_climate_list[[catchment_num]][[climate_num]]
  lag_match <- lag_match_list[[catchment_num]][[climate_num]]

  return(list(proxy_id = proxy_id,
              lag_match = lag_match,
              catchment = catchments[catchment_num],
              climate_index = climate_indices[climate_num]))
}

get_proxy_clim_data <- function(catchment,
                                climate_index,
                                proxy_id,
                                lag_match)
{

  proxy_data <- read_csv("data/proxy_dataset.csv")
  proxy_data$DatasetID <- paste0("Dataset_",proxy_data$DatasetID)
  climate_dataset <- read_csv("data/combined_catchment_data.csv")

  ## Filter proxy data & get average value if a year has multiple values
  proxy_use <- proxy_data %>%
    filter(DatasetID %in% proxy_id) %>%
    mutate(Year = floor(Year)) %>%
    group_by(DatasetID, Year) %>%
    summarise(proxy_value = mean(proxy_value)) %>%
    drop_na() %>%
    ungroup()

  ## Get the relevant climate indicator data
  indicator_data <- climate_dataset %>%
    select(index, year, catchment) %>%
    filter(index == climate_index) %>%
    rename(value = catchment,
           Year = year)

  ## Get some info related to recon years etc..
  meta_data <- proxy_use %>%
    group_by(DatasetID) %>%
    summarise(n_years = length(unique(Year)),
              min_proxy_yr = floor(min(Year)),
              max_proxy_yr = floor(max(Year))) %>%
    ungroup() %>%
    mutate( min_clim_yr = indicator_data$Year %>% min,
            max_clim_yr = indicator_data$Year %>% max,
            proxy_max_index = match(max_proxy_yr,indicator_data$Year),
            n_recon_years = ifelse((min_clim_yr - min_proxy_yr)>0,min_clim_yr - min_proxy_yr,NA)) %>%
    filter(!is.na(n_recon_years))


  meta_data$lag_match <- lag_match[match(meta_data$DatasetID,proxy_id)]


  if(nrow(meta_data != 0))
  {
    indvar_data <- tibble(Year = meta_data$min_proxy_yr[1]:meta_data$max_proxy_yr[1],
                          clim_value = c(rep(NA, meta_data$n_recon_years[1]),indicator_data$value[1:meta_data$proxy_max_index[1]]))

    tot_year <- meta_data$min_proxy_yr[1]:meta_data$max_proxy_yr[1]
    proxy_year <- proxy_use %>% filter(DatasetID == unique(meta_data$DatasetID)[1]) %>% pull(Year)
    data_avail <- match(tot_year,proxy_year)
    data_avail[which(!is.na(data_avail))] <- proxy_use %>%
      filter(DatasetID == unique(meta_data$DatasetID)[1]) %>%
      pull(proxy_value)

    proxy_use_final <- tibble(DatasetID = unique(meta_data$DatasetID)[1],
                              Year = tot_year,
                              proxy_value = data_avail)


    if(nrow(meta_data)>1)
    {
      for(i in 2:nrow(meta_data))
      {
        indvar_data_temp <- tibble(Year = meta_data$min_proxy_yr[i]:meta_data$max_proxy_yr[i],
                                   clim_value = c(rep(NA, meta_data$n_recon_years[i]),indicator_data$value[1:meta_data$proxy_max_index[i]]))

        indvar_data <- rbind(indvar_data, indvar_data_temp)

        tot_year <- meta_data$min_proxy_yr[i]:meta_data$max_proxy_yr[i]
        proxy_year <- proxy_use %>% filter(DatasetID == unique(meta_data$DatasetID)[i]) %>% pull(Year)
        data_avail <- match(tot_year,proxy_year)
        data_avail[which(!is.na(data_avail))] <- proxy_use %>% filter(DatasetID == unique(meta_data$DatasetID)[i]) %>% pull(proxy_value)

        proxy_use_temp <- tibble(DatasetID = unique(meta_data$DatasetID)[i], Year = tot_year, proxy_value = data_avail)

        proxy_use_final <- rbind(proxy_use_final, proxy_use_temp)

      }
    }


    proxy_clim_data <- proxy_use_final  %>%
      mutate(clim_value = indvar_data$clim_value)
}
    return(list(proxy_clim_data = proxy_clim_data,
                indicator_data = indicator_data,
                meta_data = meta_data))
}


get_jags_data <- function(proxy_clim_data,
                          indicator_data,
                          meta_data,
                          include_lag = FALSE)
{

all_years <- min(meta_data$min_proxy_yr):max(meta_data$max_proxy_yr)

if(include_lag)
{
lag_match_all <- rep(meta_data$lag_match, proxy_clim_data %>%
                                          group_by(DatasetID) %>%
                                          summarise(n = n()) %>%
                                          pull(n))
}
if(!include_lag)
{
  lag_match_all = 0
}

## Scale the proxy data
data_scale <- proxy_clim_data %>%
  arrange(DatasetID,Year) %>%
  group_by(DatasetID) %>%
  mutate(proxy_scale = (proxy_value - mean(proxy_value,na.rm = TRUE))/sd(proxy_value, na.rm = TRUE)) %>%
  mutate(clim_scale = (clim_value - mean(clim_value,na.rm = TRUE))/sd(clim_value, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(year_ind = match((Year+lag_match_all),all_years))


## filter out the NAs
data_scale_filter <- data_scale %>%
  filter(!is.na(proxy_value)) %>%
  filter(!is.na(year_ind))

## get climate data for the calibration period
data_calib <- indicator_data %>%
  mutate(clim_scale = (value - mean(value))/sd(value)) %>%
  filter(Year <= max(meta_data$max_proxy_yr))

## get no. of reconstruction years
n_recon <-  data_scale %>%
  filter(is.na(clim_scale)) %>%
  group_by(DatasetID) %>%
  summarise(n = n()) %>%
  pull(n) %>% max

## bget number of proxies used
n_proxys <- nrow(meta_data)


# create jags data list
jags_data =  list(y_i = data_scale_filter$proxy_scale,
                  x_i = data_calib$clim_scale,
                  year_ind = data_scale_filter$year_ind,
                  ind = as.numeric(as.factor(data_scale_filter$DatasetID)),
                  n_calib = nrow(data_calib),
                  n_recon = n_recon,
                  Ntot = nrow(data_scale_filter),
                  n_proxys = n_proxys)

return(list(jags_data = jags_data,
            data_calib = data_calib,
            all_years = all_years))
}
