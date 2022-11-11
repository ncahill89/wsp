#' Get catchment, climate and proxy IDs.
#'
#' @param catchment_num index for the catchment out of the list of all catchment areas
#' @param climate_num index for climate out of the list of all climate variables
#' @param choose_proxy specify a single proxy dataset ID or a vector of IDs in the format c("Dataset_XXX","Dataset_YYY")
#' @param catchment_scale Indicates whether using catchment scale data or not. Defaults to TRUE.
#'
#' @return A list of IDs
#' @export

get_id <- function(catchment_num = 1,
                   climate_num = 1,
                   choose_proxy = NULL,
                   catchment_scale = TRUE)
{

  ### data files
  if(catchment_scale == TRUE)
  {
  catchment_climate_list <- readRDS("data_all/catchment_climate_list.rds")
  lag_match_list <- readRDS("data_all/lag_match_list.rds")
  catchments <- readRDS("data_all/catchments.rds")
  climate_indices <- readRDS("data_all/climate_indices.rds")
  }

  if(catchment_scale == FALSE)
  {
    catchment_climate_list <- readRDS("data/catchment_climate_list.rds")
    lag_match_list <- readRDS("data/lag_match_list.rds")
    catchments <- readRDS("data/catchments.rds")
    climate_indices <- readRDS("data/climate_indices.rds")
  }


  ## Get the list of proxies corresponding to the catchment, climate combo.
  if(is.null(choose_proxy))
  {
  proxy_id <- catchment_climate_list[[catchment_num]][[climate_num]]
  lag_match <- lag_match_list[[catchment_num]][[climate_num]]
  }
  if(!is.null(choose_proxy)){
  proxy_id_all <- catchment_climate_list[[catchment_num]][[climate_num]]
  proxy_id <- proxy_id_all[match(choose_proxy,proxy_id_all)]
  lag_match_all <- lag_match_list[[catchment_num]][[climate_num]]
  lag_match <- lag_match_all[match(choose_proxy,proxy_id_all)]
  }


  dir.create("output", showWarnings = FALSE)
  dir.create(paste0("output/",catchments[catchment_num]),showWarnings = F)

  cat("Output folder created for the ",catchments[catchment_num], "catchment ")

  return(list(proxy_id = proxy_id,
              lag_match = lag_match,
              catchment = catchments[catchment_num],
              climate_index = climate_indices[climate_num],
              catchment_num = catchment_num,
              climate_num = climate_num,
              catchment_scale = catchment_scale))
}


#' Pre-processing the data
#'
#' @param catchment
#' @param climate_index
#' @param proxy_id
#' @param lag_match
#' @param catchment_num
#' @param climate_num
#' @param catchment_scale
#' @param filter_for_overlap
#' @param valid
#' @param divergence_cutoff
#'
#' @return A list of data for input to `get_jags_data()`
#' @export

get_proxy_clim_data <- function(catchment,
                                climate_index,
                                proxy_id,
                                lag_match,
                                catchment_num,
                                climate_num,
                                catchment_scale,
                                filter_for_overlap = FALSE,
                                valid = FALSE,
                                divergence_cutoff = 3.5)
{


  if(catchment_scale == TRUE) proxy_data <- read_csv("data_all/proxy_dataset.csv")

  if(catchment_scale == FALSE) proxy_data <- read_csv("data/proxy_dataset.csv")

  proxy_data$DatasetID <- paste0("Dataset_",proxy_data$DatasetID)

  ### get training data if validation run
  if(valid == FALSE)
  {
    if(catchment_scale == TRUE) climate_dataset <- read_csv("data_all/combined_catchment_data.csv")
    if(catchment_scale == FALSE) climate_dataset <- read_csv("data/combined_catchment_data.csv")

    }
  else
  {
    climate_dataset <- read_csv("data/training_catchment_data.csv")
    proxy_data <- proxy_data %>% filter(Year > 1800)
  }



  ### filter proxy data & get average value if a year has multiple values
  proxy_use <- proxy_data %>%
    filter(DatasetID %in% proxy_id) %>%
    mutate(Year = floor(Year)) %>%
    group_by(DatasetID, Year) %>%
    dplyr::summarise(proxy_value = mean(proxy_value)) %>%
    drop_na() %>%
    ungroup() %>%
    filter(Year > 1000)

  ### get the relevant climate indicator data
  indicator_data <- climate_dataset %>%
    select(index, year, catchment) %>%
    filter(index == climate_index) %>%
    dplyr::rename(value = catchment,
           Year = year)

  if(climate_index == "rain__1yr_wy7")
  {
    t_ind <- BCres(indicator_data$value)
    indicator_data$value_transformed <- t_ind$transformed
  }
  else
  {
    t_ind <- NULL
    indicator_data$value_transformed <- indicator_data$value
  }

  ### Get some info related to recon years etc..
  meta_data <- proxy_use %>%
    group_by(DatasetID) %>%
    dplyr::summarise(n_years = length(unique(Year)),
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
                          clim_value = c(rep(NA, meta_data$n_recon_years[1]),indicator_data$value_transformed[1:meta_data$proxy_max_index[1]]))

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
                                   clim_value = c(rep(NA, meta_data$n_recon_years[i]),indicator_data$value_transformed[1:meta_data$proxy_max_index[i]]))

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
      mutate(clim_value = indvar_data$clim_value) %>%
      mutate(period = ifelse(is.na(clim_value), "recon","calib"))

    ### filter the data based on KL divergence of recon proxy data and calib proxy data
    if(filter_for_overlap)
    {
      if(catchment_scale == TRUE) KL_Values <- readRDS("data_all/KL_Values.rds")
      if(catchment_scale == FALSE) KL_Values <- readRDS("data/KL_Values.rds")

      proxy_variables <- proxy_id
      for (i in 1:length(proxy_variables)) {
        Dataset <- proxy_clim_data[proxy_clim_data$DatasetID == proxy_variables[i],]
        calib <- Dataset[Dataset$period == "calib",]

        catchmentID = catchment
        KL_div <- as.numeric(KL_Values[[catchment_num]][[climate_num]])[i]

        if (KL_div > divergence_cutoff | is.na(KL_div) | sum(!is.na(unique(calib$clim_value))) < 2) {
          meta_data <- meta_data[meta_data$DatasetID != proxy_variables[i],]
          proxy_clim_data <- proxy_clim_data[proxy_clim_data$DatasetID != proxy_variables[i],]
        }
      }
    }


  }

    return(list(proxy_clim_data = proxy_clim_data,
                indicator_data = indicator_data,
                meta_data = meta_data,
                climate_index = climate_index,
                t_ind = t_ind
                ))
}


#' Format the data for model input
#'
#' @param proxy_clim_data
#' @param indicator_data
#' @param meta_data
#' @param climate_index
#' @param include_lag
#'
#' @return A list of data for input to `jags()`
#' @export

get_jags_data <- function(proxy_clim_data,
                          indicator_data,
                          meta_data,
                          climate_index,
                          include_lag = FALSE)
{

all_years <- min(meta_data$min_proxy_yr):max(meta_data$max_proxy_yr)

if(include_lag)
{
lag_match_all <- rep(meta_data$lag_match, proxy_clim_data %>%
                                          group_by(DatasetID) %>%
                                          dplyr::summarise(n = n()) %>%
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
  mutate(year_ind = match((Year+lag_match_all),all_years)) %>%
  filter(year_ind > 0)


## filter out the NAs
data_scale_filter <- data_scale %>%
  filter(!is.na(proxy_value)) %>%
  filter(!is.na(year_ind))

## get climate data for the calibration period
data_calib <- indicator_data %>%
  mutate(clim_scale = (value_transformed - mean(value_transformed))/sd(value_transformed)) %>%
  filter(Year <= max(meta_data$max_proxy_yr))


## get no. of reconstruction years
n_recon <- length(all_years) - nrow(data_calib)

## get number of proxies used
n_proxys <- nrow(meta_data)
n_id <- data_scale_filter %>% dplyr::count(DatasetID) %>% pull(n)

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
