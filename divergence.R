library(tidyverse)
library(FNN)

catchment_names <- readRDS("data/catchments.rds")
climate_variable_names <- readRDS("data/climate_indices.rds")

proxy_data <- read_csv("data/proxy_dataset.csv")
proxy_data$DatasetID <- paste0("Dataset_",proxy_data$DatasetID)

proxies_available <- readRDS("data/catchment_climate_list.rds")
output <- readRDS("data/catchment_climate_list.rds")
climate_dataset <- read_csv("data/combined_catchment_data.csv")


get_divergence <- function(calib, recon, adjust = 1) {
  if (sum(!is.na(unique(calib))) >= 2 & sum(!is.na(unique(recon))) >= 2) {

   # browser()
    mean_calib = mean(calib)
    sd_calib = sd(calib)

    min_recon <- min(recon)
    max_recon <- max(recon)

    extreme_min <- abs(mean_calib - min_recon)/sd_calib
    extreme_max <- abs(max_recon - mean_calib)/sd_calib

    value <- max(extreme_min, extreme_max)
    value

  } else {
    NA
  }
}

for (catchment in catchment_names) {

  for (climate_variable in climate_variable_names) {

    climate_var_list <- list()
    catchment_index = match(catchment, catchment_names)
    climate_variable_index = match(climate_variable, climate_variable_names)

    print(str_c(catchment_index,climate_variable_index), sep = ", ")

    climate_variable_data <- climate_dataset %>%
      select(index, year, catchment) %>%
      filter(index == climate_variable) %>%
      rename(value = catchment,
             Year = year)

    calib_lb = min(climate_variable_data$Year)
    calib_ub = max(climate_variable_data$Year)

    for (ProxyID in proxies_available[[catchment_index]][[climate_variable_index]]) {
      proxy_index = match(ProxyID, proxies_available[[catchment_index]][[climate_variable_index]])

      proxy_variable_data <- proxy_data %>%
        filter(DatasetID %in% ProxyID) %>%
        mutate(Year = floor(Year)) %>%
        group_by(DatasetID, Year) %>%
        summarise(proxy_value = mean(proxy_value)) %>%
        drop_na() %>%
        ungroup()


      proxy_variable_data <- proxy_variable_data %>%
        mutate(period = ifelse(Year %in% climate_variable_data$Year, "calib", "recon")) %>%
        mutate(period = ifelse((period == "recon") & (Year >= calib_lb) & (Year <= calib_ub), NA, period))

      calib = proxy_variable_data$proxy_value[proxy_variable_data$period == "calib"]
      recon = proxy_variable_data$proxy_value[proxy_variable_data$period == "recon"]

      divergence_value = get_divergence(calib, recon)
      print(str_c(ProxyID, divergence_value, sep = ": "))
      output[[catchment_index]][[climate_variable_index]][proxy_index] <- divergence_value

  }
  }
}
saveRDS(output, file = "data/KL_Values.rds")



