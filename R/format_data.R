
format_list <- function()
{
proxy_dataset <- read_csv("data/proxy_dataset.csv")
proxy_dataset$DatasetID <- paste0("Dataset_",proxy_dataset$DatasetID)
combined_catchment_data <- read_csv("data/combined_catchment_data.csv")
selection_long <- read_csv("data/selection_long.csv") %>% drop_na()
selection_long_filtered <- read_csv("data/selection_long_filtered_lags.csv") %>% drop_na()

catchments <- selection_long$catchment %>% unique()
climate_indices <- selection_long_filtered$climate_index %>% unique()

saveRDS(catchments, "data/catchments.rds")
saveRDS(climate_indices, "data/climate_indices.rds")

N <- length(catchments)
M <- length(climate_indices)

climate_index_list <- list()
catchment_climate_list_out <- list()
lag_match_list <- list()
lag_match_list_out <- list()

for(i in 1:N)
{
  for(j in 1:M)
  {
    data_index <- selection_long %>% filter(catchment == catchments[i]) %>%
      filter(climate_index == climate_indices[j]) %>%
      pull(DatasetID)

    lag_match <- selection_long %>% filter(catchment == catchments[i]) %>%
      filter(climate_index == climate_indices[j]) %>%
      pull(lag_match)

    climate_index_list[[j]] <- data_index %>% unique()
    lag_match_list[[j]] <- rep(0, data_index %>% unique() %>% length)
  }
  climate_index_list <- lapply(climate_index_list, function(x) if(identical(x, character(0))) NA_character_ else x)
  lag_match_list <- lapply(lag_match_list, function(x) if(identical(x, character(0))) NA_character_ else x)

  catchment_climate_list_out[[i]] <- climate_index_list
  lag_match_list_out[[i]] <- lag_match_list

}

saveRDS(catchment_climate_list_out, "data/catchment_climate_list.rds")
saveRDS(lag_match_list_out, "data/lag_match_list.rds")

}


