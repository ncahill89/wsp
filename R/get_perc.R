get_perc_dat <- function(mod)

{

  summaries <- mod$res %>%
    group_by(period) %>%
    summarise(mean = mean(climate_variable_recon),
              sd = sd(climate_variable_recon),
              min = min(climate_variable_recon),
              max = max(climate_variable_recon))


percentile_min <- percentile_max <- percentile_mean <- rep(NA, ncol(mod$recon_samps))

for(i in 1:ncol(mod$recon_samps))
{
  perc=ecdf(mod$recon_samps[,i])
  percentile_min[i] <- round(perc(summaries$min[1])*100)
  percentile_max[i] <- 100-round(perc(summaries$max[1])*100)
  percentile_mean[i] <- round(perc(summaries$mean[1])*100)
}
recon_years <- mod$res %>% filter(period == "recon") %>% pull(year)
perc_dat <- tibble(recon_years, percentile_min, percentile_max,percentile_mean)

return(perc_dat)
}
