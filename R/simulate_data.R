
get_sim_true <- function(alpha,
                        beta1,
                        beta2,
                        rho,
                        sigma_x,
                        sigma_y,
                        N) {
  txtstring <- "
data{
  ####Variances/Precisions for AR(1) distortions
  tau.st <- tau*(1-pow(rho,2))  #Precicision for first AR term for total CP
  tau <- pow(sigma_x, -2) #Precision

  ###1st AR terms
  x_i[1] <- x0
  xhat_i[1] <- xhat0

  for(i in 2:N)
  {
  x_i[i] ~ dnorm(xhat_i[i], sigma_x^(-2))
  xhat_i[i] <- rho*x_i[i-1]
  }


  #1. Likelihood
  for(i in 1:N)
  {
    y_i[i] ~ dnorm(mu[i],sigma_y^-2)
    mu[i] <- alpha + beta1*xhat_i[i] + beta2*xhat_i[i]^2

  }

}
model{
fake <- 0
}
"

  x0 <- 0.5
  xhat0 <- rho * x0

  data <- list(
    N = N,
    beta1 = beta1,
    beta2 = beta2,
    alpha = alpha,
    rho = rho,
    sigma_x = sigma_x,
    sigma_y = sigma_y,
    x0 = x0,
    xhat0 = xhat0
  )

  out <- runjags::run.jags(txtstring,
    data = data,
    monitor = c("x_i", "y_i"),
    sample = 1,
    n.chains = 1,
    summarise = FALSE
  )

  Simulated <- coda::as.mcmc(out)
  sim_x <- as.vector(Simulated[1:N])
  sim_y <- as.vector(Simulated[(N + 1):(N * 2)])

  return(list(
    sim_x = sim_x,
    sim_y = sim_y
  ))
}

format_sim_dat <- function(sim_y, sim_x) {
  N <- length(sim_y)
  M <- (N / 2) + 1

  proxyvar_data <- tibble::tibble(
    DatasetID = 1,
    proxy_value = sim_y,
    Year = 1:N
  )


  indicator_data <- tibble::tibble(
    value = sim_x[M:N],
    Year = M:N
  )


  n_years <- proxyvar_data %>%
    dplyr::group_by(DatasetID) %>%
    dplyr::summarise(
      n_years = length(unique(Year)),
      min_year = floor(min(Year)),
      max_year = floor(max(Year))
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      clim_min = indicator_data$Year %>% min(),
      clim_max = indicator_data$Year %>% max(),
      proxy_max_index = match(max_year, indicator_data$Year),
      n_recon_years = clim_min - min_year
    )


  indvar_data <- tibble::tibble(
    Year = n_years$min_year[1]:n_years$max_year[1],
    clim_value = c(rep(NA, n_years$n_recon_years[1]), indicator_data$value)
  )

  proxy_clim_data <- proxyvar_data %>%
    dplyr::mutate(clim_value = indvar_data$clim_value)

  return(list(proxy_clim_data = proxy_clim_data,
              indicator_data = indicator_data,
              n_years = n_years))
}



get_sim_alt <- function(alpha,
                         beta1,
                         sigma_x,
                         sigma_y,
                         N) {
  txtstring <- "
data{
  ####Variances/Precisions for AR(1) distortions
  tau <- pow(sigma_x, -2) #Precision

  ###1st AR terms
  x_i[1] <- x0
  xhat_i[1] <- xhat0

  for(i in 2:N)
  {
  x_i[i] ~ dnorm(xhat_i[i], sigma_x^(-2))
  xhat_i[i] <- x_i[i-1]
  }


  #1. Likelihood
  for(i in 1:N)
  {
    y_i[i] ~ dnorm(mu[i],sigma_y^-2)
    mu[i] <- alpha + beta1*xhat_i[i]

  }

}
model{
fake <- 0
}
"

x0 <- 0.5
xhat0 <- x0

data <- list(
  N = N,
  beta1 = beta1,
  alpha = alpha,
  sigma_x = sigma_x,
  sigma_y = sigma_y,
  x0 = x0,
  xhat0 = xhat0
)

out <- runjags::run.jags(txtstring,
                         data = data,
                         monitor = c("x_i", "y_i"),
                         sample = 1,
                         n.chains = 1,
                         summarise = FALSE
)

Simulated <- coda::as.mcmc(out)
sim_x <- as.vector(Simulated[1:N])
sim_y <- as.vector(Simulated[(N + 1):(N * 2)])

return(list(
  sim_x = sim_x,
  sim_y = sim_y
))
}

format_sim_dat <- function(sim_y, sim_x) {
  N <- length(sim_y)
  M <- (N / 2) + 1

  proxyvar_data <- tibble::tibble(
    DatasetID = 1,
    proxy_value = sim_y,
    Year = 1:N
  )


  indicator_data <- tibble::tibble(
    value = sim_x[M:N],
    Year = M:N
  )


  n_years <- proxyvar_data %>%
    dplyr::group_by(DatasetID) %>%
    dplyr::summarise(
      n_years = length(unique(Year)),
      min_year = floor(min(Year)),
      max_year = floor(max(Year))
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      clim_min = indicator_data$Year %>% min(),
      clim_max = indicator_data$Year %>% max(),
      proxy_max_index = match(max_year, indicator_data$Year),
      n_recon_years = clim_min - min_year
    )


  indvar_data <- tibble::tibble(
    Year = n_years$min_year[1]:n_years$max_year[1],
    clim_value = c(rep(NA, n_years$n_recon_years[1]), indicator_data$value)
  )

  proxy_clim_data <- proxyvar_data %>%
    dplyr::mutate(clim_value = indvar_data$clim_value)

  return(list(proxy_clim_data = proxy_clim_data,
              indicator_data = indicator_data,
              n_years = n_years))
}
