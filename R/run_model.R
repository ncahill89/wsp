run_jags_model <- function(dat,
                           include_lag = FALSE)

{

  ## get_jags_data() will format the data for running the model
  model_data <- get_jags_data(proxy_clim_data = dat$proxy_clim_data,
                              indicator_data = dat$indicator_data,
                              meta_data = dat$meta_data,
                              include_lag = include_lag)

  jags_data <- model_data$jags_data

  reconmodel="
  model
  {

  ####Variances/Precisions for AR(1) distortions
  tau.st <- tau*(1-pow(rho,2))  #Precicision for first AR term for total CP
  tau <- pow(sigma, -2) #Precision

  ###1st AR terms
  xrecon_j[1] ~ dnorm(0, tau.st)
  mux[1] <- 0
  for(j in 2:n_recon)
  {
  xrecon_j[j] ~ dnorm(mux[j], sigma^-2)
  mux[j] <- rho*xrecon_j[j-1]
  }

  x_i[1] ~ dnorm(mux[n_recon + 1], sigma^-2)
  mux[n_recon + 1] <- rho*xrecon_j[n_recon]

for(i in 2:n_calib)
{
x_i[i] ~ dnorm(mux[n_recon + i], sigma^-2)
mux[n_recon + i] <- rho*x_i[i-1]
}


  #1. Likelihood
  for(i in 1:Ntot)
  {
    y_i[i] ~ dnorm(mu[i],sigma_y[ind[i]]^-2)
    mu[i] <- alpha[ind[i]] + beta[ind[i]]*mux[year_ind[i]] + beta2[ind[i]]*(mux[year_ind[i]]^2)
  }

  for(k in 1:n_proxys)
{
  # Priors
  alpha[k] ~ dnorm(0,10^-2)
  beta[k]  ~ dnorm(0, 10^-2)
  beta2[k]  ~ dnorm(0, 10^-2)
  sigma_y[k] ~ dt(0, 2^-2, 1)T(0,)

  } # end k loop


  ####Variances/Precisions for AR(1)
  rho ~ dunif(0,1)
  sigma ~ dt(0, 2^-2, 1)T(0,)

 }
  "
  pars=c("alpha",
         "beta",
         "beta2",
         "xrecon_j",
         "sigma_y",
         "sigma",
         "mux",
         "mu",
         "rho")

  ## 3)RUN THE MODEL
  run.mcmc <- jags(data=jags_data,
                   parameters.to.save=pars,
                   model.file=textConnection(reconmodel),
                   n.iter = 5000,
                   n.burnin = 1000)

  mcmc.jags<-run.mcmc$BUGSoutput$sims.list

  med_scale<-c(apply(mcmc.jags$xrecon_j,2,median))
  low_scale<-c(apply(mcmc.jags$xrecon_j,2,quantile,0.05))
  high_scale<-c(apply(mcmc.jags$xrecon_j,2,quantile,0.95))

  scale_mean <- mean(model_data$data_calib$value)
  scale_sd <- sd(model_data$data_calib$value)


  med <- c(((med_scale*scale_sd) + scale_mean),model_data$data_calib$value)
  low <- c(((low_scale*scale_sd) + scale_mean),model_data$data_calib$value)
  high <- c(((high_scale*scale_sd) + scale_mean),model_data$data_calib$value)

  res <- data.frame(model_data$all_years,round(med,2),round(low,2),round(high,2))

  names(res) <- c("year","climate_variable_recon", "lower", "upper")

  return(list(res=res))

}
