run_mod <- function(id,
                    valid = FALSE,
                    include_lag = TRUE,
                    incl.trend = FALSE)
{
  ## get_proxy_clim() will return the proxy and climate data needed for the model
  ## It will be based on the catchment and climate index choice
  ## It will also return some meta data related to Dataset IDs, recon years etc
  if(sum(!is.na(id$proxy_id))>0)
  {

    dat <- get_proxy_clim_data(catchment = id$catchment,
                               climate_index = id$climate_index,
                               proxy_id = id$proxy_id,
                               lag_match = id$lag_match,
                               catchment_num = id$catchment_num,
                               climate_num = id$climate_num,
                               catchment_scale = id$catchment_scale,
                               filter_for_overlap = TRUE,
                               divergence_cutoff = 3.5,
                               valid = valid)

    if(nrow(dat$meta_data) !=0)
    {
      # Model -------------------------------------------------------------------
      ## run_jags_model() will run the model and return results
      mod <- run_jags_model(dat,
                            include_lag = include_lag,
                            incl.trend = incl.trend)


      ## create results plot
      p_recon <- ggplot(mod$res, aes(x= year, y = climate_variable_recon))+
        geom_line(colour = "red",alpha = 0.7)+
        geom_ribbon(aes(ymin=lower,ymax=upper),alpha=0.5)+
        theme_classic() +
        scale_color_manual(values=c("red", "#E69F00")) +
        xlab("Year") +
        ylab(id$climate_index)

      ## save the relevant output
      dir.create(paste0("output/",id$catchment,"/",id$climate_index),showWarnings = F)
      write_csv(mod$res, paste0("output/",id$catchment,"/",id$climate_index,"/res.csv"))
      saveRDS(mod, paste0("output/",id$catchment,"/",id$climate_index,"/mod.rds"))
      saveRDS(dat, paste0("output/",id$catchment,"/",id$climate_index,"/dat.rds"))
      ggsave(paste0("output/",id$catchment,"/",id$climate_index,"/p_recon.pdf"))

      converge <- ifelse(any(mod$rhat$rhat>1.15),"some issues ","no issues ")
      check <- ifelse(any(mod$rhat$rhat>1.15),"Further diagnostics are advised.","")
      cat("The model run for ",id$climate_index, " has flagged ",converge,"with convergence.",check,sep="",append=TRUE, file = paste0("output/",id$catchment,"/convergence.txt"), fill = TRUE)

    }
  }
  return(dat)
}



run_jags_model <- function(dat,
                           include_lag = FALSE,
                           incl.trend = FALSE)

{

  ## get_jags_data() will format the data for running the model
  model_data <- get_jags_data(proxy_clim_data = dat$proxy_clim_data,
                              indicator_data = dat$indicator_data,
                              meta_data = dat$meta_data,
                              climate_index = dat$climate_index,
                              include_lag = include_lag)

  jags_data <- model_data$jags_data
  # Since L1 regularization is equivalent to a Laplace (double exponential) prior on the relevant coefficients, you can do it as follows. Here I have three independent variables x1, x2, and x3, and y is the binary target variable. Selection of the regularization parameter lambda
  # is done here by putting a hyperprior on it, in this case just uniform over a good-sized range.


  ar1_model="
  model
  {

  ###Time series component

  ### prior for 1st AR term
  xrecon_j[1] ~ dnorm(xmod_j[1],tau_x)
  xmod_j[1] ~ dnorm(mux[1], sigma^-2)
  mux[1] <- 0
  x_hat[1] <- xrecon_j[1]

  ### variances/precisions for first AR(1) term
  tau.st <- tau*(1-pow(rho,2))  #Precicision for first AR term

  ### reconstruction period
  for(j in 2:n_recon)
  {
    xrecon_j[j] ~ dnorm(xmod_j[j],tau_x)
    xmod_j[j] ~ dnorm(mux[j], sigma^-2)
    mux[j] <- rho*xrecon_j[j-1]
    x_hat[j] <- xrecon_j[j]
  }

   x_i[1] ~ dnorm(xmod_i[1], tau_x)
   xmod_i[1] ~ dnorm(mux[n_recon + 1], sigma^-2)
   mux[n_recon + 1] <- rho*xrecon_j[n_recon]
   x_hat[n_recon + 1] <-  x_i[1]

  ### calibration period
  for(i in 2:n_calib)
  {
    x_i[i] ~ dnorm(xmod_i[i], tau_x)
    xmod_i[i] ~ dnorm(mux[n_recon + i], sigma^-2)
    mux[n_recon + i] <- rho*x_i[i-1]
    x_hat[n_recon + i] <- x_i[i]
  }


  ### Inverse regression component

  for(i in 1:Ntot)
  {

    ### individual regressions (uncomment to chose this option)
    mu[i] <- delta[ind[i]] + beta1[ind[i]]*x_hat[year_ind[i]] + beta2[ind[i]]*(x_hat[year_ind[i]]^2)
  }


  ### Likelihood

  for(i in 1:Ntot)
  {
    y_i[i] ~ dnorm(mu[i],sigma_y[ind[i]]^-2)
  }

### Priors

  for(k in 1:n_proxys)
{
  ### random effect
  delta[k]  ~ dnorm(0, 1^-2)

  ### data model sd
  sigma_y[k] ~ dt(1, 1^-2, 1)T(0,)

  ### uncomment if choosing individual regression option
    beta1[k]  ~ ddexp(0, lambda)
    beta2[k]  ~ ddexp(0, lambda)

  } # end k loop


  lambda ~ dt(0,10^-2,1)T(0,)

  #### variances/precisions for AR(1)
  rho ~ dunif(-1,1)
  sigma ~ dt(0, 1^-2, 1)T(0,)
  tau <- pow(sigma, -2) #precision
  sigma_x ~ dnorm(1,1^-2)T(0,)
  tau_x <- sigma_x^-2
  #sigma_x ~ dt(0, 0.5^-2, 1)T(0,)
  #tau_x ~ dgamma(8,2)
  #sigma_x <- pow(tau_x,-0.5)
 }
  "
  ar1_trend_model="
  model
  {

  ###Time series component

  ### prior for 1st AR term
  xrecon_j[1] ~ dnorm(xmod_j[1] + omega + gamma*1,sigma_x^-2)
  xmod_j[1] ~ dnorm(mux[1], sigma^-2)
  mux[1] <- 0
  x_hat[1] <- xmod_j[1] + omega + gamma*1

  ### variances/precisions for first AR(1) term
  tau.st <- tau*(1-pow(rho,2))  #Precicision for first AR term

  ### reconstruction period
  for(j in 2:n_recon)
  {
    xrecon_j[j] ~ dnorm(xmod_j[j] + omega + gamma*j,sigma_x^-2)
    xmod_j[j] ~ dnorm(mux[j], sigma^-2)
    mux[j] <- rho*xrecon_j[j-1]
    x_hat[j] <- xmod_j[j] + omega + gamma*j
  }

   x_i[1] ~ dnorm(xmod_i[1] + omega + gamma*(n_recon + 1), sigma_x^-2)
   xmod_i[1] ~ dnorm(mux[n_recon + 1], sigma^-2)
   mux[n_recon + 1] <- rho*xrecon_j[n_recon]
   x_hat[n_recon + 1] <-  xmod_i[1] + omega + gamma*(n_recon + 1)

  ### calibration period
  for(i in 2:n_calib)
  {
    x_i[i] ~ dnorm(xmod_i[i] + omega + gamma*(n_recon + i), sigma_x^-2)
    xmod_i[i] ~ dnorm(mux[n_recon + i], sigma^-2)
    mux[n_recon + i] <- rho*x_i[i-1]
    x_hat[n_recon + i] <- xmod_i[i] + omega + gamma*(n_recon + i)
  }


  ### Inverse regression component

  for(i in 1:Ntot)
  {

    ### individual regressions (uncomment to chose this option)
    mu[i] <- delta[ind[i]] + beta1[ind[i]]*x_hat[year_ind[i]] + beta2[ind[i]]*(x_hat[year_ind[i]]^2)
  }


  ### Likelihood

  for(i in 1:Ntot)
  {
    y_i[i] ~ dnorm(mu[i],sigma_y[ind[i]]^-2)
  }

### Priors

  for(k in 1:n_proxys)
{
  ### random effect
  delta[k]  ~ dnorm(0, 1^-2)

  ### data model sd
  sigma_y[k] ~ dt(1, 1^-2, 1)T(0,)

  ### uncomment if choosing individual regression option
    beta1[k]  ~ ddexp(0, lambda)
    beta2[k]  ~ ddexp(0, lambda)

  } # end k loop


  lambda ~ dt(0,10^-2,1)T(0,)
  gamma ~ dnorm(0,1^-2)
  omega ~ dnorm(0,1^-2)

  #### variances/precisions for AR(1)
  rho ~ dunif(-1,1)
  sigma ~ dt(0, 1^-2, 1)T(0,)
  tau <- pow(sigma, -2) #precision
  sigma_x ~ dgamma(10,100)
 }
  "

  pars=c( "beta1",
          "beta2",
          "delta",
          "lambda",
          "xrecon_j",
          "sigma_y",
          "sigma_x",
          "sigma",
          "mux",
          "x_hat",
          "mu",
          "rho")

  ## 3)RUN THE MODEL
  if(incl.trend)
  {
    run.mcmc <- suppressWarnings(jags(data=jags_data,
                                      parameters.to.save=pars,
                                      model.file=textConnection(ar1_trend_model),
                                      n.iter = 15000,
                                      n.burnin = 5000,
                                      n.thin = 10))
  }

  if(!incl.trend)
  {
    run.mcmc <- suppressWarnings(jags(data=jags_data,
                                      parameters.to.save=pars,
                                      model.file=textConnection(ar1_model),
                                      n.iter = 15000,
                                      n.burnin = 5000,
                                      n.thin = 10))
  }

  #browser()
  rhat <- tibble(par_names = rownames(run.mcmc$BUGSoutput$summary),
                 rhat = run.mcmc$BUGSoutput$summary[,8])

  rhat <- rhat %>% filter(stringr::str_detect(par_names, "beta") | stringr::str_detect(par_names, "delta")|stringr::str_detect(par_names, "rho")|par_names %in% c("sigma")|stringr::str_detect(par_names, "sigma_y"))

  mcmc.jags<-run.mcmc$BUGSoutput$sims.list
  x_recon_j <- array(NA,c(dim(mcmc.jags$xrecon_j)[1],dim(mcmc.jags$xrecon_j)[2]))
  for(i in 1:dim(mcmc.jags$xrecon_j)[1])
    x_recon_j[i,] <- mcmc.jags$xrecon_j[i,] #+ rnorm(dim(mcmc.jags$xrecon_j)[2],0,mcmc.jags$sigma_x[i])

  med_scale<-c(apply(x_recon_j,2,median, na.rm = TRUE))
  low_scale<-c(apply(x_recon_j,2,quantile,probs = 0.025, na.rm = TRUE))
  high_scale<-c(apply(x_recon_j,2,quantile,probs = 0.975, na.rm = TRUE))

  if(is.null(dat$t_ind))
  {
    scale_mean <- mean(model_data$data_calib$value)
    scale_sd <- sd(model_data$data_calib$value)
    recon_samps <- (x_recon_j*scale_sd) + scale_mean

    med <- c(c(apply(recon_samps,2,median,na.rm = TRUE)),model_data$data_calib$value)
    low <- c(c(apply(recon_samps,2,quantile, probs = 0.025,na.rm = TRUE)),model_data$data_calib$value)
    high <- c(c(apply(recon_samps,2,quantile, probs = 0.975,na.rm = TRUE)),model_data$data_calib$value)



  }

  if(!is.null(dat$t_ind))
  {
    scale_mean <- mean(model_data$data_calib$value_transformed)
    scale_sd <- sd(model_data$data_calib$value_transformed)
    recon_samps <- (x_recon_j*scale_sd) + scale_mean

    med <- c(apply(recon_samps,2,median,na.rm = TRUE))
    low <- c(apply(recon_samps,2,quantile, probs = 0.025,na.rm = TRUE))
    high <- c(apply(recon_samps,2,quantile, probs = 0.975,na.rm = TRUE))

  }



  if(!is.null(dat$t_ind))
  {
    t_ind <- dat$t_ind

    med <- c((med*t_ind$lambda +1)^(1/t_ind$lambda),model_data$data_calib$value)
    low <- c((low*t_ind$lambda +1)^(1/t_ind$lambda),model_data$data_calib$value)
    high <- c((high*t_ind$lambda +1)^(1/t_ind$lambda),model_data$data_calib$value)
    recon_samps <- (recon_samps*t_ind$lambda +1)^(1/t_ind$lambda)

  }


  ## save the results
  res <- data.frame(model_data$all_years,round(med,2),round(low,2),round(high,2),round(c(med_scale,model_data$data_calib$clim_scale),2),round(c(low_scale,model_data$data_calib$clim_scale),2),round(c(high_scale,model_data$data_calib$clim_scale),2))
  names(res) <- c("year","climate_variable_recon", "lower", "upper","climate_variable_recon_scale", "lower_scale", "upper_scale")

  return(list(res=res,
              recon_samps = recon_samps,
              rhat = rhat))

}
