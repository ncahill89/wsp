run_mod <- function(id)
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
                               filter_for_overlap = TRUE,
                               divergence_cutoff = 3.5)

    if(nrow(dat$meta_data) !=0)
    {
      # Model -------------------------------------------------------------------
      ## run_jags_model() will run the model and return results
      mod <- run_jags_model(dat)


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

      converge <- ifelse(any(mod$rhat$rhat>1.4),"failed","passed")
      cat("The model run for ",id$climate_index, " has ",converge," convergence checks",sep="",append=TRUE, file = paste0("output/",id$catchment,"/pass.txt"), fill = TRUE)

    }
  }
}


run_jags_model <- function(dat,
                           include_lag = FALSE)

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
  reconmodel="
  model
  {

  ###Time series component

  ### prior for 1st AR term
  xrecon_j[1] ~ dnorm(0, tau.st)
  mux[1] <- 0

  ### variances/precisions for first AR(1) term
  tau.st <- tau*(1-pow(rho,2))  #Precicision for first AR term

  ### reconstruction period
  for(j in 2:n_recon)
  {
    xrecon_j[j] ~ dnorm(mux[j], sigma^-2)
    mux[j] <- rho*xrecon_j[j-1]
  }

   x_i[1] ~ dnorm(mux[n_recon + 1], sigma^-2)
   mux[n_recon + 1] <- rho*xrecon_j[n_recon]

  ### calibration period
  for(i in 2:n_calib)
  {
    x_i[i] ~ dnorm(mux[n_recon + i], sigma^-2)
    eps_i[i] ~ dnorm(0, sigma^-2)
    mux[n_recon + i] <- rho*x_i[i-1]
  }


  ### Inverse regression component

  for(i in 1:Ntot)
  {

    ### individual regressions (uncomment to chose this option)
    mu[i] <- delta[ind[i]] + beta1[ind[i]]*mux[year_ind[i]] +  beta2[ind[i]]*(mux[year_ind[i]]^2)
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
  rho ~ dunif(0,1)
  sigma ~ dt(0, 1^-2, 1)T(0,)
  tau <- pow(sigma, -2) #precision
 }
  "

  pars=c( "beta1",
          "beta2",
          "delta",
          "lambda",
          "xrecon_j",
          "sigma_y",
          "sigma",
          "mux",
          "mu",
          "rho")

    ## 3)RUN THE MODEL
  run.mcmc <- suppressWarnings(jags(data=jags_data,
                   parameters.to.save=pars,
                   model.file=textConnection(reconmodel),
                   n.iter = 5000,
                   n.burnin = 1000))


  rhat <- tibble(par_names = rownames(run.mcmc$BUGSoutput$summary),
                  rhat = run.mcmc$BUGSoutput$summary[,8])

  rhat <- rhat %>% filter(stringr::str_detect(par_names, "beta") | stringr::str_detect(par_names, "delta")|par_names %in% c("sigma","rho")|stringr::str_detect(par_names, "sigma_y"))

  mcmc.jags<-run.mcmc$BUGSoutput$sims.list

  med_scale<-c(apply(mcmc.jags$xrecon_j,2,median))
  low_scale<-c(apply(mcmc.jags$xrecon_j,2,quantile,0.025))
  high_scale<-c(apply(mcmc.jags$xrecon_j,2,quantile,0.975))

  if(is.null(dat$t_ind))
  {
    scale_mean <- mean(model_data$data_calib$value)
    scale_sd <- sd(model_data$data_calib$value)

    med <- c(((med_scale*scale_sd) + scale_mean),model_data$data_calib$value)
    low <- c(((low_scale*scale_sd) + scale_mean),model_data$data_calib$value)
    high <- c(((high_scale*scale_sd) + scale_mean),model_data$data_calib$value)

  }

  if(!is.null(dat$t_ind))
  {
    scale_mean <- mean(model_data$data_calib$value_transformed)
    scale_sd <- sd(model_data$data_calib$value_transformed)

    med <- (med_scale*scale_sd) + scale_mean
    low <- (low_scale*scale_sd) + scale_mean
    high <- (high_scale*scale_sd) + scale_mean

  }

    recon_samps <- (mcmc.jags$xrecon_j*scale_sd) + scale_mean


  if(!is.null(dat$t_ind))
  {
    t_ind <- dat$t_ind

    med <- c((med*t_ind$lambda +1)^(1/t_ind$lambda),model_data$data_calib$value)
    low <- c((low*t_ind$lambda +1)^(1/t_ind$lambda),model_data$data_calib$value)
    high <- c((high*t_ind$lambda +1)^(1/t_ind$lambda),model_data$data_calib$value)

  }
  ## save the results
  res <- data.frame(model_data$all_years,round(med,2),round(low,2),round(high,2),round(c(med_scale,model_data$data_calib$clim_scale),2),round(c(low_scale,model_data$data_calib$clim_scale),2),round(c(high_scale,model_data$data_calib$clim_scale),2))
  names(res) <- c("year","climate_variable_recon", "lower", "upper","climate_variable_recon_scale", "lower_scale", "upper_scale")

  return(list(res=res,
              recon_samps = recon_samps,
              rhat = rhat))

}
