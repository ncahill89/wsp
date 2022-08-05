Preliminary Time Series Analysis for instrumental Hydroclimate data
================

## Introduction
In this vignette we work through a time series analysis of instrumental hydroclimate data using the `fable` and `fpp3` R packages and case study examples from the paper `A Bayesian Hierarchical Time Series Model for Reconstructing Hydroclimate from Multiple Proxies`. Data has been obtained from the [PaleoWise Database](https://palaeoclimate.com.au/project-outputs/access-the-palaeowise-database/). 

## Required packages and functions

```{r}
library(fpp3)
library(fable)
library(tidyverse)

## function for boxcox transformation
BCres <- function(datax) {
  
  result <- MASS::boxcox(datax~1, lambda = seq(-10,10,0.25))
  mylambda <-  result$x[which.max(result$y)]
  
  tformed <- (datax^mylambda-1)/mylambda
  
  reversed <- (tformed*mylambda +1)^(1/mylambda)
  
  return(tformed)  
}
```
## Required data

These data files can be found in [this github repository](https://github.com/ncahill89/wsp).

```{r}
climate_indices <- readRDS("data_all/climate_indices.rds")
combined_catchment_data <- read_csv("data_all/combined_catchment_data.csv")
```

## Analysis of rainfall in Fitzroy

Firstly, we need to format the data for use with the `fable` package. We'll start with the rainfall index (RFI) in the Fitzroy catchment. Note the RFI is transformed using a boxcox transformation. Here is the code for formatting the data: 

```{r}
dat <- tsibble(
  year = combined_catchment_data$year,
  group = combined_catchment_data$index,
  value = combined_catchment_data$fitzroy,
  key = group,
  index = year)

dat_rf <- dat %>%
  filter(group == climate_indices[3]) %>%
  mutate(value = BCres(value))
```

Now that the data are formatted, we can look at the time series, ACF and PACF plots.

```{r}
dat_rf %>% 
  gg_tsdisplay(value, plot_type='partial') +
  ggtitle("Time Series Analysis for RFI - Fitzroy")
```

Based on the plots above, which show a significant lag in the ACF and PCF plots, we will compare 3 time series models for these data:

1. Moving average of order 1 - MA(1) 

2. Autoregressive of order 1 -  AR(1)

3. Autoregessive Moving average - ARMA(1,1) 


### MA(1) model

The code below will fit the MA(1) model and produce a residual analysis. If the model is adequate in capturing the information in the time series then the residuals should be uncorrelated, white noise. 


```{r}
fit1 <- dat_rf %>%
  model(arima = ARIMA(value ~  pdq(0,0,1))) 

report(fit1)

fit1 %>% 
  gg_tsresiduals()
```

### model output
```
Series: value 
Model: ARIMA(0,0,1) w/ mean 

Coefficients:
         ma1  constant
      0.2129    9.1613
s.e.  0.0874    0.0581

sigma^2 estimated as 0.301:  log likelihood=-104.62
AIC=215.25   AICc=215.44   BIC=223.83
```

### AR(1) model

The code below will fit the AR(1) model and produce a residual analysis. If the model is adequate in capturing the information in the time series then the residuals should be uncorrelated, white noise. 

```{r}
fit2 <- dat_rf %>%
  model(arima = ARIMA(value ~  pdq(1,0,0))) 

report(fit2)

fit2 %>% 
  gg_tsresiduals()
```

### model output
```
Series: value 
Model: ARIMA(1,0,0) w/ mean 

Coefficients:
         ar1  constant
      0.1996    7.3330
s.e.  0.0884    0.0479

sigma^2 estimated as 0.302:  log likelihood=-104.83
AIC=215.65   AICc=215.84   BIC=224.23
```

Both the MA(1) and AR(1) models have AIC values of ~215 and residual analysis suggests that either model is appropriate. 

### ARMA(1,1) model

The code below will fit the AR(1,1) model and produce a residual analysis. 

```{r}
fit3 <- dat_rf %>%
  model(arima = ARIMA(value ~  pdq(1,0,1))) 

report(fit3)

fit3 %>% 
  gg_tsresiduals()
```

### model output
```
Series: value 
Model: ARIMA(1,0,1) w/ mean 

Coefficients:
          ar1     ma1  constant
      -0.0389  0.2492    9.5171
s.e.   0.3711  0.3547    0.0598

sigma^2 estimated as 0.3034:  log likelihood=-104.62
AIC=217.24   AICc=217.56   BIC=228.68
```

The added complexity of the ARMA(1,1) does not appear necessary and does not improve the AIC (217). 


## Analysis of rainfall in Brisbane

Now, we'll look at the rainfall index (RFI) in the Brisbane catchment. Note the RFI is transformed using a boxcox transformation. Here is the code for formatting the data: 

```{r}
dat <- tsibble(
  year = combined_catchment_data$year,
  group = combined_catchment_data$index,
  value = combined_catchment_data$brisbane,
  key = group,
  index = year)

dat_rf <- dat %>%
  filter(group == climate_indices[3]) %>%
  mutate(value = BCres(value))
```

Now that the data are formatted, we can look at the time series, ACF and PACF plots.

```{r, echo = FALSE}
dat_rf %>% 
  gg_tsdisplay(value, plot_type='partial') +
  ggtitle("Time Series Analysis for RFI - Brisbane")
```

The ACF and PACF plots don't indicate any significant autocorrelation so we will compare two time series models for these data:

1. Moving average of order 1 - MA(1) 

2. Autoregressive of order 1 -  AR(1)

### MA(1) model

The code below will fit the MA(1) model and produce a residual analysis. If the model is adequate in capturing the information in the time series then the residuals should be uncorrelated, white noise. 


```{r}
fit1 <- dat_rf %>%
  model(arima = ARIMA(value ~  pdq(0,0,1))) 

report(fit1)

fit1 %>% 
  gg_tsresiduals()
```

### model output
```
Series: value 
Model: ARIMA(0,0,1) w/ mean 

Coefficients:
         ma1  constant
      0.0587    1.9150
s.e.  0.0900    0.0008

sigma^2 estimated as 7.048e-05:  log likelihood=434.59
AIC=-863.18   AICc=-862.99   BIC=-854.6
```

### AR(1) model

The code below will fit the AR(1) model and produce a residual analysis. If the model is adequate in capturing the information in the time series then the residuals should be uncorrelated, white noise. 

```{r}
fit2 <- dat_rf %>%
  model(arima = ARIMA(value ~  pdq(1,0,0))) 

report(fit2)

fit2 %>% 
  gg_tsresiduals()
```

### model output
```
Series: value 
Model: ARIMA(1,0,0) w/ mean 

Coefficients:
         ar1  constant
      0.0585    1.8030
s.e.  0.0894    0.0007

sigma^2 estimated as 7.048e-05:  log likelihood=434.59
AIC=-863.18   AICc=-862.99   BIC=-854.6
```

Both the MA(1) and AR(1) models have AIC values of -863 and residual analysis suggests that either model is appropriate. 

## Looking at other climate indices

To look at SPEI(12) in the Fitzroy/Brisbane catchment(s), choose `climate_indices[4]` when formatting the data and remove the boxcox transformation. Here's an example for Fitzroy:

```{r}
dat <- tsibble(
  year = combined_catchment_data$year,
  group = combined_catchment_data$index,
  value = combined_catchment_data$fitzroy,
  key = group,
  index = year)

dat_spei <- dat %>%
  filter(group == climate_indices[4]) %>%
  mutate(value = value)
```

Then rerun the code, similar to above. Note results for SPEI(12) are very similar to those for the RFI in both Fitzroy and Brisbane.

