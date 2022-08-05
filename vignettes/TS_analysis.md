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

