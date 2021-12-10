BCres <- function(datax) {

  result <- MASS::boxcox(datax~1, lambda = seq(-10,10,0.25))
  mylambda <-  result$x[which.max(result$y)]

  tformed <- (datax^mylambda-1)/mylambda

  reversed <- (tformed*mylambda +1)^(1/mylambda)

  list(lambda = mylambda,
       original = datax,
       transformed = tformed,
       reversed = reversed)

}
