# Implementation of estimators

# augmented IPW for backdoor functional
aipw <- function(pi.model, mu.model, data) {
  
  A <- data$A ; Y <- data$Y
  pi.hats <- pi.model$fitted.values
  data$A <- 0 ; mu.hats0 <- predict(mu.model, data)
  data$A <- 1 ; mu.hats1 <- predict(mu.model, data)
  
  return((A/pi.hats*(Y - mu.hats1) + mu.hats1) - ((1-A)/(1-pi.hats)*(Y - mu.hats0) + mu.hats0))
}

# augmented primal IPW for frontdoor function
apipw <- function(A.model, M.model, Y.model, data) {
  
  A <- data$A ; M <- data$M ; Y <- data$Y
  A.hats <- A.model$fitted.values
  dataA0 <- data ; dataA0$A <- 0;
  dataA1 <- data ; dataA1$A <- 1;
  dataM0 <- data ; dataM0$M <- 0;
  dataM1 <- data ; dataM1$M <- 1;
  dataA0M0 <- data ; dataA0M0$A <- 0 ; dataA0M0$M <- 0
  dataA0M1 <- data ; dataA0M1$A <- 0 ; dataA0M1$M <- 1
  dataA1M0 <- data ; dataA1M0$A <- 1 ; dataA1M0$M <- 0
  dataA1M1 <- data ; dataA1M1$A <- 1 ; dataA1M1$M <- 1
  M.hatsA0 <- predict(M.model, dataA0, type="response")
  M.hatsA1 <- predict(M.model, dataA1, type="response")
  
  # compute \sum_a p(A|C)xE[Y|A,M,C]
  sumA.EY = (1-A.hats)*predict(Y.model, dataA0) + A.hats*predict(Y.model, dataA1)
  
  # compute \sum_m p(M|A=0,C) x \sum_a p(A|C)xE[Y|A,M,C]
  # and \sum_m p(M|A=1,C) x \sum_a p(A|C)xE[Y|A,M,C]
  sumM.A0EY = ( (1-M.hatsA0)*((1-A.hats)*predict(Y.model, dataA0M0) + A.hats*predict(Y.model, dataA1M0))
                + (M.hatsA0)*((1-A.hats)*predict(Y.model, dataA0M1) + A.hats*predict(Y.model, dataA1M1)) )

  sumM.A1EY = ( (1-M.hatsA1)*((1-A.hats)*predict(Y.model, dataA0M0) + A.hats*predict(Y.model, dataA1M0))
                + (M.hatsA1)*((1-A.hats)*predict(Y.model, dataA0M1) + A.hats*predict(Y.model, dataA1M1)) )
  
  # compute first term in IF
  term1A0 = (1-A)/(1-A.hats)*( sumA.EY - sumM.A0EY )
  term1A1 = A/A.hats * ( sumA.EY - sumM.A1EY )
  term1 = term1A1 - term1A0
  
  # compute second term in IF
  numA0 = M*M.hatsA0 + (1-M)*(1-M.hatsA0) # p(M|A=0,C)
  numA1 = M*M.hatsA1 + (1-M)*(1-M.hatsA1) # p(M|A=1,C)
  Y.hats = predict(Y.model, data)
  denom = M*predict(M.model, data, type="response") + (1-M)*(1-predict(M.model, data, type="response"))
  term2 =  numA1/denom*(Y - Y.hats) - numA0/denom*(Y - Y.hats)
  
  # compute third term in IF
  term3A0 = (1-M.hatsA0)*predict(Y.model, dataM0) + (M.hatsA0)*predict(Y.model, dataM1)
  term3A1 = (1-M.hatsA1)*predict(Y.model, dataM0) + (M.hatsA1)*predict(Y.model, dataM1)
  term3 = term3A1 - term3A0
  
  return(term1 + term2 + term3)
  #term3

}

# instrumental variable
uiv_v1 <- function(z.model, za.model, zy.model, data){
  
  Y <- data$Y; Z <- data$Z; A <- data$A
  pi.hats <- z.model$fitted.values
  # pi.hats <- mean(data$Z)
  data$Z <- 0 ; mu.hats0 <- predict(zy.model, data); a.hats0 <- predict(za.model, data)
  data$Z <- 1 ; mu.hats1 <- predict(zy.model, data); a.hats1 <- predict(za.model, data)
  
  # numerator and denominator ICs are obtained by two backdoor ICs
  ic_numerator <- (Z/pi.hats*(Y - mu.hats1) + mu.hats1) - ((1-Z)/(1-pi.hats)*(Y - mu.hats0) + mu.hats0)
  ic_denominator <- (Z/pi.hats*(A - a.hats1) + a.hats1) - ((1-Z)/(1-pi.hats)*(A - a.hats0) + a.hats0)
  
  numerator.est <- mean(ic_numerator)
  denominator.est <- mean(ic_denominator)
  
  numerator.eif <- ic_numerator - mean(ic_numerator)
  denominator.eif <- ic_denominator - mean(ic_denominator)
  
  # by delta method
  eif <- numerator.eif/denominator.est - numerator.est*denominator.eif/denominator.est^2
  
  return(list(eif = eif, est = numerator.est/denominator.est))
  
}  
  
  
uiv <- function(data){
  
  Y <- data$Y; Z <- data$Z; A <- data$A
  pi.hats <- mean(Z)
  # data$Z <- 0 ; mu.hats0 <- predict(zy.model, data); a.hats0 <- predict(za.model, data)
  # data$Z <- 1 ; mu.hats1 <- predict(zy.model, data); a.hats1 <- predict(za.model, data)
  # eif <-  (((Y - mu.hats1)*(a.hats1 - a.hats0) - (A - a.hats1)*(mu.hats1 - mu.hats0))*(Z/pi.hats)/(a.hats1 - a.hats0)^2
  #          - ((Y - mu.hats0)*(a.hats1 - a.hats0) - (A - a.hats0)*(mu.hats1 - mu.hats0))*((1-Z)/(1-pi.hats))/(a.hats1 - a.hats0)^2)
  
  mu.hats0 <- mean(Y[Z == 0])
  mu.hats1 <- mean(Y[Z == 1])
  a.hats0 <- mean(A[Z == 0])
  a.hats1 <- mean(A[Z == 1])

  eif <- ((Z/pi.hats)*(Y - mu.hats1) + mu.hats1 - ((1-Z)/(1-pi.hats)*(Y - mu.hats0) + mu.hats0))/(a.hats1 - a.hats0)
  - (mu.hats1 - mu.hats0)/(a.hats1 - a.hats0)^2*((Z/pi.hats)*(A - a.hats1) + a.hats1 - (((1-Z)/(1-pi.hats)*(A - a.hats0)) + a.hats0))
  
  return(eif)
  
}    
  




