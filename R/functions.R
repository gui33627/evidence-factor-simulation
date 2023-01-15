
# expit function
expit <- function(x) 1 / (1 + exp(-x))


# estimate using AIPW (backdoor IF)
estimate_backdooor <- function(data) {
  pi.model <- gam(A ~ s(X1) + s(X2) + s(X3) + s(X4), family = 'binomial', data = data)
  mu.model <- gam(Y ~ A + s(X1) + s(X2) + s(X3) + s(X4), family = 'gaussian', data = data)
  backdoor.eif <- aipw(pi.model, mu.model, data)
  backdoor.est <- mean(backdoor.eif)
  backdoor.eif <- backdoor.eif - backdoor.est
  
  return(list(backdoor.est = backdoor.est, backdoor.eif = backdoor.eif))
}

# estimate using APIPW (front door IF)
estimate_frontdoor <- function(data){
  A.model <- gam(A ~ s(X1) + s(X2) + s(X3) + s(X4), family = 'binomial', data = data)
  M.model <- gam(M ~ A + s(X1) + s(X2) + s(X3) + s(X4), family = 'binomial', data = data)
  Y.model <- gam(Y ~ A + M + s(X1) + s(X2) + s(X3) + s(X4), family = 'gaussian', data = data)
  frontdoor.eif <- apipw(A.model, M.model, Y.model, data)
  frontdoor.est <- mean(frontdoor.eif)
  frontdoor.eif <- frontdoor.eif - frontdoor.est
  
  return(list(frontdoor.est = frontdoor.est, frontdoor.eif = frontdoor.eif))
}

# estimate using unconditional IV (IV IF)
estimate_uiv <- function(data){
  
  z.model <- gam(Z ~ 1, family = 'binomial', data = data)
  za.model <- gam(A ~ Z, family = 'binomial', data = data)
  zy.model <- gam(Y ~ Z, family = 'gaussian', data = data)
  
  uiv <- uiv(z.model, za.model, zy.model, data)
  iv.est <- uiv$est
  iv.eif <- uiv$eif
  
  return(list(iv.est = iv.est, iv.eif = iv.eif))
}


# Evidence factor
evidence_factor <- function(est, eif){
  # est: a vector of estimates from two or more causal models, e.g. c(backdoor.est, frontdoor.est)
  # eif: a dataframe in which columns are efficient influence functions from two or more causal models, e.g. cbind(backdoor.eif, frontdoor.eif)
  
  all.est <- est
  all.eif <- eif
  Sigma.n <- cov(all.eif) # Influence function-based Covariance matrix estimate
  
  gamma.n <- c()
  for (i in 1:length(all.est)) {
    gamma.n <- c(gamma.n, prod(all.est[-i]))
  }
  
  delta.var <- c(t(gamma.n) %*% Sigma.n %*% gamma.n) # estimated variance of the product of the estimators based on delta method
  
  stat <- sqrt(n) * abs(prod(all.est)) / sqrt(delta.var)
  
  return(pnorm(stat, lower.tail = FALSE) * 2)
  
}

# # Example
# est <- c(backdoor.est, frontdoor.est)
# eif <- cbind(backdoor.eif, frontdoor.eif)
# evidence_factor(est = est, eif = eif)

