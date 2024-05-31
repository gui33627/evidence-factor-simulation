library(mgcv)
library(tidyverse)
library(blockingChallenge)
data(wls)

# hsdm57: TRUE = Catholic schooling; FALSE = Public schooling.
# gwiiq_bm: Student’s IQ, 
# res57Dic: Urban or Rural. TRUE = Urban; FALSE = Rural.
# edfa57q.miss: logical vector indicating whether edfa57q is missing,
# edmo57q.miss: logical vectors indicating whether edmo57q is missing,
# bmpin1.miss: logical vectors indicating whether bmpin1 is missing,
# ocsf57.miss: logical vectors indicating whether ocsf57 is missing,
# edfa57q.NoNA: Father’s education with NAs replaced by the average,
# edmo57q.NoNA: Mother’s education with NAs replaced by the average,
# bmpin1.NoNA: Parent’s income with NAs replaced by the average,
# ocsf57.NoNA: Occupation score with NAs replaced by the average,
# ocpf57.NoNA: Occupational prestige score with NAs replaced by the average


## data pre-processing following the same procedure in blockingChallenge 
## documentation for replicating Karmakar, Small, and Rosenbaum (2018)
d2 <- wls
religion<-d2$relfml*1
religion<-factor(religion,levels=c(1,0),
                 labels=c("Catholic","Other"),ordered=T)
urban<-d2$res57Dic*1
urban<-factor(urban,levels=c(1,0),labels=c("Urban","Rural"),ordered=T)
school<-factor(d2$hsdm57*1,levels=c(1,0),
               labels=c("Catholic","Public"),ordered=T)
d2<-cbind(d2,religion,urban,school)
rm(religion,urban,school)
d2$yrwg74[d2$yrwg74<0]<-NA

wages<-d2$yrwg74/10 # convert from hundreds to thousands
wages[wages<0]<-NA
d2<-cbind(d2,wages)
d2<-d2[!is.na(d2$wages),]

## data and functions for the analysis
# IV = urban/rural
data_urban <- d2 %>% mutate(hsdm57 = ifelse(hsdm57 == TRUE, 1,0),
                            res57Dic = ifelse(res57Dic == TRUE, 1,0),
                            l2bmpin1.NoNA = log2(1+bmpin1.NoNA)) %>% 
  select(wages, hsdm57, res57Dic, gwiiq_bm, 
         edfa57q.miss, edmo57q.miss, bmpin1.miss, ocsf57.miss, 
         edfa57q.NoNA, edmo57q.NoNA, l2bmpin1.NoNA, ocsf57.NoNA, 
         ocpf57.NoNA) %>% 
  rename(Y = wages, A = hsdm57, Z = res57Dic,
         X1 = gwiiq_bm, X2 = edfa57q.NoNA, X3 = edmo57q.NoNA, X4 = l2bmpin1.NoNA, 
         X5 = ocsf57.NoNA, X6 = ocpf57.NoNA, X8 = edfa57q.miss,
         X9 = edmo57q.miss, X10 = bmpin1.miss, X11 = ocsf57.miss)

n <- nrow(data_urban)

# IV = catholic/non-catholic 
data_catholic <- d2 %>% mutate(hsdm57 = ifelse(hsdm57 == TRUE, 1,0),
                               relfml = ifelse(relfml == TRUE, 1,0),
                               l2bmpin1.NoNA = log2(1+bmpin1.NoNA)) %>% 
  select(wages, hsdm57, relfml, gwiiq_bm, 
         edfa57q.miss, edmo57q.miss, bmpin1.miss, ocsf57.miss, 
         edfa57q.NoNA, edmo57q.NoNA, l2bmpin1.NoNA, ocsf57.NoNA, 
         ocpf57.NoNA) %>% 
  rename(Y = wages, A = hsdm57, Z = relfml,
         X1 = gwiiq_bm, X2 = edfa57q.NoNA, X3 = edmo57q.NoNA, X4 = l2bmpin1.NoNA, 
         X5 = ocsf57.NoNA, X6 = ocpf57.NoNA, X8 = edfa57q.miss,
         X9 = edmo57q.miss, X10 = bmpin1.miss, X11 = ocsf57.miss)

# IV = urban/rural, under the alternative, assuming there is a effect of Catholic schooling
data_urban_500 <- d2 %>% mutate(hsdm57 = ifelse(hsdm57 == TRUE, 1,0),
                                res57Dic = ifelse(res57Dic == TRUE, 1,0),
                                l2bmpin1.NoNA = log2(1+bmpin1.NoNA),
                                wages1 = (yrwg74*100)-500*1*(school=="Catholic")) %>% 
  select(wages1, hsdm57, res57Dic, gwiiq_bm, 
         edfa57q.miss, edmo57q.miss, bmpin1.miss, ocsf57.miss, 
         edfa57q.NoNA, edmo57q.NoNA, l2bmpin1.NoNA, ocsf57.NoNA, 
         ocpf57.NoNA) %>% 
  rename(Y = wages1, A = hsdm57, Z = res57Dic,
         X1 = gwiiq_bm, X2 = edfa57q.NoNA, X3 = edmo57q.NoNA, X4 = l2bmpin1.NoNA, 
         X5 = ocsf57.NoNA, X6 = ocpf57.NoNA, X8 = edfa57q.miss,
         X9 = edmo57q.miss, X10 = bmpin1.miss, X11 = ocsf57.miss)

# IV = catholic/non-catholic, under the alternative, assuming there is a effect of Catholic schooling
data_catholic_500 <- d2 %>% mutate(hsdm57 = ifelse(hsdm57 == TRUE, 1,0),
                                   relfml = ifelse(relfml == TRUE, 1,0),
                                   l2bmpin1.NoNA = log2(1+bmpin1.NoNA),
                                   wages1 = (yrwg74*100)-500*1*(school=="Catholic")) %>% 
  select(wages1, hsdm57, relfml, gwiiq_bm, 
         edfa57q.miss, edmo57q.miss, bmpin1.miss, ocsf57.miss, 
         edfa57q.NoNA, edmo57q.NoNA, l2bmpin1.NoNA, ocsf57.NoNA, 
         ocpf57.NoNA) %>% 
  rename(Y = wages1, A = hsdm57, Z = relfml,
         X1 = gwiiq_bm, X2 = edfa57q.NoNA, X3 = edmo57q.NoNA, X4 = l2bmpin1.NoNA, 
         X5 = ocsf57.NoNA, X6 = ocpf57.NoNA, X8 = edfa57q.miss,
         X9 = edmo57q.miss, X10 = bmpin1.miss, X11 = ocsf57.miss)

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
  
  # two sided test
  return(pnorm(stat, lower.tail = FALSE) * 2)
  
}

# augmented IPW for backdoor functional
aipw <- function(pi.model, mu.model, data) {
  
  A <- data$A ; Y <- data$Y
  pi.hats <- pi.model$fitted.values
  data$A <- 0 ; mu.hats0 <- predict(mu.model, data)
  data$A <- 1 ; mu.hats1 <- predict(mu.model, data)
  
  return((A/pi.hats*(Y - mu.hats1) + mu.hats1) - ((1-A)/(1-pi.hats)*(Y - mu.hats0) + mu.hats0))
}

uiv <- function(data){
  
  Y <- data$Y; Z <- data$Z; A <- data$A
  pi.hats <- mean(Z)
  
  mu.hats0 <- mean(Y[Z == 0])
  mu.hats1 <- mean(Y[Z == 1])
  a.hats0 <- mean(A[Z == 0])
  a.hats1 <- mean(A[Z == 1])
  
  eif <- ((Z/pi.hats)*(Y - mu.hats1) + mu.hats1 - ((1-Z)/(1-pi.hats)*(Y - mu.hats0) + mu.hats0))/(a.hats1 - a.hats0)
  - (mu.hats1 - mu.hats0)/(a.hats1 - a.hats0)^2*((Z/pi.hats)*(A - a.hats1) + a.hats1 - (((1-Z)/(1-pi.hats)*(A - a.hats0)) + a.hats0))
  
  return(eif)
  
}    

# estimate using AIPW (backdoor IF)
estimate_backdooor <- function(data) {
  pi.model <- gam(A ~ s(X1) + X2 + X3 + s(X4) + s(X5) + s(X6) + X8 + X9 + X10 + X11, family = 'binomial', data = data)
  mu.model <- gam(Y ~ s(X1) + X2 + X3 + s(X4) + s(X5) + s(X6) + X8 + X9 + X10 + X11, family = 'gaussian', data = data)
  backdoor.eif <- aipw(pi.model, mu.model, data)
  backdoor.est <- mean(backdoor.eif)
  backdoor.eif <- backdoor.eif - backdoor.est
  
  return(list(backdoor.est = backdoor.est, backdoor.eif = backdoor.eif))
}

# estimate using unconditional IV (IV IF)
estimate_uiv <- function(data){
  
  iv.eif <- uiv(data)
  iv.est <- mean(iv.eif)
  iv.eif <- iv.eif - iv.est
  
  return(list(iv.est = iv.est, iv.eif = iv.eif))
}

######################### Asymptotic joint test ################################
### beta = 0, unconditional IV
# estimate using AIPW (backdoor IF)
ef_bdoor <- estimate_backdooor(data_catholic)
backdoor.est <- ef_bdoor$backdoor.est
backdoor.eif <- ef_bdoor$backdoor.eif

# estimate using UIV (IV = urban/rural)
ef_iv_urban <- estimate_uiv(data_urban)
iv.est_urban <- ef_iv_urban$iv.est
iv.eif_urban <- ef_iv_urban$iv.eif

# estimate using UIV (IV = catholic/non-catholic)
ef_iv_catholic <- estimate_uiv(data_catholic)
iv.est_catholic <- ef_iv_catholic$iv.est
iv.eif_catholic <- ef_iv_catholic$iv.eif

# CIs for backdoor and iv
backdoor.ci <- backdoor.est + c(-1,1) * qnorm(.975) * sd(backdoor.eif) / sqrt(n)
iv.ci_urban <- iv.est_urban + c(-1,1) * qnorm(.975) * sd(iv.eif_urban) / sqrt(n)
iv.ci_catholic <- iv.est_catholic + c(-1,1) * qnorm(.975) * sd(iv.eif_catholic) / sqrt(n)

# Marginal tests for backdoor and iv
backdoor.p <- pnorm(sqrt(n) * abs(backdoor.est) / sd(backdoor.eif), lower.tail = FALSE) * 2
iv.p_urban <- pnorm(sqrt(n) * abs(iv.est_urban) / sd(iv.eif_urban), lower.tail = FALSE) * 2
iv.p_catholic <- pnorm(sqrt(n) * abs(iv.est_catholic) / sd(iv.eif_catholic), lower.tail = FALSE) * 2

est <- c(backdoor.est, iv.est_urban, iv.est_catholic)
eif <- cbind(backdoor.eif, iv.eif_urban, iv.eif_catholic)
ef <- evidence_factor(est = est, eif = eif)

print(data.frame(Case = "H0:beta=0",
                 Marginal_test_bdoor = backdoor.p,
                 Marginal_test_iv_urban = iv.p_urban,
                 Marginal_test_iv_catholic = iv.p_catholic,
                 Asy_joint_test = ef,
                 check.names = FALSE))

cor(eif)

