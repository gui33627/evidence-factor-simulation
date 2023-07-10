# Framingham Data Analysis

# Outcome Y (continuous): glucose
# Treatment A (Binary): smoking status (smoke_bin) 
# Mediator (Binary): Hypertension (hyperten)
# Instrumental Variable (Binary): past history of hypertension (prevhyp)
# Confounders: age, sex, BMI, past history of heart disease (prevchd), past glucose level


rm(list=ls())

library(mgcv)
library(tidyverse)
prefix <- "/Users/junhui/Library/CloudStorage/GoogleDrive-junhuiyang@umass.edu/My Drive/UMASS/simulations.v7.hpc/"
sapply(list.files(pattern=".R", path = paste0(prefix, "R"), full.names = TRUE), source)

df <- read.csv("/Users/junhui/Library/CloudStorage/GoogleDrive-junhuiyang@umass.edu/My Drive/UMASS/Framingham/framingham_data_out.csv")
data <- df %>% select(diabetes_cont_2, smoke_bin_1, hyperten_bin_2, prevhyp_bin_1, 
                      age_cont_1, sex_bin_1, bmi_cont_1, prevchd_bin_1, prediabetes_cont_1) %>% 
  rename(Y = diabetes_cont_2, A = smoke_bin_1, M = hyperten_bin_2, Z = prevhyp_bin_1,
         X1 = age_cont_1, X2 = sex_bin_1, X3 = bmi_cont_1, X4 = prevchd_bin_1, X5 = prediabetes_cont_1)

n <- nrow(data)

# estimate using AIPW (backdoor IF)
pi.model <- gam(A ~ s(X1) + X2 + s(X3) + X4 + s(X5), family = 'binomial', data = data)
mu.model <- gam(Y ~ A + s(X1) + X2 + s(X3) + X4 + s(X5), family = 'gaussian', data = data)
backdoor.eif <- aipw(pi.model, mu.model, data)
backdoor.est <- mean(backdoor.eif)
backdoor.eif <- backdoor.eif - backdoor.est

# estimate using APIPW (front door IF)
A.model <- gam(A ~ s(X1) + X2 + s(X3) + X4 + s(X5), family = 'binomial', data = data)
M.model <- gam(M ~ A + s(X1) + X2 + s(X3) + X4 + s(X5), family = 'binomial', data = data)
Y.model <- gam(Y ~ A + M + s(X1) + X2 + s(X3) + X4 + s(X5), family = 'gaussian', data = data)
frontdoor.eif <- apipw(A.model, M.model, Y.model, data)
frontdoor.est <- mean(frontdoor.eif)
frontdoor.eif <- frontdoor.eif - frontdoor.est

# estimate using UIV (IV IF)
iv.eif <- uiv(data)
iv.est <- mean(iv.eif)
iv.eif <- iv.eif - iv.est

# CIs for backdoor, front-door, and iv
print(backdoor.est + c(-1,1) * qnorm(.975) * sd(backdoor.eif) / sqrt(n))
print(frontdoor.est + c(-1,1) * qnorm(.975) * sd(frontdoor.eif) / sqrt(n))
print(iv.est + c(-1,1) * qnorm(.975) * sd(iv.eif) / sqrt(n))

# Marginal tests for backdoor, front-door, and iv
print(pnorm(sqrt(n) * abs(backdoor.est) / sd(backdoor.eif), lower.tail = FALSE) * 2)
print(pnorm(sqrt(n) * abs(frontdoor.est) / sd(frontdoor.eif), lower.tail = FALSE) * 2)
print(pnorm(sqrt(n) * abs(iv.est) / sd(iv.eif), lower.tail = FALSE) * 2)

# Evidence factor
all.est <- c(backdoor.est, frontdoor.est, iv.est)
all.eif <- cbind(backdoor.eif, frontdoor.eif, iv.eif)
Sigma.n <- cov(all.eif) # Influence function-based Covariance matrix estimate
gamma <- c(frontdoor.est*iv.est, backdoor.est*iv.est, backdoor.est*frontdoor.est)
delta.var <- c(t(gamma ) %*% Sigma.n %*% gamma) # estimated variance of the product of the estimators based on delta method
stat <- sqrt(n) * abs(prod(all.est)) / sqrt(delta.var)
print(pnorm(stat, lower.tail = FALSE) * 2)








