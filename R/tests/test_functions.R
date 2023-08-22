# to test the correctness of the following functions under the setting of all assumptions satisfied: 
# estimate_backdoor(), estimate_frontdoor(), estimate_uiv(), and evidence_factor()

rm(list=ls())

library(mgcv)
prefix <- "/Users/junhui/Library/CloudStorage/GoogleDrive-junhuiyang@umass.edu/My Drive/UMASS/Courses/Ted_research/simulations.v7.hpc/"
sapply(list.files(pattern=".R", path = paste0(prefix, "R"), full.names = TRUE), source)

n <- 1000

######################### Backdoor, Front-door, and IV #########################
# simulate data where backdoor, front-door, and IV are correct
df <- dgp_backdoor_frontdoor_iv_all_correct(n, beta = 10)
data <- df$df

# original code to estimate and construct evidence factor p-value
# estimate using AIPW (backdoor IF)
pi.model <- gam(A ~ s(X1) + s(X2) + s(X3) + s(X4), family = 'binomial', data = data)
mu.model <- gam(Y ~ A + s(X1) + s(X2) + s(X3) + s(X4), family = 'gaussian', data = data)
backdoor.eif <- aipw(pi.model, mu.model, data)
backdoor.est <- mean(backdoor.eif)
backdoor.eif <- backdoor.eif - backdoor.est

# estimate using APIPW (front door IF)
A.model <- gam(A ~ s(X1) + s(X2) + s(X3) + s(X4), family = 'binomial', data = data)
M.model <- gam(M ~ A + s(X1) + s(X2) + s(X3) + s(X4), family = 'binomial', data = data)
Y.model <- gam(Y ~ A + M + s(X1) + s(X2) + s(X3) + s(X4), family = 'gaussian', data = data)
frontdoor.eif <- apipw(A.model, M.model, Y.model, data)
frontdoor.est <- mean(frontdoor.eif)
frontdoor.eif <- frontdoor.eif - frontdoor.est

# estimate using UIV (IV IF)
iv.eif <- uiv(data)
iv.est <- mean(iv.eif)
iv.eif <- iv.eif - iv.est

# Marginal tests for backdoor and frontdoor
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


# call the functions for the calculation to test the correctness of the functions
# estimate using AIPW (backdoor IF)
backdoor <- estimate_backdooor(data)
backdoor.est <- backdoor$backdoor.est
backdoor.eif <- backdoor$backdoor.eif

# estimate using APIPW (front door IF)
frontdoor <- estimate_frontdoor(data)
frontdoor.est <- frontdoor$frontdoor.est
frontdoor.eif <- frontdoor$frontdoor.eif

# estimate using UIV (IV IF)
iv <- estimate_uiv(data)
iv.est <- iv$iv.est
iv.eif <- iv$iv.eif

all.est <- c(backdoor.est, frontdoor.est, iv.est)
all.eif <- cbind(backdoor.eif, frontdoor.eif, iv.eif)
evidence_factor(all.est, all.eif)


############################## Backdoor and IV ################################
# simulate data where both backdoor and IV are correct
df <- dgp_backdoor_iv_both_correct(n, beta = 10)
data <- df$df

# original code to estimate and construct evidence factor p-value
# estimate using AIPW (backdoor IF)
pi.model <- gam(A ~ s(X1) + s(X2) + s(X3) + s(X4), family = 'binomial', data = data)
mu.model <- gam(Y ~ A + s(X1) + s(X2) + s(X3) + s(X4), family = 'gaussian', data = data)
backdoor.eif <- aipw(pi.model, mu.model, data)
backdoor.est <- mean(backdoor.eif)
backdoor.eif <- (backdoor.eif - backdoor.est)

# estimate using UIV (IV IF)
iv.eif <- uiv(data)
iv.est <- mean(iv.eif)
iv.eif <- iv.eif - iv.est

# Marginal tests for backdoor and frontdoor
print(pnorm(sqrt(n) * abs(backdoor.est) / sd(backdoor.eif), lower.tail = FALSE) * 2)
print(pnorm(sqrt(n) * abs(iv.est) / sd(iv.eif), lower.tail = FALSE) * 2)

# Evidence factor
both.est <- c(backdoor.est, iv.est)
both.eif <- cbind(backdoor.eif, iv.eif)
Sigma.n <- cov(both.eif) # Influence function-based Covariance matrix estimate
delta.var <- c(t(c(iv.est, backdoor.est) ) %*% Sigma.n %*% c(iv.est, backdoor.est)) # estimated variance of the product of the estimators based on delta method
stat <- sqrt(n) * abs(prod(both.est)) / sqrt(delta.var)
print(pnorm(stat, lower.tail = FALSE) * 2)

# call the functions for the calculation to test the correctness of the functions
# estimate using AIPW (backdoor IF)
backdoor <- estimate_backdooor(data)
backdoor.est <- backdoor$backdoor.est
backdoor.eif <- backdoor$backdoor.eif

# estimate using UIV (IV IF)
iv <- estimate_uiv(data)
iv.est <- iv$iv.est
iv.eif <- iv$iv.eif

both.est <- c(backdoor.est, iv.est)
both.eif <- cbind(backdoor.eif, iv.eif)
evidence_factor(both.est, both.eif)



############################## Front-door and IV ###############################
# simulate data where both frontdoor and IV are correct
df <- dgp_frontdoor_iv_both_correct(n, beta = 10)
data <- df$df

# original code to estimate and construct evidence factor p-value
# estimate using APIPW (front door IF)
A.model <- gam(A ~ s(X1) + s(X2) + s(X3) + s(X4), family = 'binomial', data = data)
M.model <- gam(M ~ A + s(X1) + s(X2) + s(X3) + s(X4), family = 'binomial', data = data)
Y.model <- gam(Y ~ A + M + s(X1) + s(X2) + s(X3) + s(X4), family = 'gaussian', data = data)
frontdoor.eif <- apipw(A.model, M.model, Y.model, data)
frontdoor.est <- mean(frontdoor.eif)
frontdoor.eif <- frontdoor.eif - frontdoor.est

# estimate using UIV (IV IF)
iv.eif <- uiv(data)
iv.est <- mean(iv.eif)
iv.eif <- iv.eif - iv.est

# Marginal tests for backdoor and frontdoor
print(pnorm(sqrt(n) * abs(frontdoor.est) / sd(frontdoor.eif), lower.tail = FALSE) * 2)
print(pnorm(sqrt(n) * abs(iv.est) / sd(iv.eif), lower.tail = FALSE) * 2)

# Evidence factor
both.est <- c(frontdoor.est, iv.est)
both.eif <- cbind(frontdoor.eif, iv.eif)
Sigma.n <- cov(both.eif) # Influence function-based Covariance matrix estimate
delta.var <- c(t(c(iv.est,frontdoor.est) ) %*% Sigma.n %*% c(iv.est, frontdoor.est)) # estimated variance of the product of the estimators based on delta method
stat <- sqrt(n) * abs(prod(both.est)) / sqrt(delta.var)
print(pnorm(stat, lower.tail = FALSE) * 2)


# call the functions for the calculation to test the correctness of the functions
# estimate using APIPW (front door IF)
frontdoor <- estimate_frontdoor(data)
frontdoor.est <- frontdoor$frontdoor.est
frontdoor.eif <- frontdoor$frontdoor.eif

# estimate using UIV (IV IF)
iv <- estimate_uiv(data)
iv.est <- iv$iv.est
iv.eif <- iv$iv.eif

both.est <- c(frontdoor.est, iv.est)
both.eif <- cbind(frontdoor.eif, iv.eif)
evidence_factor(both.est, both.eif)





