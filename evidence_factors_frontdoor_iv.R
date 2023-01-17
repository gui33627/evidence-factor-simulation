
rm(list=ls())

library(mgcv)
sapply(list.files(pattern=".R", path="./R", full.names = TRUE), source)

# simulation times
N <- 500
# sample size
n <- 1000


############################## Front-door and IV ###############################
# simulate data where both frontdoor and IV are correct and they should agree
df <- dgp_frontdoor_iv_both_correct(n)
data <- df$df

# estimate using APIPW (front door IF)
frontdoor <- estimate_frontdoor(data)
frontdoor.est <- frontdoor$frontdoor.est
frontdoor.eif <- frontdoor$frontdoor.eif

# estimate using UIV (IV IF)
iv <- estimate_uiv(data)
iv.est <- iv$iv.est
iv.eif <- iv$iv.eif

# Marginal tests for backdoor and frontdoor
print(pnorm(sqrt(n) * abs(frontdoor.est) / sd(frontdoor.eif), lower.tail = FALSE) * 2)
print(pnorm(sqrt(n) * abs(iv.est) / sd(iv.eif), lower.tail = FALSE) * 2)

# Evidence factor
both.est <- c(frontdoor.est, iv.est)
both.eif <- cbind(frontdoor.eif, iv.eif)
p <- evidence_factor(both.est, both.eif)
p




# ----------------------------- Simulation study -------------------------------
########################## 1. Under Null Hypothesis ############################
### a) both models are correct
p_values_null_both_correct <- c()
for (i in 1:N) {
  
  df <- dgp_frontdoor_iv_both_correct(n = n, beta = 0)
  data <- df$df
  
  # estimate using APIPW (front door IF)
  frontdoor <- estimate_frontdoor(data)
  frontdoor.est <- frontdoor$frontdoor.est
  frontdoor.eif <- frontdoor$frontdoor.eif
  
  # estimate using UIV (IV IF)
  iv <- estimate_uiv(data)
  iv.est <- iv$iv.est
  iv.eif <- iv$iv.eif
  
  # Evidence factor
  est <- c(frontdoor.est, iv.est)
  eif <- cbind(frontdoor.eif, iv.eif)
  p <- evidence_factor(est = est, eif = eif)
  p_values_null_both_correct <- c(p_values_null_both_correct, p)
  print(paste0(i,"th simulation done"))
}

typeI <- sum(p_values_null_both_correct <= 0.05)/length(p_values_null_both_correct)
power <- typeI
size <- power

# [1] 0


### b) front-door is true, iv is not
# i1 is violated (there is a backdoor path from Z to Y) 
# so when beta = 0, *the identified iv functional is not zero*
p_values_null_fdoor_correct_i1_violated <- c()
for (i in 1:N) {
  
  df <- dgp_frontdoor_iv_fdoor_correct_i1_violated(n = n, beta = 0)
  data <- df$df
  
  # estimate using APIPW (front door IF)
  frontdoor <- estimate_frontdoor(data)
  frontdoor.est <- frontdoor$frontdoor.est
  frontdoor.eif <- frontdoor$frontdoor.eif
  
  # estimate using UIV (IV IF)
  iv <- estimate_uiv(data)
  iv.est <- iv$iv.est
  iv.eif <- iv$iv.eif
  
  # Evidence factor
  est <- c(frontdoor.est, iv.est)
  eif <- cbind(frontdoor.eif, iv.eif)
  p <- evidence_factor(est = est, eif = eif)
  p_values_null_fdoor_correct_i1_violated <- c(p_values_null_fdoor_correct_i1_violated, p)
  print(paste0(i,"th simulation done"))
}

typeI <- sum(p_values_null_fdoor_correct_i1_violated <= 0.05)/length(p_values_null_fdoor_correct_i1_violated)
power <- typeI
size <- power

# [1] 0.032


# i2 is violated (there is a direct effect from Z to Y),
# so when beta = 0, *the identified iv functional is not zero*
p_values_null_fdoor_correct_i2_violated <- c()
for (i in 1:N) {
  
  df <- dgp_frontdoor_iv_fdoor_correct_i2_violated(n = n, beta = 0)
  data <- df$df
  
  # estimate using APIPW (front door IF)
  frontdoor <- estimate_frontdoor(data)
  frontdoor.est <- frontdoor$frontdoor.est
  frontdoor.eif <- frontdoor$frontdoor.eif
  
  # estimate using UIV (IV IF)
  iv <- estimate_uiv(data)
  iv.est <- iv$iv.est
  iv.eif <- iv$iv.eif
  
  # Evidence factor
  est <- c(frontdoor.est, iv.est)
  eif <- cbind(frontdoor.eif, iv.eif)
  p <- evidence_factor(est = est, eif = eif)
  p_values_null_fdoor_correct_i2_violated <- c(p_values_null_fdoor_correct_i2_violated, p)
  print(paste0(i,"th simulation done"))
}

typeI <- sum(p_values_null_fdoor_correct_i2_violated <= 0.05)/length(p_values_null_fdoor_correct_i2_violated)
power <- typeI
size <- power

# [1] 0.044

# i3 is violated
# there is no backdoor path/direct effect from Z to Y, and the front door
# is true (M fully mediate and beta = 0),  *so the identified iv functional is zero*
p_values_null_fdoor_correct_i3_violated <- c()
for (i in 1:N) {
  
  df <- dgp_frontdoor_iv_fdoor_correct_i3_violated(n = n, beta = 0)
  data <- df$df
  
  # estimate using APIPW (front door IF)
  frontdoor <- estimate_frontdoor(data)
  frontdoor.est <- frontdoor$frontdoor.est
  frontdoor.eif <- frontdoor$frontdoor.eif
  
  # estimate using UIV (IV IF)
  iv <- estimate_uiv(data)
  iv.est <- iv$iv.est
  iv.eif <- iv$iv.eif
  
  # Evidence factor
  est <- c(frontdoor.est, iv.est)
  eif <- cbind(frontdoor.eif, iv.eif)
  p <- evidence_factor(est = est, eif = eif)
  p_values_null_fdoor_correct_i3_violated <- c(p_values_null_fdoor_correct_i3_violated, p)
  print(paste0(i,"th simulation done"))
}

typeI <- sum(p_values_null_fdoor_correct_i3_violated <= 0.05)/length(p_values_null_fdoor_correct_i3_violated)
power <- typeI
size <- power

# [1] 0


# i4 is violated 
# no association between Z and A, and no direct effect from Z to Y, 
# *so the identified iv functional is zero*
p_values_null_fdoor_correct_i4_violated <- c()
for (i in 1:N) {
  
  df <- dgp_frontdoor_iv_fdoor_correct_i4_violated(n = n, beta = 0)
  data <- df$df
  
  # estimate using APIPW (front door IF)
  frontdoor <- estimate_frontdoor(data)
  frontdoor.est <- frontdoor$frontdoor.est
  frontdoor.eif <- frontdoor$frontdoor.eif
  
  # estimate using UIV (IV IF)
  iv <- estimate_uiv(data)
  iv.est <- iv$iv.est
  iv.eif <- iv$iv.eif
  
  # Evidence factor
  est <- c(frontdoor.est, iv.est)
  eif <- cbind(frontdoor.eif, iv.eif)
  p <- evidence_factor(est = est, eif = eif)
  p_values_null_fdoor_correct_i4_violated <- c(p_values_null_fdoor_correct_i4_violated, p)
  print(paste0(i,"th simulation done"))
}

typeI <- sum(p_values_null_fdoor_correct_i4_violated <= 0.05)/length(p_values_null_fdoor_correct_i4_violated)
power <- typeI
size <- power

# [1] 0


### c) iv is true, front-door is not 
# f1 is violated, *the identified front-door functional is zero*
p_values_null_iv_correct_f1_violated <- c()
for (i in 1:N) {
  
  df <- dgp_frontdoor_iv_iv_correct_f1_violated(n = n, beta = 0)
  data <- df$df
  
  # estimate using APIPW (front door IF)
  frontdoor <- estimate_frontdoor(data)
  frontdoor.est <- frontdoor$frontdoor.est
  frontdoor.eif <- frontdoor$frontdoor.eif
  
  # estimate using UIV (IV IF)
  iv <- estimate_uiv(data)
  iv.est <- iv$iv.est
  iv.eif <- iv$iv.eif
  
  # Evidence factor
  est <- c(frontdoor.est, iv.est)
  eif <- cbind(frontdoor.eif, iv.eif)
  p <- evidence_factor(est = est, eif = eif)
  p_values_null_iv_correct_f1_violated <- c(p_values_null_iv_correct_f1_violated, p)
  print(paste0(i,"th simulation done"))
}

typeI <- sum(p_values_null_iv_correct_f1_violated <= 0.05)/length(p_values_null_iv_correct_f1_violated)
power <- typeI
size <- power

# [1] 0


# f3 is violated 
# there is a backdoor path from M to Y, so when beta = 0,
# *the identified front-door functional is not zero*
p_values_null_iv_correct_f3_violated <- c()
for (i in 1:N) {
  
  df <- dgp_frontdoor_iv_iv_correct_f3_violated(n = n, beta = 0)
  data <- df$df
  
  # estimate using APIPW (front door IF)
  frontdoor <- estimate_frontdoor(data)
  frontdoor.est <- frontdoor$frontdoor.est
  frontdoor.eif <- frontdoor$frontdoor.eif
  
  # estimate using UIV (IV IF)
  iv <- estimate_uiv(data)
  iv.est <- iv$iv.est
  iv.eif <- iv$iv.eif
  
  # Evidence factor
  est <- c(frontdoor.est, iv.est)
  eif <- cbind(frontdoor.eif, iv.eif)
  p <- evidence_factor(est = est, eif = eif)
  p_values_null_iv_correct_f3_violated <- c(p_values_null_iv_correct_f3_violated, p)
  print(paste0(i,"th simulation done"))
}

typeI <- sum(p_values_null_iv_correct_f3_violated <= 0.05)/length(p_values_null_iv_correct_f3_violated)
power <- typeI
size <- power

# [1] 0.05



####################### 2. Under Alternative Hypothesis ########################
### a) both phi_k != 0, one of the models is correct 
# a.1) front door is correct, iv is wrong
p_values_alternative_fdoor_correct_i2_violated <- c()
for (i in 1:N) {
  
  df <- dgp_frontdoor_iv_fdoor_correct_i2_violated(n = n, beta = 10)
  data <- df$df
  
  # estimate using APIPW (front door IF)
  frontdoor <- estimate_frontdoor(data)
  frontdoor.est <- frontdoor$frontdoor.est
  frontdoor.eif <- frontdoor$frontdoor.eif
  
  # estimate using UIV (IV IF)
  iv <- estimate_uiv(data)
  iv.est <- iv$iv.est
  iv.eif <- iv$iv.eif
  
  # Evidence factor
  est <- c(frontdoor.est, iv.est)
  eif <- cbind(frontdoor.eif, iv.eif)
  p <- evidence_factor(est = est, eif = eif)
  p_values_alternative_fdoor_correct_i2_violated <- c(p_values_alternative_fdoor_correct_i2_violated, p)
  print(paste0(i,"th simulation done"))
}

typeII <- sum(p_values_alternative_fdoor_correct_i2_violated > 0.05)/length(p_values_alternative_fdoor_correct_i2_violated)
power <- 1-typeII

# [1] 1


# a.2) iv is correct, front door is wrong
p_values_alternative_iv_correct_f3_violated <- c()
for (i in 1:N) {
  
  df <- dgp_frontdoor_iv_iv_correct_f3_violated(n = n, beta = 10)
  data <- df$df
  
  # estimate using APIPW (front door IF)
  frontdoor <- estimate_frontdoor(data)
  frontdoor.est <- frontdoor$frontdoor.est
  frontdoor.eif <- frontdoor$frontdoor.eif
  
  # estimate using UIV (IV IF)
  iv <- estimate_uiv(data)
  iv.est <- iv$iv.est
  iv.eif <- iv$iv.eif
  
  # Evidence factor
  est <- c(frontdoor.est, iv.est)
  eif <- cbind(frontdoor.eif, iv.eif)
  p <- evidence_factor(est = est, eif = eif)
  p_values_alternative_iv_correct_f3_violated <- c(p_values_alternative_iv_correct_f3_violated, p)
  print(paste0(i,"th simulation done"))
}

typeII <- sum(p_values_alternative_iv_correct_f3_violated > 0.05)/length(p_values_alternative_iv_correct_f3_violated)
power <- 1-typeII

# [1] 1



### b) both phi_k != 0, both models are correct
p_values_alternative_both_correct <- c()
for (i in 1:N) {
  
  df <- dgp_frontdoor_iv_both_correct(n = n, beta = 10)
  data <- df$df
  
  # estimate using APIPW (front door IF)
  frontdoor <- estimate_frontdoor(data)
  frontdoor.est <- frontdoor$frontdoor.est
  frontdoor.eif <- frontdoor$frontdoor.eif
  
  # estimate using UIV (IV IF)
  iv <- estimate_uiv(data)
  iv.est <- iv$iv.est
  iv.eif <- iv$iv.eif
  
  # Evidence factor
  est <- c(frontdoor.est, iv.est)
  eif <- cbind(frontdoor.eif, iv.eif)
  p <- evidence_factor(est = est, eif = eif)
  p_values_alternative_both_correct <- c(p_values_alternative_both_correct, p)
  print(paste0(i,"th simulation done"))
}

typeII <- sum(p_values_alternative_both_correct > 0.05)/length(p_values_alternative_both_correct)
power <- 1-typeII

# [1] 1


### c) one of phi_k = 0, one of the model is correct 
# c.1) iv correct, front door is wrong
p_values_alternative_iv_correct_f1_violated <- c()
for (i in 1:N) {
  
  df <- dgp_frontdoor_iv_iv_correct_f1_violated(n = n, beta = 10)
  data <- df$df
  
  # estimate using APIPW (front door IF)
  frontdoor <- estimate_frontdoor(data)
  frontdoor.est <- frontdoor$frontdoor.est
  frontdoor.eif <- frontdoor$frontdoor.eif
  
  # estimate using UIV (IV IF)
  iv <- estimate_uiv(data)
  iv.est <- iv$iv.est
  iv.eif <- iv$iv.eif
  
  # Evidence factor
  est <- c(frontdoor.est, iv.est)
  eif <- cbind(frontdoor.eif, iv.eif)
  p <- evidence_factor(est = est, eif = eif)
  p_values_alternative_iv_correct_f1_violated <- c(p_values_alternative_iv_correct_f1_violated, p)
  print(paste0(i,"th simulation done"))
}

typeII <- sum(p_values_alternative_iv_correct_f1_violated > 0.05)/length(p_values_alternative_iv_correct_f1_violated)
power <- 1-typeII

# [1] 0.06

# c.2) front door correct, iv is wrong
p_values_alternative_fdoor_correct_i4_violated <- c()
for (i in 1:N) {
  
  df <- dgp_frontdoor_iv_fdoor_correct_i4_violated(n = n, beta = 10)
  data <- df$df
  
  # estimate using APIPW (front door IF)
  frontdoor <- estimate_frontdoor(data)
  frontdoor.est <- frontdoor$frontdoor.est
  frontdoor.eif <- frontdoor$frontdoor.eif
  
  # estimate using UIV (IV IF)
  iv <- estimate_uiv(data)
  iv.est <- iv$iv.est
  iv.eif <- iv$iv.eif
  
  # Evidence factor
  est <- c(frontdoor.est, iv.est)
  eif <- cbind(frontdoor.eif, iv.eif)
  p <- evidence_factor(est = est, eif = eif)
  p_values_alternative_fdoor_correct_i4_violated <- c(p_values_alternative_fdoor_correct_i4_violated, p)
  print(paste0(i,"th simulation done"))
}

typeII <- sum(p_values_alternative_fdoor_correct_i4_violated > 0.05)/length(p_values_alternative_fdoor_correct_i4_violated)
power <- 1-typeII

# [1] 0


