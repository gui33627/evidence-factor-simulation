
rm(list=ls())

library(mgcv)
sapply(list.files(pattern=".R", path="./R", full.names = TRUE), source)

# simulation times
N <- 500
# sample size
n <- 1000


############################## Backdoor and IV ################################
# simulate data where both backdoor and IV are correct and they should agree
df <- dgp_backdoor_iv_both_correct(n)
data <- df$df

# estimate using AIPW (backdoor IF)
backdoor <- estimate_backdooor(data)
backdoor.est <- backdoor$backdoor.est
backdoor.eif <- backdoor$backdoor.eif

# estimate using UIV (IV IF)
iv <- estimate_uiv(data)
iv.est <- iv$iv.est
iv.eif <- iv$iv.eif

# Marginal tests for backdoor and frontdoor
print(pnorm(sqrt(n) * abs(backdoor.est) / sd(backdoor.eif), lower.tail = FALSE) * 2)
print(pnorm(sqrt(n) * abs(iv.est) / sd(iv.eif), lower.tail = FALSE) * 2)

# Evidence factor
both.est <- c(backdoor.est, iv.est)
both.eif <- cbind(backdoor.eif, iv.eif)
p <- evidence_factor(both.est, both.eif)
p




# ----------------------------- Simulation study -------------------------------
########################## 1. Under Null Hypothesis ############################
### a) both models are correct
p_values_null_both_correct <- c()
for (i in 1:N) {
  
  df <- dgp_backdoor_iv_both_correct(n = n, beta = 0)
  data <- df$df
  
  # estimate using AIPW (backdoor IF)
  backdoor <- estimate_backdooor(data)
  backdoor.est <- backdoor$backdoor.est
  backdoor.eif <- backdoor$backdoor.eif
  
  # estimate using UIV (IV IF)
  iv <- estimate_uiv(data)
  iv.est <- iv$iv.est
  iv.eif <- iv$iv.eif
  
  # Evidence factor
  est <- c(backdoor.est, iv.est)
  eif <- cbind(backdoor.eif, iv.eif)
  p <- evidence_factor(est = est, eif = eif)
  p_values_null_both_correct <- c(p_values_null_both_correct, p)
  print(paste0(i,"th simulation done"))
}

typeI <- sum(p_values_null_both_correct <= 0.05)/length(p_values_null_both_correct)
power <- typeI
size <- power

# [1] 0

### b) backdoor is correct, iv is not
# i3 is violated
# since Z only affects Y through A and beta = 0, *the identified iv functional is zero*
p_values_null_bdoor_correct_i3_violated <- c()
for (i in 1:N) {
  
  df <- dgp_backdoor_iv_bdoor_correct_i3_violated(n = n, beta = 0, beta_defier = 0)
  data <- df$df
  
  # estimate using AIPW (backdoor IF)
  backdoor <- estimate_backdooor(data)
  backdoor.est <- backdoor$backdoor.est
  backdoor.eif <- backdoor$backdoor.eif
  
  # estimate using UIV (IV IF)
  iv <- estimate_uiv(data)
  iv.est <- iv$iv.est
  iv.eif <- iv$iv.eif
  
  # Evidence factor
  est <- c(backdoor.est, iv.est)
  eif <- cbind(backdoor.eif, iv.eif)
  p <- evidence_factor(est = est, eif = eif)
  p_values_null_bdoor_correct_i3_violated <- c(p_values_null_bdoor_correct_i3_violated, p)
  print(paste0(i,"th simulation done"))
}

typeI <- sum(p_values_null_bdoor_correct_i3_violated <= 0.05)/length(p_values_null_bdoor_correct_i3_violated)
power <- typeI
size <- power

# [1] 0


# i4 violated
# there is no association between Z and A, and no direct effect from Z to Y,
# *the identified iv functional is zero*
p_values_null_bdoor_correct_i4_violated <- c()
for (i in 1:N) {
  
  df <- dgp_backdoor_iv_bdoor_correct_i4_violated(n = n, beta = 0)
  data <- df$df
  
  # estimate using AIPW (backdoor IF)
  backdoor <- estimate_backdooor(data)
  backdoor.est <- backdoor$backdoor.est
  backdoor.eif <- backdoor$backdoor.eif
  
  # estimate using UIV (IV IF)
  iv <- estimate_uiv(data)
  iv.est <- iv$iv.est
  iv.eif <- iv$iv.eif
  
  # Evidence factor
  est <- c(backdoor.est, iv.est)
  eif <- cbind(backdoor.eif, iv.eif)
  p <- evidence_factor(est = est, eif = eif)
  p_values_null_bdoor_correct_i4_violated <- c(p_values_null_bdoor_correct_i4_violated, p)
  print(paste0(i,"th simulation done"))
}

typeI <- sum(p_values_null_bdoor_correct_i4_violated <= 0.05)/length(p_values_null_bdoor_correct_i4_violated)
power <- typeI
size <- power

# [1] 0


### c) iv is correct, backdoor is not 
# b2 is violated, conditioning on C opens a path from A to Y,
# so when beta = 0, *the identified backdoor functional is not zero*
p_values_null_iv_correct_b2_violated <- c()
for (i in 1:N) {
  
  df <- dgp_backdoor_iv_iv_correct_b2_violated(n = n, beta = 0)
  data <- df$df
  
  # estimate using AIPW (backdoor IF)
  backdoor <- estimate_backdooor(data)
  backdoor.est <- backdoor$backdoor.est
  backdoor.eif <- backdoor$backdoor.eif
  
  # estimate using UIV (IV IF)
  iv <- estimate_uiv(data)
  iv.est <- iv$iv.est
  iv.eif <- iv$iv.eif
  
  # Evidence factor
  est <- c(backdoor.est, iv.est)
  eif <- cbind(backdoor.eif, iv.eif)
  p <- evidence_factor(est = est, eif = eif)
  p_values_null_iv_correct_b2_violated <- c(p_values_null_iv_correct_b2_violated, p)
  print(paste0(i,"th simulation done"))
}

typeI <- sum(p_values_null_iv_correct_b2_violated <= 0.05)/length(p_values_null_iv_correct_b2_violated)
power <- typeI
size <- power

# [1] 0.05

# b1 is violated, there is a backdoor path from A to Y,
# so when beta = 0, *the identified backdoor functional is not zero*
p_values_null_iv_correct_b1_violated <- c()
for (i in 1:N) {
  
  df <- dgp_backdoor_iv_iv_correct_b1_violated(n = n, beta = 0)
  data <- df$df
  
  # estimate using AIPW (backdoor IF)
  backdoor <- estimate_backdooor(data)
  backdoor.est <- backdoor$backdoor.est
  backdoor.eif <- backdoor$backdoor.eif
  
  # estimate using UIV (IV IF)
  iv <- estimate_uiv(data)
  iv.est <- iv$iv.est
  iv.eif <- iv$iv.eif
  
  # Evidence factor
  est <- c(backdoor.est, iv.est)
  eif <- cbind(backdoor.eif, iv.eif)
  p <- evidence_factor(est = est, eif = eif)
  p_values_null_iv_correct_b1_violated <- c(p_values_null_iv_correct_b1_violated, p)
  print(paste0(i,"th simulation done"))
}

typeI <- sum(p_values_null_iv_correct_b1_violated <= 0.05)/length(p_values_null_iv_correct_b1_violated)
power <- typeI
size <- power

# [1] 0.038


####################### 2. Under Alternative Hypothesis ########################
### a) both phi_k != 0, one of the models is correct 
# a.1) backdoor is correct, iv is wrong (i3 is violated)
p_values_alternative_bdoor_correct <- c()
for (i in 1:N) {
  
  df <- dgp_backdoor_iv_bdoor_correct_i3_violated(n = n, beta = 10, beta_defier = 2)
  data <- df$df
  
  # estimate using AIPW (backdoor IF)
  backdoor <- estimate_backdooor(data)
  backdoor.est <- backdoor$backdoor.est
  backdoor.eif <- backdoor$backdoor.eif
  
  # estimate using UIV (IV IF)
  iv <- estimate_uiv(data)
  iv.est <- iv$iv.est
  iv.eif <- iv$iv.eif
  
  # Evidence factor
  est <- c(backdoor.est, iv.est)
  eif <- cbind(backdoor.eif, iv.eif)
  p <- evidence_factor(est = est, eif = eif)
  p_values_alternative_bdoor_correct <- c(p_values_alternative_bdoor_correct, p)
  print(paste0(i,"th simulation done"))
}

typeII <- sum(p_values_alternative_bdoor_correct > 0.05)/length(p_values_alternative_bdoor_correct)
power <- 1-typeII

# [1] 1

# a.2) iv is correct, backdoor is wrong 
# b2 is violated
p_values_alternative_iv_correct_b2_violated <- c()
for (i in 1:N) {
  
  df <- dgp_backdoor_iv_iv_correct_b2_violated(n = n, beta = 10)
  data <- df$df
  
  # estimate using AIPW (backdoor IF)
  backdoor <- estimate_backdooor(data)
  backdoor.est <- backdoor$backdoor.est
  backdoor.eif <- backdoor$backdoor.eif
  
  # estimate using UIV (IV IF)
  iv <- estimate_uiv(data)
  iv.est <- iv$iv.est
  iv.eif <- iv$iv.eif
  
  # Evidence factor
  est <- c(backdoor.est, iv.est)
  eif <- cbind(backdoor.eif, iv.eif)
  p <- evidence_factor(est = est, eif = eif)
  p_values_alternative_iv_correct_b2_violated <- c(p_values_alternative_iv_correct_b2_violated, p)
  print(paste0(i,"th simulation done"))
}

typeII <- sum(p_values_alternative_iv_correct_b2_violated > 0.05)/length(p_values_alternative_iv_correct_b2_violated)
power <- 1-typeII

# [1] 1

# b1 is violated
p_values_alternative_iv_correct_b1_violated <- c()
for (i in 1:N) {
  
  df <- dgp_backdoor_iv_iv_correct_b1_violated(n = n, beta = 10)
  data <- df$df
  
  # estimate using AIPW (backdoor IF)
  backdoor <- estimate_backdooor(data)
  backdoor.est <- backdoor$backdoor.est
  backdoor.eif <- backdoor$backdoor.eif
  
  # estimate using UIV (IV IF)
  iv <- estimate_uiv(data)
  iv.est <- iv$iv.est
  iv.eif <- iv$iv.eif
  
  # Evidence factor
  est <- c(backdoor.est, iv.est)
  eif <- cbind(backdoor.eif, iv.eif)
  p <- evidence_factor(est = est, eif = eif)
  p_values_alternative_iv_correct_b1_violated <- c(p_values_alternative_iv_correct_b1_violated, p)
  print(paste0(i,"th simulation done"))
}

typeII <- sum(p_values_alternative_iv_correct_b1_violated > 0.05)/length(p_values_alternative_iv_correct_b1_violated)
power <- 1-typeII

# [1] 1


### b) both phi_k != 0, both models are correct
p_values_alternative_both_correct <- c()
for (i in 1:N) {
  
  df <- dgp_backdoor_iv_both_correct(n = n, beta = 10)
  data <- df$df
  
  # estimate using AIPW (backdoor IF)
  backdoor <- estimate_backdooor(data)
  backdoor.est <- backdoor$backdoor.est
  backdoor.eif <- backdoor$backdoor.eif
  
  # estimate using UIV (IV IF)
  iv <- estimate_uiv(data)
  iv.est <- iv$iv.est
  iv.eif <- iv$iv.eif
  
  # Evidence factor
  est <- c(backdoor.est, iv.est)
  eif <- cbind(backdoor.eif, iv.eif)
  p <- evidence_factor(est = est, eif = eif)
  p_values_alternative_both_correct <- c(p_values_alternative_both_correct, p)
  print(paste0(i,"th simulation done"))
}

typeII <- sum(p_values_alternative_both_correct > 0.05)/length(p_values_alternative_both_correct)
power <- 1-typeII

# [1] 1



### c) one of phi_k = 0, one of the model is correct 
# backdoor is correct, iv is wrong 
# (i4 is violated, zero association between Z and A, and no direct effect from Z to Y)
p_values_alternative_bdoor_correct_i4_violated <- c()
for (i in 1:N) {
  
  df <- dgp_backdoor_iv_bdoor_correct_i4_violated(n = n, beta = 10)
  data <- df$df
  
  # estimate using AIPW (backdoor IF)
  backdoor <- estimate_backdooor(data)
  backdoor.est <- backdoor$backdoor.est
  backdoor.eif <- backdoor$backdoor.eif
  
  # estimate using UIV (IV IF)
  iv <- estimate_uiv(data)
  iv.est <- iv$iv.est
  iv.eif <- iv$iv.eif
  
  # Evidence factor
  est <- c(backdoor.est, iv.est)
  eif <- cbind(backdoor.eif, iv.eif)
  p <- evidence_factor(est = est, eif = eif)
  p_values_alternative_bdoor_correct_i4_violated <- c(p_values_alternative_bdoor_correct_i4_violated, p)
  print(paste0(i,"th simulation done"))
}

typeII <- sum(p_values_alternative_bdoor_correct_i4_violated > 0.05)/length(p_values_alternative_bdoor_correct_i4_violated)
power <- 1-typeII

# [1] 0









