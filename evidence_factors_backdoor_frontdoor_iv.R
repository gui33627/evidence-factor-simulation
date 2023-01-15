
rm(list=ls())

library(mgcv)
sapply(list.files(pattern=".R", path="./R", full.names = TRUE), source)

# simulation times
N <- 500
# sample size
n <- 1000


######################### Backdoor, Front-door, and IV #########################
# simulate data where backdoor, front-door, and IV are correct and they should agree
df <- dgp_backdoor_frontdoor_iv_all_correct(n)
data <- df$df

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

# Marginal tests for backdoor and frontdoor
print(pnorm(sqrt(n) * abs(backdoor.est) / sd(backdoor.eif), lower.tail = FALSE) * 2)
print(pnorm(sqrt(n) * abs(frontdoor.est) / sd(frontdoor.eif), lower.tail = FALSE) * 2)
print(pnorm(sqrt(n) * abs(iv.est) / sd(iv.eif), lower.tail = FALSE) * 2)

# Evidence factor
all.est <- c(backdoor.est, frontdoor.est, iv.est)
all.eif <- cbind(backdoor.eif, frontdoor.eif, iv.eif)
p <- evidence_factor(all.est, all.eif)
p


# ----------------------------- Simulation study -------------------------------
########################## 1. Under Null Hypothesis ############################
### a) all models are correct
p_values_null_all_correct <- c()
for (i in 1:N) {
  
  df <- dgp_backdoor_frontdoor_iv_all_correct(n = n, beta = 0)
  data <- df$df
  
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
  
  # Evidence factor
  est <- c(backdoor.est, frontdoor.est, iv.est)
  eif <- cbind(backdoor.eif, frontdoor.eif, iv.eif)
  p <- evidence_factor(est = est, eif = eif)
  p_values_null_all_correct <- c(p_values_null_all_correct, p)
  print(paste0(i,"th simulation done"))
}

typeI <- sum(p_values_null_all_correct <= 0.05)/length(p_values_null_all_correct)
power <- typeI
size <- power

# [1] 0


### b) two of the models are correct
# b.1) backdoor and iv are true, front-door is not 
# f3 violated, there is a backdoor path from M to Y,
# so when beta = 0, *the identified front-door functional is not zero*
p_values_null_backdoor_iv_correct_f3_violated <- c()
for (i in 1:N) {
  
  df <- dgp_backdoor_frontdoor_iv_f3_violated(n = n, beta = 0)
  data <- df$df
  
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
  
  # Evidence factor
  est <- c(backdoor.est, frontdoor.est, iv.est)
  eif <- cbind(backdoor.eif, frontdoor.eif, iv.eif)
  p <- evidence_factor(est = est, eif = eif)
  p_values_null_backdoor_iv_correct_f3_violated <- c(p_values_null_backdoor_iv_correct_f3_violated, p)
  print(paste0(i,"th simulation done"))
}

typeI <- sum(p_values_null_backdoor_iv_correct_f3_violated <= 0.05)/length(p_values_null_backdoor_iv_correct_f3_violated)
power <- typeI
size <- power

# [1] 0

# b.2) backdoor and front-door are true, iv is not 
# Z cannot have a direct effect to Y because the backdoor has to be true, and 
# the front door is true (M fully mediate and beta = 0),  *so the identified iv functional is zero*
p_values_null_backdoor_frontdoor_correct_i3_violated <- c()
for (i in 1:N) {
  
  df <- dgp_backdoor_frontdoor_iv_i3_violated(n = n, beta = 0)
  data <- df$df
  
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
  
  # Evidence factor
  est <- c(backdoor.est, frontdoor.est, iv.est)
  eif <- cbind(backdoor.eif, frontdoor.eif, iv.eif)
  p <- evidence_factor(est = est, eif = eif)
  p_values_null_backdoor_frontdoor_correct_i3_violated <- c(p_values_null_backdoor_frontdoor_correct_i3_violated, p)
  print(paste0(i,"th simulation done"))
}

typeI <- sum(p_values_null_backdoor_frontdoor_correct_i3_violated <= 0.05)/length(p_values_null_backdoor_frontdoor_correct_i3_violated)
power <- typeI
size <- power

# [1] 0

# b.3) front door and iv are true, backdoor is not
# b1 is violated (there is a backdoor path from A to Y), 
# so when beta = 0, *the identified backdoor functional is not zero*
p_values_null_fdoor_iv_correct_b1_violated <- c()
for (i in 1:N) {
  
  df <- dgp_backdoor_frontdoor_iv_b1_violated(n = n, beta = 0)
  data <- df$df
  
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
  
  # Evidence factor
  est <- c(backdoor.est, frontdoor.est, iv.est)
  eif <- cbind(backdoor.eif, frontdoor.eif, iv.eif)
  p <- evidence_factor(est = est, eif = eif)
  p_values_null_fdoor_iv_correct_b1_violated <- c(p_values_null_fdoor_iv_correct_b1_violated, p)
  print(paste0(i,"th simulation done"))
}

typeI <- sum(p_values_null_fdoor_iv_correct_b1_violated <= 0.05)/length(p_values_null_fdoor_iv_correct_b1_violated)
power <- typeI
size <- power

# [1] 0


### c) one of the models is correct
# c.1) front door is correct, backdoor and iv are wrong 
# b1 and i1 violated (there is backdoor paths from A to Y and from Z to Y), 
# when the effect through M is zero (beta = 0), *the identified iv and backdoor are not zero*
p_values_null_fdoor_correct_b1i1_violated <- c()
for (i in 1:N) {
  
  df <- dgp_backdoor_frontdoor_iv_fdoor_correct_b1i1_violated(n = n, beta = 0)
  data <- df$df
  
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
  
  # Evidence factor
  est <- c(backdoor.est, frontdoor.est, iv.est)
  eif <- cbind(backdoor.eif, frontdoor.eif, iv.eif)
  p <- evidence_factor(est = est, eif = eif)
  p_values_null_fdoor_correct_b1i1_violated <- c(p_values_null_fdoor_correct_b1i1_violated, p)
  print(paste0(i,"th simulation done"))
}

typeI <- sum(p_values_null_fdoor_correct_b1i1_violated <= 0.05)/length(p_values_null_fdoor_correct_b1i1_violated)
power <- typeI
size <- power

# [1] 0.088


# c.2) iv is correct, backdoor and front door are wrong 
# b1, f2, and f3 violated, there are backdoor paths from A to Y, from A to M, and from M to Y
# when beta = 0, *the identified backdoor and iv functionals are not zero*
p_values_null_iv_correct_b1f2f3_violated <- c()
for (i in 1:N) {
  
  df <- dgp_backdoor_frontdoor_iv_iv_correct_b1f2f3_violated(n = n, beta = 0)
  data <- df$df
  
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
  
  # Evidence factor
  est <- c(backdoor.est, frontdoor.est, iv.est)
  eif <- cbind(backdoor.eif, frontdoor.eif, iv.eif)
  p <- evidence_factor(est = est, eif = eif)
  p_values_null_iv_correct_b1f2f3_violated <- c(p_values_null_iv_correct_b1f2f3_violated, p)
  print(paste0(i,"th simulation done"))
}

typeI <- sum(p_values_null_iv_correct_b1f2f3_violated <= 0.05)/length(p_values_null_iv_correct_b1f2f3_violated)
power <- typeI
size <- power

# [1] 0.036


# c.3) backdoor is correct, frontdoor and iv are wrong
# Z cannot have a direct effect to Y because the backdoor has to be true, 
# f3 violated, there is a backdoor path from M to Y, so when beta = 0, 
# *the identified frontdoor is not zero and iv is zero*
p_values_null_bdoor_correct_f3i3_violated <- c()
for (i in 1:N) {
  
  df <- dgp_backdoor_frontdoor_iv_bdoor_correct_f3i3_violated(n = n, beta = 0)
  data <- df$df
  
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
  
  # Evidence factor
  est <- c(backdoor.est, frontdoor.est, iv.est)
  eif <- cbind(backdoor.eif, frontdoor.eif, iv.eif)
  p <- evidence_factor(est = est, eif = eif)
  p_values_null_bdoor_correct_f3i3_violated <- c(p_values_null_bdoor_correct_f3i3_violated, p)
  print(paste0(i,"th simulation done"))
}

typeI <- sum(p_values_null_bdoor_correct_f3i3_violated <= 0.05)/length(p_values_null_bdoor_correct_f3i3_violated)
power <- typeI
size <- power

# [1] 0


####################### 2. Under Alternative Hypothesis ########################
### a) all phi_k != 0, one of the models is correct 
# a.1) front door is correct, backdoor and iv are wrong 
# b1 and i1 are violated
p_values_alternative_fdoor_correct_b1i1_violated <- c()
for (i in 1:N) {
  
  df <- dgp_backdoor_frontdoor_iv_fdoor_correct_b1i1_violated(n = n, beta = 10)
  data <- df$df
  
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
  
  # Evidence factor
  est <- c(backdoor.est, frontdoor.est, iv.est)
  eif <- cbind(backdoor.eif, frontdoor.eif, iv.eif)
  p <- evidence_factor(est = est, eif = eif)
  p_values_alternative_fdoor_correct_b1i1_violated <- c(p_values_alternative_fdoor_correct_b1i1_violated, p)
  print(paste0(i,"th simulation done"))
}

typeII <- sum(p_values_alternative_fdoor_correct_b1i1_violated > 0.05)/length(p_values_alternative_fdoor_correct_b1i1_violated)
power <- 1-typeII

# [1] 1

# b1 and i2 are violated
p_values_alternative_fdoor_correct_b1i2_violated <- c()
for (i in 1:N) {
  
  df <- dgp_backdoor_frontdoor_iv_fdoor_correct_b1i2_violated(n = n, beta = 10)
  data <- df$df
  
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
  
  # Evidence factor
  est <- c(backdoor.est, frontdoor.est, iv.est)
  eif <- cbind(backdoor.eif, frontdoor.eif, iv.eif)
  p <- evidence_factor(est = est, eif = eif)
  p_values_alternative_fdoor_correct_b1i2_violated <- c(p_values_alternative_fdoor_correct_b1i2_violated, p)
  print(paste0(i,"th simulation done"))
}

typeII <- sum(p_values_alternative_fdoor_correct_b1i2_violated > 0.05)/length(p_values_alternative_fdoor_correct_b1i2_violated)
power <- 1-typeII

# [1] 1


# a.2) iv is correct, backdoor and front door are wrong 
p_values_alternative_iv_correct_b1f2f3_violated <- c()
for (i in 1:N) {
  
  df <- dgp_backdoor_frontdoor_iv_iv_correct_b1f2f3_violated(n = n, beta = 10)
  data <- df$df
  
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
  
  # Evidence factor
  est <- c(backdoor.est, frontdoor.est, iv.est)
  eif <- cbind(backdoor.eif, frontdoor.eif, iv.eif)
  p <- evidence_factor(est = est, eif = eif)
  p_values_alternative_iv_correct_b1f2f3_violated <- c(p_values_alternative_iv_correct_b1f2f3_violated, p)
  print(paste0(i,"th simulation done"))
}

typeII <- sum(p_values_alternative_iv_correct_b1f2f3_violated > 0.05)/length(p_values_alternative_iv_correct_b1f2f3_violated)
power <- 1-typeII

# [1] 1


# a.3) backdoor is correct, front door and iv are wrong
# f3 and i3 are violated
p_values_alternative_bdoor_correct_f3i3_violated <- c()
for (i in 1:N) {
  
  df <- dgp_backdoor_frontdoor_iv_bdoor_correct_f3i3_violated(n = n, beta = 10)
  data <- df$df
  
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
  
  # Evidence factor
  est <- c(backdoor.est, frontdoor.est, iv.est)
  eif <- cbind(backdoor.eif, frontdoor.eif, iv.eif)
  p <- evidence_factor(est = est, eif = eif)
  p_values_alternative_bdoor_correct_f3i3_violated <- c(p_values_alternative_bdoor_correct_f3i3_violated, p)
  print(paste0(i,"th simulation done"))
}

typeII <- sum(p_values_alternative_bdoor_correct_f3i3_violated > 0.05)/length(p_values_alternative_bdoor_correct_f3i3_violated)
power <- 1-typeII

# [1] 0.938

# f1 and i3 are violated
p_values_alternative_bdoor_correct_f1i3_violated <- c()
for (i in 1:N) {
  
  df <- dgp_backdoor_frontdoor_iv_bdoor_correct_f1i3_violated(n = n, beta = 10, beta_defier = 1)
  data <- df$df
  
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
  
  # Evidence factor
  est <- c(backdoor.est, frontdoor.est, iv.est)
  eif <- cbind(backdoor.eif, frontdoor.eif, iv.eif)
  p <- evidence_factor(est = est, eif = eif)
  p_values_alternative_bdoor_correct_f1i3_violated <- c(p_values_alternative_bdoor_correct_f1i3_violated, p)
  print(paste0(i,"th simulation done"))
}

typeII <- sum(p_values_alternative_bdoor_correct_f1i3_violated > 0.05)/length(p_values_alternative_bdoor_correct_f1i3_violated)
power <- 1-typeII

# [1] 1


### b) all phi_k != 0, two of the models are correct 
# b.1) backdoor and iv are correct, front door is wrong
p_values_alternative_bdoor_iv_correct_f3_violated <- c()
for (i in 1:N) {
  
  df <- dgp_backdoor_frontdoor_iv_f3_violated(n = n, beta = 10)
  data <- df$df
  
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
  
  # Evidence factor
  est <- c(backdoor.est, frontdoor.est, iv.est)
  eif <- cbind(backdoor.eif, frontdoor.eif, iv.eif)
  p <- evidence_factor(est = est, eif = eif)
  p_values_alternative_bdoor_iv_correct_f3_violated <- c(p_values_alternative_bdoor_iv_correct_f3_violated, p)
  print(paste0(i,"th simulation done"))
}

typeII <- sum(p_values_alternative_bdoor_iv_correct_f3_violated > 0.05)/length(p_values_alternative_bdoor_iv_correct_f3_violated)
power <- 1-typeII

# [1] 1

# b.2) backdoor and front door are correct, iv is wrong
p_values_alternative_bdoor_fdoor_correct_i3_violated <- c()
for (i in 1:N) {
  
  df <- dgp_backdoor_frontdoor_iv_i3_violated(n = n, beta = 10)
  data <- df$df
  
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
  
  # Evidence factor
  est <- c(backdoor.est, frontdoor.est, iv.est)
  eif <- cbind(backdoor.eif, frontdoor.eif, iv.eif)
  p <- evidence_factor(est = est, eif = eif)
  p_values_alternative_bdoor_fdoor_correct_i3_violated <- c(p_values_alternative_bdoor_fdoor_correct_i3_violated, p)
  print(paste0(i,"th simulation done"))
}

typeII <- sum(p_values_alternative_bdoor_fdoor_correct_i3_violated > 0.05)/length(p_values_alternative_bdoor_fdoor_correct_i3_violated)
power <- 1-typeII

# [1] 1

# b.3) front door and iv are correct, backdoor is wrong
p_values_alternative_fdoor_iv_correct_b1_violated <- c()
for (i in 1:N) {
  
  df <- dgp_backdoor_frontdoor_iv_b1_violated(n = n, beta = 10)
  data <- df$df
  
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
  
  # Evidence factor
  est <- c(backdoor.est, frontdoor.est, iv.est)
  eif <- cbind(backdoor.eif, frontdoor.eif, iv.eif)
  p <- evidence_factor(est = est, eif = eif)
  p_values_alternative_fdoor_iv_correct_b1_violated <- c(p_values_alternative_fdoor_iv_correct_b1_violated, p)
  print(paste0(i,"th simulation done"))
}

typeII <- sum(p_values_alternative_fdoor_iv_correct_b1_violated > 0.05)/length(p_values_alternative_fdoor_iv_correct_b1_violated)
power <- 1-typeII

# [1] 1


### c) all phi_k != 0, all of the models are correct
p_values_alternative_all_correct <- c()
for (i in 1:N) {
  
  df <- dgp_backdoor_frontdoor_iv_all_correct(n = n, beta = 10)
  data <- df$df
  
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
  
  # Evidence factor
  est <- c(backdoor.est, frontdoor.est, iv.est)
  eif <- cbind(backdoor.eif, frontdoor.eif, iv.eif)
  p <- evidence_factor(est = est, eif = eif)
  p_values_alternative_all_correct <- c(p_values_alternative_all_correct, p)
  print(paste0(i,"th simulation done"))
}

typeII <- sum(p_values_alternative_all_correct > 0.05)/length(p_values_alternative_all_correct)
power <- 1-typeII

# [1] 1


### d) one of phi_k = 0, one of the model is correct 
# d.1) backdoor and iv are correct, front door is wrong
# (the identified frontdoor functional is zero)
p_values_alternative_bdoor_iv_correct_f1_violated <- c()
for (i in 1:N) {
  
  df <- dgp_backdoor_frontdoor_iv_f1_violated(n = n, beta = 10)
  data <- df$df
  
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
  
  # Evidence factor
  est <- c(backdoor.est, frontdoor.est, iv.est)
  eif <- cbind(backdoor.eif, frontdoor.eif, iv.eif)
  p <- evidence_factor(est = est, eif = eif)
  p_values_alternative_bdoor_iv_correct_f1_violated <- c(p_values_alternative_bdoor_iv_correct_f1_violated, p)
  print(paste0(i,"th simulation done"))
}

typeII <- sum(p_values_alternative_bdoor_iv_correct_f1_violated > 0.05)/length(p_values_alternative_bdoor_iv_correct_f1_violated)
power <- 1-typeII

# [1] 0.302

# d.2) backdoor and frontdoor are correct, iv is wrong
# (the identified iv functional is zero)
p_values_alternative_bdoor_fdoor_correct_i4_violated <- c()
for (i in 1:N) {
  
  df <- dgp_backdoor_frontdoor_iv_i4_violated(n = n, beta = 10)
  data <- df$df
  
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
  
  # Evidence factor
  est <- c(backdoor.est, frontdoor.est, iv.est)
  eif <- cbind(backdoor.eif, frontdoor.eif, iv.eif)
  p <- evidence_factor(est = est, eif = eif)
  p_values_alternative_bdoor_fdoor_correct_i4_violated <- c(p_values_alternative_bdoor_fdoor_correct_i4_violated, p)
  print(paste0(i,"th simulation done"))
}

typeII <- sum(p_values_alternative_bdoor_fdoor_correct_i4_violated > 0.05)/length(p_values_alternative_bdoor_fdoor_correct_i4_violated)
power <- 1-typeII

# [1] 0


# d.3) iv is correct, backdoor and front door are wrong 
# (the identified frontdoor functional is zero)
p_values_alternative_iv_correct_b2f1f3_violated <- c()
for (i in 1:N) {
  
  df <- dgp_backdoor_frontdoor_iv_iv_correct_b2f1f3_violated(n = n, beta = 10)
  data <- df$df
  
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
  
  # Evidence factor
  est <- c(backdoor.est, frontdoor.est, iv.est)
  eif <- cbind(backdoor.eif, frontdoor.eif, iv.eif)
  p <- evidence_factor(est = est, eif = eif)
  p_values_alternative_iv_correct_b2f1f3_violated <- c(p_values_alternative_iv_correct_b2f1f3_violated, p)
  print(paste0(i,"th simulation done"))
}

typeII <- sum(p_values_alternative_iv_correct_b2f1f3_violated > 0.05)/length(p_values_alternative_iv_correct_b2f1f3_violated)
power <- 1-typeII

# [1] 0.502

# (the identified frontdoor functional is zero)
p_values_alternative_iv_correct_b2f1f2_violated <- c()
for (i in 1:N) {
  
  df <- dgp_backdoor_frontdoor_iv_iv_correct_b2f1f2_violated(n = n, beta = 10)
  data <- df$df
  
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
  
  # Evidence factor
  est <- c(backdoor.est, frontdoor.est, iv.est)
  eif <- cbind(backdoor.eif, frontdoor.eif, iv.eif)
  p <- evidence_factor(est = est, eif = eif)
  p_values_alternative_iv_correct_b2f1f2_violated <- c(p_values_alternative_iv_correct_b2f1f2_violated, p)
  print(paste0(i,"th simulation done"))
}

typeII <- sum(p_values_alternative_iv_correct_b2f1f2_violated > 0.05)/length(p_values_alternative_iv_correct_b2f1f2_violated)
power <- 1-typeII

# [1] 0.124


