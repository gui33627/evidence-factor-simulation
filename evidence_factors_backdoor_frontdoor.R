
rm(list=ls())

library(mgcv)
sapply(list.files(pattern=".R", path="./R", full.names = TRUE), source)

# simulation times
N <- 500
# sample size
n <- 1000


########################### Backdoor and Front-door ############################
# simulate data where both backdoor and front door are true and they should agree
data <- dgp_backdoor_frontdoor_both_correct(n)

# estimate using AIPW (backdoor IF)
backdoor <- estimate_backdooor(data)
backdoor.est <- backdoor$backdoor.est
backdoor.eif <- backdoor$backdoor.eif

# estimate using APIPW (front door IF)
frontdoor <- estimate_frontdoor(data)
frontdoor.est <- frontdoor$frontdoor.est
frontdoor.eif <- frontdoor$frontdoor.eif

# Marginal tests for backdoor and frontdoor
print(pnorm(sqrt(n) * abs(backdoor.est) / sd(backdoor.eif), lower.tail = FALSE) * 2)
print(pnorm(sqrt(n) * abs(frontdoor.est) / sd(frontdoor.eif), lower.tail = FALSE) * 2)

# Evidence factor
est <- c(backdoor.est, frontdoor.est)
eif <- cbind(backdoor.eif, frontdoor.eif)
p <- evidence_factor(est = est, eif = eif)
p





# ----------------------------- Simulation study -------------------------------
########################## 1. Under Null Hypothesis ############################
### a) both models are correct
p_values_null_both_correct <- c()
for (i in 1:N) {
  
  data <- dgp_backdoor_frontdoor_both_correct(n = n, beta = 0)
  
  # estimate using AIPW (backdoor IF)
  backdoor <- estimate_backdooor(data)
  backdoor.est <- backdoor$backdoor.est
  backdoor.eif <- backdoor$backdoor.eif
  
  # estimate using APIPW (front door IF)
  frontdoor <- estimate_frontdoor(data)
  frontdoor.est <- frontdoor$frontdoor.est
  frontdoor.eif <- frontdoor$frontdoor.eif
  
  # Evidence factor
  est <- c(backdoor.est, frontdoor.est)
  eif <- cbind(backdoor.eif, frontdoor.eif)
  p <- evidence_factor(est = est, eif = eif)
  p_values_null_both_correct <- c(p_values_null_both_correct, p)
  print(paste0(i,"th simulation done"))
}

typeI <- sum(p_values_null_both_correct <= 0.05)/length(p_values_null_both_correct)
power <- typeI
size <- power

# [1] 0


### b) backdoor is correct, front-door is not
# b.1) f1 is violated (the identified front door functional is 0)
p_values_null_bdoor_correct_f1_violated <- c()
for (i in 1:N) {
  
  data <- dgp_backdoor_frontdoor_bdoor_correct_f1_violated(n = n, beta = 0)
  
  # estimate using AIPW (backdoor IF)
  backdoor <- estimate_backdooor(data)
  backdoor.est <- backdoor$backdoor.est
  backdoor.eif <- backdoor$backdoor.eif
  
  # estimate using APIPW (front door IF)
  frontdoor <- estimate_frontdoor(data)
  frontdoor.est <- frontdoor$frontdoor.est
  frontdoor.eif <- frontdoor$frontdoor.eif
  
  # Evidence factor
  est <- c(backdoor.est, frontdoor.est)
  eif <- cbind(backdoor.eif, frontdoor.eif)
  p <- evidence_factor(est = est, eif = eif)
  p_values_null_bdoor_correct_f1_violated <- c(p_values_null_bdoor_correct_f1_violated, p)
  print(paste0(i,"th simulation done"))
}

typeI <- sum(p_values_null_bdoor_correct_f1_violated <= 0.05)/length(p_values_null_bdoor_correct_f1_violated)
power <- typeI
size <- power

# [1] 0

# b.2) f3 is violated, there is a backdoor path from M to Y,
# so when beta = 0, *the identified front door functional is not 0*
p_values_null_bdoor_correct_f3_violated <- c()
for (i in 1:N) {
  
  data <- dgp_backdoor_frontdoor_bdoor_correct_f3_violated_new(n = n, beta = 0)
  
  # estimate using AIPW (backdoor IF)
  backdoor <- estimate_backdooor(data)
  backdoor.est <- backdoor$backdoor.est
  backdoor.eif <- backdoor$backdoor.eif
  
  # estimate using APIPW (front door IF)
  frontdoor <- estimate_frontdoor(data)
  frontdoor.est <- frontdoor$frontdoor.est
  frontdoor.eif <- frontdoor$frontdoor.eif
  
  # Evidence factor
  est <- c(backdoor.est, frontdoor.est)
  eif <- cbind(backdoor.eif, frontdoor.eif)
  p <- evidence_factor(est = est, eif = eif)
  p_values_null_bdoor_correct_f3_violated <- c(p_values_null_bdoor_correct_f3_violated, p)
  print(paste0(i,"th simulation done"))
}

typeI <- sum(p_values_null_bdoor_correct_f3_violated <= 0.05)/length(p_values_null_bdoor_correct_f3_violated)
power <- typeI
size <- power

# [1] 0.06


### c) front-door is correct, backdoor is not
# b1 is violated, there is a backdoor path from A to Y,
# so when beta = 0, *the identified backdoor functional is not 0*
p_values_null_fdoor_correct <- c()
for (i in 1:N) {
  
  data <- dgp_backdoor_frontdoor_fdoor_correct_b1_violated(n = n, beta = 0)
  
  # estimate using AIPW (backdoor IF)
  backdoor <- estimate_backdooor(data)
  backdoor.est <- backdoor$backdoor.est
  backdoor.eif <- backdoor$backdoor.eif
  
  # estimate using APIPW (front door IF)
  frontdoor <- estimate_frontdoor(data)
  frontdoor.est <- frontdoor$frontdoor.est
  frontdoor.eif <- frontdoor$frontdoor.eif
  
  # Evidence factor
  est <- c(backdoor.est, frontdoor.est)
  eif <- cbind(backdoor.eif, frontdoor.eif)
  p <- evidence_factor(est = est, eif = eif)
  p_values_null_fdoor_correct <- c(p_values_null_fdoor_correct, p)
  print(paste0(i,"th simulation done"))
}

typeI <- sum(p_values_null_fdoor_correct <= 0.05)/length(p_values_null_fdoor_correct)
power <- typeI
size <- power

# [1] 0.046


####################### 2. Under Alternative Hypothesis ########################
### a) all phi_k != 0, one of the model is correct
# a.1) front door is correct, backdoor is wrong
p_values_alternative_fdoor_correct <- c()
for (i in 1:N) {
  
  data <- dgp_backdoor_frontdoor_fdoor_correct_b1_violated(n = n, beta = 10)
  
  # estimate using AIPW (backdoor IF)
  backdoor <- estimate_backdooor(data)
  backdoor.est <- backdoor$backdoor.est
  backdoor.eif <- backdoor$backdoor.eif
  
  # estimate using APIPW (front door IF)
  frontdoor <- estimate_frontdoor(data)
  frontdoor.est <- frontdoor$frontdoor.est
  frontdoor.eif <- frontdoor$frontdoor.eif
  
  # Evidence factor
  est <- c(backdoor.est, frontdoor.est)
  eif <- cbind(backdoor.eif, frontdoor.eif)
  p <- evidence_factor(est = est, eif = eif)
  p_values_alternative_fdoor_correct <- c(p_values_alternative_fdoor_correct, p)
  print(paste0(i,"th simulation done"))
}

typeII <- sum(p_values_alternative_fdoor_correct > 0.05)/length(p_values_alternative_fdoor_correct)
power <- 1-typeII

# [1] 1

# a.2) backdoor is correct, front door is wrong
p_values_alternative_bdoor_correct <- c()
for (i in 1:N) {
  
  data <- dgp_backdoor_frontdoor_bdoor_correct_f3_violated_new(n = n, beta = 10)
  
  # estimate using AIPW (backdoor IF)
  backdoor <- estimate_backdooor(data)
  backdoor.est <- backdoor$backdoor.est
  backdoor.eif <- backdoor$backdoor.eif
  
  # estimate using APIPW (front door IF)
  frontdoor <- estimate_frontdoor(data)
  frontdoor.est <- frontdoor$frontdoor.est
  frontdoor.eif <- frontdoor$frontdoor.eif
  
  # Evidence factor
  est <- c(backdoor.est, frontdoor.est)
  eif <- cbind(backdoor.eif, frontdoor.eif)
  p <- evidence_factor(est = est, eif = eif)
  p_values_alternative_bdoor_correct <- c(p_values_alternative_bdoor_correct, p)
  print(paste0(i,"th simulation done"))
}

typeII <- sum(p_values_alternative_bdoor_correct > 0.05)/length(p_values_alternative_bdoor_correct)
power <- 1-typeII

# [1] 0.968


### b) all phi_k != 0, both models are correct
p_values_alternative_both_correct <- c()
for (i in 1:N) {
  
  data <- dgp_backdoor_frontdoor_both_correct(n = n, beta = 10)
  
  # estimate using AIPW (backdoor IF)
  backdoor <- estimate_backdooor(data)
  backdoor.est <- backdoor$backdoor.est
  backdoor.eif <- backdoor$backdoor.eif
  
  # estimate using APIPW (front door IF)
  frontdoor <- estimate_frontdoor(data)
  frontdoor.est <- frontdoor$frontdoor.est
  frontdoor.eif <- frontdoor$frontdoor.eif
  
  # Evidence factor
  est <- c(backdoor.est, frontdoor.est)
  eif <- cbind(backdoor.eif, frontdoor.eif)
  p <- evidence_factor(est = est, eif = eif)
  p_values_alternative_both_correct <- c(p_values_alternative_both_correct, p)
  print(paste0(i,"th simulation done"))
}

typeII <- sum(p_values_alternative_both_correct > 0.05)/length(p_values_alternative_both_correct)
power <- 1-typeII

# [1] 1


### c) one of phi_k = 0, one of the model is correct 
# backdoor is correct, front door is wrong
p_values_alternative_bdoor_correct_f1_violated <- c()
for (i in 1:N) {
  
  data <- dgp_backdoor_frontdoor_bdoor_correct_f1_violated(n = n, beta = 10)
  
  # estimate using AIPW (backdoor IF)
  backdoor <- estimate_backdooor(data)
  backdoor.est <- backdoor$backdoor.est
  backdoor.eif <- backdoor$backdoor.eif
  
  # estimate using APIPW (front door IF)
  frontdoor <- estimate_frontdoor(data)
  frontdoor.est <- frontdoor$frontdoor.est
  frontdoor.eif <- frontdoor$frontdoor.eif
  
  # Evidence factor
  est <- c(backdoor.est, frontdoor.est)
  eif <- cbind(backdoor.eif, frontdoor.eif)
  p <- evidence_factor(est = est, eif = eif)
  p_values_alternative_bdoor_correct_f1_violated <- c(p_values_alternative_bdoor_correct_f1_violated, p)
  print(paste0(i,"th simulation done"))
}

typeII <- sum(p_values_alternative_bdoor_correct_f1_violated > 0.05)/length(p_values_alternative_bdoor_correct_f1_violated)
power <- 1-typeII

# [1] 0.07


