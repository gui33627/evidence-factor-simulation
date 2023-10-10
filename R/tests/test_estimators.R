# Simulate data where both backdoor and front door are true and they should agree
rm(list=ls())

library(mgcv)
prefix <- "./simulations.v8.hpc/"
sapply(list.files(pattern=".R", path = paste0(prefix, "R"), full.names = TRUE), source)

n <- 1000000

########################### Backdoor and Front-door ############################

# get data where both frontdoor and backdoor are correct
data <- dgp_backdoor_frontdoor_fdoor_correct_b1_violated_null(n, beta = 0)

# estimate using AIPW (backdoor IF)
backdoor <- estimate_backdooor(data)
backdoor.est <- backdoor$backdoor.est
backdoor.eif <- backdoor$backdoor.eif

# estimate using APIPW (front door IF)
frontdoor <- estimate_frontdoor(data)
frontdoor.est <- frontdoor$frontdoor.est
frontdoor.eif <- frontdoor$frontdoor.eif

# E[Y(1) - Y(0)] in true DGP
print(mean(data$PO.diff))

# CIs for backdoor and frontdoor
print(backdoor.est + c(-1,1) * qnorm(.975) * sd(backdoor.eif) / sqrt(n))
print(frontdoor.est + c(-1,1) * qnorm(.975) * sd(frontdoor.eif) / sqrt(n))


######################### Backdoor, Front-door, and IV #########################
coef_seq <- seq(from = 1.6, to = 2.3, by = 0.1)
coef_seq <- seq(from = 2.12, to = 2.19, by = 0.01)

dgp_backdoor_frontdoor_iv_i3_violated <- function(n, beta = 3, coef_test) {
  
  # Unmeasured confounder
  U <- matrix(runif(n, min=-2,max=2))
  
  # Measured confounders (not a descendant of A, (B2) satisfied)
  C <- data.frame(matrix(runif(n * 4, min=-2,max=2), ncol=4))
  
  # Instrumental variable (randomized, (I1) satisfied)
  Z <- rbinom(n = n, size = 1, prob = 0.5)
  
  # Treatment (not a function of U, (B1) satisfied; there is no unblocked backdoor path from A to M given C, (F2) satisfied))
  pA.UC <- expit(C[,1] + expit(C[,2]) + sin(C[,3]))
  A1 <- rbinom(n, size = 1, prob = pA.UC)
  A0 <- rbinom(n, size = 1, prob = 1-pA.UC)
  
  # (defiers exist, (I3) violated)
  idx_complier <- which(A1 == 1 & A0 == 0)
  idx_never <- which(A1 == 0 & A0 == 0)
  idx_always <- which(A1 == 1 & A0 == 1)
  idx_defier <- which(A1 == 0 & A0 == 1)
  
  #  A is determined by Z based on the four types of people, (I4) satisfied
  A <- ifelse(Z == 1, A1, A0)
  
  # potential outcomes of mediator (not a function of U, so no unblocked backdoor path from M to Y given C, (F3) satisfied)
  pM1.AUC <- expit(5 * 1 - 1 + C[,2]) # pM1.AUC and pM0.AUC are the same for never takers and always takers, exclusion restriction satisfied
  pM0.AUC <- expit(5 * 0 - 1 + C[,2])
  pM1.AUC[idx_complier] <- expit(coef_test* 1 - 1 + C[,2][idx_complier]) 
  pM0.AUC[idx_complier] <- expit(coef_test * 0 - 1 + C[,2][idx_complier])
  M1 <- rbinom(n, size = 1, prob = pM1.AUC) 
  M0 <- rbinom(n, size = 1, prob = pM0.AUC)
  M <- ifelse(A == 1, M1, M0) # Z affects M only through A
  
  # potential outcomes of Y (not a function of A, (F1) satisfied; not a function directly of Z, (I2) satisfied)
  error <- rnorm(n, mean = 0)
  Y1 <- beta*M1 + U + 2* sqrt(abs(C[,1])) + sin(C[,4]) + error
  Y0 <- beta*M0 + U + 2* sqrt(abs(C[,1])) + sin(C[,4]) + error
  Y <- ifelse(A == 1, Y1, Y0) # Z affects Y only through A via M
  
  PO.diff.CACE <- Y1[idx_complier] - Y0[idx_complier]
  
  return(list(df = data.frame(Y, Z, M, A, C), idx_complier = idx_complier, CACE =  mean(PO.diff.CACE)))
}

coef_ci <- data.frame(coef = NA, CI_lower = NA, CI_upper = NA)
# get data where backdoor, front-door, and IV are correct
for (i in 1:length(coef_seq)) {
  df <- dgp_backdoor_frontdoor_iv_i3_violated(n, beta = 10, coef_test = coef_seq[i])
  data <- df$df
  
  # estimate using AIPW (backdoor IF)
  backdoor <- estimate_backdooor(data, confounderZ = TRUE)
  backdoor.est <- backdoor$backdoor.est
  backdoor.eif <- backdoor$backdoor.eif
  
  # estimate using APIPW (front door IF)
  frontdoor <- estimate_frontdoor(data, confounderZ = TRUE)
  frontdoor.est <- frontdoor$frontdoor.est
  frontdoor.eif <- frontdoor$frontdoor.eif
  
  # estimate using UIV (IV IF)
  iv <- estimate_uiv(data)
  iv.est <- iv$iv.est
  iv.eif <- iv$iv.eif
  
  # CIs for backdoor and iv
  print(backdoor.est + c(-1,1) * qnorm(.975) * sd(backdoor.eif) / sqrt(n))
  print(frontdoor.est + c(-1,1) * qnorm(.975) * sd(frontdoor.eif) / sqrt(n))
  print(iv.est + c(-1,1) * qnorm(.975) * sd(iv.eif) / sqrt(n))
  
  coef_ci[i,] <- c(coef_seq[i], iv.est + c(-1,1) * qnorm(.975) * sd(iv.eif) / sqrt(n))
  
}

# coef   CI_lower    CI_upper
# 1  1.6 -2.4072220 -1.67620354
# 2  1.7 -2.4148063 -1.67806587
# 3  1.8 -2.0769102 -1.32550873
# 4  1.9 -1.4458927 -0.70045434
# 5  2.0 -1.1078100 -0.36511777
# 6  2.1 -0.8295590 -0.07219321
# 7  2.2 -0.2976043  0.44282532
# 8  2.3 -0.2478912  0.51990232

# n = 1000000
# IV CI: [1] 0.007196874 0.243838803

start_time <- Sys.time()
df <- dgp_backdoor_frontdoor_iv_i3_violated(n, beta = 10, coef_test = 2.17)
data <- df$df

# estimate using AIPW (backdoor IF)
backdoor <- estimate_backdooor(data, confounderZ = TRUE)
backdoor.est <- backdoor$backdoor.est
backdoor.eif <- backdoor$backdoor.eif

# estimate using APIPW (front door IF)
frontdoor <- estimate_frontdoor(data, confounderZ = TRUE)
frontdoor.est <- frontdoor$frontdoor.est
frontdoor.eif <- frontdoor$frontdoor.eif

# estimate using UIV (IV IF)
iv <- estimate_uiv(data)
iv.est <- iv$iv.est
iv.eif <- iv$iv.eif

# CIs for backdoor and iv
print(backdoor.est + c(-1,1) * qnorm(.975) * sd(backdoor.eif) / sqrt(n))
print(frontdoor.est + c(-1,1) * qnorm(.975) * sd(frontdoor.eif) / sqrt(n))
print(iv.est + c(-1,1) * qnorm(.975) * sd(iv.eif) / sqrt(n))

end_time <- Sys.time()
end_time - start_time

dgp_backdoor_frontdoor_iv_bdoor_correct_f3i3_violated <- function(n, beta = 3) {
  
  # Unmeasured confounder
  U <- matrix(runif(n, min=-2,max=2))
  
  # Measured confounders (not a descendant of A, (B2) satisfied)
  C <- data.frame(matrix(runif(n * 4, min=-2,max=2), ncol=4))
  
  # Instrumental variable (randomized, (I1) satisfied)
  Z <- rbinom(n = n, size = 1, prob = 0.5)
  
  # Treatment (not a function of U, (B1) satisfied; there is no unblocked backdoor path from A to M given C, (F2) satisfied))
  pA.UC <- expit(C[,1] + expit(C[,2]) + sin(C[,3]))
  A1 <- rbinom(n, size = 1, prob = pA.UC)
  A0 <- rbinom(n, size = 1, prob = 1-pA.UC)
  
  # (defiers exist, (I3) violated)
  idx_complier <- which(A1 == 1 & A0 == 0)
  idx_never <- which(A1 == 0 & A0 == 0)
  idx_always <- which(A1 == 1 & A0 == 1)
  idx_defier <- which(A1 == 0 & A0 == 1)
  
  #  A is determined by Z based on the four types of people, (I4) satisfied
  A <- ifelse(Z == 1, A1, A0)
  
  # potential outcomes of mediator (a function of U, so there is an unblocked backdoor path from M to Y given C, (F3) violated)
  pM1.AUC <- expit(0 * 1 - 1 + C[,2] + U) # pM1.AUC and pM0.AUC are the same for never takers and always takers, exclusion restriction satisfied
  pM0.AUC <- expit(0 * 0 - 1 + C[,2] + U)
  pM1.AUC[idx_complier] <- expit(5 * 1 - 1 + C[,2][idx_complier] + U[idx_complier]) 
  pM0.AUC[idx_complier] <- expit(5 * 0 - 1 + C[,2][idx_complier] + U[idx_complier])
  M1 <- rbinom(n, size = 1, prob = pM1.AUC) 
  M0 <- rbinom(n, size = 1, prob = pM0.AUC)
  M <- ifelse(A == 1, M1, M0) # Z affects M only through A
  
  # potential outcomes of Y (not a function of A, (F1) satisfied; not a function directly of Z, (I2) satisfied)
  error <- rnorm(n, mean = 0)
  Y1 <- beta*M1 - 3*U + 2* sqrt(abs(C[,1])) + sin(C[,4]) + error
  Y0 <- beta*M0 - 3*U + 2* sqrt(abs(C[,1])) + sin(C[,4]) + error
  Y <- ifelse(A == 1, Y1, Y0) # Z affects Y only through A via M
  
  PO.diff.CACE <- Y1[idx_complier] - Y0[idx_complier]
  
  return(list(df = data.frame(Y, Z, M, A, C), idx_complier = idx_complier, CACE =  mean(PO.diff.CACE)))
}


# dgp_backdoor_frontdoor_iv_iv_correct_b2f3_violated <- function(n, beta = 3) {
#   
#   # Unmeasured confounder
#   U <- matrix(runif(n, min=-2,max=2))
#   
#   # Measured confounders 
#   C <- data.frame(matrix(runif(n * 4, min=-2,max=2), ncol=4))
#   
#   # Instrumental variable (randomized, (I1) satisfied)
#   Z <- rbinom(n = n, size = 1, prob = 0.5)
#   
#   # Treatment (Y is not a function of U, (B1) satisfied; (F2) satisfied)
#   pA.UC <- expit(C[,4] + expit(C[,2]) + sin(C[,3]))
#   A1 <- rbinom(n, size = 1, prob = pA.UC)
#   A0 <- rbinom(n, size = 1, prob = 1-pA.UC)
#   
#   # no defier, convert to compliers ((I3) satisfied)
#   idx_defier <- which(A1 == 0 & A0 == 1)
#   A1[idx_defier] <- 1
#   A0[idx_defier] <- 0
#   
#   idx_complier <- which(A1 == 1 & A0 == 0)
#   idx_never <- which(A1 == 0 & A0 == 0)
#   idx_always <- which(A1 == 1 & A0 == 1)
#   #  A is determined by Z based on the four types of people, (I4) satisfied
#   A <- ifelse(Z == 1, A1, A0)
#   
#   # potential outcomes of mediator (a function of U, so there is an unblocked backdoor path from M to Y given C, (F3) violated)
#   pM1.AUC <- expit(5 * 1 - 1 + C[,2] + 2*U) # pM1.AUC and pM0.AUC are the same for never takers and always takers, exclusion restriction satisfied
#   pM0.AUC <- expit(5 * 0 - 1 + C[,2] + 2*U)
#   M1 <- rbinom(n, size = 1, prob = pM1.AUC) 
#   M0 <- rbinom(n, size = 1, prob = pM0.AUC)
#   M <- ifelse(A == 1, M1, M0) # Z affects M only through A
#   
#   # potential outcomes of Y (not a function of A, (F1) satisfied; not a function directly of Z, (I2) satisfied)
#   error <- rnorm(n, mean = 0)
#   Y1 <- beta*M1 + sin(C[,4]) + U + error
#   Y0 <- beta*M0 + sin(C[,4]) + U + error
#   Y <- ifelse(A == 1, Y1, Y0) # Z affects Y only through A via M
#   
#   # Measured confounders (a node in C is a collider, (B2) violated)
#   C[,1] <- rnorm(n, mean = (3*A - Y))
#   
#   PO.diff.CACE <- Y1[idx_complier] - Y0[idx_complier]
#   
#   return(list(df = data.frame(Y, Z, M, A, C), idx_complier = idx_complier, CACE =  mean(PO.diff.CACE)))
# }


############################## Backdoor and IV ################################
# simulate data where both backdoor and IV are correct
df <- dgp_backdoor_iv_bdoor_correct_i2_violated(n = n, beta = 0, beta_defier = 0)
data <- df$df

# estimate using AIPW (backdoor IF)
backdoor <- estimate_backdooor(data, confounderZ = TRUE)
backdoor.est <- backdoor$backdoor.est
backdoor.eif <- backdoor$backdoor.eif

# estimate using UIV (IV IF)
iv.eif <- uiv(data)
iv.est <- mean(iv.eif)
iv.eif <- iv.eif - iv.est

# CACE in true DGP
print(df$CACE)

# CIs for backdoor and iv
print(backdoor.est + c(-1,1) * qnorm(.975) * sd(backdoor.eif) / sqrt(n))
print(iv.est + c(-1,1) * qnorm(.975) * sd(iv.eif) / sqrt(n))



dgp_backdoor_iv_bdoor_correct_i3_violated <- function(n, beta = 10, beta_defier = 2) {
  
  # Unmeasured confounder
  U <- matrix(runif(n, min=-2,max=2))
  
  # Measured confounders (not a descendant of A, (B2) satisfied)
  C <- data.frame(matrix(runif(n * 4, min=-2,max=2), ncol=4))
  
  # Instrumental variable (randomized, (I1) satisfied)
  Z <- rbinom(n = n, size = 1, prob = 0.5)
  
  # Treatment (a function of C, not of U, (B1) satisfied)
  pA.UC <- expit(C[,1] + expit(C[,2]) + sin(C[,3]))
  A1 <- rbinom(n, size = 1, prob = pA.UC)
  A0 <- rbinom(n, size = 1, prob = 1-pA.UC)
  
  # (defiers exist, (I3) violated)
  idx_complier <- which(A1 == 1 & A0 == 0)
  idx_never <- which(A1 == 0 & A0 == 0)
  idx_always <- which(A1 == 1 & A0 == 1)
  idx_defier <- which(A1 == 0 & A0 == 1)
  
  #  A is determined by Z based on the four types of people, (I4) satisfied
  A <- ifelse(Z == 1, A1, A0)
  
  # Outcome (not a function directly of Z, (I2) satisfied)
  # Potential outcomes of Y
  error <- rnorm(n, mean = 0)
  Y1 <- beta*1 - 2*U + 2*sqrt(abs(C[,1])) + sin(C[,4]) + error
  Y0 <- beta*0 - 2*U + 2*sqrt(abs(C[,1])) + sin(C[,4]) + error
  Y1[idx_defier] <- beta_defier*1 + 5*U[idx_defier] - sin(C[,4][idx_defier]) + error[idx_defier]
  Y0[idx_defier] <- beta_defier*0 + 5*U[idx_defier] - sin(C[,4][idx_defier]) + error[idx_defier]
  Y <- ifelse(A == 1, Y1, Y0)
  
  PO.diff.CACE <- Y1[idx_complier] - Y0[idx_complier]
  
  return(list(df = data.frame(Y, Z, A, C), idx_complier = idx_complier, CACE =  mean(PO.diff.CACE)))
}

# dgp_backdoor_iv_iv_correct_b2_violated_alt <- function(n, beta = 4) {
#   
#   # Unmeasured confounder
#   U <- matrix(runif(n, min=-2,max=2))
#   
#   # Instrumental variable (randomized, (I1) satisfied)
#   Z <- rbinom(n = n, size = 1, prob = 0.5)
#   
#   # Measured confounders
#   C <- data.frame(matrix(runif(n * 4, min=-2,max=2), ncol=4))
#   
#   # Treatment (a function of C, not of U, (B1) satisfied)
#   pA.UC <- expit(C[,4] + expit(C[,2]) + sin(C[,3]))
#   A1 <- rbinom(n, size = 1, prob = pA.UC)
#   A0 <- rbinom(n, size = 1, prob = 1-pA.UC)
#   
#   # no defier, convert to compliers ((I3) satisfied)
#   idx_defier <- which(A1 == 0 & A0 == 1)
#   A1[idx_defier] <- 1
#   A0[idx_defier] <- 0
#   
#   idx_complier <- which(A1 == 1 & A0 == 0)
#   idx_never <- which(A1 == 0 & A0 == 0)
#   idx_always <- which(A1 == 1 & A0 == 1)
#   #  A is determined by Z based on the four types of people, (I4) satisfied
#   A <- ifelse(Z == 1, A1, A0)
#   
#   # Outcome (not a function directly of Z, Z only affects the outcome through the treatment, (I2) satisfied)
#   # potential outcomes of Y
#   error <- rnorm(n, mean = 0)
#   Y1 <- beta*1 - 3*U - sin(C[,4]) + error
#   Y0 <- beta*0 - 3*U - sin(C[,4]) + error
#   Y <- ifelse(A == 1, Y1, Y0) 
#   
#   # Measured confounders (a node in C is a collider, (B2) violated)
#   C[,1] <- rnorm(n, mean = ((0.5)*A + 2*Y))
#   
#   PO.diff.CACE <- Y1[idx_complier] - Y0[idx_complier]
#   
#   return(list(df = data.frame(Y, Z, A, C), idx_complier = idx_complier, CACE =  mean(PO.diff.CACE)))
# }




############################## Front-door and IV ###############################
# simulate data where both frontdoor and IV are correct
df <- dgp_frontdoor_iv_fdoor_correct_i3_violated(n, beta = 5)
data <- df$df

# estimate using APIPW (front door IF)
frontdoor <- estimate_frontdoor(data)
frontdoor.est <- frontdoor$frontdoor.est
frontdoor.eif <- frontdoor$frontdoor.eif

# estimate using UIV (IV IF)
iv <- estimate_uiv(data)
iv.est <- iv$iv.est
iv.eif <- iv$iv.eif

# CACE in true DGP
print(df$CACE)

# CIs for backdoor and iv
print(frontdoor.est + c(-1,1) * qnorm(.975) * sd(frontdoor.eif) / sqrt(n))
print(iv.est + c(-1,1) * qnorm(.975) * sd(iv.eif) / sqrt(n))








