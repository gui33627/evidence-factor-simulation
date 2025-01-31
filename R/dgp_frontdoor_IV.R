
############################## Front-door and IV ###############################
# 1. Both criteria are correct
# Front-door criteria: (F1) M intercepts all directed paths from A to Y;
#                      (F2) no unblocked back-door paths from A to M;
#                      (F3) all back-door paths from M to Y are blocked (by A)
# IV assumptions: (I1) ignorability of the instrument (it is randomized or conditionally randomized);
#                 (I2) exclusion restriction;
#                 (I3) monotonicity (no defiers);
#                 (I4) non-zero correlation between instrument and treatment


dgp_frontdoor_iv_both_correct <- function(n, beta = 3) {
  
  # Unmeasured confounder
  U <- matrix(runif(n, min=-2,max=2))
  
  # Measured confounders 
  C <- data.frame(matrix(runif(n * 4, min=-2,max=2), ncol=4))
  
  # Instrumental variable (randomized, (I1) satisfied)
  Z <- rbinom(n = n, size = 1, prob = 0.5)
  
  # Treatment (a function of U, so backdoor cannot be used)
  pA.UC <- expit(C[,1] + expit(C[,2]) + sin(C[,3]) + U)
  A1 <- rbinom(n, size = 1, prob = pA.UC)
  A0 <- rbinom(n, size = 1, prob = 1-pA.UC)
  
  # no defier, convert to compliers ((I3) satisfied)
  idx_defier <- which(A1 == 0 & A0 == 1)
  A1[idx_defier] <- 1
  A0[idx_defier] <- 0
  
  idx_complier <- which(A1 == 1 & A0 == 0)
  idx_never <- which(A1 == 0 & A0 == 0)
  idx_always <- which(A1 == 1 & A0 == 1)
  
  #  A is determined by Z based on the four types of people, (I4) satisfied
  A <- ifelse(Z == 1, A1, A0)
  
  # potential outcomes of mediator (not a function of U, so there is no unblocked backdoor path from A to M given C, (F2) satisfied; no unblocked backdoor path from M to Y given C, (F3) satisfied)
  pM1.AUC <- expit(5 * 1 - 1 + C[,2]) # pM1.AUC and pM0.AUC are the same for never takers and always takers, exclusion restriction satisfied
  pM0.AUC <- expit(5 * 0 - 1 + C[,2])
  M1 <- rbinom(n, size = 1, prob = pM1.AUC) 
  M0 <- rbinom(n, size = 1, prob = pM0.AUC)
  M <- ifelse(A == 1, M1, M0) # Z affects M only through A
  
  # potential outcomes of Y (not a function of A, (F1) satisfied; not a function directly of Z, (I2) satisfied)
  error <- rnorm(n, mean = 0)
  Y1 <- beta*M1 + 3*U + 2* sqrt(abs(C[,1])) + sin(C[,4]) + error
  Y0 <- beta*M0 + 3*U + 2* sqrt(abs(C[,1])) + sin(C[,4]) + error
  Y <- ifelse(A == 1, Y1, Y0) # Z affects Y only through A via M
  
  PO.diff.CACE <- Y1[idx_complier] - Y0[idx_complier]
  
  return(list(df = data.frame(Y, Z, M, A, C), idx_complier = idx_complier, CACE =  mean(PO.diff.CACE)))
}



# 2. Front-door criterion satisfied, but IV assumptions violated
# Violate the assumption (I1): there is an unblocked backdoor path from Z to Y
# Violate the assumption (I2): there is an direct effect from Z to Y
# Violate the assumption (I3): defiers exist
# Violate the assumption (I4): zero correlation between instrument and treatment

dgp_frontdoor_iv_fdoor_correct_i2_violated <- function(n, beta = 3) {
  
  # Unmeasured confounder
  U <- matrix(runif(n, min=-2,max=2))
  
  # Measured confounders 
  C <- data.frame(matrix(runif(n * 4, min=-2,max=2), ncol=4))
  
  # Instrumental variable (randomized, (I1) satisfied)
  Z <- rbinom(n = n, size = 1, prob = 0.5)
  
  # Treatment (a function of U, so backdoor cannot be used) 
  pA.UC <- expit(C[,1] + expit(C[,2]) + sin(C[,3]) + U)
  A1 <- rbinom(n, size = 1, prob = pA.UC)
  A0 <- rbinom(n, size = 1, prob = 1-pA.UC)
  
  # no defier, convert to compliers ((I3) satisfied)
  idx_defier <- which(A1 == 0 & A0 == 1)
  A1[idx_defier] <- 1
  A0[idx_defier] <- 0
  
  idx_complier <- which(A1 == 1 & A0 == 0)
  idx_never <- which(A1 == 0 & A0 == 0)
  idx_always <- which(A1 == 1 & A0 == 1)
  #  A is determined by Z based on the four types of people, (I4) satisfied
  A <- ifelse(Z == 1, A1, A0)
  
  # potential outcomes of mediator (not a function of U, so there is no unblocked backdoor path from A to M given C, (F2) satisfied; no unblocked backdoor path from M to Y given C, (F3) satisfied)
  pM1.AUC <- expit(5 * 1 - 1 + C[,2]) # pM1.AUC and pM0.AUC are the same for never takers and always takers, exclusion restriction satisfied
  pM0.AUC <- expit(5 * 0 - 1 + C[,2])
  M1 <- rbinom(n, size = 1, prob = pM1.AUC) 
  M0 <- rbinom(n, size = 1, prob = pM0.AUC)
  M <- ifelse(A == 1, M1, M0) # Z affects M only through A
  
  # potential outcomes of Y (not a function of A, (F1) satisfied; a function of Z, (I2) violated)
  error <- rnorm(n, mean = 0)
  Y1 <- beta*M1 + 3*U + 2* sqrt(abs(C[,1])) + sin(C[,4]) + 2*Z + error
  Y0 <- beta*M0 + 3*U + 2* sqrt(abs(C[,1])) + sin(C[,4]) + 2*Z + error
  Y <- ifelse(A == 1, Y1, Y0) # Z affects Y only through A via M
  
  PO.diff.CACE <- Y1[idx_complier] - Y0[idx_complier]
  
  return(list(df = data.frame(Y, Z, M, A, C), idx_complier = idx_complier, CACE =  mean(PO.diff.CACE)))
}


dgp_frontdoor_iv_fdoor_correct_i3_violated <- function(n, beta = 3) {
  
  # Unmeasured confounder
  U <- matrix(runif(n, min=-2,max=2))
  
  # Measured confounders 
  C <- data.frame(matrix(runif(n * 4, min=-2,max=2), ncol=4))
  
  # Instrumental variable (randomized, (I1) satisfied)
  Z <- rbinom(n = n, size = 1, prob = 0.5)
  
  # Treatment (a function of U, so backdoor cannot be used) 
  pA.UC <- expit(C[,1] + expit(C[,2]) + sin(C[,3]) + U)
  A1 <- rbinom(n, size = 1, prob = pA.UC)
  A0 <- rbinom(n, size = 1, prob = 1-pA.UC)
  
  # (defiers exist, (I3) violated)
  idx_complier <- which(A1 == 1 & A0 == 0)
  idx_never <- which(A1 == 0 & A0 == 0)
  idx_always <- which(A1 == 1 & A0 == 1)
  idx_defier <- which(A1 == 0 & A0 == 1)
  
  #  A is determined by Z based on the four types of people, (I4) satisfied
  A <- ifelse(Z == 1, A1, A0)
  
  # potential outcomes of mediator (not a function of U, so there is no unblocked backdoor path from A to M given C, (F2) satisfied; no unblocked backdoor path from M to Y given C, (F3) satisfied)
  pM1.AUC <- expit(5 * 1 - 1 + C[,2]) # pM1.AUC and pM0.AUC are the same for never takers and always takers, exclusion restriction satisfied
  pM0.AUC <- expit(5 * 0 - 1 + C[,2])
  pM1.AUC[idx_complier] <- expit(2 * 1 - 1 + C[,2][idx_complier]) # the effects of A for defiers and compliers are different
  pM0.AUC[idx_complier] <- expit(2 * 0 - 1 + C[,2][idx_complier])
  M1 <- rbinom(n, size = 1, prob = pM1.AUC) 
  M0 <- rbinom(n, size = 1, prob = pM0.AUC)
  M <- ifelse(A == 1, M1, M0) # Z affects M only through A
  
  # potential outcomes of Y (not a function of A, (F1) satisfied; not a function directly of Z, (I2) satisfied)
  error <- rnorm(n, mean = 0)
  Y1 <- beta*M1 + 2*U + 2* sqrt(abs(C[,1])) + sin(C[,4]) + error
  Y0 <- beta*M0 + 2*U + 2* sqrt(abs(C[,1])) + sin(C[,4]) + error
  Y <- ifelse(A == 1, Y1, Y0) # Z affects Y only through A via M
  
  PO.diff.CACE <- Y1[idx_complier] - Y0[idx_complier]
  
  return(list(df = data.frame(Y, Z, M, A, C), idx_complier = idx_complier, CACE =  mean(PO.diff.CACE)))
}

# under the null
# n = 10^6, IV CI: [1] -0.03650516  0.10040055

dgp_frontdoor_iv_fdoor_correct_i3_violated_alt_zero <- function(n, beta = 3, coef_test = 2.38) {
  
  # Unmeasured confounder
  U <- matrix(runif(n, min=-2,max=2))
  
  # Measured confounders 
  C <- data.frame(matrix(runif(n * 4, min=-2,max=2), ncol=4))
  
  # Instrumental variable (randomized, (I1) satisfied)
  Z <- rbinom(n = n, size = 1, prob = 0.5)
  
  # Treatment (a function of U, so backdoor cannot be used) 
  pA.UC <- expit(C[,1] + expit(C[,2]) + sin(C[,3]) + U)
  A1 <- rbinom(n, size = 1, prob = pA.UC)
  A0 <- rbinom(n, size = 1, prob = 1-pA.UC)
  
  # (defiers exist, (I3) violated)
  idx_complier <- which(A1 == 1 & A0 == 0)
  idx_never <- which(A1 == 0 & A0 == 0)
  idx_always <- which(A1 == 1 & A0 == 1)
  idx_defier <- which(A1 == 0 & A0 == 1)
  
  #  A is determined by Z based on the four types of people, (I4) satisfied
  A <- ifelse(Z == 1, A1, A0)
  
  # potential outcomes of mediator (not a function of U, so there is no unblocked backdoor path from A to M given C, (F2) satisfied; no unblocked backdoor path from M to Y given C, (F3) satisfied)
  pM1.AUC <- expit(5 * 1 - 1 + C[,2]) # pM1.AUC and pM0.AUC are the same for never takers and always takers, exclusion restriction satisfied
  pM0.AUC <- expit(5 * 0 - 1 + C[,2])
  pM1.AUC[idx_complier] <- expit(coef_test * 1 - 1 + C[,2][idx_complier]) # the effects of A for defiers and compliers are different
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

# under the alternative
#! n = 2000000, 2.38: IV CI: [1] -0.09589117  0.08792879

# 3. IV assumptions satisfied, but front-door criterion violated
# Violate the assumption (F1): there is a direct effect of A on Y
# Violate the assumption (F2): there is an unblocked backdoor path from A to M
# Violate the assumption (F3): there is a unblocked backdoor path from M to Y


dgp_frontdoor_iv_iv_correct_f1_violated <- function(n, beta = 1.5) {
  
  # Unmeasured confounder
  U <- matrix(runif(n, min=-2,max=2))
  
  # Measured confounders 
  C <- data.frame(matrix(runif(n * 4, min=-2,max=2), ncol=4))
  
  # Instrumental variable (randomized, (I1) satisfied)
  Z <- rbinom(n = n, size = 1, prob = 0.5)
  
  # Treatment (a function of C, not of U, there is no unblocked backdoor path from A to M given C, (F2) satisfied)
  pA.UC <- expit(C[,1] + expit(C[,2]) + sin(C[,3]))
  A1 <- rbinom(n, size = 1, prob = pA.UC)
  A0 <- rbinom(n, size = 1, prob = 1-pA.UC)
  
  # no defier, convert to compliers ((I3) satisfied)
  idx_defier <- which(A1 == 0 & A0 == 1)
  A1[idx_defier] <- 1
  A0[idx_defier] <- 0
  
  idx_complier <- which(A1 == 1 & A0 == 0)
  idx_never <- which(A1 == 0 & A0 == 0)
  idx_always <- which(A1 == 1 & A0 == 1)
  #  A is determined by Z based on the four types of people, (I4) satisfied
  A <- ifelse(Z == 1, A1, A0)
  
  # potential outcomes of mediator (not a function of U, so no unblocked backdoor path from M to Y given C, (F3) satisfied)
  pM1.AUC <- expit(5 * 1 - 1 + C[,2]) # pM1.AUC and pM0.AUC are the same for never takers and always takers, exclusion restriction satisfied
  pM0.AUC <- expit(5 * 0 - 1 + C[,2])
  M1 <- rbinom(n, size = 1, prob = pM1.AUC) 
  M0 <- rbinom(n, size = 1, prob = pM0.AUC)
  M <- ifelse(A == 1, M1, M0) # Z affects M only through A
  
  # potential outcomes of Y (a function of A, so there is a direct A->Y, (F1) violated; not a function directly of Z, (I2) satisfied)
  error <- rnorm(n, mean = 0)
  Y1 <- beta*1 + 3*U + 2* sqrt(abs(C[,1])) + sin(C[,4]) + error
  Y0 <- beta*0 + 3*U + 2* sqrt(abs(C[,1])) + sin(C[,4]) + error
  Y <- ifelse(A == 1, Y1, Y0) # Z affects Y only through A 
  
  PO.diff.CACE <- Y1[idx_complier] - Y0[idx_complier]
  
  return(list(df = data.frame(Y, Z, M, A, C), idx_complier = idx_complier, CACE =  mean(PO.diff.CACE)))
}

# Under the alternative
# n = 10^6, front-door CI: [1] -0.03292823  0.02081113
# Under the null
# n = 10^6, front-door CI: [1] -0.04193629  0.01204803

dgp_frontdoor_iv_iv_correct_f3_violated <- function(n, beta = 3) {
  
  # Unmeasured confounder
  U <- matrix(runif(n, min=-2,max=2))
  
  # Measured confounders 
  C <- data.frame(matrix(runif(n * 4, min=-2,max=2), ncol=4))
  
  # Instrumental variable (randomized, (I1) satisfied)
  Z <- rbinom(n = n, size = 1, prob = 0.5)
  
  # Treatment (a function of C, not of U, there is no unblocked backdoor path from A to M given C, (F2) satisfied)
  pA.UC <- expit(C[,1] + expit(C[,2]) + sin(C[,3]))
  A1 <- rbinom(n, size = 1, prob = pA.UC)
  A0 <- rbinom(n, size = 1, prob = 1-pA.UC)
  
  # no defier, convert to compliers ((I3) satisfied)
  idx_defier <- which(A1 == 0 & A0 == 1)
  A1[idx_defier] <- 1
  A0[idx_defier] <- 0
  
  idx_complier <- which(A1 == 1 & A0 == 0)
  idx_never <- which(A1 == 0 & A0 == 0)
  idx_always <- which(A1 == 1 & A0 == 1)
  #  A is determined by Z based on the four types of people, (I4) satisfied
  A <- ifelse(Z == 1, A1, A0)
  
  # potential outcomes of mediator (a function of U, so there is an unblocked backdoor path from M to Y given C, (F3) violated)
  pM1.AUC <- expit(3 * 1 - 1 + C[,2] + U) # pM1.AUC and pM0.AUC are the same for never takers and always takers, exclusion restriction satisfied
  pM0.AUC <- expit(3 * 0 - 1 + C[,2] + U)
  M1 <- rbinom(n, size = 1, prob = pM1.AUC) 
  M0 <- rbinom(n, size = 1, prob = pM0.AUC)
  M <- ifelse(A == 1, M1, M0) # Z affects M only through A
  
  # potential outcomes of Y (not a function of A, (F1) satisfied; not a function directly of Z, (I2) satisfied)
  error <- rnorm(n, mean = 0)
  Y1 <- beta*M1 + 3*U + 2* sqrt(abs(C[,1])) + sin(C[,4]) + error
  Y0 <- beta*M0 + 3*U + 2* sqrt(abs(C[,1])) + sin(C[,4]) + error
  Y <- ifelse(A == 1, Y1, Y0) # Z affects Y only through A via M
  
  PO.diff.CACE <- Y1[idx_complier] - Y0[idx_complier]
  
  return(list(df = data.frame(Y, Z, M, A, C), idx_complier = idx_complier, CACE =  mean(PO.diff.CACE)))
}
