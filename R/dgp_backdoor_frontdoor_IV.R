
######################### Backdoor, Front-door, and IV #########################
# 1. All assumptions are correct
# Backdoor criteria: (B1) measured confounders C block every path between A and Y that contains an arrow into A; 
#                    (B2) no node in C is a descendant of A
# Front-door criteria: (F1) M intercepts all directed paths from A to Y;
#                      (F2) no unblocked back-door paths from A to M;
#                      (F3) all back-door paths from M to Y are blocked (by A)
# IV assumptions: (I1) ignorability of the instrument (it is randomized or conditionally randomized);
#                 (I2) exclusion restriction;
#                 (I3) monotonicity (no defiers);
#                 (I4) non-zero correlation between instrument and treatment


dgp_backdoor_frontdoor_iv_all_correct <- function(n, beta = 3) {
  
  # Unmeasured confounder
  U <- matrix(runif(n, min=-2,max=2))
  
  # Measured confounders (not a descendant of A, (B2) satisfied)
  C <- data.frame(matrix(runif(n * 4, min=-2,max=2), ncol=4))
  
  # Instrumental variable (randomized, (I1) satisfied)
  Z <- rbinom(n = n, size = 1, prob = 0.5)
  
  # Treatment (not a function of U, (B1) satisfied; there is no unblocked backdoor path from A to M given C, (F2) satisfied)
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
  
  # potential outcomes of Y (not a function of A, (F1) satisfied; not a function directly of Z, (I2) satisfied)
  error <- rnorm(n, mean = 0)
  Y1 <- beta*M1 + 3*U + 2* sqrt(abs(C[,1])) + sin(C[,4]) + error
  Y0 <- beta*M0 + 3*U + 2* sqrt(abs(C[,1])) + sin(C[,4]) + error
  Y <- ifelse(A == 1, Y1, Y0) # Z affects Y only through A via M
  
  PO.diff.CACE <- Y1[idx_complier] - Y0[idx_complier]
  
  return(list(df = data.frame(Y, Z, M, A, C), idx_complier = idx_complier, CACE =  mean(PO.diff.CACE)))
}




# 2. Backdoor and front-door criterion satisfied, but IV assumptions violated 
# Violate the assumption (I1): there is an unblocked backdoor path from Z to Y
# Violate the assumption (I2): there is an direct effect from Z to Y
# Violate the assumption (I3): defiers exist
# Violate the assumption (I4): zero correlation between instrument and treatment

dgp_backdoor_frontdoor_iv_i2_violated <- function(n, beta = 3) {
  
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
  
  # potential outcomes of Y (not a function of A, (F1) satisfied; a function directly of Z, (I2) violated)
  # in the estimation, Z is controlled so backdoor assumptions still satisfied (confounderZ = TRUE)
  error <- rnorm(n, mean = 0)
  Y1 <- beta*M1 + U + 2* sqrt(abs(C[,1])) + sin(C[,4]) + 2*Z + error
  Y0 <- beta*M0 + U + 2* sqrt(abs(C[,1])) + sin(C[,4]) + 2*Z + error
  Y <- ifelse(A == 1, Y1, Y0) # Z affects Y only through A via M
  
  PO.diff.CACE <- Y1[idx_complier] - Y0[idx_complier]
  
  return(list(df = data.frame(Y, Z, M, A, C), idx_complier = idx_complier, CACE =  mean(PO.diff.CACE)))
}


dgp_backdoor_frontdoor_iv_i3_violated <- function(n, beta = 3) {
  
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
  pM1.AUC[idx_complier] <- expit(2 * 1 - 1 + C[,2][idx_complier]) 
  pM0.AUC[idx_complier] <- expit(2 * 0 - 1 + C[,2][idx_complier])
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

# Under the null
#! n = 10^6, IV CI: [1] -0.009082381  0.072305458

dgp_backdoor_frontdoor_iv_i3_violated_alt_nonzero <- function(n, beta = 3) {
  # the effect of A for deifers and others are the same
  
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
  pM1.AUC <- expit(2 * 1 - 1 + C[,2]) # pM1.AUC and pM0.AUC are the same for never takers and always takers, exclusion restriction satisfied
  pM0.AUC <- expit(2 * 0 - 1 + C[,2])
  pM1.AUC[idx_complier] <- expit(5 * 1 - 1 + C[,2][idx_complier]) 
  pM0.AUC[idx_complier] <- expit(5 * 0 - 1 + C[,2][idx_complier])
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

dgp_backdoor_frontdoor_iv_i3_violated_alt_zero <- function(n, beta = 3, coef_test = 2.162) {
  
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
# Under the alternative
#! n = 1500000, coef_test = 2.162, IV CI: [1] -0.08278859  0.10843418


# 3. Backdoor and IV assumptions satisfied, but front-door criterion violated
# Violate the assumption (F1): there is a direct effect of A on Y
# Violate the assumption (F2): there is an unblocked backdoor path from A to M, 
#                              but this also opens a backdoor path from A to Y, 
#                              violating the backdoor criteria, *so the simulation 
#                              is not going to violate (I2)*
# Violate the assumption (F3): there is an unblocked backdoor path from M to Y


dgp_backdoor_frontdoor_iv_f3_violated <- function(n, beta = 3) {
  
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

dgp_backdoor_frontdoor_iv_f1_violated <- function(n, beta = 3) {
  
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
#! n = 10^6, front-door CI: [1] -0.02846532  0.02620956
# Under the null
#! n = 10^6, front-door CI: [1] -0.03051245  0.02366004

# 4. Front-door and IV assumptions satisfied, but backdoor criterion violated
# Violate the assumption (B1): there is an unblocked backdoor path from A to Y.

# Violate the assumption (B2): a node in C is a descendant of A, 
#                              but conditioning on C opens a path between A and Y,
#                              (F1) M fully mediation assumption is violated,
#                              *so the simulation is not going to violate (B2)*


dgp_backdoor_frontdoor_iv_b1_violated <- function(n, beta = 3) {
  
  # Unmeasured confounder
  U <- matrix(runif(n, min=-2,max=2))
  
  # Measured confounders (not a descendant of A, (B2) satisfied)
  C <- data.frame(matrix(runif(n * 4, min=-2,max=2), ncol=4))
  
  # Instrumental variable (randomized, (I1) satisfied)
  Z <- rbinom(n = n, size = 1, prob = 0.5)
  
  # Treatment (a function of U, there is an unblocked backdoor path from A and Y, (B1) violated)
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
  
  # potential outcomes of mediator (not a function of U, so no unblocked backdoor path from A to M given C, (F2) satisfied; no unblocked backdoor path from M to Y given C, (F3) satisfied)
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


dgp_backdoor_frontdoor_iv_b1_violated_alt <- function(n, beta = 3, coef_seq = 0.97) {
  
  # Unmeasured confounder
  U <- matrix(runif(n, min=-2,max=2))
  
  # Measured confounders (not a descendant of A, (B2) satisfied)
  C <- data.frame(matrix(runif(n * 4, min=-2,max=2), ncol=4))
  
  # Instrumental variable (randomized, (I1) satisfied)
  Z <- rbinom(n = n, size = 1, prob = 0.5)
  
  # Treatment (a function of U, there is an unblocked backdoor path from A and Y, (B1) violated)
  pA.UC <- expit(-0.5 + 5*Z + C[,1] + expit(C[,2]) -coef_seq*U)
  A <- rbinom(n, size = 1, prob = pA.UC) # here we suppose there is no defier in the sample
  
  # potential outcomes of mediator (not a function of U, so no unblocked backdoor path from A to M given C, (F2) satisfied; no unblocked backdoor path from M to Y given C, (F3) satisfied)
  pM1.AUC <- expit(2 * 1 - 1 + C[,2]) # pM1.AUC and pM0.AUC are the same for never takers and always takers, exclusion restriction satisfied
  pM0.AUC <- expit(2 * 0 - 1 + C[,2])
  M1 <- rbinom(n, size = 1, prob = pM1.AUC) 
  M0 <- rbinom(n, size = 1, prob = pM0.AUC)
  M <- ifelse(A == 1, M1, M0) # Z affects M only through A
  
  # potential outcomes of Y (not a function of A, (F1) satisfied; not a function directly of Z, (I2) satisfied)
  error <- rnorm(n, mean = 0)
  Y1 <- beta*M1 + 5*U - 2* sqrt(abs(C[,1])) + sin(C[,4]) + error
  Y0 <- beta*M0 + 5*U - 2* sqrt(abs(C[,1])) + sin(C[,4]) + error
  Y <- ifelse(A == 1, Y1, Y0) # Z affects Y only through A via M
  
  return(list(df = data.frame(Y, Z, M, A, C)))
}

# Under the alternative
#! n = 10^6, coef_seq = 0.97, backdoor CI: [1] -0.04475339  0.02101321


dgp_backdoor_frontdoor_iv_b1_violated_null_zero <- function(n, beta = 3) {
  
  # Unmeasured confounder
  U <- matrix(runif(n, min=-2,max=2))
  
  # Measured confounders (not a descendant of A, (B2) satisfied)
  C <- data.frame(matrix(runif(n * 4, min=-2,max=2), ncol=4))
  
  # Instrumental variable (randomized, (I1) satisfied)
  Z <- rbinom(n = n, size = 1, prob = 0.5)
  
  # Treatment (a function of U, there is an unblocked backdoor path from A and Y, (B1) violated)
  pA.UC <- expit(C[,1] + expit(C[,2]) + sin(C[,3]) - U)
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
  
  # potential outcomes of mediator (not a function of U, so no unblocked backdoor path from A to M given C, (F2) satisfied; no unblocked backdoor path from M to Y given C, (F3) satisfied)
  pM1.AUC <- expit(5 * 1 - 1 + C[,2]) # pM1.AUC and pM0.AUC are the same for never takers and always takers, exclusion restriction satisfied
  pM0.AUC <- expit(5 * 0 - 1 + C[,2])
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

# Under the null
#! n = 10^6, backdoor CI: [1] -0.005369013  0.006632432


# 5. Backdoor assumptions satisfied, but front-door and IV assumptions violated
# Violate the assumption (I1)/(I2): there is an unblocked backdoor path from Z to Y/there is a direct effect of Z on Y
# Violate the assumption (I3): defiers exist
# Violate the assumption (I4): zero correlation between instrument and treatment (the identified iv functional will be zero)
# Violate the assumption (F1): there is a direct effect of A on Y (the identified frontdoor functional will be zero)
# Violate the assumption (F2): there is an unblocked backdoor path from A to M. 
#                              This opens a backdoor path between A and Y, violating (B1) assumption, 
#                              *so the simulation is not going to violate (F2)*
# Violate the assumption (F3): there is a unblocked backdoor path from M to Y

dgp_backdoor_frontdoor_iv_bdoor_correct_f1i3_violated <- function(n, beta = 1.5) {
  
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
  pM1.AUC[idx_complier] <- expit(2 * 1 - 1 + C[,2][idx_complier]) 
  pM0.AUC[idx_complier] <- expit(2 * 0 - 1 + C[,2][idx_complier])
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
#! n = 10^6, front-door CI: [1] -0.01434822  0.01052447


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
  pM1.AUC <- expit(2 * 1 - 1 + C[,2] + U) # pM1.AUC and pM0.AUC are the same for never takers and always takers, exclusion restriction satisfied
  pM0.AUC <- expit(2 * 0 - 1 + C[,2] + U)
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

# Under the null
#! n = 2*10^6, IV CI: [1] -0.07459704  0.04311529



dgp_backdoor_frontdoor_iv_bdoor_correct_f3i2_violated <- function(n, beta = 3) {
  
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
  pM1.AUC <- expit(2 * 1 - 1 + C[,2] + U) # pM1.AUC and pM0.AUC are the same for never takers and always takers, exclusion restriction satisfied
  pM0.AUC <- expit(2 * 0 - 1 + C[,2] + U)
  M1 <- rbinom(n, size = 1, prob = pM1.AUC) 
  M0 <- rbinom(n, size = 1, prob = pM0.AUC)
  M <- ifelse(A == 1, M1, M0) # Z affects M only through A
  
  # potential outcomes of Y (not a function of A, (F1) satisfied; a function directly of Z, (I2) violated)
  #  in the estimation, Z is controlled so backdoor assumptions still satisfied (confounderZ = TRUE)
  error <- rnorm(n, mean = 0)
  Y1 <- beta*M1 - 3*U + 2* sqrt(abs(C[,1])) + sin(C[,4]) + 2*Z + error
  Y0 <- beta*M0 - 3*U + 2* sqrt(abs(C[,1])) + sin(C[,4]) + 2*Z + error
  Y <- ifelse(A == 1, Y1, Y0) # Z affects Y only through A via M
  
  PO.diff.CACE <- Y1[idx_complier] - Y0[idx_complier]
  
  return(list(df = data.frame(Y, Z, M, A, C), idx_complier = idx_complier, CACE =  mean(PO.diff.CACE)))
}



# 6. Front-door criterion satisfied, but backdoor and IV assumptions are violated
# Violate the assumption (B1): there is an unblocked backdoor path from A to Y.
# Violate the assumption (B2): a node in C is a descendant of A. 
#                              Violate B2 will also violate F1, 
#                              so *the simulation is not going to violate (B2)*
# Violate the assumption (I1): there is an unblocked backdoor path from Z to Y. 
# Violate the assumption (I2): there is an direct effect from Z to Y
# Violate the assumption (I3): defiers exist
# Violate the assumption (I4): zero correlation between instrument and treatment



dgp_backdoor_frontdoor_iv_fdoor_correct_b1i1_violated <- function(n, beta = 3) {
  
  # Unmeasured confounder
  U <- matrix(runif(n, min=-2,max=2))
  U1 <- matrix(rnorm(n, mean = 2, sd = 1))
  
  # Measured confounders (not a descendant of A, (B2) satisfied)
  C <- data.frame(matrix(runif(n * 4, min=-2,max=2), ncol=4))
  
  # Instrumental variable (a function of U, (I1) violated)
  pZ.C <- expit(2 + 2*U)
  Z <- rbinom(n = n, size = 1, prob = pZ.C)
  
  # Treatment (a function of U1, there is an unblocked backdoor path from A and Y, (B1) violated)
  pA.UC <- expit(C[,1] + expit(C[,2]) + sin(C[,3]) + U1)
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
  
  # potential outcomes of mediator (not a function of U, so no unblocked backdoor path from A to M given C, (F2) satisfied; no unblocked backdoor path from M to Y given C, (F3) satisfied)
  pM1.AUC <- expit(2 * 1 - 1 + C[,2]) # pM1.AUC and pM0.AUC are the same for never takers and always takers, exclusion restriction satisfied
  pM0.AUC <- expit(2 * 0 - 1 + C[,2])
  M1 <- rbinom(n, size = 1, prob = pM1.AUC) 
  M0 <- rbinom(n, size = 1, prob = pM0.AUC)
  M <- ifelse(A == 1, M1, M0) # Z affects M only through A
  
  # potential outcomes of Y (not a function of A, (F1) satisfied; not a function directly of Z, (I2) satisfied)
  error <- rnorm(n, mean = 0)
  Y1 <- beta*M1 + 2*U + U1 + 2* sqrt(abs(C[,1])) + sin(C[,4]) + error
  Y0 <- beta*M0 + 2*U + U1 + 2* sqrt(abs(C[,1])) + sin(C[,4]) + error
  Y <- ifelse(A == 1, Y1, Y0) # Z affects Y only through A via M
  
  PO.diff.CACE <- Y1[idx_complier] - Y0[idx_complier]
  
  return(list(df = data.frame(Y, Z, M, A, C), idx_complier = idx_complier, CACE =  mean(PO.diff.CACE)))
}


dgp_backdoor_frontdoor_iv_fdoor_correct_b1i2_violated <- function(n, beta = 3) {
  
  # Unmeasured confounder
  U <- matrix(runif(n, min=-2,max=2))
  
  # Measured confounders (not a descendant of A, (B2) satisfied)
  C <- data.frame(matrix(runif(n * 4, min=-2,max=2), ncol=4))
  
  # Instrumental variable (randomized, (I1) satisfied)
  Z <- rbinom(n = n, size = 1, prob = 0.5)
  
  # Treatment (a function of U, there is an unblocked backdoor path from A and Y, (B1) violated)
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
  
  # potential outcomes of mediator (not a function of U, so no unblocked backdoor path from A to M given C, (F2) satisfied; no unblocked backdoor path from M to Y given C, (F3) satisfied)
  pM1.AUC <- expit(2 * 1 - 1 + C[,2]) # pM1.AUC and pM0.AUC are the same for never takers and always takers, exclusion restriction satisfied
  pM0.AUC <- expit(2 * 0 - 1 + C[,2])
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


dgp_backdoor_frontdoor_iv_fdoor_correct_b1i3_violated <- function(n, beta = 3) {
  
  # Unmeasured confounder
  U <- matrix(runif(n, min=-2,max=2))
  
  # Measured confounders (not a descendant of A, (B2) satisfied)
  C <- data.frame(matrix(runif(n * 4, min=-2,max=2), ncol=4))
  
  # Instrumental variable (randomized, (I1) satisfied)
  Z <- rbinom(n = n, size = 1, prob = 0.5)
  
  # Treatment (a function of U, there is an unblocked backdoor path from A and Y, (B1) violated)
  pA.UC <- expit(C[,1] + expit(C[,2]) + sin(C[,3]) + U)
  A1 <- rbinom(n, size = 1, prob = pA.UC)
  A0 <- rbinom(n, size = 1, prob = 1-pA.UC)
  
  # have defier, (I3) violated
  idx_complier <- which(A1 == 1 & A0 == 0)
  idx_never <- which(A1 == 0 & A0 == 0)
  idx_always <- which(A1 == 1 & A0 == 1)
  idx_defier <- which(A1 == 0 & A0 == 1)
  
  #  A is determined by Z based on the four types of people, (I4) satisfied
  A <- ifelse(Z == 1, A1, A0)
  
  # potential outcomes of mediator (not a function of U, so no unblocked backdoor path from A to M given C, (F2) satisfied; no unblocked backdoor path from M to Y given C, (F3) satisfied)
  pM1.AUC <- expit(5 * 1 - 1 + C[,2]) # pM1.AUC and pM0.AUC are the same for never takers and always takers, exclusion restriction satisfied
  pM0.AUC <- expit(5 * 0 - 1 + C[,2])
  pM1.AUC[idx_complier] <- expit(2 * 1 - 1 + C[,2][idx_complier]) # the effects of A for defiers and compliers are different
  pM0.AUC[idx_complier] <- expit(2 * 0 - 1 + C[,2][idx_complier])
  M1 <- rbinom(n, size = 1, prob = pM1.AUC) 
  M0 <- rbinom(n, size = 1, prob = pM0.AUC)
  M <- ifelse(A == 1, M1, M0) # Z affects M only through A
  
  # potential outcomes of Y (not a function of A, (F1) satisfied; not a function of Z, (I2) satisfied)
  error <- rnorm(n, mean = 0)
  Y1 <- beta*M1 + U + 2* sqrt(abs(C[,1])) + sin(C[,4]) + error
  Y0 <- beta*M0 + U + 2* sqrt(abs(C[,1])) + sin(C[,4]) + error
  Y <- ifelse(A == 1, Y1, Y0) # Z affects Y only through A via M
  
  PO.diff.CACE <- Y1[idx_complier] - Y0[idx_complier]
  
  return(list(df = data.frame(Y, Z, M, A, C), idx_complier = idx_complier, CACE =  mean(PO.diff.CACE)))
}

# Under the null
#! n = 10^6, backdoor CI: [1] -0.009164041  0.002827996

dgp_backdoor_frontdoor_iv_fdoor_correct_b1i3_violated_alt <- function(n, beta = 3, coef_seq = 2.37) {
  
  # Unmeasured confounder
  U <- matrix(runif(n, min=-2,max=2))
  
  # Measured confounders (not a descendant of A, (B2) satisfied)
  C <- data.frame(matrix(runif(n * 4, min=-2,max=2), ncol=4))
  
  # Instrumental variable (randomized, (I1) satisfied)
  Z <- rbinom(n = n, size = 1, prob = 0.5)
  
  # Treatment (a function of U, there is an unblocked backdoor path from A and Y, (B1) violated)
  pA.UC <- expit(C[,1] + expit(C[,2]) + sin(C[,3]) + U)
  A1 <- rbinom(n, size = 1, prob = pA.UC)
  A0 <- rbinom(n, size = 1, prob = 1-pA.UC)
  
  # have defier, (I3) violated
  idx_complier <- which(A1 == 1 & A0 == 0)
  idx_never <- which(A1 == 0 & A0 == 0)
  idx_always <- which(A1 == 1 & A0 == 1)
  idx_defier <- which(A1 == 0 & A0 == 1)
  
  #  A is determined by Z based on the four types of people, (I4) satisfied
  A <- ifelse(Z == 1, A1, A0)
  
  # potential outcomes of mediator (not a function of U, so no unblocked backdoor path from A to M given C, (F2) satisfied; no unblocked backdoor path from M to Y given C, (F3) satisfied)
  pM1.AUC <- expit(5 * 1 - 1 + C[,2]) # pM1.AUC and pM0.AUC are the same for never takers and always takers, exclusion restriction satisfied
  pM0.AUC <- expit(5 * 0 - 1 + C[,2])
  pM1.AUC[idx_complier] <- expit(coef_seq * 1 - 1 + C[,2][idx_complier]) # the effects of A for defiers and compliers are different
  pM0.AUC[idx_complier] <- expit(coef_seq * 0 - 1 + C[,2][idx_complier])
  M1 <- rbinom(n, size = 1, prob = pM1.AUC) 
  M0 <- rbinom(n, size = 1, prob = pM0.AUC)
  M <- ifelse(A == 1, M1, M0) # Z affects M only through A
  
  # potential outcomes of Y (not a function of A, (F1) satisfied; not a function of Z, (I2) satisfied)
  error <- rnorm(n, mean = 0)
  Y1 <- beta*M1 + U + 2* sqrt(abs(C[,1])) + sin(C[,4]) + error
  Y0 <- beta*M0 + U + 2* sqrt(abs(C[,1])) + sin(C[,4]) + error
  Y <- ifelse(A == 1, Y1, Y0) # Z affects Y only through A via M
  
  PO.diff.CACE <- Y1[idx_complier] - Y0[idx_complier]
  
  return(list(df = data.frame(Y, Z, M, A, C), idx_complier = idx_complier, CACE =  mean(PO.diff.CACE)))
}

# Under the alternative
#! n = 2*10^6, coef_seq = 2.37, IV CI: [1] -0.10130145  0.08345881


# 7. IV assumptions correct, but backdoor and front door assumptions are violated
# Violate the assumption (B1): there is an unblocked backdoor path from A to Y
# Violate the assumption (B2): a node in C is a descendant of A
#                              Violate B2 will also violate F1
# Violate the assumption (F1): there is a direct effect of A on Y
# Violate the assumption (F2): there is an unblocked backdoor path from A to M
#                              Violate F2 will also violate B1
# Violate the assumption (F3): there is an unblocked backdoor path from M to Y


dgp_backdoor_frontdoor_iv_iv_correct_b1f2f3_violated <- function(n, beta = 3) {
  
  # Unmeasured confounder
  U <- matrix(runif(n, min=-2,max=2))
  
  # Measured confounders (not a descendant of A, (B2) satisfied)
  C <- data.frame(matrix(runif(n * 4, min=-2,max=2), ncol=4))
  
  # Instrumental variable (randomized, (I1) satisfied)
  Z <- rbinom(n = n, size = 1, prob = 0.5)
  
  # Treatment (a function of U, (B1) violated; there is an unblocked backdoor path from A to M given C, (F2) violated)
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

# Under the null
#! n = 10^6, backdoor CI: [1] -0.01293883  0.01532863

dgp_backdoor_frontdoor_iv_iv_correct_b2f1f2_violated <- function(n, beta = 3) {
  
  # Unmeasured confounder
  U <- matrix(runif(n, min=-2,max=2))
  
  # Measured confounders 
  C <- data.frame(matrix(runif(n * 4, min=-2,max=2), ncol=4))
  
  # Instrumental variable (randomized, (I1) satisfied)
  Z <- rbinom(n = n, size = 1, prob = 0.5)
  
  # Treatment (Y is not a function of U, (B1) satisfied; there is an unblocked backdoor path from A to M given C, (F2) violated)
  pA.UC <- expit(C[,4] + sin(C[,3]) - U)
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
  
  # potential outcomes of mediator (Y is not a function of U, so there is no unblocked backdoor path from M to Y given C, (F3) satisfied)
  pM1.AUC <- expit(5 * 1 - 1 + C[,2] - 2*U) # pM1.AUC and pM0.AUC are the same for never takers and always takers, exclusion restriction satisfied
  pM0.AUC <- expit(5 * 0 - 1 + C[,2] - 2*U)
  M1 <- rbinom(n, size = 1, prob = pM1.AUC) 
  M0 <- rbinom(n, size = 1, prob = pM0.AUC)
  M <- ifelse(A == 1, M1, M0) # Z affects M only through A
  
  # potential outcomes of Y (not a function directly of Z, (I2) satisfied)
  error <- rnorm(n, mean = 0)
  Y1 <- beta*M1 - 5*sin(C[,4]) + error
  Y0 <- beta*M0 - 5*sin(C[,4]) + error
  Y <- ifelse(A == 1, Y1, Y0) # Z affects Y only through A via M
  
  # Measured confounders (a node in C is a collider, (B2) violated; 
  #                       conditioning on the collider opens a path from A to Y, violating (F1))
  C[,1] <- rnorm(n, mean = (-2*A - 5*Y))
  
  PO.diff.CACE <- Y1[idx_complier] - Y0[idx_complier]
  
  return(list(df = data.frame(Y, Z, M, A, C), idx_complier = idx_complier, CACE =  mean(PO.diff.CACE)))
}

# Under the alternative
#! n = 10^6, the above coefficients, front-door CI: [1] -1.668616e-07  3.319552e-07


dgp_backdoor_frontdoor_iv_iv_correct_b2f3_violated <- function(n, beta = 3) {
  
  # Unmeasured confounder
  U <- matrix(runif(n, min=-2,max=2))
  
  # Measured confounders 
  C <- data.frame(matrix(runif(n * 4, min=-2,max=2), ncol=4))
  
  # Instrumental variable (randomized, (I1) satisfied)
  Z <- rbinom(n = n, size = 1, prob = 0.5)
  
  # Treatment (Y is not a function of U, (B1) satisfied; (F2) satisfied)
  pA.UC <- expit(C[,4] + expit(C[,2]) + sin(C[,3]))
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
  pM1.AUC <- expit(5 * 1 - 1 + C[,2] + 2*U) # pM1.AUC and pM0.AUC are the same for never takers and always takers, exclusion restriction satisfied
  pM0.AUC <- expit(5 * 0 - 1 + C[,2] + 2*U)
  M1 <- rbinom(n, size = 1, prob = pM1.AUC) 
  M0 <- rbinom(n, size = 1, prob = pM0.AUC)
  M <- ifelse(A == 1, M1, M0) # Z affects M only through A
  
  # potential outcomes of Y (not a function of A, (F1) satisfied; not a function directly of Z, (I2) satisfied)
  error <- rnorm(n, mean = 0)
  Y1 <- beta*M1 + sin(C[,4]) + U + error
  Y0 <- beta*M0 + sin(C[,4]) + U + error
  Y <- ifelse(A == 1, Y1, Y0) # Z affects Y only through A via M
  
  # Measured confounders (a node in C is a collider, (B2) violated)
  C[,1] <- rnorm(n, mean = (3*A - Y))
  
  PO.diff.CACE <- Y1[idx_complier] - Y0[idx_complier]
  
  return(list(df = data.frame(Y, Z, M, A, C), idx_complier = idx_complier, CACE =  mean(PO.diff.CACE)))
}


