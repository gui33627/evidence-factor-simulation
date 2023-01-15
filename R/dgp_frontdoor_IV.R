
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
  
  # Treatment (a function of C, not of U, there is no unblocked backdoor path from A to M given C, (F2) satisfied)
  pA.UC <- expit(C[,1] + expit(C[,2]) + sin(C[,3]))
  A1 <- rbinom(n, size = 1, prob = pA.UC)
  A0 <- rbinom(n, size = 1, prob = 1-pA.UC)
  
  # no defier, convert to compliers ((I3) satisfied)
  A1[which(A1 == 0 & A0 == 1)] <- 1
  A0[which(A1 == 0 & A0 == 1)] <- 0
  
  idx_complier <- which(A1 == 1 & A0 == 0)
  idx_never <- which(A1 == 0 & A0 == 0)
  idx_always <- which(A1 == 1 & A0 == 1)
  #  A is determined by Z based on the four types of people, (I4) satisfied
  A <- ifelse(Z == 1, A1, A0)
  
  # potential outcomes of mediator (not a function of U, so no unblocked backdoor path from M to Y given C, (F3) satisfied)
  pM1.AUC <- expit(2 * A1 - 1 + C[,2]) # pM1.AUC and pM0.AUC are the same for never takers and always takers, exclusion restriction satisfied
  pM0.AUC <- expit(2 * A0 - 1 + C[,2])
  M1 <- rbinom(n, size = 1, prob = pM1.AUC) 
  M0 <- rbinom(n, size = 1, prob = pM0.AUC)
  M <- ifelse(Z == 1, M1, M0) # Z affects M only through A
  
  # potential outcomes of Y (not a function of A, (F1) satisfied; not a function directly of Z, (I2) satisfied)
  muY1.MAUCZ <- beta*M1 + U - 2* sqrt(abs(C[,1])) + sin(C[,4])
  muY0.MAUCZ <- beta*M0 + U - 2* sqrt(abs(C[,1])) + sin(C[,4])
  Y1 <- rnorm(n, mean = muY1.MAUCZ)
  Y0 <- rnorm(n, mean = muY0.MAUCZ)
  Y <- ifelse(Z == 1, Y1, Y0) # Z affects Y only through A via M
  
  PO.diff.CACE <- Y1[idx_complier] - Y0[idx_complier]
  
  return(list(df = data.frame(Y, Z, M, A, C), idx_complier = idx_complier, CACE =  mean(PO.diff.CACE)))
}



# 2. Front-door criterion satisfied, but IV assumptions violated
# Violate the assumption (I1): there is an unblocked backdoor path from Z to Y
# Violate the assumption (I2): there is an direct effect from Z to Y
# Violate the assumption (I3): defiers exist
# Violate the assumption (I4): zero correlation between instrument and treatment

dgp_frontdoor_iv_fdoor_correct_i1_violated <- function(n, beta = 3) {
  
  # Unmeasured confounder
  U <- matrix(runif(n, min=-2,max=2))
  
  # Measured confounders 
  C <- data.frame(matrix(runif(n * 4, min=-2,max=2), ncol=4))
  
  # Instrumental variable (a function of U, (I1) violated)
  pZ.C <- expit(2 + U)
  Z <- rbinom(n = n, size = 1, prob = pZ.C)
  
  # Treatment (a function of C, not of U, there is no unblocked backdoor path from A to M given C, (F2) satisfied)
  pA.UC <- expit(C[,1] + expit(C[,2]) + sin(C[,3]))
  A1 <- rbinom(n, size = 1, prob = pA.UC)
  A0 <- rbinom(n, size = 1, prob = 1-pA.UC)
  
  # no defier, convert to compliers ((I3) satisfied)
  A1[which(A1 == 0 & A0 == 1)] <- 1
  A0[which(A1 == 0 & A0 == 1)] <- 0
  
  idx_complier <- which(A1 == 1 & A0 == 0)
  idx_never <- which(A1 == 0 & A0 == 0)
  idx_always <- which(A1 == 1 & A0 == 1)
  #  A is determined by Z based on the four types of people, (I4) satisfied
  A <- ifelse(Z == 1, A1, A0)
  
  # potential outcomes of mediator (not a function of U, so no unblocked backdoor path from M to Y given C, (F3) satisfied)
  pM1.AUC <- expit(2 * A1 - 1 + C[,2]) # pM1.AUC and pM0.AUC are the same for never takers and always takers, exclusion restriction satisfied
  pM0.AUC <- expit(2 * A0 - 1 + C[,2])
  M1 <- rbinom(n, size = 1, prob = pM1.AUC) 
  M0 <- rbinom(n, size = 1, prob = pM0.AUC)
  M <- ifelse(Z == 1, M1, M0) # Z affects M only through A
  
  # potential outcomes of Y (not a function of A, (F1) satisfied; not a function directly of Z, (I2) satisfied)
  muY1.MAUCZ <- beta*M1 + U - 2* sqrt(abs(C[,1])) + sin(C[,4])
  muY0.MAUCZ <- beta*M0 + U - 2* sqrt(abs(C[,1])) + sin(C[,4])
  Y1 <- rnorm(n, mean = muY1.MAUCZ)
  Y0 <- rnorm(n, mean = muY0.MAUCZ)
  Y <- ifelse(Z == 1, Y1, Y0) # Z affects Y only through A via M
  
  PO.diff.CACE <- Y1[idx_complier] - Y0[idx_complier]
  
  return(list(df = data.frame(Y, Z, M, A, C), idx_complier = idx_complier, CACE =  mean(PO.diff.CACE)))
}



dgp_frontdoor_iv_fdoor_correct_i2_violated <- function(n, beta = 3) {
  
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
  A1[which(A1 == 0 & A0 == 1)] <- 1
  A0[which(A1 == 0 & A0 == 1)] <- 0
  
  idx_complier <- which(A1 == 1 & A0 == 0)
  idx_never <- which(A1 == 0 & A0 == 0)
  idx_always <- which(A1 == 1 & A0 == 1)
  #  A is determined by Z based on the four types of people, (I4) satisfied
  A <- ifelse(Z == 1, A1, A0)
  
  # potential outcomes of mediator (not a function of U, so no unblocked backdoor path from M to Y given C, (F3) satisfied)
  pM1.AUC <- expit(2 * A1 - 1 + C[,2]) # pM1.AUC and pM0.AUC are the same for never takers and always takers, exclusion restriction satisfied
  pM0.AUC <- expit(2 * A0 - 1 + C[,2])
  M1 <- rbinom(n, size = 1, prob = pM1.AUC) 
  M0 <- rbinom(n, size = 1, prob = pM0.AUC)
  M <- ifelse(Z == 1, M1, M0) # Z affects M only through A
  
  # potential outcomes of Y (not a function of A, (F1) satisfied; a function of Z, (I2) violated)
  muY1.MAUCZ <- beta*M1 + U - 2* sqrt(abs(C[,1])) + sin(C[,4]) + 2*Z
  muY0.MAUCZ <- beta*M0 + U - 2* sqrt(abs(C[,1])) + sin(C[,4]) + 2*Z
  Y1 <- rnorm(n, mean = muY1.MAUCZ)
  Y0 <- rnorm(n, mean = muY0.MAUCZ)
  Y <- ifelse(Z == 1, Y1, Y0) # Z affects Y only through A via M
  
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
  
  # Treatment (a function of C, not of U, there is no unblocked backdoor path from A to M given C, (F2) satisfied)
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
  pM1.AUC <- expit(20 * A1 - 1 + C[,2]) # pM1.AUC and pM0.AUC are the same for never takers and always takers, exclusion restriction satisfied
  pM0.AUC <- expit(20 * A0 - 1 + C[,2])
  pM1.AUC[idx_defier] <- expit(2 * A1[idx_defier] - 1 + C[,2][idx_defier])
  pM0.AUC[idx_defier] <- expit(2 * A0[idx_defier] - 1 + C[,2][idx_defier])
  M1 <- rbinom(n, size = 1, prob = pM1.AUC) 
  M0 <- rbinom(n, size = 1, prob = pM0.AUC)
  M <- ifelse(Z == 1, M1, M0) # Z affects M only through A
  
  # potential outcomes of Y (not a function of A, (F1) satisfied; not a function directly of Z, (I2) satisfied)
  muY1.MAUCZ <- beta*M1 + U - 2* sqrt(abs(C[,1])) + sin(C[,4])
  muY0.MAUCZ <- beta*M0 + U - 2* sqrt(abs(C[,1])) + sin(C[,4])
  Y1 <- rnorm(n, mean = muY1.MAUCZ)
  Y0 <- rnorm(n, mean = muY0.MAUCZ)
  Y <- ifelse(Z == 1, Y1, Y0) # Z affects Y only through A via M
  
  PO.diff.CACE <- Y1[idx_complier] - Y0[idx_complier]
  
  return(list(df = data.frame(Y, Z, M, A, C), idx_complier = idx_complier, CACE =  mean(PO.diff.CACE)))
}


dgp_frontdoor_iv_fdoor_correct_i4_violated <- function(n, beta = 3) {
  
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
  A1[which(A1 == 0 & A0 == 1)] <- 1
  A0[which(A1 == 0 & A0 == 1)] <- 0
  
  idx_complier <- which(A1 == 1 & A0 == 0)
  idx_never <- which(A1 == 0 & A0 == 0)
  idx_always <- which(A1 == 1 & A0 == 1)
  # A is randomized, no connection with Z ((I4) violated)
  A <- A1
  
  # Mediator (not a function of U, so no unblocked backdoor path from M to Y given C, (F3) satisfied)
  pM.AUC <- expit(2 * A - 1 + C[,2])
  M <- rbinom(n, size = 1, prob = pM.AUC) 
  
  # Outcome (not a function of A, (F1) satisfied; M and Y are not functions of Z, (I2) satisfied)
  muY.MAUCZ <- beta*M + U - 2* sqrt(abs(C[,1])) + sin(C[,4])
  Y <- rnorm(n, mean = muY.MAUCZ) 
  
  # potential outcomes of mediator 
  pM1.AUC <- expit(2 * 1 - 1 + C[,2]) 
  pM0.AUC <- expit(2 * 0 - 1 + C[,2])
  M1 <- rbinom(n, size = 1, prob = pM1.AUC) 
  M0 <- rbinom(n, size = 1, prob = pM0.AUC)
  
  # potential outcomes of Y 
  muY1.MAUCZ <- beta*M1 + U - 2* sqrt(abs(C[,1])) + sin(C[,4])
  muY0.MAUCZ <- beta*M0 + U - 2* sqrt(abs(C[,1])) + sin(C[,4])
  Y1 <- rnorm(n, mean = muY1.MAUCZ)
  Y0 <- rnorm(n, mean = muY0.MAUCZ)
  
  PO.diff.CACE <- Y1[idx_complier] - Y0[idx_complier]
  
  return(list(df = data.frame(Y, Z, M, A, C), idx_complier = idx_complier, CACE =  mean(PO.diff.CACE)))
}



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
  A1[which(A1 == 0 & A0 == 1)] <- 1
  A0[which(A1 == 0 & A0 == 1)] <- 0
  
  idx_complier <- which(A1 == 1 & A0 == 0)
  idx_never <- which(A1 == 0 & A0 == 0)
  idx_always <- which(A1 == 1 & A0 == 1)
  #  A is determined by Z based on the four types of people, (I4) satisfied
  A <- ifelse(Z == 1, A1, A0)
  
  # potential outcomes of mediator (not a function of U, so no unblocked backdoor path from M to Y given C, (F3) satisfied)
  pM1.AUC <- expit(2 * A1 - 1 + C[,2]) # pM1.AUC and pM0.AUC are the same for never takers and always takers, exclusion restriction satisfied
  pM0.AUC <- expit(2 * A0 - 1 + C[,2])
  M1 <- rbinom(n, size = 1, prob = pM1.AUC) 
  M0 <- rbinom(n, size = 1, prob = pM0.AUC)
  M <- ifelse(Z == 1, M1, M0) # Z affects M only through A
  
  # potential outcomes of Y (a function of A, so there is a direct A->Y, (F1) violated; not a function directly of Z, (I2) satisfied)
  muY1.MAUCZ <- beta*A1 + U - 2* sqrt(abs(C[,1])) + sin(C[,4])
  muY0.MAUCZ <- beta*A0 + U - 2* sqrt(abs(C[,1])) + sin(C[,4])
  Y1 <- rnorm(n, mean = muY1.MAUCZ)
  Y0 <- rnorm(n, mean = muY0.MAUCZ)
  Y <- ifelse(Z == 1, Y1, Y0) # Z affects Y only through A 
  
  PO.diff.CACE <- Y1[idx_complier] - Y0[idx_complier]
  
  return(list(df = data.frame(Y, Z, M, A, C), idx_complier = idx_complier, CACE =  mean(PO.diff.CACE)))
}


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
  A1[which(A1 == 0 & A0 == 1)] <- 1
  A0[which(A1 == 0 & A0 == 1)] <- 0
  
  idx_complier <- which(A1 == 1 & A0 == 0)
  idx_never <- which(A1 == 0 & A0 == 0)
  idx_always <- which(A1 == 1 & A0 == 1)
  #  A is determined by Z based on the four types of people, (I4) satisfied
  A <- ifelse(Z == 1, A1, A0)
  
  # potential outcomes of mediator (a function of U, so there is an unblocked backdoor path from M to Y given C, (F3) violated)
  pM1.AUC <- expit(20 * A1 - 1 + C[,2] + U) # pM1.AUC and pM0.AUC are the same for never takers and always takers, exclusion restriction satisfied
  pM0.AUC <- expit(20 * A0 - 1 + C[,2] + U)
  M1 <- rbinom(n, size = 1, prob = pM1.AUC) 
  M0 <- rbinom(n, size = 1, prob = pM0.AUC)
  M <- ifelse(Z == 1, M1, M0) # Z affects M only through A
  
  # potential outcomes of Y (not a function of A, (F1) satisfied; not a function directly of Z, (I2) satisfied)
  muY1.MAUCZ <- beta*M1 + 5*U - 2* sqrt(abs(C[,1])) + sin(C[,4])
  muY0.MAUCZ <- beta*M0 + 5*U - 2* sqrt(abs(C[,1])) + sin(C[,4])
  Y1 <- rnorm(n, mean = muY1.MAUCZ)
  Y0 <- rnorm(n, mean = muY0.MAUCZ)
  Y <- ifelse(Z == 1, Y1, Y0) # Z affects Y only through A via M
  
  PO.diff.CACE <- Y1[idx_complier] - Y0[idx_complier]
  
  return(list(df = data.frame(Y, Z, M, A, C), idx_complier = idx_complier, CACE =  mean(PO.diff.CACE)))
}
