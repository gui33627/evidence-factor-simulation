
########################### Backdoor and IV ############################
# 1. Both criteria are correct
# Backdoor criteria: (B1) measured confounders C block every path between A and Y that contains an arrow into A; 
#                    (B2) no node in C is a descendant of A
# IV assumptions: (I1) ignorability of the instrument (it is randomized or conditionally randomized);
#                 (I2) exclusion restriction;
#                 (I3) monotonicity (no defiers);
#                 (I4) non-zero correlation between instrument and treatment

dgp_backdoor_iv_both_correct <- function(n, beta = 4) {
  
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
  
  # no defier, convert to compliers ((I3) satisfied)
  A1[which(A1 == 0 & A0 == 1)] <- 1
  A0[which(A1 == 0 & A0 == 1)] <- 0
  
  idx_complier <- which(A1 == 1 & A0 == 0)
  idx_never <- which(A1 == 0 & A0 == 0)
  idx_always <- which(A1 == 1 & A0 == 1)
  #  A is determined by Z based on the four types of people, (I4) satisfied
  A <- ifelse(Z == 1, A1, A0)
  
  # Outcome (not a function directly of Z, Z only affects the outcome through the treatment, (I2) satisfied)
  # potential outcomes of Y
  error <- rnorm(n, mean = 0)
  Y1 <- beta*A1 + 2*U + 2* sqrt(abs(C[,1])) + sin(C[,4]) + error
  Y0 <- beta*A0 + 2*U + 2* sqrt(abs(C[,1])) + sin(C[,4]) + error
  Y <- ifelse(Z == 1, Y1, Y0) 
  
  PO.diff.CACE <- Y1[idx_complier] - Y0[idx_complier]
  
  return(list(df = data.frame(Y, Z, A, C), idx_complier = idx_complier, CACE =  mean(PO.diff.CACE)))
}




# 2. Backdoor criterion satisfied, but IV assumptions violated
# Violate the assumption (I1): there is an unblocked backdoor path from Z to Y,
#                              but this also opens a backdoor path from A to Y, 
#                              violating the backdoor criteria, *so the simulation 
#                              is not going to violate (I1)*

# Violate the assumption (I2): there is an direct effect from Z to Y,
#                              but this also opens a backdoor path from A to Y, 
#                              violating the backdoor criteria, *so the simulation 
#                              is not going to violate (I2)*

# Violate the assumption (I3): defiers exist
# Violate the assumption (I4): zero correlation between instrument and treatment 
#                              (close to zero denominator, will be unstable)


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
  Y1 <- beta*A1 + 2*U + 2*sqrt(abs(C[,1])) + sin(C[,4]) + error
  Y0 <- beta*A0 + 2*U + 2*sqrt(abs(C[,1])) + sin(C[,4]) + error
  Y1[idx_defier] <- beta*A1[idx_defier] + 2*U[idx_defier] + 2*sqrt(abs(C[,1][idx_defier])) + sin(C[,4][idx_defier]) + error[idx_defier]
  Y0[idx_defier] <- beta*A0[idx_defier] + 2*U[idx_defier] + 2*sqrt(abs(C[,1][idx_defier])) + sin(C[,4][idx_defier]) + error[idx_defier]
  Y <- ifelse(Z == 1, Y1, Y0)
  
  PO.diff.CACE <- Y1[idx_complier] - Y0[idx_complier]
  
  return(list(df = data.frame(Y, Z, A, C), idx_complier = idx_complier, CACE =  mean(PO.diff.CACE)))
}



# 3. IV assumptions satisfied, but backdoor criterion violated
# Violate the assumption (B1): there is an unblocked backdoor path from A to Y
# Violate the assumption (B2): a node in C is a descendant of A


dgp_backdoor_iv_iv_correct_b1_violated <- function(n, beta = 4) {
  
  # Unmeasured confounder
  U <- matrix(runif(n, min=-2,max=2))
  
  # Measured confounders (not a descendant of A, (B2) satisfied)
  C <- data.frame(matrix(runif(n * 4, min=-2,max=2), ncol=4))
  
  # Instrumental variable (randomized, (I1) satisfied)
  Z <- rbinom(n = n, size = 1, prob = 0.5)
  
  # Treatment (a function of U, (B1) violated)
  pA.UC <- expit(C[,1] + expit(C[,2]) + sin(C[,3]) + U)
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
  
  # Outcome (not a function directly of Z, Z only affects the outcome through the treatment, (I2) satisfied)
  # potential outcomes of Y
  error <- rnorm(n, mean = 0)
  Y1 <- beta*A1 + 2*U + 2* sqrt(abs(C[,1])) + sin(C[,4]) + error
  Y0 <- beta*A0 + 2*U + 2* sqrt(abs(C[,1])) + sin(C[,4]) + error
  Y <- ifelse(Z == 1, Y1, Y0) 
  
  PO.diff.CACE <- Y1[idx_complier] - Y0[idx_complier]
  
  return(list(df = data.frame(Y, Z, A, C), idx_complier = idx_complier, CACE =  mean(PO.diff.CACE)))
}


dgp_backdoor_iv_iv_correct_b2_violated <- function(n, beta = 4) {
  
  # Unmeasured confounder
  U <- matrix(runif(n, min=-2,max=2))
  
  # Instrumental variable (randomized, (I1) satisfied)
  Z <- rbinom(n = n, size = 1, prob = 0.5)
  
  # Measured confounders
  C <- data.frame(matrix(runif(n * 4, min=-2,max=2), ncol=4))
  
  # Treatment (a function of C, not of U, (B1) satisfied)
  pA.UC <- expit(C[,4] + expit(C[,2]) + sin(C[,3]))
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
  
  # Outcome (not a function directly of Z, Z only affects the outcome through the treatment, (I2) satisfied)
  # potential outcomes of Y
  error <- rnorm(n, mean = 0)
  Y1 <- beta*A1 + 2*U + sin(C[,4]) + error
  Y0 <- beta*A0 + 2*U + sin(C[,4]) + error
  Y <- ifelse(Z == 1, Y1, Y0) 
  
  # Measured confounders (a node in C is a collider, (B2) violated)
  C[,1] <- rnorm(n, mean = (A + Y))
  
  PO.diff.CACE <- Y1[idx_complier] - Y0[idx_complier]
  
  return(list(df = data.frame(Y, Z, A, C), idx_complier = idx_complier, CACE =  mean(PO.diff.CACE)))
}

