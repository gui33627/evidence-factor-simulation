
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
  idx_defier <- which(A1 == 0 & A0 == 1)
  A1[idx_defier] <- 1
  A0[idx_defier] <- 0
  
  idx_complier <- which(A1 == 1 & A0 == 0)
  idx_never <- which(A1 == 0 & A0 == 0)
  idx_always <- which(A1 == 1 & A0 == 1)
  #  A is determined by Z based on the four types of people, (I4) satisfied
  A <- ifelse(Z == 1, A1, A0)
  
  # Outcome (not a function directly of Z, Z only affects the outcome through the treatment, (I2) satisfied)
  # potential outcomes of Y
  error <- rnorm(n, mean = 0)
  Y1 <- beta*1 + 2*U + 2* sqrt(abs(C[,1])) + sin(C[,4]) + error
  Y0 <- beta*0 + 2*U + 2* sqrt(abs(C[,1])) + sin(C[,4]) + error
  Y <- ifelse(A == 1, Y1, Y0) 
  
  PO.diff.CACE <- Y1[idx_complier] - Y0[idx_complier]
  
  return(list(df = data.frame(Y, Z, A, C), idx_complier = idx_complier, CACE =  mean(PO.diff.CACE)))
}




# 2. Backdoor criterion satisfied, but IV assumptions violated
# Violate the assumption (I1): there is an unblocked backdoor path from Z to Y
# Violate the assumption (I2): there is an direct effect from Z to Y
# Violate the assumption (I3): defiers exist
# Violate the assumption (I4): zero correlation between instrument and treatment 
#                              (close to zero denominator, will be unstable)

# Bias due to violation of monoticity is bigger when there is a big difference 
# between the average causal effects of A for the compliers and the defiers
# If these effects are equal then monotonicity violations yield no bias


dgp_backdoor_iv_bdoor_correct_i2_violated <- function(n, beta = 10) {
  
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
  idx_defier <- which(A1 == 0 & A0 == 1)
  A1[idx_defier] <- 1
  A0[idx_defier] <- 0
  
  idx_complier <- which(A1 == 1 & A0 == 0)
  idx_never <- which(A1 == 0 & A0 == 0)
  idx_always <- which(A1 == 1 & A0 == 1)
  
  #  A is determined by Z based on the four types of people, (I4) satisfied
  A <- ifelse(Z == 1, A1, A0)
  
  # Outcome (a function directly of Z, (I2) violated)
  # in the estimation, Z is controlled so backdoor assumptions still satisfied (confounderZ = TRUE)
  error <- rnorm(n, mean = 0)
  Y1 <- beta*1 + 2*U + 2*sqrt(abs(C[,1])) + sin(C[,4]) + 2*Z + error
  Y0 <- beta*0 + 2*U + 2*sqrt(abs(C[,1])) + sin(C[,4]) + 2*Z + error
  Y <- ifelse(A == 1, Y1, Y0)
  
  PO.diff.CACE <- Y1[idx_complier] - Y0[idx_complier]
  
  return(list(df = data.frame(Y, Z, A, C), idx_complier = idx_complier, CACE =  mean(PO.diff.CACE)))
}



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
  Y1 <- beta*1 + 2*U + 2*sqrt(abs(C[,1])) + sin(C[,4]) + error
  Y0 <- beta*0 + 2*U + 2*sqrt(abs(C[,1])) + sin(C[,4]) + error
  Y1[idx_defier] <- beta_defier*1 + 2*U[idx_defier] + 2*sqrt(abs(C[,1][idx_defier])) + sin(C[,4][idx_defier]) + error[idx_defier]
  Y0[idx_defier] <- beta_defier*0 + 2*U[idx_defier] + 2*sqrt(abs(C[,1][idx_defier])) + sin(C[,4][idx_defier]) + error[idx_defier]
  Y <- ifelse(A == 1, Y1, Y0)
  
  PO.diff.CACE <- Y1[idx_complier] - Y0[idx_complier]
  
  return(list(df = data.frame(Y, Z, A, C), idx_complier = idx_complier, CACE =  mean(PO.diff.CACE)))
}

# Under the null
# IV CI: [1] -0.07456177  0.04643818

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
  idx_defier <- which(A1 == 0 & A0 == 1)
  A1[idx_defier] <- 1
  A0[idx_defier] <- 0
  
  idx_complier <- which(A1 == 1 & A0 == 0)
  idx_never <- which(A1 == 0 & A0 == 0)
  idx_always <- which(A1 == 1 & A0 == 1)
  #  A is determined by Z based on the four types of people, (I4) satisfied
  A <- ifelse(Z == 1, A1, A0)
  
  # Outcome (not a function directly of Z, Z only affects the outcome through the treatment, (I2) satisfied)
  # potential outcomes of Y
  error <- rnorm(n, mean = 0)
  Y1 <- beta*1 + 2*U + 2* sqrt(abs(C[,1])) + sin(C[,4]) + error
  Y0 <- beta*0 + 2*U + 2* sqrt(abs(C[,1])) + sin(C[,4]) + error
  Y <- ifelse(A == 1, Y1, Y0) 
  
  PO.diff.CACE <- Y1[idx_complier] - Y0[idx_complier]
  
  return(list(df = data.frame(Y, Z, A, C), idx_complier = idx_complier, CACE =  mean(PO.diff.CACE)))
}

# Under the null
# backdoor CI: [1] -0.010258848  0.009483815

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
  idx_defier <- which(A1 == 0 & A0 == 1)
  A1[idx_defier] <- 1
  A0[idx_defier] <- 0
  
  idx_complier <- which(A1 == 1 & A0 == 0)
  idx_never <- which(A1 == 0 & A0 == 0)
  idx_always <- which(A1 == 1 & A0 == 1)
  #  A is determined by Z based on the four types of people, (I4) satisfied
  A <- ifelse(Z == 1, A1, A0)
  
  # Outcome (not a function directly of Z, Z only affects the outcome through the treatment, (I2) satisfied)
  # potential outcomes of Y
  error <- rnorm(n, mean = 0)
  Y1 <- beta*1 + 2*U + sin(C[,4]) + error
  Y0 <- beta*0 + 2*U + sin(C[,4]) + error
  Y <- ifelse(A == 1, Y1, Y0) 
  
  # Measured confounders (a node in C is a collider, (B2) violated)
  C[,1] <- rnorm(n, mean = (2*A + Y))
  
  PO.diff.CACE <- Y1[idx_complier] - Y0[idx_complier]
  
  return(list(df = data.frame(Y, Z, A, C), idx_complier = idx_complier, CACE =  mean(PO.diff.CACE)))
}

dgp_backdoor_iv_iv_correct_b2_violated_alt_nonzero <- function(n, beta = 4) {
  
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
  idx_defier <- which(A1 == 0 & A0 == 1)
  A1[idx_defier] <- 1
  A0[idx_defier] <- 0
  
  idx_complier <- which(A1 == 1 & A0 == 0)
  idx_never <- which(A1 == 0 & A0 == 0)
  idx_always <- which(A1 == 1 & A0 == 1)
  #  A is determined by Z based on the four types of people, (I4) satisfied
  A <- ifelse(Z == 1, A1, A0)
  
  # Outcome (not a function directly of Z, Z only affects the outcome through the treatment, (I2) satisfied)
  # potential outcomes of Y
  error <- rnorm(n, mean = 0)
  Y1 <- beta*1 + 2*U + sin(C[,4]) + error
  Y0 <- beta*0 + 2*U + sin(C[,4]) + error
  Y <- ifelse(A == 1, Y1, Y0) 
  
  # Measured confounders (a node in C is a collider, (B2) violated)
  C[,1] <- rnorm(n, mean = (A + Y))
  
  PO.diff.CACE <- Y1[idx_complier] - Y0[idx_complier]
  
  return(list(df = data.frame(Y, Z, A, C), idx_complier = idx_complier, CACE =  mean(PO.diff.CACE)))
}


dgp_backdoor_iv_iv_correct_b2_violated_alt_zero <- function(n, beta = 4, coef_seq = 0.6) {
  
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
  idx_defier <- which(A1 == 0 & A0 == 1)
  A1[idx_defier] <- 1
  A0[idx_defier] <- 0
  
  idx_complier <- which(A1 == 1 & A0 == 0)
  idx_never <- which(A1 == 0 & A0 == 0)
  idx_always <- which(A1 == 1 & A0 == 1)
  #  A is determined by Z based on the four types of people, (I4) satisfied
  A <- ifelse(Z == 1, A1, A0)
  
  # Outcome (not a function directly of Z, Z only affects the outcome through the treatment, (I2) satisfied)
  # potential outcomes of Y
  error <- rnorm(n, mean = 0)
  Y1 <- beta*1 - 3*U - sin(C[,4]) + error
  Y0 <- beta*0 - 3*U - sin(C[,4]) + error
  Y <- ifelse(A == 1, Y1, Y0) 
  
  # Measured confounders (a node in C is a collider, (B2) violated)
  C[,1] <- rnorm(n, mean = (coef_seq*A + 2*Y))
  
  PO.diff.CACE <- Y1[idx_complier] - Y0[idx_complier]
  
  return(list(df = data.frame(Y, Z, A, C), idx_complier = idx_complier, CACE =  mean(PO.diff.CACE)))
}

# under the alternative
#! n=10^6, coef_seq = 0.60, backdoor CI: -0.01925129 0.007066126

