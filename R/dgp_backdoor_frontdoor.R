
########################### Backdoor and Front-door ############################
# 1. Both criteria are correct
# Backdoor criteria: (B1) measured confounders C block every path between A and Y that contains an arrow into A; 
#                    (B2) no node in C is a descendant of A
# Front-door criteria: (F1) M intercepts all directed paths from A to Y;
#                      (F2) no unblocked back-door paths from A to M;
#                      (F3) all back-door paths from M to Y are blocked (by A)

dgp_backdoor_frontdoor_both_correct <- function(n, beta = 3) {
  
  # Unmeasured confounder
  U <- matrix(runif(n, min=-2,max=2))
  
  # Measured confounders (not a descendant of A, (B2) satisfied)
  C <- data.frame(matrix(runif(n * 4, min=-2,max=2), ncol=4))
  
  # Treatment (a function only of measured confounders C, not a function of U, (B1) satisfied;
  #            there is no unblocked backdoor path from A to M given C, (F2) satisfied)
  pA.UC <- expit(C[,1] + expit(C[,2]) + sin(C[,3]))
  A <- rbinom(n, size = 1, prob = pA.UC) 
  
  # Mediator (not a function of U, so no unblocked backdoor path from M to Y given C, (F3) satisfied)
  pM.AUC <- expit(2 * A - 1 + C[,2])
  M <- rbinom(n, size = 1, prob = pM.AUC) 
  
  # Outcome (not a function of A, (F1) satisfied)
  muY.MAUC <- beta*M + U - 2* sqrt(abs(C[,1])) + sin(C[,4])
  Y <- rnorm(n, mean = muY.MAUC) 
  
  # also create a column for finding true causal effect
  # potential outcomes of M
  pM1.AUC <- expit(2 * 1 - 1 + C[,2])
  pM0.AUC <- expit(2 * 0 - 1 + C[,2])
  M1 <- rbinom(n, size = 1, prob = pM1.AUC) 
  M0 <- rbinom(n, size = 1, prob = pM0.AUC)
  # potential outcomes of Y
  muY1.MAUC <- beta*M1 + U - 2* sqrt(abs(C[,1])) + sin(C[,4])
  muY0.MAUC <- beta*M0 + U - 2* sqrt(abs(C[,1])) + sin(C[,4])
  Y1 <- rnorm(n, mean = muY1.MAUC)
  Y0 <- rnorm(n, mean = muY0.MAUC)
  PO.diff <- Y1 - Y0
  
  return(data.frame(Y, M, A, C, PO.diff, pM.AUC))
}



# 2. Backdoor criterion satisfied, but front-door criterion violated
# Violate the assumption (F1): there is a direct effect of A on Y
# Violate the assumption (F2): there is an unblocked backdoor path from A to M, 
#                              but this also opens a backdoor path from A to Y, 
#                              violating the backdoor criteria, *so the simulation 
#                              is not going to violate (F2)*
# Violate the assumption (F3): there is an unblocked backdoor path from M to Y

dgp_backdoor_frontdoor_bdoor_correct_f1_violated <- function(n, beta = 1.5) {
  
  # Unmeasured confounder
  U <- matrix(runif(n, min=-2,max=2))
  
  # Measured confounders (not a descendant of A, (B2) satisfied)
  C <- data.frame(matrix(runif(n * 4, min=-2,max=2), ncol=4))
  
  # Treatment (a function only of measured confounders C, not a function of U, (B1) satisfied;
  #            there is no unblocked backdoor path from A to M given C, (F2) satisfied)
  pA.UC <- expit(C[,1] + expit(C[,2]) + sin(C[,3]))
  A <- rbinom(n, size = 1, prob = pA.UC)
  
  # Mediator (not a function of U, so there is no unblocked backdoor path from M to Y, (F3) satisfied)
  pM.AUC <- expit(2 * A - 1 + C[,2])
  M <- rbinom(n, size = 1, prob = pM.AUC)
  
  # Outcome (a function of A, (F1) violated)
  muY.MAUC <- beta*A + U - 2* sqrt(abs(C[,1])) + sin(C[,4])
  Y <- rnorm(n, mean = muY.MAUC)
  
  # also create a column for finding true causal effect
  # potential outcomes of Y
  muY1.MAUC <- beta*1 + U - 2* sqrt(abs(C[,1])) + sin(C[,4]) 
  muY0.MAUC <- beta*0 + U - 2* sqrt(abs(C[,1])) + sin(C[,4]) 
  Y1 <- rnorm(n, mean = muY1.MAUC)
  Y0 <- rnorm(n, mean = muY0.MAUC)
  PO.diff <- Y1 - Y0
  
  return(data.frame(Y, M, A, C, PO.diff, pM.AUC))
}


dgp_backdoor_frontdoor_bdoor_correct_f3_violated <- function(n, beta = 3) {
  
  # Unmeasured confounder
  U <- matrix(runif(n, min=-2,max=2))
  
  # Measured confounders (not a descendant of A, (B2) satisfied)
  C <- data.frame(matrix(runif(n * 4, min=-2,max=2), ncol=4))
  
  # Treatment (a function only of measured confounders C, not a function of U, (B1) satisfied;
  #            there is no unblocked backdoor path from A to M given C, (F2) satisfied)
  pA.UC <- expit(C[,1] + expit(C[,2]) + sin(C[,3]))
  A <- rbinom(n, size = 1, prob = pA.UC)
  
  # Mediator (a function of U, so there is an unblocked backdoor path M to Y, (F3) violated)
  pM.AUC <- expit(2 * A - 1 + C[,2] + 3*U) 
  M <- rbinom(n, size = 1, prob = pM.AUC)
  
  # Outcome (not a function of A, (F1) satisfied)
  muY.MAUC <- beta*M + 5*U - 2* sqrt(abs(C[,1])) + sin(C[,4])
  Y <- rnorm(n, mean = muY.MAUC)
  
  # potential outcomes of M
  pM1.AUC <- expit(2 * 1 - 1 + C[,2] + U)
  pM0.AUC <- expit(2 * 0 - 1 + C[,2] + U)
  M1 <- rbinom(n, size = 1, prob = pM1.AUC) 
  M0 <- rbinom(n, size = 1, prob = pM0.AUC)
  # potential outcomes of Y
  muY1.MAUC <- beta*M1 + U - 2* sqrt(abs(C[,1])) + sin(C[,4])
  muY0.MAUC <- beta*M0 + U - 2* sqrt(abs(C[,1])) + sin(C[,4])
  Y1 <- rnorm(n, mean = muY1.MAUC)
  Y0 <- rnorm(n, mean = muY0.MAUC)
  PO.diff <- Y1 - Y0
  
  return(data.frame(Y, M, A, C, PO.diff, pM.AUC))
}

dgp_backdoor_frontdoor_bdoor_correct_f3_violated_new <- function(n, beta = 3) {
  
  # Unmeasured confounder
  U <- matrix(runif(n, min=-2,max=2))
  
  # Measured confounders (not a descendant of A, (B2) satisfied)
  C <- data.frame(matrix(runif(n * 4, min=-2,max=2), ncol=4))
  
  # Treatment (a function only of measured confounders C, not a function of U, (B1) satisfied;
  #            there is no unblocked backdoor path from A to M given C, (F2) satisfied)
  pA.UC <- expit(C[,1] + expit(C[,2]) + sin(C[,3]))
  A <- rbinom(n, size = 1, prob = pA.UC)
  
  # Mediator (a function of U, so there is an unblocked backdoor path M to Y, (F3) violated)
  pM.AUC <- expit(2 * A - 1 + U) 
  M <- rbinom(n, size = 1, prob = pM.AUC)
  
  # Outcome (not a function of A, (F1) satisfied)
  muY.MAUC <- beta*M + 5*U - 2* sqrt(abs(C[,1])) + sin(C[,4])
  Y <- rnorm(n, mean = muY.MAUC)
  
  # potential outcomes of M
  pM1.AUC <- expit(2 * 1 - 1 + U)
  pM0.AUC <- expit(2 * 0 - 1 + U)
  M1 <- rbinom(n, size = 1, prob = pM1.AUC) 
  M0 <- rbinom(n, size = 1, prob = pM0.AUC)
  # potential outcomes of Y
  muY1.MAUC <- beta*M1 + 5*U - 2* sqrt(abs(C[,1])) + sin(C[,4])
  muY0.MAUC <- beta*M0 + 5*U - 2* sqrt(abs(C[,1])) + sin(C[,4])
  Y1 <- rnorm(n, mean = muY1.MAUC)
  Y0 <- rnorm(n, mean = muY0.MAUC)
  PO.diff <- Y1 - Y0
  
  return(data.frame(Y, M, A, C, PO.diff, pM.AUC))
}


# 3. Front-door criterion satisfied, but backdoor criterion violated
# Violate the assumption (B1): there is an unblocked backdoor path from A to Y 
# Violate the assumption (B2): a node in C is a descendant of A, 
#                              but conditioning on C opens a path between A and Y,
#                              M no longer fully mediate the effect from A to Y, (F1) violated, 
#                              *so the simulation is not going to violate (B2)*

dgp_backdoor_frontdoor_fdoor_correct_b1_violated <- function(n, beta = 3) {
  
  # Unmeasured confounder
  U <- matrix(runif(n, min=-2,max=2))
  
  # Measured confounders (not a descendant of A, (B2) satisfied)
  C <- data.frame(matrix(runif(n * 4, min=-2,max=2), ncol=4))
  
  # Treatment (a function of U, (B1) violated;
  #            there is no unblocked backdoor path from A to M given C, (F2) satisfied)
  pA.UC <- expit(C[,1] + expit(C[,2]) + sin(C[,3]) + U)
  A <- rbinom(n, size = 1, prob = pA.UC) 
  
  # Mediator (not a function of U, so no unblocked backdoor path from M to Y given C, (F3) satisfied)
  pM.AUC <- expit(2 * A - 1 + C[,2])
  M <- rbinom(n, size = 1, prob = pM.AUC) 
  
  # Outcome (not a function of A, (F1) satisfied)
  muY.MAUC <- beta*M + U - 2* sqrt(abs(C[,1])) + sin(C[,4])
  Y <- rnorm(n, mean = muY.MAUC) 
  
  # also create a column for finding true causal effect
  # potential outcomes of M
  pM1.AUC <- expit(2 * 1 - 1 + C[,2])
  pM0.AUC <- expit(2 * 0 - 1 + C[,2])
  M1 <- rbinom(n, size = 1, prob = pM1.AUC) 
  M0 <- rbinom(n, size = 1, prob = pM0.AUC)
  # potential outcomes of Y
  muY1.MAUC <- beta*M1 + U - 2* sqrt(abs(C[,1])) + sin(C[,4])
  muY0.MAUC <- beta*M0 + U - 2* sqrt(abs(C[,1])) + sin(C[,4])
  Y1 <- rnorm(n, mean = muY1.MAUC)
  Y0 <- rnorm(n, mean = muY0.MAUC)
  PO.diff <- Y1 - Y0
  
  return(data.frame(Y, M, A, C, PO.diff, pM.AUC))
}


