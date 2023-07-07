
# Simulation for multiple backdoor adjustment sets with only one of them being valid
# Backdoor criteria: (B1) measured confounders C block every path between A and Y that contains an arrow into A; 
#                    (B2) no node in C is a descendant of A

# 1. valid backdoor adjustment
dgp_backdoor_valid <- function(n, beta = 3) {
  
  # Unmeasured confounder
  U <- matrix(runif(n, min=-2,max=2))
  
  # Measured confounders (not a descendant of A, (B2) satisfied)
  C <- data.frame(matrix(runif(n * 4, min=-2,max=2), ncol=4))
  
  # Treatment (a function only of measured confounders C, not a function of U, (B1) satisfied)
  pA.UC <- expit(C[,1] + expit(C[,2]) + sin(C[,3]))
  A <- rbinom(n, size = 1, prob = pA.UC) 
  
  # Outcome 
  muY1.AUC <- beta*1 + 2*U + 2* sqrt(abs(C[,1])) + sin(C[,4])
  muY0.AUC <- beta*0 + 2*U + 2* sqrt(abs(C[,1])) + sin(C[,4])
  Y1 <- rnorm(n, mean = muY1.AUC)
  Y0 <- rnorm(n, mean = muY0.AUC)
  Y <- ifelse(A==1, Y1, Y0)
  
  PO.diff <- Y1 - Y0
  
  return(data.frame(Y, A, C, PO.diff))
}


# 2. invalid backdoor adjustment: over-control covariates (control colliders)
dgp_backdoor_invalid_collider <- function(n, beta = 3) {
  
  # Unmeasured confounder
  U <- matrix(runif(n, min=-2,max=2))
  
  # Measured confounders 
  C <- data.frame(matrix(runif(n * 4, min=-2,max=2), ncol=4))
  
  # Treatment (a function only of measured confounders C, not a function of U, (B1) satisfied)
  pA.UC <- expit(C[,4] + expit(C[,2]) + sin(C[,3]))
  A <- rbinom(n, size = 1, prob = pA.UC) 
  
  # Outcome 
  muY1.AUC <- beta*1 + 2*U + 2* sqrt(abs(C[,2])) + sin(C[,4])
  muY0.AUC <- beta*0 + 2*U + 2* sqrt(abs(C[,2])) + sin(C[,4])
  Y1 <- rnorm(n, mean = muY1.AUC)
  Y0 <- rnorm(n, mean = muY0.AUC)
  Y <- ifelse(A==1, Y1, Y0)
  
  # Measured confounders (a node in C is a collider, (B2) violated)
  C[,1] <- rnorm(n, mean = (3*A - Y))
  
  PO.diff <- Y1 - Y0
  
  return(data.frame(Y, A, C, PO.diff))
}



# 3. under-control covaraits (there is an unblocked backdoor path)
dgp_backdoor_invalid_unblocked <- function(n, beta = 3) {
  
  # Unmeasured confounder
  U <- matrix(runif(n, min=-2,max=2))
  
  # Measured confounders (not a descendant of A, (B2) satisfied)
  C <- data.frame(matrix(runif(n * 4, min=-2,max=2), ncol=4))
  
  # Treatment (a function of U, (B1) violated)
  pA.UC <- expit(C[,1] + expit(C[,2]) + sin(C[,3]) + U)
  A <- rbinom(n, size = 1, prob = pA.UC) 
  
  # Outcome 
  muY1.AUC <- beta*1 + 2*U + 2* sqrt(abs(C[,1])) + sin(C[,4])
  muY0.AUC <- beta*0 + 2*U + 2* sqrt(abs(C[,1])) + sin(C[,4])
  Y1 <- rnorm(n, mean = muY1.AUC)
  Y0 <- rnorm(n, mean = muY0.AUC)
  Y <- ifelse(A==1, Y1, Y0)
  
  PO.diff <- Y1 - Y0
  
  return(data.frame(Y, A, C, PO.diff))
}






