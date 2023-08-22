
library(parallel)
# if(!requireNamespace("foreach")) install.packages("foreach", repos = "https://cloud.r-project.org")
library(foreach)
# if(!requireNamespace("doParallel")) install.packages("doParallel", repos = "https://cloud.r-project.org")
library(doParallel)
num_core <- 20
doParallel::registerDoParallel(cores = num_core)

############################## Front-door and IV ###############################

backdoor_values = NA
backdoor_true_functional = NA
b1 = NA
b2 = NA

frontdoor_values = c()
iv_values = c() 
frontdoor_true_functional = c()
iv_true_functional = c() 
f1 = c()
f2 = c() 
f3 = c() 
i1 = c() 
i2 = c() 
i3 = c() 
i4 = c() 

hypothesis = c()
beta = c()
size_values = c()
power_values = c()

# ----------------------------- Simulation study -------------------------------
########################## Under the Null ############################
### 1. both models are correct
p_values_null_both_correct <- foreach(i = 1:N, .combine = c) %dopar% {
  
  df <- dgp_frontdoor_iv_both_correct(n = n, beta = 0)
  data <- df$df
  
  # estimate using APIPW (front door IF)
  frontdoor <- estimate_frontdoor(data)
  frontdoor.est <- frontdoor$frontdoor.est
  frontdoor.eif <- frontdoor$frontdoor.eif
  
  # estimate using UIV (IV IF)
  iv <- estimate_uiv(data)
  iv.est <- iv$iv.est
  iv.eif <- iv$iv.eif
  
  # Evidence factor
  est <- c(frontdoor.est, iv.est)
  eif <- cbind(frontdoor.eif, iv.eif)
  evidence_factor(est = est, eif = eif)
  
}

typeI <- sum(p_values_null_both_correct <= 0.05)/length(p_values_null_both_correct)
power <- typeI
size <- power


frontdoor_values = c(frontdoor_values, TRUE)
iv_values = c(iv_values, TRUE) 
frontdoor_true_functional = c(frontdoor_true_functional, 0)
iv_true_functional = c(iv_true_functional, 0) 
f1 = c(f1, TRUE)
f2 = c(f2, TRUE) 
f3 = c(f3, TRUE) 
i1 = c(i1, TRUE) 
i2 = c(i2, TRUE) 
i3 = c(i3, TRUE) 
i4 = c(i4, TRUE) 
hypothesis = c(hypothesis, "N")
beta = c(beta, 0)
size_values = c(size_values, size)
power_values = c(power_values, NA)


### 2. iv is true, front-door is not 
# the identified front-door functional is zero
p_values_null_iv_correct_f1_violated <- foreach(i = 1:N, .combine = c) %dopar% {
  
  df <- dgp_frontdoor_iv_iv_correct_f1_violated(n = n, beta = 0)
  data <- df$df
  
  # estimate using APIPW (front door IF)
  frontdoor <- estimate_frontdoor(data)
  frontdoor.est <- frontdoor$frontdoor.est
  frontdoor.eif <- frontdoor$frontdoor.eif
  
  # estimate using UIV (IV IF)
  iv <- estimate_uiv(data)
  iv.est <- iv$iv.est
  iv.eif <- iv$iv.eif
  
  # Evidence factor
  est <- c(frontdoor.est, iv.est)
  eif <- cbind(frontdoor.eif, iv.eif)
  evidence_factor(est = est, eif = eif)
  
}

typeI <- sum(p_values_null_iv_correct_f1_violated <= 0.05)/length(p_values_null_iv_correct_f1_violated)
power <- typeI
size <- power

frontdoor_values = c(frontdoor_values, FALSE)
iv_values = c(iv_values, TRUE) 
frontdoor_true_functional = c(frontdoor_true_functional, 0)
iv_true_functional = c(iv_true_functional, 0) 
f1 = c(f1, FALSE)
f2 = c(f2, TRUE) 
f3 = c(f3, TRUE) 
i1 = c(i1, TRUE) 
i2 = c(i2, TRUE) 
i3 = c(i3, TRUE) 
i4 = c(i4, TRUE) 
hypothesis = c(hypothesis, "N")
beta = c(beta, 0)
size_values = c(size_values, size)
power_values = c(power_values, NA)

### 3. front-door is correct, IV is wrong
# the identified iv functional is zero
p_values_null_fdoor_correct_i3_violated <- foreach(i = 1:N, .combine = c) %dopar% {
  
  df <- dgp_frontdoor_iv_fdoor_correct_i3_violated(n = n, beta = 0)
  data <- df$df
  
  # estimate using APIPW (front door IF)
  frontdoor <- estimate_frontdoor(data)
  frontdoor.est <- frontdoor$frontdoor.est
  frontdoor.eif <- frontdoor$frontdoor.eif
  
  # estimate using UIV (IV IF)
  iv <- estimate_uiv(data)
  iv.est <- iv$iv.est
  iv.eif <- iv$iv.eif
  
  # Evidence factor
  est <- c(frontdoor.est, iv.est)
  eif <- cbind(frontdoor.eif, iv.eif)
  evidence_factor(est = est, eif = eif)
  
}

typeI <- sum(p_values_null_fdoor_correct_i3_violated <= 0.05)/length(p_values_null_fdoor_correct_i3_violated)
power <- typeI
size <- power

frontdoor_values = c(frontdoor_values, TRUE)
iv_values = c(iv_values, FALSE) 
frontdoor_true_functional = c(frontdoor_true_functional, 0)
iv_true_functional = c(iv_true_functional, 0) 
f1 = c(f1, TRUE)
f2 = c(f2, TRUE) 
f3 = c(f3, TRUE) 
i1 = c(i1, TRUE) 
i2 = c(i2, TRUE) 
i3 = c(i3, FALSE) 
i4 = c(i4, TRUE) 
hypothesis = c(hypothesis, "N")
beta = c(beta, 0)
size_values = c(size_values, size)
power_values = c(power_values, NA)


### 4. IV is correct, front-door is wrong
# there is a backdoor path from M to Y
# the identified front-door functional is not zero
p_values_null_iv_correct_f3_violated <- foreach(i = 1:N, .combine = c) %dopar% {
  
  df <- dgp_frontdoor_iv_iv_correct_f3_violated(n = n, beta = 0)
  data <- df$df
  
  # estimate using APIPW (front door IF)
  frontdoor <- estimate_frontdoor(data)
  frontdoor.est <- frontdoor$frontdoor.est
  frontdoor.eif <- frontdoor$frontdoor.eif
  
  # estimate using UIV (IV IF)
  iv <- estimate_uiv(data)
  iv.est <- iv$iv.est
  iv.eif <- iv$iv.eif
  
  # Evidence factor
  est <- c(frontdoor.est, iv.est)
  eif <- cbind(frontdoor.eif, iv.eif)
  evidence_factor(est = est, eif = eif)
  
}

typeI <- sum(p_values_null_iv_correct_f3_violated <= 0.05)/length(p_values_null_iv_correct_f3_violated)
power <- typeI
size <- power

frontdoor_values = c(frontdoor_values, FALSE)
iv_values = c(iv_values, TRUE) 
frontdoor_true_functional = c(frontdoor_true_functional, 1)
iv_true_functional = c(iv_true_functional, 0) 
f1 = c(f1, TRUE)
f2 = c(f2, TRUE) 
f3 = c(f3, FALSE) 
i1 = c(i1, TRUE) 
i2 = c(i2, TRUE) 
i3 = c(i3, TRUE) 
i4 = c(i4, TRUE) 
hypothesis = c(hypothesis, "N")
beta = c(beta, 0)
size_values = c(size_values, size)
power_values = c(power_values, NA)


### 5. front-door is correct, IV is wrong
# there is a direct effect from Z to Y
# the identified iv functional is not zero
p_values_null_fdoor_correct_i2_violated <- foreach(i = 1:N, .combine = c) %dopar% {
  
  df <- dgp_frontdoor_iv_fdoor_correct_i2_violated(n = n, beta = 0)
  data <- df$df
  
  # estimate using APIPW (front door IF)
  frontdoor <- estimate_frontdoor(data)
  frontdoor.est <- frontdoor$frontdoor.est
  frontdoor.eif <- frontdoor$frontdoor.eif
  
  # estimate using UIV (IV IF)
  iv <- estimate_uiv(data)
  iv.est <- iv$iv.est
  iv.eif <- iv$iv.eif
  
  # Evidence factor
  est <- c(frontdoor.est, iv.est)
  eif <- cbind(frontdoor.eif, iv.eif)
  evidence_factor(est = est, eif = eif)
  
}

typeI <- sum(p_values_null_fdoor_correct_i2_violated <= 0.05)/length(p_values_null_fdoor_correct_i2_violated)
power <- typeI
size <- power

frontdoor_values = c(frontdoor_values, TRUE)
iv_values = c(iv_values, FALSE) 
frontdoor_true_functional = c(frontdoor_true_functional, 0)
iv_true_functional = c(iv_true_functional, 1) 
f1 = c(f1, TRUE)
f2 = c(f2, TRUE) 
f3 = c(f3, TRUE) 
i1 = c(i1, TRUE) 
i2 = c(i2, FALSE) 
i3 = c(i3, TRUE) 
i4 = c(i4, TRUE) 
hypothesis = c(hypothesis, "N")
beta = c(beta, 0)
size_values = c(size_values, size)
power_values = c(power_values, NA)



########################## Under the Alternative ############################
### 1. both models are correct
p_values_alternative_both_correct <- foreach(i = 1:N, .combine = c) %dopar% {
  
  df <- dgp_frontdoor_iv_both_correct(n = n, beta = 10)
  data <- df$df
  
  # estimate using APIPW (front door IF)
  frontdoor <- estimate_frontdoor(data)
  frontdoor.est <- frontdoor$frontdoor.est
  frontdoor.eif <- frontdoor$frontdoor.eif
  
  # estimate using UIV (IV IF)
  iv <- estimate_uiv(data)
  iv.est <- iv$iv.est
  iv.eif <- iv$iv.eif
  
  # Evidence factor
  est <- c(frontdoor.est, iv.est)
  eif <- cbind(frontdoor.eif, iv.eif)
  evidence_factor(est = est, eif = eif)
  
}

typeII <- sum(p_values_alternative_both_correct > 0.05)/length(p_values_alternative_both_correct)
power <- 1-typeII


frontdoor_values = c(frontdoor_values, TRUE)
iv_values = c(iv_values, TRUE) 
frontdoor_true_functional = c(frontdoor_true_functional, 1)
iv_true_functional = c(iv_true_functional, 1) 
f1 = c(f1, TRUE)
f2 = c(f2, TRUE) 
f3 = c(f3, TRUE) 
i1 = c(i1, TRUE) 
i2 = c(i2, TRUE) 
i3 = c(i3, TRUE) 
i4 = c(i4, TRUE) 
hypothesis = c(hypothesis, "A")
beta = c(beta, 10)
size_values = c(size_values, NA)
power_values = c(power_values, power)


### 2. iv correct, front door is wrong
# the identified front-door functional is zero
p_values_alternative_iv_correct_f1_violated <- foreach(i = 1:N, .combine = c) %dopar% {
  
  df <- dgp_frontdoor_iv_iv_correct_f1_violated(n = n, beta = 10)
  data <- df$df
  
  # estimate using APIPW (front door IF)
  frontdoor <- estimate_frontdoor(data)
  frontdoor.est <- frontdoor$frontdoor.est
  frontdoor.eif <- frontdoor$frontdoor.eif
  
  # estimate using UIV (IV IF)
  iv <- estimate_uiv(data)
  iv.est <- iv$iv.est
  iv.eif <- iv$iv.eif
  
  # Evidence factor
  est <- c(frontdoor.est, iv.est)
  eif <- cbind(frontdoor.eif, iv.eif)
  evidence_factor(est = est, eif = eif)
  
}

typeII <- sum(p_values_alternative_iv_correct_f1_violated > 0.05)/length(p_values_alternative_iv_correct_f1_violated)
power <- 1-typeII

frontdoor_values = c(frontdoor_values, FALSE)
iv_values = c(iv_values, TRUE) 
frontdoor_true_functional = c(frontdoor_true_functional, 0)
iv_true_functional = c(iv_true_functional, 1) 
f1 = c(f1, FALSE)
f2 = c(f2, TRUE) 
f3 = c(f3, TRUE) 
i1 = c(i1, TRUE) 
i2 = c(i2, TRUE) 
i3 = c(i3, TRUE) 
i4 = c(i4, TRUE) 
hypothesis = c(hypothesis, "A")
beta = c(beta, 10)
size_values = c(size_values, NA)
power_values = c(power_values, power)


### 3. frontdoor is correct, iv is wrong
# the identified IV functional is zero
p_values_alternative_fdoor_correct_i3_violated <- foreach(i = 1:N, .combine = c) %dopar% {
  
  df <- dgp_frontdoor_iv_fdoor_correct_i3_violated_alt_zero(n = n, beta = 10)
  data <- df$df
  
  # estimate using APIPW (front door IF)
  frontdoor <- estimate_frontdoor(data)
  frontdoor.est <- frontdoor$frontdoor.est
  frontdoor.eif <- frontdoor$frontdoor.eif
  
  # estimate using UIV (IV IF)
  iv <- estimate_uiv(data)
  iv.est <- iv$iv.est
  iv.eif <- iv$iv.eif
  
  # Evidence factor
  est <- c(frontdoor.est, iv.est)
  eif <- cbind(frontdoor.eif, iv.eif)
  evidence_factor(est = est, eif = eif)
  
}

typeII <- sum(p_values_alternative_fdoor_correct_i3_violated > 0.05, na.rm = T)/length(p_values_alternative_fdoor_correct_i3_violated)
power <- 1-typeII


frontdoor_values = c(frontdoor_values, TRUE)
iv_values = c(iv_values, FALSE) 
frontdoor_true_functional = c(frontdoor_true_functional, 1)
iv_true_functional = c(iv_true_functional, 0) 
f1 = c(f1, TRUE)
f2 = c(f2, TRUE) 
f3 = c(f3, TRUE) 
i1 = c(i1, TRUE) 
i2 = c(i2, TRUE) 
i3 = c(i3, FALSE) 
i4 = c(i4, TRUE) 
hypothesis = c(hypothesis, "A")
beta = c(beta, 5)
size_values = c(size_values, NA)
power_values = c(power_values, power)


### 4. iv is correct, front door is wrong
p_values_alternative_iv_correct_f3_violated <- foreach(i = 1:N, .combine = c) %dopar% {
  
  df <- dgp_frontdoor_iv_iv_correct_f3_violated(n = n, beta = 10)
  data <- df$df
  
  # estimate using APIPW (front door IF)
  frontdoor <- estimate_frontdoor(data)
  frontdoor.est <- frontdoor$frontdoor.est
  frontdoor.eif <- frontdoor$frontdoor.eif
  
  # estimate using UIV (IV IF)
  iv <- estimate_uiv(data)
  iv.est <- iv$iv.est
  iv.eif <- iv$iv.eif
  
  # Evidence factor
  est <- c(frontdoor.est, iv.est)
  eif <- cbind(frontdoor.eif, iv.eif)
  evidence_factor(est = est, eif = eif)
  
}

typeII <- sum(p_values_alternative_iv_correct_f3_violated > 0.05)/length(p_values_alternative_iv_correct_f3_violated)
power <- 1-typeII

frontdoor_values = c(frontdoor_values, FALSE)
iv_values = c(iv_values, TRUE) 
frontdoor_true_functional = c(frontdoor_true_functional, 1)
iv_true_functional = c(iv_true_functional, 1) 
f1 = c(f1, TRUE)
f2 = c(f2, TRUE) 
f3 = c(f3, FALSE) 
i1 = c(i1, TRUE) 
i2 = c(i2, TRUE) 
i3 = c(i3, TRUE) 
i4 = c(i4, TRUE) 
hypothesis = c(hypothesis, "A")
beta = c(beta, 10)
size_values = c(size_values, NA)
power_values = c(power_values, power)


### 5. front door is correct, iv is wrong
p_values_alternative_fdoor_correct_i2_violated <- foreach(i = 1:N, .combine = c) %dopar% {
  
  df <- dgp_frontdoor_iv_fdoor_correct_i2_violated(n = n, beta = 10)
  data <- df$df
  
  # estimate using APIPW (front door IF)
  frontdoor <- estimate_frontdoor(data)
  frontdoor.est <- frontdoor$frontdoor.est
  frontdoor.eif <- frontdoor$frontdoor.eif
  
  # estimate using UIV (IV IF)
  iv <- estimate_uiv(data)
  iv.est <- iv$iv.est
  iv.eif <- iv$iv.eif
  
  # Evidence factor
  est <- c(frontdoor.est, iv.est)
  eif <- cbind(frontdoor.eif, iv.eif)
  evidence_factor(est = est, eif = eif)
  
}

typeII <- sum(p_values_alternative_fdoor_correct_i2_violated > 0.05)/length(p_values_alternative_fdoor_correct_i2_violated)
power <- 1-typeII

frontdoor_values = c(frontdoor_values, TRUE)
iv_values = c(iv_values, FALSE) 
frontdoor_true_functional = c(frontdoor_true_functional, 1)
iv_true_functional = c(iv_true_functional, 1) 
f1 = c(f1, TRUE)
f2 = c(f2, TRUE) 
f3 = c(f3, TRUE) 
i1 = c(i1, TRUE) 
i2 = c(i2, FALSE) 
i3 = c(i3, TRUE) 
i4 = c(i4, TRUE) 
hypothesis = c(hypothesis, "A")
beta = c(beta, 10)
size_values = c(size_values, NA)
power_values = c(power_values, power)







