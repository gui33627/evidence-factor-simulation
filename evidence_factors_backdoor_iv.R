
library(parallel)
# if(!requireNamespace("foreach")) install.packages("foreach", repos = "https://cloud.r-project.org")
library(foreach)
# if(!requireNamespace("doParallel")) install.packages("doParallel", repos = "https://cloud.r-project.org")
library(doParallel)
num_core <- 20
doParallel::registerDoParallel(cores = num_core)

############################## Backdoor and IV ################################

frontdoor_values = NA
frontdoor_true_functional = NA
f1 = NA
f2 = NA 
f3 = NA 

backdoor_values = c()
iv_values = c() 
backdoor_true_functional = c()
iv_true_functional = c() 
b1 = c()
b2 = c()
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
  
  df <- dgp_backdoor_iv_both_correct(n = n, beta = 0)
  data <- df$df
  
  # estimate using AIPW (backdoor IF)
  backdoor <- estimate_backdooor(data)
  backdoor.est <- backdoor$backdoor.est
  backdoor.eif <- backdoor$backdoor.eif
  
  # estimate using UIV (IV IF)
  iv <- estimate_uiv(data)
  iv.est <- iv$iv.est
  iv.eif <- iv$iv.eif
  
  # Evidence factor
  est <- c(backdoor.est, iv.est)
  eif <- cbind(backdoor.eif, iv.eif)
  evidence_factor(est = est, eif = eif)
  
}

typeI <- sum(p_values_null_both_correct <= 0.05)/length(p_values_null_both_correct)
power <- typeI
size <- power


backdoor_values = c(backdoor_values, TRUE)
iv_values = c(iv_values, TRUE) 
backdoor_true_functional = c(backdoor_true_functional, 0)
iv_true_functional = c(iv_true_functional, 0) 
b1 = c(b1, TRUE)
b2 = c(b2, TRUE)
i1 = c(i1, TRUE) 
i2 = c(i2, TRUE) 
i3 = c(i3, TRUE) 
i4 = c(i4, TRUE) 
hypothesis = c(hypothesis, "N")
beta = c(beta, 0)
size_values = c(size_values, size)
power_values = c(power_values, NA)


### 2. IV is correct, backdoor is wrong
# there is a backdoor path from A to Y
# the identified backdoor functional is zero
p_values_null_iv_correct_b1_violated <- foreach(i = 1:N, .combine = c) %dopar% {
  
  df <- dgp_backdoor_iv_iv_correct_b1_violated(n = n, beta = 0)
  data <- df$df
  
  # estimate using AIPW (backdoor IF)
  backdoor <- estimate_backdooor(data)
  backdoor.est <- backdoor$backdoor.est
  backdoor.eif <- backdoor$backdoor.eif
  
  # estimate using UIV (IV IF)
  iv <- estimate_uiv(data)
  iv.est <- iv$iv.est
  iv.eif <- iv$iv.eif
  
  # Evidence factor
  est <- c(backdoor.est, iv.est)
  eif <- cbind(backdoor.eif, iv.eif)
  evidence_factor(est = est, eif = eif)
  
}

typeI <- sum(p_values_null_iv_correct_b1_violated <= 0.05)/length(p_values_null_iv_correct_b1_violated)
power <- typeI
size <- power


backdoor_values = c(backdoor_values, FALSE)
iv_values = c(iv_values, TRUE) 
backdoor_true_functional = c(backdoor_true_functional, 0)
iv_true_functional = c(iv_true_functional, 0) 
b1 = c(b1, FALSE)
b2 = c(b2, TRUE)
i1 = c(i1, TRUE) 
i2 = c(i2, TRUE) 
i3 = c(i3, TRUE) 
i4 = c(i4, TRUE) 
hypothesis = c(hypothesis, "N")
beta = c(beta, 0)
size_values = c(size_values, size)
power_values = c(power_values, NA)

### 3. backdoor is correct, iv is not
# the identified iv functional is zero
p_values_null_bdoor_correct_i3_violated <- foreach(i = 1:N, .combine = c) %dopar% {
  
  df <- dgp_backdoor_iv_bdoor_correct_i3_violated(n = n, beta = 0, beta_defier = 0)
  data <- df$df
  
  # estimate using AIPW (backdoor IF)
  backdoor <- estimate_backdooor(data)
  backdoor.est <- backdoor$backdoor.est
  backdoor.eif <- backdoor$backdoor.eif
  
  # estimate using UIV (IV IF)
  iv <- estimate_uiv(data)
  iv.est <- iv$iv.est
  iv.eif <- iv$iv.eif
  
  # Evidence factor
  est <- c(backdoor.est, iv.est)
  eif <- cbind(backdoor.eif, iv.eif)
  evidence_factor(est = est, eif = eif)
  
}

typeI <- sum(p_values_null_bdoor_correct_i3_violated <= 0.05)/length(p_values_null_bdoor_correct_i3_violated)
power <- typeI
size <- power

backdoor_values = c(backdoor_values, TRUE)
iv_values = c(iv_values, FALSE) 
backdoor_true_functional = c(backdoor_true_functional, 0)
iv_true_functional = c(iv_true_functional, 0) 
b1 = c(b1, TRUE)
b2 = c(b2, TRUE)
i1 = c(i1, TRUE) 
i2 = c(i2, TRUE) 
i3 = c(i3, FALSE) 
i4 = c(i4, TRUE) 
hypothesis = c(hypothesis, "N")
beta = c(beta, 0)
size_values = c(size_values, size)
power_values = c(power_values, NA)


### 4. iv is correct, backdoor is not 
# conditioning on C opens a path from A to Y,
# the identified backdoor functional is not zero
p_values_null_iv_correct_b2_violated <- foreach(i = 1:N, .combine = c) %dopar% {
  
  df <- dgp_backdoor_iv_iv_correct_b2_violated(n = n, beta = 0)
  data <- df$df
  
  # estimate using AIPW (backdoor IF)
  backdoor <- estimate_backdooor(data)
  backdoor.est <- backdoor$backdoor.est
  backdoor.eif <- backdoor$backdoor.eif
  
  # estimate using UIV (IV IF)
  iv <- estimate_uiv(data)
  iv.est <- iv$iv.est
  iv.eif <- iv$iv.eif
  
  # Evidence factor
  est <- c(backdoor.est, iv.est)
  eif <- cbind(backdoor.eif, iv.eif)
  evidence_factor(est = est, eif = eif)
  
}

typeI <- sum(p_values_null_iv_correct_b2_violated <= 0.05)/length(p_values_null_iv_correct_b2_violated)
power <- typeI
size <- power


backdoor_values = c(backdoor_values, FALSE)
iv_values = c(iv_values, TRUE) 
backdoor_true_functional = c(backdoor_true_functional, 1)
iv_true_functional = c(iv_true_functional, 0) 
b1 = c(b1, TRUE)
b2 = c(b2, FALSE)
i1 = c(i1, TRUE) 
i2 = c(i2, TRUE) 
i3 = c(i3, TRUE) 
i4 = c(i4, TRUE) 
hypothesis = c(hypothesis, "N")
beta = c(beta, 0)
size_values = c(size_values, size)
power_values = c(power_values, NA)


### 5. backdoor is correct, IV is wrong
# the identified IV functional is not zero
p_values_null_bdoor_correct_i2_violated <- foreach(i = 1:N, .combine = c) %dopar% {
  
  df <- dgp_backdoor_iv_bdoor_correct_i2_violated(n = n, beta = 0)
  data <- df$df
  
  # estimate using AIPW (backdoor IF)
  backdoor <- estimate_backdooor(data, confounderZ = TRUE)
  backdoor.est <- backdoor$backdoor.est
  backdoor.eif <- backdoor$backdoor.eif
  
  # estimate using UIV (IV IF)
  iv <- estimate_uiv(data)
  iv.est <- iv$iv.est
  iv.eif <- iv$iv.eif
  
  # Evidence factor
  est <- c(backdoor.est, iv.est)
  eif <- cbind(backdoor.eif, iv.eif)
  evidence_factor(est = est, eif = eif)
  
}

typeI <- sum(p_values_null_bdoor_correct_i2_violated <= 0.05)/length(p_values_null_bdoor_correct_i2_violated)
power <- typeI
size <- power

backdoor_values = c(backdoor_values, TRUE)
iv_values = c(iv_values, FALSE) 
backdoor_true_functional = c(backdoor_true_functional, 0)
iv_true_functional = c(iv_true_functional, 1) 
b1 = c(b1, TRUE)
b2 = c(b2, TRUE)
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
  
  df <- dgp_backdoor_iv_both_correct(n = n, beta = 10)
  data <- df$df
  
  # estimate using AIPW (backdoor IF)
  backdoor <- estimate_backdooor(data)
  backdoor.est <- backdoor$backdoor.est
  backdoor.eif <- backdoor$backdoor.eif
  
  # estimate using UIV (IV IF)
  iv <- estimate_uiv(data)
  iv.est <- iv$iv.est
  iv.eif <- iv$iv.eif
  
  # Evidence factor
  est <- c(backdoor.est, iv.est)
  eif <- cbind(backdoor.eif, iv.eif)
  evidence_factor(est = est, eif = eif)
  
}

typeII <- sum(p_values_alternative_both_correct > 0.05)/length(p_values_alternative_both_correct)
power <- 1-typeII

# [1] 1

backdoor_values = c(backdoor_values, TRUE)
iv_values = c(iv_values, TRUE) 
backdoor_true_functional = c(backdoor_true_functional, 1)
iv_true_functional = c(iv_true_functional, 1) 
b1 = c(b1, TRUE)
b2 = c(b2, TRUE)
i1 = c(i1, TRUE) 
i2 = c(i2, TRUE) 
i3 = c(i3, TRUE) 
i4 = c(i4, TRUE) 
hypothesis = c(hypothesis, "A")
beta = c(beta, 10)
size_values = c(size_values, NA)
power_values = c(power_values, power)


### 2. iv is correct, backdoor is wrong 
# the identified backdoor functional is zero
p_values_alternative_iv_correct_b2_violated_alt_zero <- foreach(i = 1:N, .combine = c) %dopar% {
  
  df <- dgp_backdoor_iv_iv_correct_b2_violated_alt_zero(n = n, beta = 10)
  data <- df$df
  
  # estimate using AIPW (backdoor IF)
  backdoor <- estimate_backdooor(data)
  backdoor.est <- backdoor$backdoor.est
  backdoor.eif <- backdoor$backdoor.eif
  
  # estimate using UIV (IV IF)
  iv <- estimate_uiv(data)
  iv.est <- iv$iv.est
  iv.eif <- iv$iv.eif
  
  # Evidence factor
  est <- c(backdoor.est, iv.est)
  eif <- cbind(backdoor.eif, iv.eif)
  evidence_factor(est = est, eif = eif)
  
}

typeII <- sum(p_values_alternative_iv_correct_b2_violated_alt_zero > 0.05)/length(p_values_alternative_iv_correct_b2_violated_alt_zero)
power <- 1-typeII


backdoor_values = c(backdoor_values, FALSE)
iv_values = c(iv_values, TRUE) 
backdoor_true_functional = c(backdoor_true_functional, 0)
iv_true_functional = c(iv_true_functional, 1) 
b1 = c(b1, TRUE)
b2 = c(b2, FALSE)
i1 = c(i1, TRUE) 
i2 = c(i2, TRUE) 
i3 = c(i3, TRUE) 
i4 = c(i4, TRUE) 
hypothesis = c(hypothesis, "A")
beta = c(beta, 10)
size_values = c(size_values, NA)
power_values = c(power_values, power)

### 3. backdoor is correct, iv is wrong 
p_values_alternative_bdoor_correct_i3_violated_alt_zero <- foreach(i = 1:N, .combine = c) %dopar% {
  
  df <- dgp_backdoor_iv_bdoor_correct_i3_violated(n = n, beta = 5.75, beta_defier = 10)
  data <- df$df
  
  # estimate using AIPW (backdoor IF)
  backdoor <- estimate_backdooor(data)
  backdoor.est <- backdoor$backdoor.est
  backdoor.eif <- backdoor$backdoor.eif
  
  # estimate using UIV (IV IF)
  iv <- estimate_uiv(data)
  iv.est <- iv$iv.est
  iv.eif <- iv$iv.eif
  
  # Evidence factor
  est <- c(backdoor.est, iv.est)
  eif <- cbind(backdoor.eif, iv.eif)
  evidence_factor(est = est, eif = eif)
  
}

typeII <- sum(p_values_alternative_bdoor_correct_i3_violated_alt_zero > 0.05)/length(p_values_alternative_bdoor_correct_i3_violated_alt_zero)
power <- 1-typeII


backdoor_values = c(backdoor_values, TRUE)
iv_values = c(iv_values, FALSE) 
backdoor_true_functional = c(backdoor_true_functional, 1)
iv_true_functional = c(iv_true_functional, 0) 
b1 = c(b1, TRUE)
b2 = c(b2, TRUE)
i1 = c(i1, TRUE) 
i2 = c(i2, TRUE) 
i3 = c(i3, FALSE) 
i4 = c(i4, TRUE) 
hypothesis = c(hypothesis, "A")
beta = c(beta, 10)
size_values = c(size_values, NA)
power_values = c(power_values, power)


### 4. iv is correct, backdoor is wrong 
p_values_alternative_iv_correct_b2_violated <- foreach(i = 1:N, .combine = c) %dopar% {
  
  df <- dgp_backdoor_iv_iv_correct_b2_violated_alt_nonzero(n = n, beta = 10)
  data <- df$df
  
  # estimate using AIPW (backdoor IF)
  backdoor <- estimate_backdooor(data)
  backdoor.est <- backdoor$backdoor.est
  backdoor.eif <- backdoor$backdoor.eif
  
  # estimate using UIV (IV IF)
  iv <- estimate_uiv(data)
  iv.est <- iv$iv.est
  iv.eif <- iv$iv.eif
  
  # Evidence factor
  est <- c(backdoor.est, iv.est)
  eif <- cbind(backdoor.eif, iv.eif)
  evidence_factor(est = est, eif = eif)
  
}

typeII <- sum(p_values_alternative_iv_correct_b2_violated > 0.05)/length(p_values_alternative_iv_correct_b2_violated)
power <- 1-typeII

# [1] 1

backdoor_values = c(backdoor_values, FALSE)
iv_values = c(iv_values, TRUE) 
backdoor_true_functional = c(backdoor_true_functional, 1)
iv_true_functional = c(iv_true_functional, 1) 
b1 = c(b1, TRUE)
b2 = c(b2, FALSE)
i1 = c(i1, TRUE) 
i2 = c(i2, TRUE) 
i3 = c(i3, TRUE) 
i4 = c(i4, TRUE) 
hypothesis = c(hypothesis, "A")
beta = c(beta, 10)
size_values = c(size_values, NA)
power_values = c(power_values, power)

### 5. backdoor is correct, iv is wrong 
p_values_alternative_bdoor_correct_i3_violated <- foreach(i = 1:N, .combine = c) %dopar% {
  
  df <- dgp_backdoor_iv_bdoor_correct_i3_violated(n = n, beta = 10, beta_defier = 2)
  data <- df$df
  
  # estimate using AIPW (backdoor IF)
  backdoor <- estimate_backdooor(data)
  backdoor.est <- backdoor$backdoor.est
  backdoor.eif <- backdoor$backdoor.eif
  
  # estimate using UIV (IV IF)
  iv <- estimate_uiv(data)
  iv.est <- iv$iv.est
  iv.eif <- iv$iv.eif
  
  # Evidence factor
  est <- c(backdoor.est, iv.est)
  eif <- cbind(backdoor.eif, iv.eif)
  evidence_factor(est = est, eif = eif)
  
}

typeII <- sum(p_values_alternative_bdoor_correct_i3_violated > 0.05)/length(p_values_alternative_bdoor_correct_i3_violated)
power <- 1-typeII

backdoor_values = c(backdoor_values, TRUE)
iv_values = c(iv_values, FALSE) 
backdoor_true_functional = c(backdoor_true_functional, 1)
iv_true_functional = c(iv_true_functional, 1) 
b1 = c(b1, TRUE)
b2 = c(b2, TRUE)
i1 = c(i1, TRUE) 
i2 = c(i2, TRUE) 
i3 = c(i3, FALSE) 
i4 = c(i4, TRUE) 
hypothesis = c(hypothesis, "A")
beta = c(beta, 10)
size_values = c(size_values, NA)
power_values = c(power_values, power)

