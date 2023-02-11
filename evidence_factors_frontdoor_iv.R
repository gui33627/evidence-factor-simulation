
library(parallel)
if(!requireNamespace("foreach")) install.packages("foreach", repos = "https://cloud.r-project.org")
library(foreach)
if(!requireNamespace("doParallel")) install.packages("doParallel", repos = "https://cloud.r-project.org")
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
########################## 1. Under Null Hypothesis ############################
### a) both models are correct
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

# [1] 0

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


### b) front-door is true, iv is not
# i1 is violated (there is a backdoor path from Z to Y) 
# so when beta = 0, *the identified iv functional is not zero*
p_values_null_fdoor_correct_i1_violated <- foreach(i = 1:N, .combine = c) %dopar% {
  
  df <- dgp_frontdoor_iv_fdoor_correct_i1_violated(n = n, beta = 0)
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

typeI <- sum(p_values_null_fdoor_correct_i1_violated <= 0.05)/length(p_values_null_fdoor_correct_i1_violated)
power <- typeI
size <- power

# [1] 0.032

frontdoor_values = c(frontdoor_values, TRUE)
iv_values = c(iv_values, FALSE) 
frontdoor_true_functional = c(frontdoor_true_functional, 0)
iv_true_functional = c(iv_true_functional, 1) 
f1 = c(f1, TRUE)
f2 = c(f2, TRUE) 
f3 = c(f3, TRUE) 
i1 = c(i1, FALSE) 
i2 = c(i2, TRUE) 
i3 = c(i3, TRUE) 
i4 = c(i4, TRUE) 
hypothesis = c(hypothesis, "N")
beta = c(beta, 0)
size_values = c(size_values, size)
power_values = c(power_values, NA)



# i2 is violated (there is a direct effect from Z to Y),
# so when beta = 0, *the identified iv functional is not zero*
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

# [1] 0.044

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


# i3 is violated
# there is no backdoor path/direct effect from Z to Y, and the front door
# is true (M fully mediate and beta = 0),  *so the identified iv functional is zero*
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

# [1] 0

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


### c) iv is true, front-door is not 
# f1 is violated, *the identified front-door functional is zero*
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

# [1] 0

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


# f3 is violated 
# there is a backdoor path from M to Y, so when beta = 0,
# *the identified front-door functional is not zero*
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

# [1] 0.05

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


####################### 2. Under Alternative Hypothesis ########################
### a) both phi_k != 0, one of the models is correct 
# a.1) front door is correct, iv is wrong
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

# [1] 1

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


# a.2) iv is correct, front door is wrong
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

# [1] 1

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


### b) both phi_k != 0, both models are correct
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

# [1] 1


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


### c) one of phi_k = 0, one of the model is correct 
# c.1) iv correct, front door is wrong
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

# [1] 0.06

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



