
library(parallel)
if(!requireNamespace("foreach")) install.packages("foreach", repos = "https://cloud.r-project.org")
library(foreach)
if(!requireNamespace("doParallel")) install.packages("doParallel", repos = "https://cloud.r-project.org")
library(doParallel)
num_core <- 20
doParallel::registerDoParallel(cores = num_core)

########################### Backdoor and Front-door ############################

iv_values = NA
iv_true_functional = NA
i1 = NA
i2 = NA
i3 = NA
i4 = NA

backdoor_values = c()
frontdoor_values = c()
backdoor_true_functional = c()
frontdoor_true_functional = c()
b1 = c()
b2 = c()
f1 = c()
f2 = c() 
f3 = c() 

hypothesis = c()
beta = c()
size_values = c()
power_values = c()

# ----------------------------- Simulation study -------------------------------
########################## 1. Under Null Hypothesis ############################
### a) both models are correct
p_values_null_both_correct <- foreach(i = 1:N, .combine = c) %dopar% {
  
  data <- dgp_backdoor_frontdoor_both_correct(n = n, beta = 0)
  
  # estimate using AIPW (backdoor IF)
  backdoor <- estimate_backdooor(data)
  backdoor.est <- backdoor$backdoor.est
  backdoor.eif <- backdoor$backdoor.eif
  
  # estimate using APIPW (front door IF)
  frontdoor <- estimate_frontdoor(data)
  frontdoor.est <- frontdoor$frontdoor.est
  frontdoor.eif <- frontdoor$frontdoor.eif
  
  # Evidence factor
  est <- c(backdoor.est, frontdoor.est)
  eif <- cbind(backdoor.eif, frontdoor.eif)
  evidence_factor(est = est, eif = eif)
  
}

typeI <- sum(p_values_null_both_correct <= 0.05)/length(p_values_null_both_correct)
power <- typeI
size <- power

# [1] 0

# write the result to the table
backdoor_values = c(backdoor_values, TRUE)
frontdoor_values = c(frontdoor_values, TRUE)
backdoor_true_functional = c(backdoor_true_functional, 0)
frontdoor_true_functional = c(frontdoor_true_functional, 0)
b1 = c(b1, TRUE)
b2 = c(b2, TRUE)
f1 = c(f1, TRUE)
f2 = c(f2, TRUE) 
f3 = c(f3, TRUE) 
hypothesis = c(hypothesis, "N")
beta = c(beta, 0)
size_values = c(size_values, size)
power_values = c(power_values, NA)


### b) backdoor is correct, front-door is not
# b.1) f1 is violated (the identified front door functional is 0)
p_values_null_bdoor_correct_f1_violated <- foreach(i = 1:N, .combine = c) %dopar% {
  
  data <- dgp_backdoor_frontdoor_bdoor_correct_f1_violated(n = n, beta = 0)
  
  # estimate using AIPW (backdoor IF)
  backdoor <- estimate_backdooor(data)
  backdoor.est <- backdoor$backdoor.est
  backdoor.eif <- backdoor$backdoor.eif
  
  # estimate using APIPW (front door IF)
  frontdoor <- estimate_frontdoor(data)
  frontdoor.est <- frontdoor$frontdoor.est
  frontdoor.eif <- frontdoor$frontdoor.eif
  
  # Evidence factor
  est <- c(backdoor.est, frontdoor.est)
  eif <- cbind(backdoor.eif, frontdoor.eif)
  evidence_factor(est = est, eif = eif)
  
}

typeI <- sum(p_values_null_bdoor_correct_f1_violated <= 0.05)/length(p_values_null_bdoor_correct_f1_violated)
power <- typeI
size <- power

# [1] 0

# write the result to the table
backdoor_values = c(backdoor_values, TRUE)
frontdoor_values = c(frontdoor_values, FALSE)
backdoor_true_functional = c(backdoor_true_functional, 0)
frontdoor_true_functional = c(frontdoor_true_functional, 0)
b1 = c(b1, TRUE)
b2 = c(b2, TRUE)
f1 = c(f1, FALSE)
f2 = c(f2, TRUE) 
f3 = c(f3, TRUE) 
hypothesis = c(hypothesis, "N")
beta = c(beta, 0)
size_values = c(size_values, size)
power_values = c(power_values, NA)


# b.2) f3 is violated, there is a backdoor path from M to Y,
# so when beta = 0, *the identified front door functional is not 0*
p_values_null_bdoor_correct_f3_violated <- foreach(i = 1:N, .combine = c) %dopar% {
  
  data <- dgp_backdoor_frontdoor_bdoor_correct_f3_violated(n = n, beta = 0)
  
  # estimate using AIPW (backdoor IF)
  backdoor <- estimate_backdooor(data)
  backdoor.est <- backdoor$backdoor.est
  backdoor.eif <- backdoor$backdoor.eif
  
  # estimate using APIPW (front door IF)
  frontdoor <- estimate_frontdoor(data)
  frontdoor.est <- frontdoor$frontdoor.est
  frontdoor.eif <- frontdoor$frontdoor.eif
  
  # Evidence factor
  est <- c(backdoor.est, frontdoor.est)
  eif <- cbind(backdoor.eif, frontdoor.eif)
  evidence_factor(est = est, eif = eif)
  
}

typeI <- sum(p_values_null_bdoor_correct_f3_violated <= 0.05)/length(p_values_null_bdoor_correct_f3_violated)
power <- typeI
size <- power

# [1] 0.06


# write the result to the table
backdoor_values = c(backdoor_values, TRUE)
frontdoor_values = c(frontdoor_values, FALSE)
backdoor_true_functional = c(backdoor_true_functional, 0)
frontdoor_true_functional = c(frontdoor_true_functional, 1)
b1 = c(b1, TRUE)
b2 = c(b2, TRUE)
f1 = c(f1, TRUE)
f2 = c(f2, TRUE) 
f3 = c(f3, FALSE) 
hypothesis = c(hypothesis, "N")
beta = c(beta, 0)
size_values = c(size_values, size)
power_values = c(power_values, NA)



### c) front-door is correct, backdoor is not
# b1 is violated, there is a backdoor path from A to Y,
# so when beta = 0, *the identified backdoor functional is not 0*
p_values_null_fdoor_correct_b1_violated <- foreach(i = 1:N, .combine = c) %dopar% {
  
  data <- dgp_backdoor_frontdoor_fdoor_correct_b1_violated(n = n, beta = 0)
  
  # estimate using AIPW (backdoor IF)
  backdoor <- estimate_backdooor(data)
  backdoor.est <- backdoor$backdoor.est
  backdoor.eif <- backdoor$backdoor.eif
  
  # estimate using APIPW (front door IF)
  frontdoor <- estimate_frontdoor(data)
  frontdoor.est <- frontdoor$frontdoor.est
  frontdoor.eif <- frontdoor$frontdoor.eif
  
  # Evidence factor
  est <- c(backdoor.est, frontdoor.est)
  eif <- cbind(backdoor.eif, frontdoor.eif)
  evidence_factor(est = est, eif = eif)
  
}

typeI <- sum(p_values_null_fdoor_correct_b1_violated <= 0.05)/length(p_values_null_fdoor_correct_b1_violated)
power <- typeI
size <- power

# [1] 0.046

# write the result to the table
backdoor_values = c(backdoor_values, FALSE)
frontdoor_values = c(frontdoor_values, TRUE)
backdoor_true_functional = c(backdoor_true_functional, 1)
frontdoor_true_functional = c(frontdoor_true_functional, 0)
b1 = c(b1, FALSE)
b2 = c(b2, TRUE)
f1 = c(f1, TRUE)
f2 = c(f2, TRUE) 
f3 = c(f3, TRUE) 
hypothesis = c(hypothesis, "N")
beta = c(beta, 0)
size_values = c(size_values, size)
power_values = c(power_values, NA)


# backdoor is wrong, but the identified backdoor functional is 0 
p_values_null_fdoor_correct_b1_violated_null_zero <- foreach(i = 1:N, .combine = c) %dopar% {
  
  data <- dgp_backdoor_frontdoor_fdoor_correct_b1_violated_null(n = n, beta = 0)
  
  # estimate using AIPW (backdoor IF)
  backdoor <- estimate_backdooor(data)
  backdoor.est <- backdoor$backdoor.est
  backdoor.eif <- backdoor$backdoor.eif
  
  # estimate using APIPW (front door IF)
  frontdoor <- estimate_frontdoor(data)
  frontdoor.est <- frontdoor$frontdoor.est
  frontdoor.eif <- frontdoor$frontdoor.eif
  
  # Evidence factor
  est <- c(backdoor.est, frontdoor.est)
  eif <- cbind(backdoor.eif, frontdoor.eif)
  evidence_factor(est = est, eif = eif)
  
}

typeI <- sum(p_values_null_fdoor_correct_b1_violated_null_zero <= 0.05)/length(p_values_null_fdoor_correct_b1_violated_null_zero)
power <- typeI
size <- power


# write the result to the table
backdoor_values = c(backdoor_values, FALSE)
frontdoor_values = c(frontdoor_values, TRUE)
backdoor_true_functional = c(backdoor_true_functional, 0)
frontdoor_true_functional = c(frontdoor_true_functional, 0)
b1 = c(b1, FALSE)
b2 = c(b2, TRUE)
f1 = c(f1, TRUE)
f2 = c(f2, TRUE) 
f3 = c(f3, TRUE) 
hypothesis = c(hypothesis, "N")
beta = c(beta, 0)
size_values = c(size_values, size)
power_values = c(power_values, NA)




####################### 2. Under Alternative Hypothesis ########################
### a) all phi_k != 0, one of the model is correct
# a.1) front door is correct, backdoor is wrong
p_values_alternative_fdoor_correct_b1_violated <- foreach(i = 1:N, .combine = c) %dopar% {
  
  data <- dgp_backdoor_frontdoor_fdoor_correct_b1_violated(n = n, beta = 10)
  
  # estimate using AIPW (backdoor IF)
  backdoor <- estimate_backdooor(data)
  backdoor.est <- backdoor$backdoor.est
  backdoor.eif <- backdoor$backdoor.eif
  
  # estimate using APIPW (front door IF)
  frontdoor <- estimate_frontdoor(data)
  frontdoor.est <- frontdoor$frontdoor.est
  frontdoor.eif <- frontdoor$frontdoor.eif
  
  # Evidence factor
  est <- c(backdoor.est, frontdoor.est)
  eif <- cbind(backdoor.eif, frontdoor.eif)
  evidence_factor(est = est, eif = eif)
  
}

typeII <- sum(p_values_alternative_fdoor_correct_b1_violated > 0.05)/length(p_values_alternative_fdoor_correct_b1_violated)
power <- 1-typeII

# [1] 1

# write the result to the table
backdoor_values = c(backdoor_values, FALSE)
frontdoor_values = c(frontdoor_values, TRUE)
backdoor_true_functional = c(backdoor_true_functional, 1)
frontdoor_true_functional = c(frontdoor_true_functional, 1)
b1 = c(b1, FALSE)
b2 = c(b2, TRUE)
f1 = c(f1, TRUE)
f2 = c(f2, TRUE) 
f3 = c(f3, TRUE) 
hypothesis = c(hypothesis, "A")
beta = c(beta, 10)
size_values = c(size_values, NA)
power_values = c(power_values, power)



# a.2) backdoor is correct, front door is wrong
p_values_alternative_bdoor_correct_f3_violated <- foreach(i = 1:N, .combine = c) %dopar% {
  
  data <- dgp_backdoor_frontdoor_bdoor_correct_f3_violated(n = n, beta = 10)
  
  # estimate using AIPW (backdoor IF)
  backdoor <- estimate_backdooor(data)
  backdoor.est <- backdoor$backdoor.est
  backdoor.eif <- backdoor$backdoor.eif
  
  # estimate using APIPW (front door IF)
  frontdoor <- estimate_frontdoor(data)
  frontdoor.est <- frontdoor$frontdoor.est
  frontdoor.eif <- frontdoor$frontdoor.eif
  
  # Evidence factor
  est <- c(backdoor.est, frontdoor.est)
  eif <- cbind(backdoor.eif, frontdoor.eif)
  evidence_factor(est = est, eif = eif)
  
}

typeII <- sum(p_values_alternative_bdoor_correct_f3_violated > 0.05)/length(p_values_alternative_bdoor_correct_f3_violated)
power <- 1-typeII

# [1] 0.968

# write the result to the table
backdoor_values = c(backdoor_values, TRUE)
frontdoor_values = c(frontdoor_values, FALSE)
backdoor_true_functional = c(backdoor_true_functional, 1)
frontdoor_true_functional = c(frontdoor_true_functional, 1)
b1 = c(b1, TRUE)
b2 = c(b2, TRUE)
f1 = c(f1, TRUE)
f2 = c(f2, TRUE) 
f3 = c(f3, FALSE) 
hypothesis = c(hypothesis, "A")
beta = c(beta, 10)
size_values = c(size_values, NA)
power_values = c(power_values, power)


### b) all phi_k != 0, both models are correct
p_values_alternative_both_correct <- foreach(i = 1:N, .combine = c) %dopar% {
  
  data <- dgp_backdoor_frontdoor_both_correct(n = n, beta = 10)
  
  # estimate using AIPW (backdoor IF)
  backdoor <- estimate_backdooor(data)
  backdoor.est <- backdoor$backdoor.est
  backdoor.eif <- backdoor$backdoor.eif
  
  # estimate using APIPW (front door IF)
  frontdoor <- estimate_frontdoor(data)
  frontdoor.est <- frontdoor$frontdoor.est
  frontdoor.eif <- frontdoor$frontdoor.eif
  
  # Evidence factor
  est <- c(backdoor.est, frontdoor.est)
  eif <- cbind(backdoor.eif, frontdoor.eif)
  evidence_factor(est = est, eif = eif)
  
}

typeII <- sum(p_values_alternative_both_correct > 0.05)/length(p_values_alternative_both_correct)
power <- 1-typeII

# [1] 1

# write the result to the table
backdoor_values = c(backdoor_values, TRUE)
frontdoor_values = c(frontdoor_values, TRUE)
backdoor_true_functional = c(backdoor_true_functional, 1)
frontdoor_true_functional = c(frontdoor_true_functional, 1)
b1 = c(b1, TRUE)
b2 = c(b2, TRUE)
f1 = c(f1, TRUE)
f2 = c(f2, TRUE) 
f3 = c(f3, TRUE) 
hypothesis = c(hypothesis, "A")
beta = c(beta, 10)
size_values = c(size_values, NA)
power_values = c(power_values, power)



### c) one of phi_k = 0, one of the model is correct 
# backdoor is correct, front door is wrong
p_values_alternative_bdoor_correct_f1_violated <- foreach(i = 1:N, .combine = c) %dopar% {
  
  data <- dgp_backdoor_frontdoor_bdoor_correct_f1_violated(n = n, beta = 10)
  
  # estimate using AIPW (backdoor IF)
  backdoor <- estimate_backdooor(data)
  backdoor.est <- backdoor$backdoor.est
  backdoor.eif <- backdoor$backdoor.eif
  
  # estimate using APIPW (front door IF)
  frontdoor <- estimate_frontdoor(data)
  frontdoor.est <- frontdoor$frontdoor.est
  frontdoor.eif <- frontdoor$frontdoor.eif
  
  # Evidence factor
  est <- c(backdoor.est, frontdoor.est)
  eif <- cbind(backdoor.eif, frontdoor.eif)
  evidence_factor(est = est, eif = eif)
  
}

typeII <- sum(p_values_alternative_bdoor_correct_f1_violated > 0.05)/length(p_values_alternative_bdoor_correct_f1_violated)
power <- 1-typeII

# [1] 0.07


# write the result to the table
backdoor_values = c(backdoor_values, TRUE)
frontdoor_values = c(frontdoor_values, FALSE)
backdoor_true_functional = c(backdoor_true_functional, 1)
frontdoor_true_functional = c(frontdoor_true_functional, 0)
b1 = c(b1, TRUE)
b2 = c(b2, TRUE)
f1 = c(f1, FALSE)
f2 = c(f2, TRUE) 
f3 = c(f3, TRUE) 
hypothesis = c(hypothesis, "A")
beta = c(beta, 10)
size_values = c(size_values, NA)
power_values = c(power_values, power)


# front-door is correct, backdoor is wrong
p_values_alternative_fdoor_correct_b1_violated_alt_zero <- foreach(i = 1:N, .combine = c) %dopar% {
  
  data <- dgp_backdoor_frontdoor_fdoor_correct_b1_violated_alt(n = n, beta = 3)
  
  # estimate using AIPW (backdoor IF)
  backdoor <- estimate_backdooor(data)
  backdoor.est <- backdoor$backdoor.est
  backdoor.eif <- backdoor$backdoor.eif
  
  # estimate using APIPW (front door IF)
  frontdoor <- estimate_frontdoor(data)
  frontdoor.est <- frontdoor$frontdoor.est
  frontdoor.eif <- frontdoor$frontdoor.eif
  
  # Evidence factor
  est <- c(backdoor.est, frontdoor.est)
  eif <- cbind(backdoor.eif, frontdoor.eif)
  evidence_factor(est = est, eif = eif)
  
}

typeII <- sum(p_values_alternative_fdoor_correct_b1_violated_alt_zero > 0.05)/length(p_values_alternative_fdoor_correct_b1_violated_alt_zero)
power <- 1-typeII

# write the result to the table
backdoor_values = c(backdoor_values, FALSE)
frontdoor_values = c(frontdoor_values, TRUE)
backdoor_true_functional = c(backdoor_true_functional, 0)
frontdoor_true_functional = c(frontdoor_true_functional, 1)
b1 = c(b1, FALSE)
b2 = c(b2, TRUE)
f1 = c(f1, TRUE)
f2 = c(f2, TRUE) 
f3 = c(f3, TRUE) 
hypothesis = c(hypothesis, "A")
beta = c(beta, 3)
size_values = c(size_values, NA)
power_values = c(power_values, power)
