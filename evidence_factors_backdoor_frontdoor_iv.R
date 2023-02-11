
library(parallel)
if(!requireNamespace("foreach")) install.packages("foreach", repos = "https://cloud.r-project.org")
library(foreach)
if(!requireNamespace("doParallel")) install.packages("doParallel", repos = "https://cloud.r-project.org")
library(doParallel)
num_core <- 20
doParallel::registerDoParallel(cores = num_core)

######################### Backdoor, Front-door, and IV #########################

backdoor_values = c()
frontdoor_values = c()
iv_values = c() 
backdoor_true_functional = c()
frontdoor_true_functional = c()
iv_true_functional = c() 

b1 = c()
b2 = c()
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
### a) all models are correct
p_values_null_all_correct <- foreach(i = 1:N, .combine = c) %dopar% {
  df <- dgp_backdoor_frontdoor_iv_all_correct(n = n, beta = 0)
  data <- df$df
  
  # estimate using AIPW (backdoor IF)
  backdoor <- estimate_backdooor(data)
  backdoor.est <- backdoor$backdoor.est
  backdoor.eif <- backdoor$backdoor.eif
  
  # estimate using APIPW (front door IF)
  frontdoor <- estimate_frontdoor(data)
  frontdoor.est <- frontdoor$frontdoor.est
  frontdoor.eif <- frontdoor$frontdoor.eif
  
  # estimate using UIV (IV IF)
  iv <- estimate_uiv(data)
  iv.est <- iv$iv.est
  iv.eif <- iv$iv.eif
  
  # Evidence factor
  est <- c(backdoor.est, frontdoor.est, iv.est)
  eif <- cbind(backdoor.eif, frontdoor.eif, iv.eif)
  evidence_factor(est = est, eif = eif)
}

typeI <- sum(p_values_null_all_correct <= 0.05)/length(p_values_null_all_correct)
power <- typeI
size <- power


# write the result to the table
backdoor_values = c(backdoor_values, TRUE)
frontdoor_values = c(frontdoor_values, TRUE)
iv_values = c(iv_values, TRUE) 
backdoor_true_functional = c(backdoor_true_functional, 0)
frontdoor_true_functional = c(frontdoor_true_functional, 0)
iv_true_functional = c(iv_true_functional, 0) 
b1 = c(b1, TRUE)
b2 = c(b2, TRUE)
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



### b) two of the models are correct
# b.1) backdoor and iv are true, front-door is not 
# f3 violated, there is a backdoor path from M to Y,
# so when beta = 0, *the identified front-door functional is not zero*
p_values_null_backdoor_iv_correct_f3_violated <- foreach(i = 1:N, .combine = c) %dopar% {
  
  df <- dgp_backdoor_frontdoor_iv_f3_violated(n = n, beta = 0)
  data <- df$df
  
  # estimate using AIPW (backdoor IF)
  backdoor <- estimate_backdooor(data)
  backdoor.est <- backdoor$backdoor.est
  backdoor.eif <- backdoor$backdoor.eif
  
  # estimate using APIPW (front door IF)
  frontdoor <- estimate_frontdoor(data)
  frontdoor.est <- frontdoor$frontdoor.est
  frontdoor.eif <- frontdoor$frontdoor.eif
  
  # estimate using UIV (IV IF)
  iv <- estimate_uiv(data)
  iv.est <- iv$iv.est
  iv.eif <- iv$iv.eif
  
  # Evidence factor
  est <- c(backdoor.est, frontdoor.est, iv.est)
  eif <- cbind(backdoor.eif, frontdoor.eif, iv.eif)
  evidence_factor(est = est, eif = eif)
  
}

typeI <- sum(p_values_null_backdoor_iv_correct_f3_violated <= 0.05)/length(p_values_null_backdoor_iv_correct_f3_violated)
power <- typeI
size <- power



# write the result to the table
backdoor_values = c(backdoor_values, TRUE)
frontdoor_values = c(frontdoor_values, FALSE)
iv_values = c(iv_values, TRUE) 
backdoor_true_functional = c(backdoor_true_functional, 0)
frontdoor_true_functional = c(frontdoor_true_functional, 1)
iv_true_functional = c(iv_true_functional, 0) 
b1 = c(b1, TRUE)
b2 = c(b2, TRUE)
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



# b.2) backdoor and front-door are true, iv is not 
# Z cannot have a direct effect to Y because the backdoor has to be true, and 
# the front door is true (M fully mediate and beta = 0),  *so the identified iv functional is zero*
p_values_null_backdoor_frontdoor_correct_i3_violated <- foreach(i = 1:N, .combine = c) %dopar% {
  
  df <- dgp_backdoor_frontdoor_iv_i3_violated(n = n, beta = 0)
  data <- df$df
  
  # estimate using AIPW (backdoor IF)
  backdoor <- estimate_backdooor(data)
  backdoor.est <- backdoor$backdoor.est
  backdoor.eif <- backdoor$backdoor.eif
  
  # estimate using APIPW (front door IF)
  frontdoor <- estimate_frontdoor(data)
  frontdoor.est <- frontdoor$frontdoor.est
  frontdoor.eif <- frontdoor$frontdoor.eif
  
  # estimate using UIV (IV IF)
  iv <- estimate_uiv(data)
  iv.est <- iv$iv.est
  iv.eif <- iv$iv.eif
  
  # Evidence factor
  est <- c(backdoor.est, frontdoor.est, iv.est)
  eif <- cbind(backdoor.eif, frontdoor.eif, iv.eif)
  evidence_factor(est = est, eif = eif)

}

typeI <- sum(p_values_null_backdoor_frontdoor_correct_i3_violated <= 0.05)/length(p_values_null_backdoor_frontdoor_correct_i3_violated)
power <- typeI
size <- power


# write the result to the table
backdoor_values = c(backdoor_values, TRUE)
frontdoor_values = c(frontdoor_values, TRUE)
iv_values = c(iv_values, FALSE) 
backdoor_true_functional = c(backdoor_true_functional, 0)
frontdoor_true_functional = c(frontdoor_true_functional, 0)
iv_true_functional = c(iv_true_functional, 0) 
b1 = c(b1, TRUE)
b2 = c(b2, TRUE)
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



# b.3) front door and iv are true, backdoor is not
# b1 is violated (there is a backdoor path from A to Y), 
# so when beta = 0, *the identified backdoor functional is not zero*
p_values_null_fdoor_iv_correct_b1_violated <- foreach(i = 1:N, .combine = c) %dopar% {
  
  df <- dgp_backdoor_frontdoor_iv_b1_violated(n = n, beta = 0)
  data <- df$df
  
  # estimate using AIPW (backdoor IF)
  backdoor <- estimate_backdooor(data)
  backdoor.est <- backdoor$backdoor.est
  backdoor.eif <- backdoor$backdoor.eif
  
  # estimate using APIPW (front door IF)
  frontdoor <- estimate_frontdoor(data)
  frontdoor.est <- frontdoor$frontdoor.est
  frontdoor.eif <- frontdoor$frontdoor.eif
  
  # estimate using UIV (IV IF)
  iv <- estimate_uiv(data)
  iv.est <- iv$iv.est
  iv.eif <- iv$iv.eif
  
  # Evidence factor
  est <- c(backdoor.est, frontdoor.est, iv.est)
  eif <- cbind(backdoor.eif, frontdoor.eif, iv.eif)
  evidence_factor(est = est, eif = eif)

}

typeI <- sum(p_values_null_fdoor_iv_correct_b1_violated <= 0.05)/length(p_values_null_fdoor_iv_correct_b1_violated)
power <- typeI
size <- power


# write the result to the table
backdoor_values = c(backdoor_values, FALSE)
frontdoor_values = c(frontdoor_values, TRUE)
iv_values = c(iv_values, TRUE) 
backdoor_true_functional = c(backdoor_true_functional, 1)
frontdoor_true_functional = c(frontdoor_true_functional, 0)
iv_true_functional = c(iv_true_functional, 0) 
b1 = c(b1, FALSE)
b2 = c(b2, TRUE)
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



### c) one of the models is correct
# c.1) front door is correct, backdoor and iv are wrong 
# b1 and i1 violated (there is backdoor paths from A to Y and from Z to Y), 
# when the effect through M is zero (beta = 0), *the identified iv and backdoor are not zero*
p_values_null_fdoor_correct_b1i1_violated <- foreach(i = 1:N, .combine = c) %dopar% {
  
  df <- dgp_backdoor_frontdoor_iv_fdoor_correct_b1i1_violated(n = n, beta = 0)
  data <- df$df
  
  # estimate using AIPW (backdoor IF)
  backdoor <- estimate_backdooor(data)
  backdoor.est <- backdoor$backdoor.est
  backdoor.eif <- backdoor$backdoor.eif
  
  # estimate using APIPW (front door IF)
  frontdoor <- estimate_frontdoor(data)
  frontdoor.est <- frontdoor$frontdoor.est
  frontdoor.eif <- frontdoor$frontdoor.eif
  
  # estimate using UIV (IV IF)
  iv <- estimate_uiv(data)
  iv.est <- iv$iv.est
  iv.eif <- iv$iv.eif
  
  # Evidence factor
  est <- c(backdoor.est, frontdoor.est, iv.est)
  eif <- cbind(backdoor.eif, frontdoor.eif, iv.eif)
  evidence_factor(est = est, eif = eif)

}

typeI <- sum(p_values_null_fdoor_correct_b1i1_violated <= 0.05)/length(p_values_null_fdoor_correct_b1i1_violated)
power <- typeI
size <- power


# write the result to the table
backdoor_values = c(backdoor_values, FALSE)
frontdoor_values = c(frontdoor_values, TRUE)
iv_values = c(iv_values, FALSE) 
backdoor_true_functional = c(backdoor_true_functional, 1)
frontdoor_true_functional = c(frontdoor_true_functional, 0)
iv_true_functional = c(iv_true_functional, 1) 
b1 = c(b1, FALSE)
b2 = c(b2, TRUE)
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


# c.2) iv is correct, backdoor and front door are wrong 
# b1, f2, and f3 violated, there are backdoor paths from A to Y, from A to M, and from M to Y
# when beta = 0, *the identified backdoor and frontdoor functionals are not zero*
p_values_null_iv_correct_b1f2f3_violated <- foreach(i = 1:N, .combine = c) %dopar% {
  
  df <- dgp_backdoor_frontdoor_iv_iv_correct_b1f2f3_violated(n = n, beta = 0)
  data <- df$df
  
  # estimate using AIPW (backdoor IF)
  backdoor <- estimate_backdooor(data)
  backdoor.est <- backdoor$backdoor.est
  backdoor.eif <- backdoor$backdoor.eif
  
  # estimate using APIPW (front door IF)
  frontdoor <- estimate_frontdoor(data)
  frontdoor.est <- frontdoor$frontdoor.est
  frontdoor.eif <- frontdoor$frontdoor.eif
  
  # estimate using UIV (IV IF)
  iv <- estimate_uiv(data)
  iv.est <- iv$iv.est
  iv.eif <- iv$iv.eif
  
  # Evidence factor
  est <- c(backdoor.est, frontdoor.est, iv.est)
  eif <- cbind(backdoor.eif, frontdoor.eif, iv.eif)
  evidence_factor(est = est, eif = eif)

}

typeI <- sum(p_values_null_iv_correct_b1f2f3_violated <= 0.05)/length(p_values_null_iv_correct_b1f2f3_violated)
power <- typeI
size <- power


# write the result to the table
backdoor_values = c(backdoor_values, FALSE)
frontdoor_values = c(frontdoor_values, FALSE)
iv_values = c(iv_values, TRUE) 
backdoor_true_functional = c(backdoor_true_functional, 1)
frontdoor_true_functional = c(frontdoor_true_functional, 1)
iv_true_functional = c(iv_true_functional, 0) 
b1 = c(b1, FALSE)
b2 = c(b2, TRUE)
f1 = c(f1, TRUE)
f2 = c(f2, FALSE) 
f3 = c(f3, FALSE) 
i1 = c(i1, TRUE) 
i2 = c(i2, TRUE) 
i3 = c(i3, TRUE) 
i4 = c(i4, TRUE) 
hypothesis = c(hypothesis, "N")
beta = c(beta, 0)
size_values = c(size_values, size)
power_values = c(power_values, NA)



# c.3) backdoor is correct, frontdoor and iv are wrong
# Z cannot have a direct effect to Y because the backdoor has to be true, 
# f3 violated, there is a backdoor path from M to Y, so when beta = 0, 
# *the identified frontdoor is not zero and iv is zero*
p_values_null_bdoor_correct_f3i3_violated <- foreach(i = 1:N, .combine = c) %dopar% {
  
  df <- dgp_backdoor_frontdoor_iv_bdoor_correct_f3i3_violated(n = n, beta = 0)
  data <- df$df
  
  # estimate using AIPW (backdoor IF)
  backdoor <- estimate_backdooor(data)
  backdoor.est <- backdoor$backdoor.est
  backdoor.eif <- backdoor$backdoor.eif
  
  # estimate using APIPW (front door IF)
  frontdoor <- estimate_frontdoor(data)
  frontdoor.est <- frontdoor$frontdoor.est
  frontdoor.eif <- frontdoor$frontdoor.eif
  
  # estimate using UIV (IV IF)
  iv <- estimate_uiv(data)
  iv.est <- iv$iv.est
  iv.eif <- iv$iv.eif
  
  # Evidence factor
  est <- c(backdoor.est, frontdoor.est, iv.est)
  eif <- cbind(backdoor.eif, frontdoor.eif, iv.eif)
  evidence_factor(est = est, eif = eif)
  
}

typeI <- sum(p_values_null_bdoor_correct_f3i3_violated <= 0.05)/length(p_values_null_bdoor_correct_f3i3_violated)
power <- typeI
size <- power


# write the result to the table
backdoor_values = c(backdoor_values, TRUE)
frontdoor_values = c(frontdoor_values, FALSE)
iv_values = c(iv_values, FALSE) 
backdoor_true_functional = c(backdoor_true_functional, 0)
frontdoor_true_functional = c(frontdoor_true_functional, 1)
iv_true_functional = c(iv_true_functional, 0) 
b1 = c(b1, TRUE)
b2 = c(b2, TRUE)
f1 = c(f1, TRUE)
f2 = c(f2, TRUE) 
f3 = c(f3, FALSE) 
i1 = c(i1, TRUE) 
i2 = c(i2, TRUE) 
i3 = c(i3, FALSE) 
i4 = c(i4, TRUE) 
hypothesis = c(hypothesis, "N")
beta = c(beta, 0)
size_values = c(size_values, size)
power_values = c(power_values, NA)


####################### 2. Under Alternative Hypothesis ########################
### a) all phi_k != 0, one of the models is correct 
# a.1) front door is correct, backdoor and iv are wrong 
# b1 and i1 are violated
p_values_alternative_fdoor_correct_b1i1_violated <- foreach(i = 1:N, .combine = c) %dopar% {
  
  df <- dgp_backdoor_frontdoor_iv_fdoor_correct_b1i1_violated(n = n, beta = 10)
  data <- df$df
  
  # estimate using AIPW (backdoor IF)
  backdoor <- estimate_backdooor(data)
  backdoor.est <- backdoor$backdoor.est
  backdoor.eif <- backdoor$backdoor.eif
  
  # estimate using APIPW (front door IF)
  frontdoor <- estimate_frontdoor(data)
  frontdoor.est <- frontdoor$frontdoor.est
  frontdoor.eif <- frontdoor$frontdoor.eif
  
  # estimate using UIV (IV IF)
  iv <- estimate_uiv(data)
  iv.est <- iv$iv.est
  iv.eif <- iv$iv.eif
  
  # Evidence factor
  est <- c(backdoor.est, frontdoor.est, iv.est)
  eif <- cbind(backdoor.eif, frontdoor.eif, iv.eif)
  evidence_factor(est = est, eif = eif)
  
}

typeII <- sum(p_values_alternative_fdoor_correct_b1i1_violated > 0.05)/length(p_values_alternative_fdoor_correct_b1i1_violated)
power <- 1-typeII

# [1] 1

# write the result to the table
backdoor_values = c(backdoor_values, FALSE)
frontdoor_values = c(frontdoor_values, TRUE)
iv_values = c(iv_values, FALSE) 
backdoor_true_functional = c(backdoor_true_functional, 1)
frontdoor_true_functional = c(frontdoor_true_functional, 1)
iv_true_functional = c(iv_true_functional, 1) 
b1 = c(b1, FALSE)
b2 = c(b2, TRUE)
f1 = c(f1, TRUE)
f2 = c(f2, TRUE) 
f3 = c(f3, TRUE) 
i1 = c(i1, FALSE) 
i2 = c(i2, TRUE) 
i3 = c(i3, TRUE) 
i4 = c(i4, TRUE) 
hypothesis = c(hypothesis, "A")
beta = c(beta, 10)
size_values = c(size_values, NA)
power_values = c(power_values, power)



# b1 and i2 are violated
p_values_alternative_fdoor_correct_b1i2_violated <- foreach(i = 1:N, .combine = c) %dopar% {
  
  df <- dgp_backdoor_frontdoor_iv_fdoor_correct_b1i2_violated(n = n, beta = 10)
  data <- df$df
  
  # estimate using AIPW (backdoor IF)
  backdoor <- estimate_backdooor(data)
  backdoor.est <- backdoor$backdoor.est
  backdoor.eif <- backdoor$backdoor.eif
  
  # estimate using APIPW (front door IF)
  frontdoor <- estimate_frontdoor(data)
  frontdoor.est <- frontdoor$frontdoor.est
  frontdoor.eif <- frontdoor$frontdoor.eif
  
  # estimate using UIV (IV IF)
  iv <- estimate_uiv(data)
  iv.est <- iv$iv.est
  iv.eif <- iv$iv.eif
  
  # Evidence factor
  est <- c(backdoor.est, frontdoor.est, iv.est)
  eif <- cbind(backdoor.eif, frontdoor.eif, iv.eif)
  evidence_factor(est = est, eif = eif)
  
}

typeII <- sum(p_values_alternative_fdoor_correct_b1i2_violated > 0.05)/length(p_values_alternative_fdoor_correct_b1i2_violated)
power <- 1-typeII

# [1] 1


# write the result to the table
backdoor_values = c(backdoor_values, FALSE)
frontdoor_values = c(frontdoor_values, TRUE)
iv_values = c(iv_values, FALSE) 
backdoor_true_functional = c(backdoor_true_functional, 1)
frontdoor_true_functional = c(frontdoor_true_functional, 1)
iv_true_functional = c(iv_true_functional, 1) 
b1 = c(b1, FALSE)
b2 = c(b2, TRUE)
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


# a.2) iv is correct, backdoor and front door are wrong 
p_values_alternative_iv_correct_b1f2f3_violated <- foreach(i = 1:N, .combine = c) %dopar% {
  
  df <- dgp_backdoor_frontdoor_iv_iv_correct_b1f2f3_violated(n = n, beta = 10)
  data <- df$df
  
  # estimate using AIPW (backdoor IF)
  backdoor <- estimate_backdooor(data)
  backdoor.est <- backdoor$backdoor.est
  backdoor.eif <- backdoor$backdoor.eif
  
  # estimate using APIPW (front door IF)
  frontdoor <- estimate_frontdoor(data)
  frontdoor.est <- frontdoor$frontdoor.est
  frontdoor.eif <- frontdoor$frontdoor.eif
  
  # estimate using UIV (IV IF)
  iv <- estimate_uiv(data)
  iv.est <- iv$iv.est
  iv.eif <- iv$iv.eif
  
  # Evidence factor
  est <- c(backdoor.est, frontdoor.est, iv.est)
  eif <- cbind(backdoor.eif, frontdoor.eif, iv.eif)
  evidence_factor(est = est, eif = eif)
  
}

typeII <- sum(p_values_alternative_iv_correct_b1f2f3_violated > 0.05)/length(p_values_alternative_iv_correct_b1f2f3_violated)
power <- 1-typeII

# [1] 1


# write the result to the table
backdoor_values = c(backdoor_values, FALSE)
frontdoor_values = c(frontdoor_values, FALSE)
iv_values = c(iv_values, TRUE) 
backdoor_true_functional = c(backdoor_true_functional, 1)
frontdoor_true_functional = c(frontdoor_true_functional, 1)
iv_true_functional = c(iv_true_functional, 1) 
b1 = c(b1, FALSE)
b2 = c(b2, TRUE)
f1 = c(f1, TRUE)
f2 = c(f2, FALSE) 
f3 = c(f3, FALSE) 
i1 = c(i1, TRUE) 
i2 = c(i2, TRUE) 
i3 = c(i3, TRUE) 
i4 = c(i4, TRUE) 
hypothesis = c(hypothesis, "A")
beta = c(beta, 10)
size_values = c(size_values, NA)
power_values = c(power_values, power)



### b) all phi_k != 0, two of the models are correct 
# b.1) backdoor and iv are correct, front door is wrong
p_values_alternative_bdoor_iv_correct_f3_violated <- foreach(i = 1:N, .combine = c) %dopar% {
  
  df <- dgp_backdoor_frontdoor_iv_f3_violated(n = n, beta = 10)
  data <- df$df
  
  # estimate using AIPW (backdoor IF)
  backdoor <- estimate_backdooor(data)
  backdoor.est <- backdoor$backdoor.est
  backdoor.eif <- backdoor$backdoor.eif
  
  # estimate using APIPW (front door IF)
  frontdoor <- estimate_frontdoor(data)
  frontdoor.est <- frontdoor$frontdoor.est
  frontdoor.eif <- frontdoor$frontdoor.eif
  
  # estimate using UIV (IV IF)
  iv <- estimate_uiv(data)
  iv.est <- iv$iv.est
  iv.eif <- iv$iv.eif
  
  # Evidence factor
  est <- c(backdoor.est, frontdoor.est, iv.est)
  eif <- cbind(backdoor.eif, frontdoor.eif, iv.eif)
  evidence_factor(est = est, eif = eif)
  
}

typeII <- sum(p_values_alternative_bdoor_iv_correct_f3_violated > 0.05)/length(p_values_alternative_bdoor_iv_correct_f3_violated)
power <- 1-typeII

# [1] 1

# write the result to the table
backdoor_values = c(backdoor_values, TRUE)
frontdoor_values = c(frontdoor_values, FALSE)
iv_values = c(iv_values, TRUE) 
backdoor_true_functional = c(backdoor_true_functional, 1)
frontdoor_true_functional = c(frontdoor_true_functional, 1)
iv_true_functional = c(iv_true_functional, 1) 
b1 = c(b1, TRUE)
b2 = c(b2, TRUE)
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



# b.2) backdoor and front door are correct, iv is wrong
p_values_alternative_bdoor_fdoor_correct_i3_violated <- foreach(i = 1:N, .combine = c) %dopar% {
  
  df <- dgp_backdoor_frontdoor_iv_i3_violated(n = n, beta = 10)
  data <- df$df
  
  # estimate using AIPW (backdoor IF)
  backdoor <- estimate_backdooor(data)
  backdoor.est <- backdoor$backdoor.est
  backdoor.eif <- backdoor$backdoor.eif
  
  # estimate using APIPW (front door IF)
  frontdoor <- estimate_frontdoor(data)
  frontdoor.est <- frontdoor$frontdoor.est
  frontdoor.eif <- frontdoor$frontdoor.eif
  
  # estimate using UIV (IV IF)
  iv <- estimate_uiv(data)
  iv.est <- iv$iv.est
  iv.eif <- iv$iv.eif
  
  # Evidence factor
  est <- c(backdoor.est, frontdoor.est, iv.est)
  eif <- cbind(backdoor.eif, frontdoor.eif, iv.eif)
  evidence_factor(est = est, eif = eif)
  
}

typeII <- sum(p_values_alternative_bdoor_fdoor_correct_i3_violated > 0.05)/length(p_values_alternative_bdoor_fdoor_correct_i3_violated)
power <- 1-typeII

# [1] 1

# write the result to the table
backdoor_values = c(backdoor_values, TRUE)
frontdoor_values = c(frontdoor_values, TRUE)
iv_values = c(iv_values, FALSE) 
backdoor_true_functional = c(backdoor_true_functional, 1)
frontdoor_true_functional = c(frontdoor_true_functional, 1)
iv_true_functional = c(iv_true_functional, 1) 
b1 = c(b1, TRUE)
b2 = c(b2, TRUE)
f1 = c(f1, TRUE)
f2 = c(f2, TRUE) 
f3 = c(f3, TRUE) 
i1 = c(i1, TRUE) 
i2 = c(i2, TRUE) 
i3 = c(i3, FALSE) 
i4 = c(i4, TRUE) 
hypothesis = c(hypothesis, "A")
beta = c(beta, 10)
size_values = c(size_values, NA)
power_values = c(power_values, power)



# b.3) front door and iv are correct, backdoor is wrong
p_values_alternative_fdoor_iv_correct_b1_violated <- foreach(i = 1:N, .combine = c) %dopar% {
  
  df <- dgp_backdoor_frontdoor_iv_b1_violated(n = n, beta = 10)
  data <- df$df
  
  # estimate using AIPW (backdoor IF)
  backdoor <- estimate_backdooor(data)
  backdoor.est <- backdoor$backdoor.est
  backdoor.eif <- backdoor$backdoor.eif
  
  # estimate using APIPW (front door IF)
  frontdoor <- estimate_frontdoor(data)
  frontdoor.est <- frontdoor$frontdoor.est
  frontdoor.eif <- frontdoor$frontdoor.eif
  
  # estimate using UIV (IV IF)
  iv <- estimate_uiv(data)
  iv.est <- iv$iv.est
  iv.eif <- iv$iv.eif
  
  # Evidence factor
  est <- c(backdoor.est, frontdoor.est, iv.est)
  eif <- cbind(backdoor.eif, frontdoor.eif, iv.eif)
  evidence_factor(est = est, eif = eif)
  
}

typeII <- sum(p_values_alternative_fdoor_iv_correct_b1_violated > 0.05)/length(p_values_alternative_fdoor_iv_correct_b1_violated)
power <- 1-typeII

# [1] 1

# write the result to the table
backdoor_values = c(backdoor_values, FALSE)
frontdoor_values = c(frontdoor_values, TRUE)
iv_values = c(iv_values, TRUE) 
backdoor_true_functional = c(backdoor_true_functional, 1)
frontdoor_true_functional = c(frontdoor_true_functional, 1)
iv_true_functional = c(iv_true_functional, 1) 
b1 = c(b1, FALSE)
b2 = c(b2, TRUE)
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


### c) all phi_k != 0, all of the models are correct
p_values_alternative_all_correct <- foreach(i = 1:N, .combine = c) %dopar% {
  
  df <- dgp_backdoor_frontdoor_iv_all_correct(n = n, beta = 10)
  data <- df$df
  
  # estimate using AIPW (backdoor IF)
  backdoor <- estimate_backdooor(data)
  backdoor.est <- backdoor$backdoor.est
  backdoor.eif <- backdoor$backdoor.eif
  
  # estimate using APIPW (front door IF)
  frontdoor <- estimate_frontdoor(data)
  frontdoor.est <- frontdoor$frontdoor.est
  frontdoor.eif <- frontdoor$frontdoor.eif
  
  # estimate using UIV (IV IF)
  iv <- estimate_uiv(data)
  iv.est <- iv$iv.est
  iv.eif <- iv$iv.eif
  
  # Evidence factor
  est <- c(backdoor.est, frontdoor.est, iv.est)
  eif <- cbind(backdoor.eif, frontdoor.eif, iv.eif)
  evidence_factor(est = est, eif = eif)
  
}

typeII <- sum(p_values_alternative_all_correct > 0.05)/length(p_values_alternative_all_correct)
power <- 1-typeII

# [1] 1

# write the result to the table
backdoor_values = c(backdoor_values, TRUE)
frontdoor_values = c(frontdoor_values, TRUE)
iv_values = c(iv_values, TRUE) 
backdoor_true_functional = c(backdoor_true_functional, 1)
frontdoor_true_functional = c(frontdoor_true_functional, 1)
iv_true_functional = c(iv_true_functional, 1) 
b1 = c(b1, TRUE)
b2 = c(b2, TRUE)
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




### d) one of phi_k = 0, one of the model is correct 
# iv is correct, backdoor and front door are wrong 
# (the identified frontdoor functional is zero)
p_values_alternative_iv_correct_b2f1f2_violated <- foreach(i = 1:N, .combine = c) %dopar% {
  
  df <- dgp_backdoor_frontdoor_iv_iv_correct_b2f1f2_violated(n = n, beta = 10)
  data <- df$df
  
  # estimate using AIPW (backdoor IF)
  backdoor <- estimate_backdooor(data)
  backdoor.est <- backdoor$backdoor.est
  backdoor.eif <- backdoor$backdoor.eif
  
  # estimate using APIPW (front door IF)
  frontdoor <- estimate_frontdoor(data)
  frontdoor.est <- frontdoor$frontdoor.est
  frontdoor.eif <- frontdoor$frontdoor.eif
  
  # estimate using UIV (IV IF)
  iv <- estimate_uiv(data)
  iv.est <- iv$iv.est
  iv.eif <- iv$iv.eif
  
  # Evidence factor
  est <- c(backdoor.est, frontdoor.est, iv.est)
  eif <- cbind(backdoor.eif, frontdoor.eif, iv.eif)
  evidence_factor(est = est, eif = eif)
  
}

typeII <- sum(p_values_alternative_iv_correct_b2f1f2_violated > 0.05)/length(p_values_alternative_iv_correct_b2f1f2_violated)
power <- 1-typeII

# [1] 0.124


# write the result to the table
backdoor_values = c(backdoor_values, FALSE)
frontdoor_values = c(frontdoor_values, FALSE)
iv_values = c(iv_values, TRUE) 
backdoor_true_functional = c(backdoor_true_functional, 1)
frontdoor_true_functional = c(frontdoor_true_functional, 0)
iv_true_functional = c(iv_true_functional, 1) 
b1 = c(b1, TRUE)
b2 = c(b2, FALSE)
f1 = c(f1, FALSE)
f2 = c(f2, FALSE) 
f3 = c(f3, TRUE) 
i1 = c(i1, TRUE) 
i2 = c(i2, TRUE) 
i3 = c(i3, TRUE) 
i4 = c(i4, TRUE) 
hypothesis = c(hypothesis, "A")
beta = c(beta, 10)
size_values = c(size_values, NA)
power_values = c(power_values, power)

# f1 and i3 are violated (the identified frontdoor functional is zero)
p_values_alternative_bdoor_correct_f1i3_violated <- foreach(i = 1:N, .combine = c) %dopar% {
  
  df <- dgp_backdoor_frontdoor_iv_bdoor_correct_f1i3_violated(n = n, beta = 10)
  data <- df$df
  
  # estimate using AIPW (backdoor IF)
  backdoor <- estimate_backdooor(data)
  backdoor.est <- backdoor$backdoor.est
  backdoor.eif <- backdoor$backdoor.eif
  
  # estimate using APIPW (front door IF)
  frontdoor <- estimate_frontdoor(data)
  frontdoor.est <- frontdoor$frontdoor.est
  frontdoor.eif <- frontdoor$frontdoor.eif
  
  # estimate using UIV (IV IF)
  iv <- estimate_uiv(data)
  iv.est <- iv$iv.est
  iv.eif <- iv$iv.eif
  
  # Evidence factor
  est <- c(backdoor.est, frontdoor.est, iv.est)
  eif <- cbind(backdoor.eif, frontdoor.eif, iv.eif)
  evidence_factor(est = est, eif = eif)
  
}

typeII <- sum(p_values_alternative_bdoor_correct_f1i3_violated > 0.05)/length(p_values_alternative_bdoor_correct_f1i3_violated)
power <- 1-typeII


# write the result to the table
backdoor_values = c(backdoor_values, TRUE)
frontdoor_values = c(frontdoor_values, FALSE)
iv_values = c(iv_values, FALSE) 
backdoor_true_functional = c(backdoor_true_functional, 1)
frontdoor_true_functional = c(frontdoor_true_functional, 0)
iv_true_functional = c(iv_true_functional, 1) 
b1 = c(b1, TRUE)
b2 = c(b2, TRUE)
f1 = c(f1, FALSE)
f2 = c(f2, TRUE) 
f3 = c(f3, TRUE) 
i1 = c(i1, TRUE) 
i2 = c(i2, TRUE) 
i3 = c(i3, FALSE) 
i4 = c(i4, TRUE) 
hypothesis = c(hypothesis, "A")
beta = c(beta, 10)
size_values = c(size_values, NA)
power_values = c(power_values, power)

# backdoor is correct, front door and iv are wrong
# f3 and i3 are violated
p_values_alternative_bdoor_correct_f3i3_violated <- foreach(i = 1:N, .combine = c) %dopar% {
  
  df <- dgp_backdoor_frontdoor_iv_bdoor_correct_f3i3_violated(n = n, beta = 10)
  data <- df$df
  
  # estimate using AIPW (backdoor IF)
  backdoor <- estimate_backdooor(data)
  backdoor.est <- backdoor$backdoor.est
  backdoor.eif <- backdoor$backdoor.eif
  
  # estimate using APIPW (front door IF)
  frontdoor <- estimate_frontdoor(data)
  frontdoor.est <- frontdoor$frontdoor.est
  frontdoor.eif <- frontdoor$frontdoor.eif
  
  # estimate using UIV (IV IF)
  iv <- estimate_uiv(data)
  iv.est <- iv$iv.est
  iv.eif <- iv$iv.eif
  
  # Evidence factor
  est <- c(backdoor.est, frontdoor.est, iv.est)
  eif <- cbind(backdoor.eif, frontdoor.eif, iv.eif)
  evidence_factor(est = est, eif = eif)
  
}

typeII <- sum(p_values_alternative_bdoor_correct_f3i3_violated > 0.05)/length(p_values_alternative_bdoor_correct_f3i3_violated)
power <- 1-typeII

# write the result to the table
backdoor_values = c(backdoor_values, TRUE)
frontdoor_values = c(frontdoor_values, FALSE)
iv_values = c(iv_values, FALSE) 
backdoor_true_functional = c(backdoor_true_functional, 1)
frontdoor_true_functional = c(frontdoor_true_functional, 1)
iv_true_functional = c(iv_true_functional, 0) 
b1 = c(b1, TRUE)
b2 = c(b2, TRUE)
f1 = c(f1, TRUE)
f2 = c(f2, TRUE) 
f3 = c(f3, FALSE) 
i1 = c(i1, TRUE) 
i2 = c(i2, TRUE) 
i3 = c(i3, FALSE) 
i4 = c(i4, TRUE) 
hypothesis = c(hypothesis, "A")
beta = c(beta, 10)
size_values = c(size_values, NA)
power_values = c(power_values, power)


