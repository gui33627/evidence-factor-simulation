
library(parallel)
# if(!requireNamespace("foreach")) install.packages("foreach", repos = "https://cloud.r-project.org")
library(foreach)
# if(!requireNamespace("doParallel")) install.packages("doParallel", repos = "https://cloud.r-project.org")
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
##########################  Under the Null ############################
### 1. all models are correct
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



# 2. iv is correct, backdoor and front door are wrong 
# the identified backdoor functional is zero
# b1, f2, and f3 violated, there are backdoor paths from A to Y, from A to M, and from M to Y
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
backdoor_true_functional = c(backdoor_true_functional, 0)
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


# 3. front-door is correct, backdoor and IV are wrong
# the identified iv and backdoor functionals are zero
p_values_null_fdoor_correct_b1i3_violated <- foreach(i = 1:N, .combine = c) %dopar% {
  
  df <- dgp_backdoor_frontdoor_iv_fdoor_correct_b1i3_violated(n = n, beta = 0)
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

typeI <- sum(p_values_null_fdoor_correct_b1i3_violated <= 0.05)/length(p_values_null_fdoor_correct_b1i3_violated)
power <- typeI
size <- power


# write the result to the table
backdoor_values = c(backdoor_values, FALSE)
frontdoor_values = c(frontdoor_values, TRUE)
iv_values = c(iv_values, FALSE) 
backdoor_true_functional = c(backdoor_true_functional, 0)
frontdoor_true_functional = c(frontdoor_true_functional, 0)
iv_true_functional = c(iv_true_functional, 0) 
b1 = c(b1, FALSE)
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


# 4. front-door and IV are correct, backdoor is wrong
# the identified backdoor functional is zero
p_values_null_fdoor_iv_correct_b1_violated_null_zero <- foreach(i = 1:N, .combine = c) %dopar% {
  
  df <- dgp_backdoor_frontdoor_iv_b1_violated_null_zero(n = n, beta = 0)
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

typeI <- sum(p_values_null_fdoor_iv_correct_b1_violated_null_zero <= 0.05)/length(p_values_null_fdoor_iv_correct_b1_violated_null_zero)
power <- typeI
size <- power


# write the result to the table
backdoor_values = c(backdoor_values, FALSE)
frontdoor_values = c(frontdoor_values, TRUE)
iv_values = c(iv_values, TRUE) 
backdoor_true_functional = c(backdoor_true_functional, 0)
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



# 5. backdoor and IV are correct, front-door is wrong
# the identified front-door functional is zero
p_values_null_backdoor_iv_correct_f1_violated <- foreach(i = 1:N, .combine = c) %dopar% {
  
  df <- dgp_backdoor_frontdoor_iv_f1_violated(n = n, beta = 0)
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

typeI <- sum(p_values_null_backdoor_iv_correct_f1_violated <= 0.05)/length(p_values_null_backdoor_iv_correct_f1_violated)
power <- typeI
size <- power



# write the result to the table
backdoor_values = c(backdoor_values, TRUE)
frontdoor_values = c(frontdoor_values, FALSE)
iv_values = c(iv_values, TRUE) 
backdoor_true_functional = c(backdoor_true_functional, 0)
frontdoor_true_functional = c(frontdoor_true_functional, 0)
iv_true_functional = c(iv_true_functional, 0) 
b1 = c(b1, TRUE)
b2 = c(b2, TRUE)
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


# 6. backdoor is correct, front-door and iv are wrong
# the identified iv functional is zero
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

typeI <- sum(p_values_null_bdoor_correct_f3i3_violated <= 0.05, na.rm = T)/length(p_values_null_bdoor_correct_f3i3_violated)
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


# 7. backdoor and front-door are correct, iv is wrong 
# the identified iv functional is zero
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



# 8. front door is correct, backdoor and iv are wrong 
# the identified iv and backdoor are not zero
# there are backdoor paths from A to Y and from Z to Y
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



# 9. front door and iv are true, backdoor is not
# the identified backdoor functional is not zero
# there is a backdoor path from A to Y
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



# 10. iv is correct, backdoor and front-door are wrong
# the identified backdoor and front-door functionals are not zero
p_values_null_iv_correct_b2f3_violated <- foreach(i = 1:N, .combine = c) %dopar% {
  
  df <- dgp_backdoor_frontdoor_iv_iv_correct_b2f3_violated(n = n, beta = 0)
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

typeI <- sum(p_values_null_iv_correct_b2f3_violated <= 0.05)/length(p_values_null_iv_correct_b2f3_violated)
power <- typeI
size <- power


# write the result to the table
backdoor_values = c(backdoor_values, FALSE)
frontdoor_values = c(frontdoor_values, FALSE)
iv_values = c(iv_values, TRUE) 
backdoor_true_functional = c(backdoor_true_functional, 1)
frontdoor_true_functional = c(frontdoor_true_functional, 1)
iv_true_functional = c(iv_true_functional, 0) 
b1 = c(b1, TRUE)
b2 = c(b2, FALSE)
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

# 11. backdoor is correct, front-door and IV are wrong
# the identified iv and front-door functionals are non-zero
p_values_null_bdoor_correct_f3i2_violated <- foreach(i = 1:N, .combine = c) %dopar% {
  
  df <- dgp_backdoor_frontdoor_iv_bdoor_correct_f3i2_violated(n = n, beta = 0)
  data <- df$df
  
  # estimate using AIPW (backdoor IF)
  backdoor <- estimate_backdooor(data, confounderZ = TRUE)
  backdoor.est <- backdoor$backdoor.est
  backdoor.eif <- backdoor$backdoor.eif
  
  # estimate using APIPW (front door IF)
  frontdoor <- estimate_frontdoor(data, confounderZ = TRUE)
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

typeI <- sum(p_values_null_bdoor_correct_f3i2_violated <= 0.05, na.rm = T)/length(p_values_null_bdoor_correct_f3i2_violated)
power <- typeI
size <- power


# write the result to the table
backdoor_values = c(backdoor_values, TRUE)
frontdoor_values = c(frontdoor_values, FALSE)
iv_values = c(iv_values, FALSE) 
backdoor_true_functional = c(backdoor_true_functional, 0)
frontdoor_true_functional = c(frontdoor_true_functional, 1)
iv_true_functional = c(iv_true_functional, 1) 
b1 = c(b1, TRUE)
b2 = c(b2, TRUE)
f1 = c(f1, TRUE)
f2 = c(f2, TRUE) 
f3 = c(f3, FALSE) 
i1 = c(i1, TRUE) 
i2 = c(i2, FALSE) 
i3 = c(i3, TRUE) 
i4 = c(i4, TRUE) 
hypothesis = c(hypothesis, "N")
beta = c(beta, 0)
size_values = c(size_values, size)
power_values = c(power_values, NA)

# 12. backdoor and iv are true, front-door is not 
# the identified front-door functional is not zero
# there is a backdoor path from M to Y
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


# 13. backdoor and front-door are correct, IV is wrong
# the identified IV is not zero
p_values_null_backdoor_frontdoor_correct_i2_violated <- foreach(i = 1:N, .combine = c) %dopar% {
  
  df <- dgp_backdoor_frontdoor_iv_i2_violated(n = n, beta = 0)
  data <- df$df
  
  # estimate using AIPW (backdoor IF)
  backdoor <- estimate_backdooor(data, confounderZ = TRUE)
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

typeI <- sum(p_values_null_backdoor_frontdoor_correct_i2_violated <= 0.05)/length(p_values_null_backdoor_frontdoor_correct_i2_violated)
power <- typeI
size <- power


# write the result to the table
backdoor_values = c(backdoor_values, TRUE)
frontdoor_values = c(frontdoor_values, TRUE)
iv_values = c(iv_values, FALSE) 
backdoor_true_functional = c(backdoor_true_functional, 0)
frontdoor_true_functional = c(frontdoor_true_functional, 0)
iv_true_functional = c(iv_true_functional, 1) 
b1 = c(b1, TRUE)
b2 = c(b2, TRUE)
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


##########################  Under the Alternative ############################
### 1. all models are correct
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


# 2. front-door is correct, backdoor and iv are wrong
# the identified IV functional is zero
p_values_alternative_fdoor_correct_b1i3_violated <- foreach(i = 1:N, .combine = c) %dopar% {
  
  df <- dgp_backdoor_frontdoor_iv_fdoor_correct_b1i3_violated_alt(n = n, beta = 10)
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

typeII <- sum(p_values_alternative_fdoor_correct_b1i3_violated > 0.05, na.rm = T)/length(p_values_alternative_fdoor_correct_b1i3_violated)
power <- 1-typeII


# write the result to the table
backdoor_values = c(backdoor_values, FALSE)
frontdoor_values = c(frontdoor_values, TRUE)
iv_values = c(iv_values, FALSE) 
backdoor_true_functional = c(backdoor_true_functional, 1)
frontdoor_true_functional = c(frontdoor_true_functional, 1)
iv_true_functional = c(iv_true_functional, 0) 
b1 = c(b1, FALSE)
b2 = c(b2, TRUE)
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


# 3. front-door and iv are correct, backdoor is wrong
# the identified backdoor is zero
p_values_alternative_fdoor_iv_correct_b1_violated_alt_zero <- foreach(i = 1:N, .combine = c) %dopar% {
  
  df <- dgp_backdoor_frontdoor_iv_b1_violated_alt(n = n, beta = 10)
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

typeII <- sum(p_values_alternative_fdoor_iv_correct_b1_violated_alt_zero > 0.05)/length(p_values_alternative_fdoor_iv_correct_b1_violated_alt_zero)
power <- 1-typeII

# write the result to the table
backdoor_values = c(backdoor_values, FALSE)
frontdoor_values = c(frontdoor_values, TRUE)
iv_values = c(iv_values, TRUE) 
backdoor_true_functional = c(backdoor_true_functional, 0)
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
beta = c(beta, 2)
size_values = c(size_values, NA)
power_values = c(power_values, power)


# 4. iv is correct, backdoor and front door are wrong 
# the identified front-door functional is zero
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


# 5. backdoor is correct, front-door and iv are wrong
# the identified front-door functional is zero
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

# 6. backdoor and iv are correct, front-door is wrong
# the identified front-door functional is zero
p_values_alternative_bdoor_iv_correct_f1_violated <- foreach(i = 1:N, .combine = c) %dopar% {
  
  df <- dgp_backdoor_frontdoor_iv_f1_violated(n = n, beta = 10)
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

typeII <- sum(p_values_alternative_bdoor_iv_correct_f1_violated > 0.05)/length(p_values_alternative_bdoor_iv_correct_f1_violated)
power <- 1-typeII

# write the result to the table
backdoor_values = c(backdoor_values, TRUE)
frontdoor_values = c(frontdoor_values, FALSE)
iv_values = c(iv_values, TRUE) 
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
i3 = c(i3, TRUE) 
i4 = c(i4, TRUE) 
hypothesis = c(hypothesis, "A")
beta = c(beta, 10)
size_values = c(size_values, NA)
power_values = c(power_values, power)


# 7. backdoor and front-door are correct, iv is wrong
# the identified IV functional is zero
p_values_alternative_bdoor_fdoor_correct_i3_violated_alt_zero <- foreach(i = 1:N, .combine = c) %dopar% {
  
  df <- dgp_backdoor_frontdoor_iv_i3_violated_alt_zero(n = n, beta = 10)
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

typeII <- sum(p_values_alternative_bdoor_fdoor_correct_i3_violated_alt_zero > 0.05)/length(p_values_alternative_bdoor_fdoor_correct_i3_violated_alt_zero)
power <- 1-typeII

# write the result to the table
backdoor_values = c(backdoor_values, TRUE)
frontdoor_values = c(frontdoor_values, TRUE)
iv_values = c(iv_values, FALSE) 
backdoor_true_functional = c(backdoor_true_functional, 1)
frontdoor_true_functional = c(frontdoor_true_functional, 1)
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
hypothesis = c(hypothesis, "A")
beta = c(beta, 10)
size_values = c(size_values, NA)
power_values = c(power_values, power)


# 8. iv is correct, backdoor and front door are wrong 
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

# 9. front-door is correct, backdoor and IV are wrong
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


# 10. front door and iv are correct, backdoor is wrong
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


# 11. backdoor is correct, front door and iv are wrong
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
iv_true_functional = c(iv_true_functional, 1) 
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


# 12. backdoor and iv are correct, front door is wrong
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


# 13. backdoor and front door are correct, iv is wrong
p_values_alternative_bdoor_fdoor_correct_i3_violated <- foreach(i = 1:N, .combine = c) %dopar% {
  
  df <- dgp_backdoor_frontdoor_iv_i3_violated_alt_nonzero(n = n, beta = 10)
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











