# Simulate data where both backdoor and front door are true and they should agree
rm(list=ls())

library(mgcv)
sapply(list.files(pattern=".R", path="./R", full.names = TRUE), source)

n <- 2000

########################### Backdoor and Front-door ############################

# get data where both frontdoor and backdoor are correct
data <- dgp_backdoor_frontdoor_both_correct(n)

# estimate using AIPW (backdoor IF)
backdoor <- estimate_backdooor(data)
backdoor.est <- backdoor$backdoor.est
backdoor.eif <- backdoor$backdoor.eif

# estimate using APIPW (front door IF)
frontdoor <- estimate_frontdoor(data)
frontdoor.est <- frontdoor$frontdoor.est
frontdoor.eif <- frontdoor$frontdoor.eif

# E[Y(1) - Y(0)] in true DGP
print(mean(data$PO.diff))

# CIs for backdoor and frontdoor
print(backdoor.est + c(-1,1) * qnorm(.975) * sd(backdoor.eif) / sqrt(n))
print(frontdoor.est + c(-1,1) * qnorm(.975) * sd(frontdoor.eif) / sqrt(n))



############################## Backdoor and IV ################################

# get data where both backdoor and IV are correct
df <- dgp_backdoor_iv_both_correct(n)
data <- df$df

# estimate using AIPW (backdoor IF)
backdoor <- estimate_backdooor(data)
backdoor.est <- backdoor$backdoor.est
backdoor.eif <- backdoor$backdoor.eif

# estimate using UIV (IV IF)
iv <- estimate_uiv(data)
iv.est <- iv$iv.est
iv.eif <- iv$iv.eif


# CACE in true DGP
print(df$CACE)

# CIs for backdoor and iv
print(backdoor.est + c(-1,1) * qnorm(.975) * sd(backdoor.eif) / sqrt(n))
print(iv.est + c(-1,1) * qnorm(.975) * sd(iv.eif) / sqrt(n))



############################## Front-door and IV ###############################

# get data where both frontdoor and IV are correct
df <- dgp_frontdoor_iv_both_correct(n)
data <- df$df

# estimate using APIPW (front door IF)
frontdoor <- estimate_frontdoor(data)
frontdoor.est <- frontdoor$frontdoor.est
frontdoor.eif <- frontdoor$frontdoor.eif

# estimate using UIV (IV IF)
iv <- estimate_uiv(data)
iv.est <- iv$iv.est
iv.eif <- iv$iv.eif


# CACE in true DGP
print(df$CACE)

# CIs for backdoor and iv
print(frontdoor.est + c(-1,1) * qnorm(.975) * sd(frontdoor.eif) / sqrt(n))
print(iv.est + c(-1,1) * qnorm(.975) * sd(iv.eif) / sqrt(n))


######################### Backdoor, Front-door, and IV #########################

# get data where backdoor, front-door, and IV are correct
df <- dgp_backdoor_frontdoor_iv_all_correct(n)
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

# CACE in true DGP
print(df$CACE)

# CIs for backdoor and iv
print(backdoor.est + c(-1,1) * qnorm(.975) * sd(backdoor.eif) / sqrt(n))
print(frontdoor.est + c(-1,1) * qnorm(.975) * sd(frontdoor.eif) / sqrt(n))
print(iv.est + c(-1,1) * qnorm(.975) * sd(iv.eif) / sqrt(n))








