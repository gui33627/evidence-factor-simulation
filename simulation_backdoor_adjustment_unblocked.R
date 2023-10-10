library(mgcv)
library(tidyverse)
library(ggpubr)
rm(list=ls())
prefix <- "./simulations.v8.hpc/"
sapply(list.files(pattern=".R", path = paste0(prefix, "R"), full.names = TRUE), source)

library(parallel)
if(!requireNamespace("foreach")) install.packages("foreach", repos = "https://cloud.r-project.org")
library(foreach)
if(!requireNamespace("doParallel")) install.packages("doParallel", repos = "https://cloud.r-project.org")
library(doParallel)
num_core <- 7
doParallel::registerDoParallel(cores = num_core)

set.seed(123)
# simulation times
N <- 1000
# sample size
sample_size <- c(250, 500, 750, 1000)

# to save the results from simulations
n_values = c()
hypothesis = c()
beta = c()
size_values = c()
power_values = c()

## simulation

for (n in sample_size) {
  
  # 1. backdoor model with valid and invalid adjustments: under null
  p_values_null_backdoor_adjustments_ef_violation <- foreach(i = 1:N, .combine = c) %dopar% {
    
    data <- dgp_backdoor_adjustment_ef_violated(n = n, beta = 0)
    # estimate using AIPW (backdoor IF) with correct adjustments
    pi.model <- gam(A ~ s(X1) + s(X2) + s(X3) + s(X4), family = 'binomial', data = data)
    mu.model <- gam(Y ~ A + s(X1) + s(X2) + s(X3) + s(X4), family = 'gaussian', data = data)
    backdoor.eif_valid <- aipw(pi.model, mu.model, data)
    backdoor.est_valid <- mean(backdoor.eif_valid)
    backdoor.eif_valid <- backdoor.eif_valid - backdoor.est_valid
    
    # estimate using AIPW (backdoor IF) with invalid backdoor adjustments: control a collider
    pi.model <- gam(A ~ s(X2) + s(X3), family = 'binomial', data = data)
    mu.model <- gam(Y ~ A + s(X2) + s(X3), family = 'gaussian', data = data)
    backdoor.eif_unblocked_1 <- aipw(pi.model, mu.model, data)
    backdoor.est_unblocked_1 <- mean(backdoor.eif_unblocked_1)
    backdoor.eif_unblocked_1 <- backdoor.eif_unblocked_1 - backdoor.est_unblocked_1
    
    # estimate using AIPW (backdoor IF) with invalid backdoor adjustments: an unblocked backdoor path
    pi.model <- gam(A ~ s(X2) + s(X4), family = 'binomial', data = data)
    mu.model <- gam(Y ~ A + s(X2) + s(X4), family = 'gaussian', data = data)
    backdoor.eif_unblocked_2 <- aipw(pi.model, mu.model, data)
    backdoor.est_unblocked_2 <- mean(backdoor.eif_unblocked_2)
    backdoor.eif_unblocked_2 <- backdoor.eif_unblocked_2 - backdoor.est_unblocked_2
    
    # Evidence factor
    est <- c(backdoor.est_valid, backdoor.est_unblocked_1, backdoor.est_unblocked_2)
    eif <- cbind(backdoor.eif_valid, backdoor.eif_unblocked_1, backdoor.eif_unblocked_2)
    evidence_factor(est = est, eif = eif)
  }
  
  typeI <- sum(p_values_null_backdoor_adjustments_ef_violation <= 0.05)/length(p_values_null_backdoor_adjustments_ef_violation)
  power <- typeI
  size <- power
  
  n_values = c(n_values, n)
  hypothesis = c(hypothesis, "N")
  beta = c(beta, 0)
  size_values = c(size_values, size)
  power_values = c(power_values, NA)
  
  print(paste0(n, "_null DONE!"))
  
  # 2.backdoor model with valid and invalid adjustments: under alternative
  p_values_alternative_backdoor_adjustments_ef_violation <- foreach(i = 1:N, .combine = c) %dopar% {
    
    data <- dgp_backdoor_adjustment_ef_violated(n = n, beta = 10)
    # estimate using AIPW (backdoor IF) with correct adjustments
    pi.model <- gam(A ~ s(X1) + s(X2) + s(X3) + s(X4), family = 'binomial', data = data)
    mu.model <- gam(Y ~ A + s(X1) + s(X2) + s(X3) + s(X4), family = 'gaussian', data = data)
    backdoor.eif_valid <- aipw(pi.model, mu.model, data)
    backdoor.est_valid <- mean(backdoor.eif_valid)
    backdoor.eif_valid <- backdoor.eif_valid - backdoor.est_valid
    
    # estimate using AIPW (backdoor IF) with invalid backdoor adjustments: control a collider
    pi.model <- gam(A ~ s(X2) + s(X3), family = 'binomial', data = data)
    mu.model <- gam(Y ~ A + s(X2) + s(X3), family = 'gaussian', data = data)
    backdoor.eif_unblocked_1 <- aipw(pi.model, mu.model, data)
    backdoor.est_unblocked_1 <- mean(backdoor.eif_unblocked_1)
    backdoor.eif_unblocked_1 <- backdoor.eif_unblocked_1 - backdoor.est_unblocked_1
    
    # estimate using AIPW (backdoor IF) with invalid backdoor adjustments: an unblocked backdoor path
    pi.model <- gam(A ~ s(X2) + s(X4), family = 'binomial', data = data)
    mu.model <- gam(Y ~ A + s(X2) + s(X4), family = 'gaussian', data = data)
    backdoor.eif_unblocked_2 <- aipw(pi.model, mu.model, data)
    backdoor.est_unblocked_2 <- mean(backdoor.eif_unblocked_2)
    backdoor.eif_unblocked_2 <- backdoor.eif_unblocked_2 - backdoor.est_unblocked_2
    
    # Evidence factor
    est <- c(backdoor.est_valid, backdoor.est_unblocked_1, backdoor.est_unblocked_2)
    eif <- cbind(backdoor.eif_valid, backdoor.eif_unblocked_1, backdoor.eif_unblocked_2)
    evidence_factor(est = est, eif = eif)
    
  }
  
  typeII <- sum(p_values_alternative_backdoor_adjustments_ef_violation > 0.05)/length(p_values_alternative_backdoor_adjustments_ef_violation)
  power <- 1-typeII
  
  n_values = c(n_values, n)
  hypothesis = c(hypothesis, "A")
  beta = c(beta, 10)
  size_values = c(size_values, NA)
  power_values = c(power_values, power)
  
  print(paste0(n, "_alternative DONE!"))
}
  
results <- data.frame(sample_size = n_values, hypothesis = hypothesis, beta = beta, 
                      size_values = size_values, power_values = power_values)

# write.csv(results, file = paste0(prefix, "results/simulation_backdoor_adjustment_ef_violation.csv"))
results <- read.csv(paste0(prefix, "results/simulation_backdoor_adjustment_ef_violation.csv"))

## plot the simulation results
df_backdoor_adjustment_null <- results %>% filter(hypothesis == "N") %>% 
  mutate(se = sqrt(size_values*(1-size_values)/sample_size)) 

df_backdoor_adjustment_null_plot <- df_backdoor_adjustment_null %>% 
  ggplot(., aes(x = sample_size, y = size_values)) + 
  geom_line() + 
  geom_ribbon(aes(ymin = size_values - qnorm(.975)*se, ymax = size_values + qnorm(.975)*se), 
              alpha=0.2, linetype = "dashed", show.legend = FALSE) +
  geom_hline(aes(yintercept=0.05), linetype="dashed", color = "black") +
  labs(x = "Sample Size",
       y = "Type I Error Rate",
       title= "Under the Null (\u03b1 = 0.05)") +
  scale_x_continuous(breaks=c(250, 500, 750, 1000)) + 
  theme(legend.position="bottom",
        plot.title = element_text(hjust = 0.5)) 

ggsave("backdoor_adjustment_null_ef_violation.png", path =  paste0(prefix, "results/plots/"),
       width = 14, height = 10, units = "cm")


df_backdoor_adjustment_alt <- results %>% filter(hypothesis == "A") %>% 
  mutate(se = sqrt(power_values*(1-power_values)/sample_size))

df_backdoor_adjustment_alt_plot <- df_backdoor_adjustment_alt %>% 
  ggplot(., aes(x = sample_size, y = power_values)) + 
  geom_line() +
  geom_ribbon(aes(ymin = power_values - qnorm(.975)*se, ymax = pmin(power_values + qnorm(.975)*se, 1)), 
              alpha=0.2, linetype = "dashed", show.legend = FALSE) +
  labs(x = "Sample Size", y = "Power",
       title= "Under the Alternative (\u03b1 = 0.05)") +
  scale_x_continuous(breaks=c(250, 500, 750, 1000)) + ylim(0.97, 1.0) +
  theme(legend.position="bottom",
        plot.title = element_text(hjust = 0.5))

ggsave("backdoor_adjustment_alternative_ef_violation.png", path =  paste0(prefix, "results/plots/"),
       width = 14, height = 10, units = "cm")

