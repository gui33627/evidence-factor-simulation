library(mgcv)
library(tidyverse)
library(ggpubr)
prefix <- "/Users/junhui/Library/CloudStorage/GoogleDrive-junhuiyang@umass.edu/My Drive/UMASS/simulations.v7.hpc/results/"
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
b1 = c()
b2 = c()
n_values = c()
hypothesis = c()
beta = c()
size_values = c()
power_values = c()

## simulation
for (n in sample_size) {
  
  # 1. valid backdoor adjustment: under null
  p_values_null_backdoor_valid <- foreach(i = 1:N, .combine = c) %dopar% {
    
    data <- dgp_backdoor_valid(n = n, beta = 0)
    
    # estimate using AIPW (backdoor IF)
    backdoor <- estimate_backdooor(data)
    backdoor.est <- backdoor$backdoor.est
    backdoor.eif <- backdoor$backdoor.eif
    
    # Marginal tests for backdoor and frontdoor
    pnorm(sqrt(n) * abs(backdoor.est) / sd(backdoor.eif), lower.tail = FALSE) * 2
    
  }
  
  typeI <- sum(p_values_null_backdoor_valid <= 0.05)/length(p_values_null_backdoor_valid)
  power <- typeI
  size <- power
  
  b1 = c(b1, TRUE)
  b2 = c(b2, TRUE)
  n_values = c(n_values, n)
  hypothesis = c(hypothesis, "N")
  beta = c(beta, 0)
  size_values = c(size_values, size)
  power_values = c(power_values, NA)
  
  print(paste0(n, "_1.1 Done!"))
  
  # 1. valid backdoor adjustment: under alternative
  p_values_alternative_backdoor_valid <- foreach(i = 1:N, .combine = c) %dopar% {
    
    data <- dgp_backdoor_valid(n = n, beta = 10)
    
    # estimate using AIPW (backdoor IF)
    backdoor <- estimate_backdooor(data)
    backdoor.est <- backdoor$backdoor.est
    backdoor.eif <- backdoor$backdoor.eif
    
    # Marginal tests for backdoor and frontdoor
    pnorm(sqrt(n) * abs(backdoor.est) / sd(backdoor.eif), lower.tail = FALSE) * 2
    
  }
  
  typeII <- sum(p_values_alternative_backdoor_valid > 0.05)/length(p_values_alternative_backdoor_valid)
  power <- 1-typeII
  
  b1 = c(b1, TRUE)
  b2 = c(b2, TRUE)
  n_values = c(n_values, n)
  hypothesis = c(hypothesis, "A")
  beta = c(beta, 10)
  size_values = c(size_values, NA)
  power_values = c(power_values, power)
  
  print(paste0(n, "_1.2 Done!"))
  
  
  # 2. invalid backdoor adjustment b/c controlling collider: under null
  p_values_null_backdoor_invalid_collider <- foreach(i = 1:N, .combine = c) %dopar% {
    
    data <- dgp_backdoor_invalid_collider(n = n, beta = 0)
    
    # estimate using AIPW (backdoor IF)
    backdoor <- estimate_backdooor(data)
    backdoor.est <- backdoor$backdoor.est
    backdoor.eif <- backdoor$backdoor.eif
    
    # Marginal tests for backdoor and frontdoor
    pnorm(sqrt(n) * abs(backdoor.est) / sd(backdoor.eif), lower.tail = FALSE) * 2
    
  }
  
  typeI <- sum(p_values_null_backdoor_invalid_collider <= 0.05)/length(p_values_null_backdoor_invalid_collider)
  power <- typeI
  size <- power
  
  b1 = c(b1, TRUE)
  b2 = c(b2, FALSE)
  n_values = c(n_values, n)
  hypothesis = c(hypothesis, "N")
  beta = c(beta, 0)
  size_values = c(size_values, size)
  power_values = c(power_values, NA)
  
  print(paste0(n, "_2.1 Done!"))
  
  # 2. invalid backdoor adjustment b/c controlling collider: under alternative
  p_values_alternative_backdoor_invalid_collider <- foreach(i = 1:N, .combine = c) %dopar% {
    
    data <- dgp_backdoor_invalid_collider(n = n, beta = 10)
    
    # estimate using AIPW (backdoor IF)
    backdoor <- estimate_backdooor(data)
    backdoor.est <- backdoor$backdoor.est
    backdoor.eif <- backdoor$backdoor.eif
    
    # Marginal tests for backdoor and frontdoor
    pnorm(sqrt(n) * abs(backdoor.est) / sd(backdoor.eif), lower.tail = FALSE) * 2
    
  }
  
  typeII <- sum(p_values_alternative_backdoor_invalid_collider > 0.05)/length(p_values_alternative_backdoor_invalid_collider)
  power <- 1-typeII
  
  
  b1 = c(b1, TRUE)
  b2 = c(b2, FALSE)
  n_values = c(n_values, n)
  hypothesis = c(hypothesis, "A")
  beta = c(beta, 10)
  size_values = c(size_values, NA)
  power_values = c(power_values, power)
  
  print(paste0(n, "_2.2 Done!"))
  
  
  # 3. invalid backdoor adjustment b/c unblocked backdoor path: under null
  p_values_null_backdoor_invalid_unblocked <- foreach(i = 1:N, .combine = c) %dopar% {
    
    data <- dgp_backdoor_invalid_unblocked(n = n, beta = 0)
    
    # estimate using AIPW (backdoor IF)
    backdoor <- estimate_backdooor(data)
    backdoor.est <- backdoor$backdoor.est
    backdoor.eif <- backdoor$backdoor.eif
    
    # Marginal tests for backdoor and frontdoor
    pnorm(sqrt(n) * abs(backdoor.est) / sd(backdoor.eif), lower.tail = FALSE) * 2
    
  }
  
  typeI <- sum(p_values_null_backdoor_invalid_unblocked <= 0.05)/length(p_values_null_backdoor_invalid_unblocked)
  power <- typeI
  size <- power
  
  b1 = c(b1, FALSE)
  b2 = c(b2, TRUE)
  n_values = c(n_values, n)
  hypothesis = c(hypothesis, "N")
  beta = c(beta, 0)
  size_values = c(size_values, size)
  power_values = c(power_values, NA)
  
  print(paste0(n, "_3.1 Done!"))
  
  
  # 3. invalid backdoor adjustment b/c unblocked backdoor path: under alternative
  p_values_alternative_backdoor_invalid_unblocked <- foreach(i = 1:N, .combine = c) %dopar% {
    
    data <- dgp_backdoor_invalid_unblocked(n = n, beta = 10)
    
    # estimate using AIPW (backdoor IF)
    backdoor <- estimate_backdooor(data)
    backdoor.est <- backdoor$backdoor.est
    backdoor.eif <- backdoor$backdoor.eif
    
    # Marginal tests for backdoor and frontdoor
    pnorm(sqrt(n) * abs(backdoor.est) / sd(backdoor.eif), lower.tail = FALSE) * 2
    
  }
  
  typeII <- sum(p_values_alternative_backdoor_invalid_unblocked > 0.05)/length(p_values_alternative_backdoor_invalid_unblocked)
  power <- 1-typeII
  
  b1 = c(b1, FALSE)
  b2 = c(b2, TRUE)
  n_values = c(n_values, n)
  hypothesis = c(hypothesis, "A")
  beta = c(beta, 10)
  size_values = c(size_values, NA)
  power_values = c(power_values, power)
  
  print(paste0(n, "_3.2 Done!"))
  
}

results <- data.frame(b1 = b1, b2 = b2, sample_size = n_values, 
                      hypothesis = hypothesis, beta = beta, size_values = size_values,
                      power_values = power_values)

# write.csv(results, file = paste0(prefix, "simulation_backdoor_adjustment.csv"))

## plot the simulation results
df_backdoor_adjustment_null <- results %>% filter(hypothesis == "N") %>% 
  mutate(se = sqrt(size_values*(1-size_values)/sample_size),
         backdoor_adj =  case_when(b1 == TRUE & b2 == TRUE ~ "Valid",
                                   b1 == TRUE & b2 == FALSE ~ "Invalid (collider)",
                                   b1 == FALSE & b2 == TRUE ~ "Invalid (unblocked bdoor path)")) 


df_backdoor_adjustment_null_plot <- df_backdoor_adjustment_null %>% 
  ggplot(., aes(x = sample_size, y = size_values, color = backdoor_adj)) + 
  geom_line() + 
  geom_ribbon(aes(ymin = size_values - qnorm(.975)*se, ymax = size_values + qnorm(.975)*se), 
              alpha=0.2, linetype = "dashed", show.legend = FALSE) +
  geom_hline(aes(yintercept=0.05), linetype="dashed", color = "black") +
  labs(x = "Sample Size",
       y = "Type I Error Rate",
       title= "Under Null (\u03b1 = 0.05)") +
  scale_x_continuous(breaks=c(250, 500, 750, 1000)) + 
  theme(legend.position="bottom",
        plot.title = element_text(hjust = 0.5)) + 
  labs(colour = "Backdoor Adjustment") 

df_backdoor_adjustment_alt <- results %>% filter(hypothesis == "A") %>% 
  mutate(se = sqrt(power_values*(1-power_values)/sample_size),
         backdoor_adj =  case_when(b1 == TRUE & b2 == TRUE ~ "Valid",
                                   b1 == TRUE & b2 == FALSE ~ "Invalid (collider)",
                                   b1 == FALSE & b2 == TRUE ~ "Invalid (unblocked bdoor path)"))

df_backdoor_adjustment_alt_plot <- df_backdoor_adjustment_alt %>% 
  ggplot(., aes(x = sample_size, y = power_values, color = backdoor_adj)) + 
  geom_line() +
  geom_ribbon(aes(ymin = power_values - qnorm(.975)*se, ymax = power_values + qnorm(.975)*se), 
              alpha=0.2, linetype = "dashed", show.legend = FALSE) +
  labs(x = "Sample Size", y = "Power",
       title= "Under Alternative (\u03b1 = 0.05)") +
  scale_x_continuous(breaks=c(250, 500, 750, 1000)) + ylim(0.95, 1.01) +
  theme(legend.position="bottom",
        plot.title = element_text(hjust = 0.5)) 
  labs(colour = "Backdoor Adjustment")


ggarrange(df_backdoor_adjustment_null_plot, df_backdoor_adjustment_alt_plot, 
          legend = "none") %>%
  gridExtra::grid.arrange(get_legend(df_backdoor_adjustment_null_plot), heights = unit(c(80, 5), "mm"))




