
library(tidyverse)
prefix <- "/Volumes/GoogleDrive/My Drive/UMASS/simulations.v3.hpc/"

model_status <- function(df_backdoor_frontdoor){
  all_results <- c()
  for (i in 1:nrow(df_backdoor_frontdoor)) {
    result <- c()
    
    if(is.na(df_backdoor_frontdoor$backdoor[i])){ # backdoor is not in the model
      
    }else if(df_backdoor_frontdoor$backdoor[i] == TRUE){ # backdoor true
      result <- c(result, "Backdoor: TRUE")
    }else if(df_backdoor_frontdoor$backdoor[i] == FALSE){ # backdoor false
      tmp <- df_backdoor_frontdoor[i, c("b1", "b2")]
      idx_tmp <- which(tmp == FALSE)
      result <- c(result, paste0("Backdoor: ",  paste0(colnames(tmp[idx_tmp]), tmp[idx_tmp], collapse = " ")))
    }
    
    if(is.na(df_backdoor_frontdoor$frontdoor[i])){ # frontdoor is not in the model
      
    }else if(df_backdoor_frontdoor$frontdoor[i] == TRUE){ # frontdoor true
      result <- c(result, "Frontdoor: TRUE")
    }else if(df_backdoor_frontdoor$frontdoor[i] == FALSE){ # frontdoor false
      tmp <- df_backdoor_frontdoor[i, c("f1", "f2", "f3")]
      idx_tmp <- which(tmp == FALSE)
      result <- c(result, paste0("Frontdoor: ", paste0(colnames(tmp[idx_tmp]), tmp[idx_tmp], collapse = " ")) )
    }
    
    if(is.na(df_backdoor_frontdoor$iv[i])){ # iv is not in the model
      
    }else if(df_backdoor_frontdoor$iv[i] == TRUE){ # iv true 
      result <- c(result, "IV: TRUE")
    }else if(df_backdoor_frontdoor$iv[i] == FALSE){ # iv false
      tmp <- df_backdoor_frontdoor[i, c("i1", "i2", "i3", "i4")]
      idx_tmp <- which(tmp == FALSE)
      result <- c(result, paste0("IV: ", paste0(colnames(tmp[idx_tmp]), tmp[idx_tmp], collapse = " ")))
    }
    result_string <- paste0(result,  collapse = ", ")
    all_results <- c(all_results, result_string)
  }
  return(all_results)
}

df_backdoor_frontdoor_250 <- read.csv(paste0(prefix, "df_backdoor_frontdoor_250.csv"))
df_backdoor_frontdoor_500 <- read.csv(paste0(prefix, "df_backdoor_frontdoor_500.csv"))
df_backdoor_frontdoor_750 <- read.csv(paste0(prefix, "df_backdoor_frontdoor_750.csv"))
df_backdoor_frontdoor_1000 <- read.csv(paste0(prefix, "df_backdoor_frontdoor_1000.csv"))
df_backdoor_frontdoor <- as.data.frame(rbind(df_backdoor_frontdoor_250, df_backdoor_frontdoor_500, 
                                             df_backdoor_frontdoor_750, df_backdoor_frontdoor_1000))
df_backdoor_frontdoor <- df_backdoor_frontdoor %>% arrange(b1, b2, f1, f2, f3, i1, i2, i3, i4, beta)


df_backdoor_frontdoor_iv_250 <- read.csv(paste0(prefix, "df_backdoor_frontdoor_iv_250.csv"))
df_backdoor_frontdoor_iv_500 <- read.csv(paste0(prefix, "df_backdoor_frontdoor_iv_500.csv"))
df_backdoor_frontdoor_iv_750 <- read.csv(paste0(prefix, "df_backdoor_frontdoor_iv_750.csv"))
df_backdoor_frontdoor_iv_1000 <- read.csv(paste0(prefix, "df_backdoor_frontdoor_iv_1000.csv"))
df_backdoor_frontdoor_iv <- as.data.frame(rbind(df_backdoor_frontdoor_iv_250, df_backdoor_frontdoor_iv_500, 
                                                df_backdoor_frontdoor_iv_750, df_backdoor_frontdoor_iv_1000))
df_backdoor_frontdoor_iv <- df_backdoor_frontdoor_iv %>% 
  filter(!(b1 == T& b2 == T& f1 == T& f2 == T& f3 ==T& i1 == T& i2 ==T& i3 ==T& i4 ==F)) %>% 
  arrange(b1, b2, f1, f2, f3, i1, i2, i3, i4, beta)


df_bdoor_iv_250 <- read.csv(paste0(prefix, "df_backdoor_iv_250.csv"))
df_bdoor_iv_500 <- read.csv(paste0(prefix, "df_backdoor_iv_500.csv"))
df_bdoor_iv_750 <- read.csv(paste0(prefix, "df_backdoor_iv_750.csv"))
df_bdoor_iv_1000 <- read.csv(paste0(prefix, "df_backdoor_iv_1000.csv"))
df_bdoor_iv <- as.data.frame(rbind(df_bdoor_iv_250, df_bdoor_iv_500, df_bdoor_iv_750, df_bdoor_iv_1000))
# delete the case when i4 is violated because the size and power are NA
df_bdoor_iv <- df_bdoor_iv %>% filter(!(b1 == T& b2 == T& i1 == T& i2 ==T& i3 ==T& i4 ==F)) %>% 
  arrange(b1, b2, f1, f2, f3, i1, i2, i3, i4, beta)


df_fdoor_iv_250 <- read.csv(paste0(prefix, "df_frontdoor_iv_250.csv"))
df_fdoor_iv_500 <- read.csv(paste0(prefix, "df_frontdoor_iv_500.csv"))
df_fdoor_iv_750 <- read.csv(paste0(prefix, "df_frontdoor_iv_750.csv"))
df_fdoor_iv_1000 <- read.csv(paste0(prefix, "df_frontdoor_iv_1000.csv"))
df_fdoor_iv <- as.data.frame(rbind(df_fdoor_iv_250, df_fdoor_iv_500, df_fdoor_iv_750, df_fdoor_iv_1000))
# delete the case when i4 is violated because the size and power are NA
df_fdoor_iv <- df_fdoor_iv %>% filter(!(f1 == T& f2 == T& f3 ==T& i1 == T& i2 ==T& i3 ==T& i4 ==F)) %>% 
  arrange(b1, b2, f1, f2, f3, i1, i2, i3, i4, beta)

# write the summary tables
write.csv(df_backdoor_frontdoor, paste0(prefix, "backdoor_frontdoor_simulation_results.csv"))
write.csv(df_backdoor_frontdoor_iv, paste0(prefix, "backdoor_frontdoor_iv_simulation_results.csv"))
write.csv(df_fdoor_iv, paste0(prefix, "frontdoor_iv_simulation_results.csv"))
write.csv(df_bdoor_iv, paste0(prefix, "backdoor_iv_simulation_results.csv"))


df_backdoor_frontdoor$model_status <- model_status(df_backdoor_frontdoor)
df_backdoor_frontdoor_iv$model_status <- model_status(df_backdoor_frontdoor_iv)
df_bdoor_iv$model_status <- model_status(df_bdoor_iv)
df_fdoor_iv$model_status <- model_status(df_fdoor_iv)

# BACKDOOR + FRONTDOOR
# UNDER THE NULL
df_backdoor_frontdoor_null_all <- df_backdoor_frontdoor %>% filter(hypothesis == "N") %>% 
  mutate(nonzero_functionals = ifelse(frontdoor_true_functional == 1|backdoor_true_functional == 1, TRUE, FALSE)) %>% 
  ggplot(., aes(x = sample_size, y = 1-size, color = model_status, linetype = nonzero_functionals)) + 
  geom_line() + ylim(0.9,1) +
  labs(x = "Sample Size",
       y = "True Negative Rate",
       title= "Backdoor + Frontdoor, Under the Null",
       color = "Assumption Violation Status", 
       linetype = "Non-Zero Functional In Wrong Model") +
  scale_x_continuous(breaks=c(250, 500, 750, 1000)) +
  theme(legend.position = c(.95, .05),
        legend.justification = c("right", "bottom"),
        legend.box.just = "bottom",
        legend.margin = margin(6, 6, 6, 6),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 8),
        legend.box = "horizontal",
        plot.title = element_text(hjust = 0.5)) + 
  guides(linetype = guide_legend(order = 1),
         col = guide_legend(order = 2))

df_backdoor_frontdoor_null_all
ggsave("backdoor_frontdoor_null_all.png", path =  paste0(prefix, "plots/"), 
       width = 20, height = 15, units = "cm")

# UNDER THE ALTERNATIVE
df_backdoor_frontdoor_alt_all <- df_backdoor_frontdoor %>% filter(hypothesis == "A") %>% 
  mutate(nonzero_functionals = ifelse(frontdoor_true_functional == 1 & backdoor_true_functional == 1, TRUE, FALSE)) %>% 
  ggplot(., aes(x = sample_size, y = power, color = model_status, linetype = nonzero_functionals)) + 
  geom_line() +
  labs(x = "Sample Size", y = "True Positive Rate",
       title= "Backdoor + Frontdoor, Under the Alternative",
       color = "Assumption Violation Status", 
       linetype = "Non-Zero Functional In Wrong Model") +
  scale_x_continuous(breaks=c(250, 500, 750, 1000)) +
  theme(legend.position = c(.95, .2),
        legend.justification = c("right", "bottom"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 8),
        plot.title = element_text(hjust = 0.5)) + 
  guides(linetype = guide_legend(order = 1),
         col = guide_legend(order = 2))

df_backdoor_frontdoor_alt_all
ggsave("backdoor_frontdoor_alternative_all.png", path =  paste0(prefix, "plots/"),
       width = 20, height = 15, units = "cm")


# BACKDOOR + FRONTDOOR + IV
# UNDER THE NULL
df_backdoor_frontdoor_iv_null_all <- df_backdoor_frontdoor_iv %>% filter(hypothesis == "N") %>% 
  mutate(nonzero_functionals = ifelse(backdoor_true_functional == 1|frontdoor_true_functional == 1|iv_true_functional == 1, TRUE, FALSE)) %>% 
  ggplot(., aes(x = sample_size, y = 1-size, color = model_status, linetype = nonzero_functionals)) + 
  geom_line() + ylim(0.9,1) +
  labs(x = "Sample Size", y = "True Negative Rate",
       title= "Backdoor + Frontdoor + IV, Under the Null",
       color = "Assumption Violation Status",
       linetype = "Non-Zero Functional In Wrong Model") +
  scale_x_continuous(breaks=c(250, 500, 750, 1000)) +
  theme(legend.position = c(.95, .05),
        legend.justification = c("right", "bottom"),
        legend.box.just = "bottom",
        legend.margin = margin(6, 6, 6, 6),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 8),
        legend.box = "horizontal",
        plot.title = element_text(hjust = 0.5)) + 
  guides(linetype = guide_legend(order = 1),
         col = guide_legend(order = 2))

df_backdoor_frontdoor_iv_null_all
ggsave("backdoor_frontdoor_iv_null_all.png", path =  paste0(prefix, "plots/"),
       width = 20, height = 15, units = "cm")

# UNDER THE ALTERNATIVE
df_backdoor_frontdoor_iv_alt_all <- df_backdoor_frontdoor_iv %>% filter(hypothesis == "A") %>% 
  mutate(nonzero_functionals = ifelse(backdoor_true_functional == 1 & frontdoor_true_functional == 1 & iv_true_functional == 1, TRUE, FALSE)) %>% 
  ggplot(., aes(x = sample_size, y = power, color = model_status, linetype = nonzero_functionals)) + 
  geom_line() +
  labs(x = "Sample Size", y = "True Positive Rate",
       title= "Backdoor + Frontdoor + IV, Under the Alternative",
       color = "Assumption Violation Status",
       linetype = "Non-Zero Functional In Wrong Model") +
  scale_x_continuous(breaks=c(250, 500, 750, 1000)) +
  theme(legend.position = c(.95, .36),
        legend.justification = c("right", "bottom"),
        legend.box.just = "bottom",
        legend.margin = margin(6, 6, 6, 6),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 8),
        legend.box = "horizontal",
        plot.title = element_text(hjust = 0.5)) + 
  guides(linetype = guide_legend(order = 1),
         col = guide_legend(order = 2))

df_backdoor_frontdoor_iv_alt_all
ggsave("backdoor_frontdoor_iv_alternative_all.png", path =  paste0(prefix, "plots/"),
       width = 25, height = 20, units = "cm")


# BACKDOOR + IV
# UNDER THE NULL
df_bdoor_iv_null_all <- df_bdoor_iv %>% filter(hypothesis == "N") %>% 
  mutate(nonzero_functionals = ifelse(backdoor_true_functional == 1|iv_true_functional == 1, TRUE, FALSE)) %>% 
  ggplot(., aes(x = sample_size, y = 1-size, color = model_status, linetype = nonzero_functionals)) + 
  geom_line() + ylim(0.9,1) +
  labs(x = "Sample Size", y = "True Negative Rate",
       title= "Backdoor + IV, Under the Null",
       color = "Assumption Violation Status",
       linetype = "Non-Zero Functional In Wrong Model") +
  scale_x_continuous(breaks=c(250, 500, 750, 1000)) +
  theme(legend.position = c(.95, .05),
        legend.justification = c("right", "bottom"),
        legend.box.just = "bottom",
        legend.margin = margin(6, 6, 6, 6),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 8),
        legend.box = "horizontal",
        plot.title = element_text(hjust = 0.5)) + 
  guides(linetype = guide_legend(order = 1),
         col = guide_legend(order = 2))

df_bdoor_iv_null_all
ggsave("backdoor_iv_null_all.png", path =  paste0(prefix, "plots/"),
       width = 20, height = 15, units = "cm")

# UNDER THE ALTERNATIVE
df_bdoor_iv_alt_all <- df_bdoor_iv %>% filter(hypothesis == "A") %>% 
  mutate(nonzero_functionals = ifelse(backdoor_true_functional == 1 & iv_true_functional == 1, TRUE, FALSE)) %>% 
  ggplot(., aes(x = sample_size, y = power, color = model_status, linetype = nonzero_functionals)) + 
  geom_line() +
  labs(x = "Sample Size", y = "True Positive Rate",
       title= "Backdoor + IV, Under the Alternative",
       color = "Assumption Violation Status", 
       linetype = "Non-Zero Functional In Wrong Model") +
  scale_x_continuous(breaks=c(250, 500, 750, 1000)) +
  theme(legend.position = c(.95, .1),
        legend.justification = c("right", "bottom"),
        legend.box.just = "bottom",
        legend.margin = margin(6, 6, 6, 6),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 8),
        legend.box = "horizontal",
        plot.title = element_text(hjust = 0.5)) + 
  guides(linetype = guide_legend(order = 1),
         col = guide_legend(order = 2))

df_bdoor_iv_alt_all
ggsave("backdoor_iv_alternative_all.png", path =  paste0(prefix, "plots/"),
       width = 20, height = 15, units = "cm")


# FRONTDOOR + IV
# UNDER THE NULL
df_fdoor_iv_null_all <- df_fdoor_iv %>% filter(hypothesis == "N") %>% 
  mutate(nonzero_functionals = ifelse(frontdoor_true_functional == 1|iv_true_functional == 1, TRUE, FALSE)) %>% 
  ggplot(., aes(x = sample_size, y = 1-size, color = model_status, linetype = nonzero_functionals)) + 
  geom_line() + ylim(0.9,1) +
  labs(x = "Sample Size", y = "True Negative Rate",
       title= "Frontdoor + IV, Under the Null",
       color = "Assumption Violation Status", 
       linetype = "Non-Zero Functional In Wrong Model") +
  scale_x_continuous(breaks=c(250, 500, 750, 1000)) +
  theme(legend.position = c(.95, .05),
        legend.justification = c("right", "bottom"),
        legend.box.just = "bottom",
        legend.margin = margin(6, 6, 6, 6),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 8),
        legend.box = "horizontal",
        plot.title = element_text(hjust = 0.5)) + 
  guides(linetype = guide_legend(order = 1),
         col = guide_legend(order = 2))

df_fdoor_iv_null_all
ggsave("frontdoor_iv_null_all.png", path =  paste0(prefix, "plots/"),
       width = 20, height = 15, units = "cm")

# UNDER THE ALTERNATIVE
df_fdoor_iv_alt_all <- df_fdoor_iv %>% filter(hypothesis == "A") %>% 
  mutate(nonzero_functionals = ifelse(frontdoor_true_functional == 1 & iv_true_functional == 1, TRUE, FALSE)) %>% 
  ggplot(., aes(x = sample_size, y = power, color = model_status, linetype = nonzero_functionals)) + 
  geom_line() +
  labs(x = "Sample Size", y = "True Positive Rate",
       title= "Frontdoor + IV, Under the Alternative",
       color = "Assumption Violation Status",
       linetype = "Non-Zero Functional In Wrong Model") +
  scale_x_continuous(breaks=c(250, 500, 750, 1000)) +
  theme(legend.position = c(.95, .1),
        legend.justification = c("right", "bottom"),
        legend.box.just = "bottom",
        legend.margin = margin(6, 6, 6, 6),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 8),
        legend.box = "horizontal",
        plot.title = element_text(hjust = 0.5)) + 
  guides(linetype = guide_legend(order = 1),
         col = guide_legend(order = 2))


df_fdoor_iv_alt_all
ggsave("frontdoor_iv_alternative_all.png", path =  paste0(prefix, "plots/"),
       width = 20, height = 15, units = "cm")




