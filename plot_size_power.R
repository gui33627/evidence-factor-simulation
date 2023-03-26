
library(tidyverse)
prefix <- "/Users/junhui/Library/CloudStorage/GoogleDrive-junhuiyang@umass.edu/My Drive/UMASS/Courses/Ted_research/simulations.v7.hpc/results/"

# model_status <- function(df_backdoor_frontdoor){
#   all_results <- c()
#   for (i in 1:nrow(df_backdoor_frontdoor)) {
#     result <- c()
#     
#     if(is.na(df_backdoor_frontdoor$backdoor[i])){ # backdoor is not in the model
#       
#     }else if(df_backdoor_frontdoor$backdoor[i] == TRUE){ # backdoor true
#       result <- c(result, "Backdoor: TRUE")
#     }else if(df_backdoor_frontdoor$backdoor[i] == FALSE){ # backdoor false
#       tmp <- df_backdoor_frontdoor[i, c("b1", "b2")]
#       idx_tmp <- which(tmp == FALSE)
#       result <- c(result, paste0("Backdoor: ",  paste0(colnames(tmp[idx_tmp]), tmp[idx_tmp], collapse = " ")))
#     }
#     
#     if(is.na(df_backdoor_frontdoor$frontdoor[i])){ # frontdoor is not in the model
#       
#     }else if(df_backdoor_frontdoor$frontdoor[i] == TRUE){ # frontdoor true
#       result <- c(result, "Frontdoor: TRUE")
#     }else if(df_backdoor_frontdoor$frontdoor[i] == FALSE){ # frontdoor false
#       tmp <- df_backdoor_frontdoor[i, c("f1", "f2", "f3")]
#       idx_tmp <- which(tmp == FALSE)
#       result <- c(result, paste0("Frontdoor: ", paste0(colnames(tmp[idx_tmp]), tmp[idx_tmp], collapse = " ")) )
#     }
#     
#     if(is.na(df_backdoor_frontdoor$iv[i])){ # iv is not in the model
#       
#     }else if(df_backdoor_frontdoor$iv[i] == TRUE){ # iv true 
#       result <- c(result, "IV: TRUE")
#     }else if(df_backdoor_frontdoor$iv[i] == FALSE){ # iv false
#       tmp <- df_backdoor_frontdoor[i, c("i1", "i2", "i3", "i4")]
#       idx_tmp <- which(tmp == FALSE)
#       result <- c(result, paste0("IV: ", paste0(colnames(tmp[idx_tmp]), tmp[idx_tmp], collapse = " ")))
#     }
#     result_string <- paste0(result,  collapse = ", ")
#     all_results <- c(all_results, result_string)
#   }
#   return(all_results)
# }

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
  filter(!(b1 == F & b2 ==T & f1 == T & f2 ==T & f3 == T & i1 == F & i2 == T & i3 ==T & i4 == T & hypothesis == "A" & backdoor_true_functional == 1 & frontdoor_true_functional == 1 & iv_true_functional == 1)) %>%
  arrange(b1, b2, f1, f2, f3, i1, i2, i3, i4, beta)


df_bdoor_iv_250 <- read.csv(paste0(prefix, "df_backdoor_iv_250.csv"))
df_bdoor_iv_500 <- read.csv(paste0(prefix, "df_backdoor_iv_500.csv"))
df_bdoor_iv_750 <- read.csv(paste0(prefix, "df_backdoor_iv_750.csv"))
df_bdoor_iv_1000 <- read.csv(paste0(prefix, "df_backdoor_iv_1000.csv"))
df_bdoor_iv <- as.data.frame(rbind(df_bdoor_iv_250, df_bdoor_iv_500, df_bdoor_iv_750, df_bdoor_iv_1000)) %>%
  filter(!(b1 ==F & b2 == T& i1 ==T& i2 == T& i3 == T& i4 == T & hypothesis == "A" & backdoor_true_functional ==1& iv_true_functional==1)) %>%
    arrange(b1, b2, f1, f2, f3, i1, i2, i3, i4, beta)


df_fdoor_iv_250 <- read.csv(paste0(prefix, "df_frontdoor_iv_250.csv"))
df_fdoor_iv_500 <- read.csv(paste0(prefix, "df_frontdoor_iv_500.csv"))
df_fdoor_iv_750 <- read.csv(paste0(prefix, "df_frontdoor_iv_750.csv"))
df_fdoor_iv_1000 <- read.csv(paste0(prefix, "df_frontdoor_iv_1000.csv"))
# only keep one violation in each case/plot panel
df_fdoor_iv <- as.data.frame(rbind(df_fdoor_iv_250, df_fdoor_iv_500, df_fdoor_iv_750, df_fdoor_iv_1000)) %>% 
  # keep only one line per panel
  filter(!(f1 == T& f2 ==T& f3 == T& i1 ==F& i2 == T& i3 == T& i4 == T & hypothesis == "N")) %>% 
  arrange(b1, b2, f1, f2, f3, i1, i2, i3, i4, beta)

# # write the summary tables
# write.csv(df_backdoor_frontdoor, paste0(prefix, "backdoor_frontdoor_simulation_results.csv"))
# write.csv(df_backdoor_frontdoor_iv, paste0(prefix, "backdoor_frontdoor_iv_simulation_results.csv"))
# write.csv(df_fdoor_iv, paste0(prefix, "frontdoor_iv_simulation_results.csv"))
# write.csv(df_bdoor_iv, paste0(prefix, "backdoor_iv_simulation_results.csv"))


# df_backdoor_frontdoor$model_status <- model_status(df_backdoor_frontdoor)
# df_backdoor_frontdoor_iv$model_status <- model_status(df_backdoor_frontdoor_iv)
# df_bdoor_iv$model_status <- model_status(df_bdoor_iv)
# df_fdoor_iv$model_status <- model_status(df_fdoor_iv)


# df_backdoor_frontdoor <- df_backdoor_frontdoor %>% mutate(backdoor_true_functional = ifelse(backdoor_true_functional == 0, "Zero", "Non-Zero"),
#                                                           frontdoor_true_functional = ifelse(frontdoor_true_functional == 0, "Zero", "Non-Zero"))
# df_backdoor_frontdoor_iv <- df_backdoor_frontdoor_iv %>% mutate(backdoor_true_functional = ifelse(backdoor_true_functional == 0, "Zero", "Non-Zero"),
#                                                                 frontdoor_true_functional = ifelse(frontdoor_true_functional == 0, "Frontdoor: Zero", "Frontdoor: Non-Zero"),
#                                                                 iv_true_functional = ifelse(iv_true_functional == 0, "IV: Zero", "IV: Non-Zero"))
# df_bdoor_iv <- df_bdoor_iv %>% mutate(backdoor_true_functional = ifelse(backdoor_true_functional == 0, "Zero", "Non-Zero"),
#                                       iv_true_functional = ifelse(iv_true_functional == 0, "Zero", "Non-Zero"))
# df_fdoor_iv <- df_fdoor_iv %>% mutate(iv_true_functional = ifelse(iv_true_functional == 0, "Zero", "Non-Zero"),
#                                       frontdoor_true_functional = ifelse(frontdoor_true_functional == 0, "Zero", "Non-Zero"))


################################################################################
# new grid plot
# BACKDOOR + FRONTDOOR
# UNDER THE NULL
df_backdoor_frontdoor_null_all <- df_backdoor_frontdoor %>% filter(hypothesis == "N") %>% mutate(se = sqrt(size*(1-size)/sample_size)) %>% 
  mutate(nonzero_functionals = ifelse(frontdoor_true_functional == 1|backdoor_true_functional == 1, "Identified functional in wrong model \u2260 0", "Identified functional in wrong model = 0")) 


df_backdoor_frontdoor_null_plot <- df_backdoor_frontdoor_null_all %>% 
  ggplot(., aes(x = sample_size, y = size, color = nonzero_functionals)) + 
  geom_line() + 
  geom_ribbon(aes(ymin = size - qnorm(.975)*se, ymax = size + qnorm(.975)*se), alpha=0.2, linetype = "dashed", 
              data = df_backdoor_frontdoor_null_all[which(df_backdoor_frontdoor_null_all$frontdoor_true_functional == 1| df_backdoor_frontdoor_null_all$backdoor_true_functional == 1), ],
              show.legend = FALSE) +
  ggh4x::facet_nested("Frontdoor assumptions" + frontdoor ~ "Backdoor assumptions" + backdoor) +
  geom_hline(aes(yintercept=0.05), linetype="dashed", color = "black") +
  labs(x = "Sample Size",
       y = "Type I Error Rate",
       title= "Backdoor + Frontdoor, Under the Null (\u03b1 = 0.05)") +
  scale_x_continuous(breaks=c(250, 500, 750, 1000)) + 
  theme(legend.position="bottom",
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank()) 

df_backdoor_frontdoor_null_plot
ggsave("backdoor_frontdoor_null.png", path =  paste0(prefix, "plots/"),
       width = 15, height = 15, units = "cm")

# UNDER THE ALTERNATIVE
df_backdoor_frontdoor_alt_all <- df_backdoor_frontdoor %>% filter(hypothesis == "A") %>% mutate(se = sqrt(power*(1-power)/sample_size)) %>% 
  mutate(nonzero_functionals = ifelse(frontdoor_true_functional == 1 & backdoor_true_functional == 1, "Identified functional in wrong model \u2260 0", "Identified functional in wrong model = 0")) 

df_backdoor_frontdoor_alt_plot <- df_backdoor_frontdoor_alt_all %>% 
  ggplot(., aes(x = sample_size, y = power, color = nonzero_functionals)) + 
  geom_line() +
  geom_ribbon(aes(ymin = power - qnorm(.975)*se, ymax = power + qnorm(.975)*se), alpha=0.2, linetype = "dashed", 
              data = df_backdoor_frontdoor_alt_all[which(df_backdoor_frontdoor_alt_all$frontdoor_true_functional == 1 & df_backdoor_frontdoor_alt_all$backdoor_true_functional == 1), ],
              show.legend = FALSE) +
  ggh4x::facet_nested("Frontdoor assumptions" + frontdoor ~ "Backdoor assumptions" + backdoor) +
  labs(x = "Sample Size", y = "Power",
       title= "Backdoor + Frontdoor, Under the Alternative (\u03b1 = 0.05)") +
  scale_x_continuous(breaks=c(250, 500, 750, 1000)) + 
  theme(legend.position="bottom",
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank()) 

df_backdoor_frontdoor_alt_plot
ggsave("backdoor_frontdoor_alternative.png", path =  paste0(prefix, "plots/"),
       width = 15, height = 15, units = "cm")


# BACKDOOR + FRONTDOOR + IV
# UNDER THE NULL
df_backdoor_frontdoor_iv_null_all <- df_backdoor_frontdoor_iv %>% filter(hypothesis == "N") %>% mutate(se = sqrt(size*(1-size)/sample_size)) %>% 
  rowwise() %>% 
  mutate(true_model_number = sum(c(backdoor, frontdoor, iv)), nonzero_functional_number = sum(c(backdoor_true_functional, frontdoor_true_functional, iv_true_functional))) %>% 
  mutate(nonzero_functionals = ifelse((true_model_number == 1 & nonzero_functional_number == 2) | (true_model_number == 2 & nonzero_functional_number == 1), "Identified functional in wrong model \u2260 0", "Identified functional in wrong model = 0")) %>% 
  rename(Backdoor = backdoor, Frontdoor = frontdoor, IV = iv) 


df_backdoor_frontdoor_iv_null_errorbar <- df_backdoor_frontdoor_iv_null_all %>% 
  ggplot(., aes(x = sample_size, y = size, color = nonzero_functionals)) + 
  # jittering line
  geom_line(position = position_jitter(w=0, h=0.002)) + 
  geom_errorbar(aes(ymin = size - qnorm(.975)*se, ymax = size + qnorm(.975)*se), data = df_backdoor_frontdoor_iv_null_all[which(df_backdoor_frontdoor_iv_null_all$true_model_number == 1 & df_backdoor_frontdoor_iv_null_all$nonzero_functional_number == 2), ]) +
  ggh4x::facet_nested(Frontdoor + IV ~ Backdoor, labeller = label_both) +
  geom_hline(aes(yintercept=0.05), linetype="dashed", color = "black") +
  labs(x = "Sample Size",
       y = "Type I Error Rate",
       title= "Backdoor + Frontdoor + IV, Under the Null (\u03b1 = 0.05)") +
  scale_x_continuous(breaks=c(250, 500, 750, 1000)) + 
  theme(legend.position="bottom",
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank()) 

df_backdoor_frontdoor_iv_null_errorbar
ggsave("backdoor_frontdoor_iv_null_errorbar.png", path =  paste0(prefix, "plots/"),
       width = 15, height = 15, units = "cm")

df_backdoor_frontdoor_iv_null_ribbon <- df_backdoor_frontdoor_iv_null_all %>% 
  ggplot(., aes(x = sample_size, y = size, color = nonzero_functionals)) + 
  # jittering line
  geom_line(position = position_jitter(w=0, h=0.002)) + 
  geom_ribbon(aes(ymin = size - qnorm(.975)*se, ymax = size + qnorm(.975)*se), alpha=0.2, linetype = "dashed", 
              data = df_backdoor_frontdoor_iv_null_all[which(df_backdoor_frontdoor_iv_null_all$true_model_number == 1 & df_backdoor_frontdoor_iv_null_all$nonzero_functional_number == 2), ],
              show.legend = FALSE) +
  ggh4x::facet_nested(Frontdoor + IV ~ Backdoor, labeller = label_both) +
  geom_hline(aes(yintercept=0.05), linetype="dashed", color = "black") +
  labs(x = "Sample Size",
       y = "Type I Error Rate",
       title= "Backdoor + Frontdoor + IV, Under the Null (\u03b1 = 0.05)") +
  scale_x_continuous(breaks=c(250, 500, 750, 1000)) + 
  theme(legend.position="bottom",
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank()) 

df_backdoor_frontdoor_iv_null_ribbon
ggsave("backdoor_frontdoor_iv_null_ribbon.png", path =  paste0(prefix, "plots/"),
       width = 15, height = 15, units = "cm")


# UNDER THE ALTERNATIVE
df_backdoor_frontdoor_iv_alt_all <- df_backdoor_frontdoor_iv %>% filter(hypothesis == "A") %>% mutate(se = sqrt(power*(1-power)/sample_size)) %>% 
  mutate(nonzero_functionals = ifelse(backdoor_true_functional == 1 & frontdoor_true_functional == 1 & iv_true_functional == 1, "Identified functional in wrong model \u2260 0", "Identified functional in wrong model = 0")) %>% 
  rename(Backdoor = backdoor, Frontdoor = frontdoor, IV = iv) 

df_backdoor_frontdoor_iv_alt_plot <- df_backdoor_frontdoor_iv_alt_all %>% 
  ggplot(., aes(x = sample_size, y = power, color = nonzero_functionals)) + 
  geom_line() +
  geom_ribbon(aes(ymin = power - qnorm(.975)*se, ymax = power + qnorm(.975)*se), alpha=0.2, linetype = "dashed", 
              data = df_backdoor_frontdoor_iv_alt_all[which(df_backdoor_frontdoor_iv_alt_all$backdoor_true_functional == 1 & df_backdoor_frontdoor_iv_alt_all$frontdoor_true_functional == 1 & df_backdoor_frontdoor_iv_alt_all$iv_true_functional == 1), ],
              show.legend = FALSE) +
  ggh4x::facet_nested(Frontdoor + IV ~ Backdoor, labeller = label_both) +
  labs(x = "Sample Size", y = "Power",
       title= "Backdoor + Frontdoor + IV, Under the Alternative (\u03b1 = 0.05)") +
  scale_x_continuous(breaks=c(250, 500, 750, 1000)) + 
  theme(legend.position="bottom",
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank()) 

df_backdoor_frontdoor_iv_alt_plot
ggsave("backdoor_frontdoor_iv_alternative.png", path =  paste0(prefix, "plots/"),
       width = 15, height = 15, units = "cm")


# BACKDOOR + IV
# UNDER THE NULL
df_bdoor_iv_null_all <- df_bdoor_iv %>% filter(hypothesis == "N") %>% mutate(se = sqrt(size*(1-size)/sample_size)) %>% 
  mutate(nonzero_functionals = ifelse(backdoor_true_functional == 1|iv_true_functional == 1, "Identified functional in wrong model \u2260 0", "Identified functional in wrong model = 0")) 

df_bdoor_iv_null_plot <- df_bdoor_iv_null_all %>% 
  ggplot(., aes(x = sample_size, y = size, color = nonzero_functionals)) + 
  geom_line() + 
  geom_ribbon(aes(ymin = size - qnorm(.975)*se, ymax = size + qnorm(.975)*se), alpha=0.2, linetype = "dashed", 
              data = df_bdoor_iv_null_all[which(df_bdoor_iv_null_all$backdoor_true_functional == 1| df_bdoor_iv_null_all$iv_true_functional == 1), ],
              show.legend = FALSE) +
  ggh4x::facet_nested("IV assumptions" + iv ~ "Backdoor assumptions" + backdoor) +
  geom_hline(aes(yintercept=0.05), linetype="dashed", color = "black") +
  labs(x = "Sample Size",
       y = "Type I Error Rate",
       title= "Backdoor + IV, Under the Null (\u03b1 = 0.05)") +
  scale_x_continuous(breaks=c(250, 500, 750, 1000)) + 
  theme(legend.position="bottom",
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank()) 

df_bdoor_iv_null_plot
ggsave("backdoor_iv_null.png", path =  paste0(prefix, "plots/"),
       width = 15, height = 15, units = "cm")

# UNDER THE ALTERNATIVE
df_bdoor_iv_alt_all <- df_bdoor_iv %>% filter(hypothesis == "A") %>% mutate(se = sqrt(power*(1-power)/sample_size)) %>% 
  mutate(nonzero_functionals = ifelse(backdoor_true_functional == 1 & iv_true_functional == 1, "Identified functional in wrong model \u2260 0", "Identified functional in wrong model = 0")) 

df_bdoor_iv_alt_plot <- df_bdoor_iv_alt_all %>% 
  ggplot(., aes(x = sample_size, y = power, color = nonzero_functionals)) + 
  geom_line() +
  geom_ribbon(aes(ymin = power - qnorm(.975)*se, ymax = power + qnorm(.975)*se), alpha=0.2, linetype = "dashed", 
              data = df_bdoor_iv_alt_all[which(df_bdoor_iv_alt_all$backdoor_true_functional == 1 & df_bdoor_iv_alt_all$iv_true_functional == 1), ],
              show.legend = FALSE) +
  ggh4x::facet_nested("IV assumptions" + iv ~ "Backdoor assumptions" + backdoor) +
  labs(x = "Sample Size", y = "Power",
       title= "Backdoor + IV, Under the Alternative (\u03b1 = 0.05)") +
  scale_x_continuous(breaks=c(250, 500, 750, 1000)) + 
  theme(legend.position="bottom",
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank()) 

df_bdoor_iv_alt_plot
ggsave("backdoor_iv_alternative.png", path =  paste0(prefix, "plots/"),
       width = 15, height = 15, units = "cm")


# FRONTDOOR + IV
# UNDER THE NULL
df_fdoor_iv_null_all <- df_fdoor_iv %>% filter(hypothesis == "N") %>% mutate(se = sqrt(size*(1-size)/sample_size)) %>% 
  mutate(nonzero_functionals = ifelse(frontdoor_true_functional == 1|iv_true_functional == 1, "Identified functional in wrong model \u2260 0", "Identified functional in wrong model = 0")) 

df_fdoor_iv_null_plot <- df_fdoor_iv_null_all %>% 
  ggplot(., aes(x = sample_size, y = size, color = nonzero_functionals)) + 
  geom_line() + 
  geom_ribbon(aes(ymin = size - qnorm(.975)*se, ymax = size + qnorm(.975)*se), alpha=0.2, linetype = "dashed", 
              data = df_fdoor_iv_null_all[which(df_fdoor_iv_null_all$frontdoor_true_functional == 1| df_fdoor_iv_null_all$iv_true_functional == 1), ],
              show.legend = FALSE) +
  ggh4x::facet_nested("IV assumptions" + iv ~ "Frontdoor assumptions" + frontdoor) +
  geom_hline(aes(yintercept=0.05), linetype="dashed", color = "black") +
  labs(x = "Sample Size",
       y = "Type I Error Rate",
       title= "Frontdoor + IV, Under the Null (\u03b1 = 0.05)") +
  scale_x_continuous(breaks=c(250, 500, 750, 1000)) + 
  theme(legend.position="bottom",
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank()) 

df_fdoor_iv_null_plot
ggsave("frontdoor_iv_null.png", path =  paste0(prefix, "plots/"),
       width = 15, height = 15, units = "cm")

# UNDER THE ALTERNATIVE
df_fdoor_iv_alt_all <- df_fdoor_iv %>% filter(hypothesis == "A") %>% mutate(se = sqrt(power*(1-power)/sample_size)) %>% 
  mutate(nonzero_functionals = ifelse(frontdoor_true_functional == 1 & iv_true_functional == 1, "Identified functional in wrong model \u2260 0", "Identified functional in wrong model = 0"))

df_fdoor_iv_alt_plot <- df_fdoor_iv_alt_all %>% 
  ggplot(., aes(x = sample_size, y = power, color = nonzero_functionals)) + 
  geom_line() +
  geom_ribbon(aes(ymin = power - qnorm(.975)*se, ymax = power + qnorm(.975)*se), alpha=0.2, linetype = "dashed", 
              data = df_fdoor_iv_alt_all[which(df_fdoor_iv_alt_all$frontdoor_true_functional == 1 & df_fdoor_iv_alt_all$iv_true_functional == 1), ],
              show.legend = FALSE) +
  ggh4x::facet_nested("IV assumptions" + iv ~ "Frontdoor assumptions" + frontdoor) +
  labs(x = "Sample Size", y = "Power",
       title= "Frontdoor + IV, Under the Alternative (\u03b1 = 0.05)") +
  scale_x_continuous(breaks=c(250, 500, 750, 1000)) + 
  theme(legend.position="bottom",
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())


df_fdoor_iv_alt_plot
ggsave("frontdoor_iv_alternative.png", path =  paste0(prefix, "plots/"),
       width = 15, height = 15, units = "cm")


