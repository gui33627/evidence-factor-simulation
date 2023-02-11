
library(tidyverse)
prefix <- "/Users/junhui/Library/CloudStorage/GoogleDrive-junhuiyang@umass.edu/My Drive/UMASS/simulations.hpc/simulation_results/"

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
  # delete the case because the power at sample size 250 is NA
  filter(!(b1 == T& b2 == T& f1 == T& f2 == T& f3 ==F& i1 == T& i2 ==T& i3 ==F& i4 ==T)) %>% 
  arrange(b1, b2, f1, f2, f3, i1, i2, i3, i4, beta)


df_bdoor_iv_250 <- read.csv(paste0(prefix, "df_backdoor_iv_250.csv"))
df_bdoor_iv_500 <- read.csv(paste0(prefix, "df_backdoor_iv_500.csv"))
df_bdoor_iv_750 <- read.csv(paste0(prefix, "df_backdoor_iv_750.csv"))
df_bdoor_iv_1000 <- read.csv(paste0(prefix, "df_backdoor_iv_1000.csv"))
df_bdoor_iv <- as.data.frame(rbind(df_bdoor_iv_250, df_bdoor_iv_500, df_bdoor_iv_750, df_bdoor_iv_1000))

df_bdoor_iv <- df_bdoor_iv %>% arrange(b1, b2, f1, f2, f3, i1, i2, i3, i4, beta)


df_fdoor_iv_250 <- read.csv(paste0(prefix, "df_frontdoor_iv_250.csv"))
df_fdoor_iv_500 <- read.csv(paste0(prefix, "df_frontdoor_iv_500.csv"))
df_fdoor_iv_750 <- read.csv(paste0(prefix, "df_frontdoor_iv_750.csv"))
df_fdoor_iv_1000 <- read.csv(paste0(prefix, "df_frontdoor_iv_1000.csv"))
df_fdoor_iv <- as.data.frame(rbind(df_fdoor_iv_250, df_fdoor_iv_500, df_fdoor_iv_750, df_fdoor_iv_1000))
df_fdoor_iv <- df_fdoor_iv %>% arrange(b1, b2, f1, f2, f3, i1, i2, i3, i4, beta)

# # write the summary tables
# write.csv(df_backdoor_frontdoor, paste0(prefix, "backdoor_frontdoor_simulation_results.csv"))
# write.csv(df_backdoor_frontdoor_iv, paste0(prefix, "backdoor_frontdoor_iv_simulation_results.csv"))
# write.csv(df_fdoor_iv, paste0(prefix, "frontdoor_iv_simulation_results.csv"))
# write.csv(df_bdoor_iv, paste0(prefix, "backdoor_iv_simulation_results.csv"))


df_backdoor_frontdoor$model_status <- model_status(df_backdoor_frontdoor)
df_backdoor_frontdoor_iv$model_status <- model_status(df_backdoor_frontdoor_iv)
df_bdoor_iv$model_status <- model_status(df_bdoor_iv)
df_fdoor_iv$model_status <- model_status(df_fdoor_iv)


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
df_backdoor_frontdoor_null_all <- df_backdoor_frontdoor %>% filter(hypothesis == "N") %>% 
  mutate(nonzero_functionals = ifelse(frontdoor_true_functional == 1|backdoor_true_functional == 1, TRUE, FALSE)) %>% 
  ggplot(., aes(x = sample_size, y = size, color = model_status, linetype = nonzero_functionals)) + 
  geom_line() + 
  ggh4x::facet_nested("Frontdoor" + frontdoor ~ "Backdoor" + backdoor) +
  geom_hline(aes(yintercept=0.05), linetype="dashed", color = "black") +
  labs(x = "Sample Size",
       y = "Type I Error Rate",
       title= "Backdoor + Frontdoor, Under the Null (\u03b1 = 0.05)",
       linetype = "Non-Zero Identified Functional In Wrong Model",
       caption = "Note: colors represent violations of different assumptions in the wrong model") +
  scale_x_continuous(breaks=c(250, 500, 750, 1000)) + 
  theme(legend.position="bottom",
        plot.title = element_text(hjust = 0.5)) +
  guides(color = "none")

df_backdoor_frontdoor_null_all
ggsave("v2.backdoor_frontdoor_null_all.png", path =  paste0(prefix, "plots/"),
       width = 15, height = 15, units = "cm")

# UNDER THE ALTERNATIVE
df_backdoor_frontdoor_alt_all <- df_backdoor_frontdoor %>% filter(hypothesis == "A") %>% 
  mutate(nonzero_functionals = ifelse(frontdoor_true_functional == 1 & backdoor_true_functional == 1, TRUE, FALSE)) %>% 
  ggplot(., aes(x = sample_size, y = power, color = model_status, linetype = nonzero_functionals)) + 
  geom_line() +
  ggh4x::facet_nested("Frontdoor" + frontdoor ~ "Backdoor" + backdoor) +
  labs(x = "Sample Size", y = "Power",
       title= "Backdoor + Frontdoor, Under the Alternative (\u03b1 = 0.05)",
       linetype = "Non-Zero Identified Functional In Wrong Model",
       caption = "Note: colors represent violations of different assumptions in the wrong model") +
  scale_x_continuous(breaks=c(250, 500, 750, 1000)) + 
  theme(legend.position="bottom",
        plot.title = element_text(hjust = 0.5)) +
  guides(color = "none")

df_backdoor_frontdoor_alt_all
ggsave("v2.backdoor_frontdoor_alternative_all.png", path =  paste0(prefix, "plots/"),
       width = 15, height = 15, units = "cm")


# BACKDOOR + FRONTDOOR + IV
# UNDER THE NULL
df_backdoor_frontdoor_iv_null_all <- df_backdoor_frontdoor_iv %>% filter(hypothesis == "N") %>% 
  mutate(nonzero_functionals = ifelse((backdoor_true_functional == 1 & frontdoor_true_functional == 1) | (backdoor_true_functional == 1 & iv_true_functional == 1) | (frontdoor_true_functional == 1 & iv_true_functional == 1), TRUE, FALSE)) %>% 
  ggplot(., aes(x = sample_size, y = size, color = model_status, linetype = nonzero_functionals)) + 
  geom_line() + 
  ggh4x::facet_nested(frontdoor + iv ~ backdoor, labeller = label_both) +
  geom_hline(aes(yintercept=0.05), linetype="dashed", color = "black") +
  labs(x = "Sample Size",
       y = "Type I Error Rate",
       title= "Backdoor + Frontdoor + IV, Under the Null (\u03b1 = 0.05)",
       linetype = "Non-Zero Identified Functional In Wrong Model",
       caption = "Note: colors represent violations of different assumptions in the wrong model") +
  scale_x_continuous(breaks=c(250, 500, 750, 1000)) + 
  theme(legend.position="bottom",
        plot.title = element_text(hjust = 0.5)) +
  guides(color = "none")

df_backdoor_frontdoor_iv_null_all
ggsave("v2.backdoor_frontdoor_iv_null_all.png", path =  paste0(prefix, "plots/"),
       width = 15, height = 15, units = "cm")


# UNDER THE ALTERNATIVE
df_backdoor_frontdoor_iv_alt_all <- df_backdoor_frontdoor_iv %>% filter(hypothesis == "A") %>% 
  mutate(nonzero_functionals = ifelse(backdoor_true_functional == 1 & frontdoor_true_functional == 1 & iv_true_functional == 1, TRUE, FALSE)) %>% 
  ggplot(., aes(x = sample_size, y = power, color = model_status, linetype = nonzero_functionals)) + 
  geom_line() +
  ggh4x::facet_nested(frontdoor + iv ~ backdoor, labeller = label_both) +
  labs(x = "Sample Size", y = "Power",
       title= "Backdoor + Frontdoor + IV, Under the Alternative (\u03b1 = 0.05)",
       linetype = "Non-Zero Identified Functional In Wrong Model",
       caption = "Note: colors represent violations of different assumptions in the wrong model") +
  scale_x_continuous(breaks=c(250, 500, 750, 1000)) + 
  theme(legend.position="bottom",
        plot.title = element_text(hjust = 0.5)) +
  # guides(colour = guide_legend(ncol=2,nrow=6,byrow=TRUE))
  guides(color = "none")

df_backdoor_frontdoor_iv_alt_all
ggsave("v2.backdoor_frontdoor_iv_alternative_all.png", path =  paste0(prefix, "plots/"),
       width = 15, height = 15, units = "cm")


# BACKDOOR + IV
# UNDER THE NULL
df_bdoor_iv_null_all <- df_bdoor_iv %>% filter(hypothesis == "N") %>% 
  mutate(nonzero_functionals = ifelse(backdoor_true_functional == 1|iv_true_functional == 1, TRUE, FALSE)) %>% 
  ggplot(., aes(x = sample_size, y = size, color = model_status, linetype = nonzero_functionals)) + 
  geom_line() + 
  ggh4x::facet_nested("IV" + iv ~ "Backdoor" + backdoor) +
  geom_hline(aes(yintercept=0.05), linetype="dashed", color = "black") +
  labs(x = "Sample Size",
       y = "Type I Error Rate",
       title= "Backdoor + IV, Under the Null (\u03b1 = 0.05)",
       linetype = "Non-Zero Identified Functional In Wrong Model",
       caption = "Note: colors represent violations of different assumptions in the wrong model") +
  scale_x_continuous(breaks=c(250, 500, 750, 1000)) + 
  theme(legend.position="bottom",
        plot.title = element_text(hjust = 0.5)) +
  guides(color = "none")

df_bdoor_iv_null_all
ggsave("v2.backdoor_iv_null_all.png", path =  paste0(prefix, "plots/"),
       width = 15, height = 15, units = "cm")

# UNDER THE ALTERNATIVE
df_bdoor_iv_alt_all <- df_bdoor_iv %>% filter(hypothesis == "A") %>% 
  mutate(nonzero_functionals = ifelse(backdoor_true_functional == 1 & iv_true_functional == 1, TRUE, FALSE)) %>% 
  ggplot(., aes(x = sample_size, y = power, color = model_status, linetype = nonzero_functionals)) + 
  geom_line() +
  ggh4x::facet_nested("IV" + iv ~ "Backdoor" + backdoor) +
  labs(x = "Sample Size", y = "Power",
       title= "Backdoor + IV, Under the Alternative (\u03b1 = 0.05)",
       linetype = "Non-Zero Identified Functional In Wrong Model",
       caption = "Note: colors represent violations of different assumptions in the wrong model") +
  scale_x_continuous(breaks=c(250, 500, 750, 1000)) + 
  theme(legend.position="bottom",
        plot.title = element_text(hjust = 0.5)) +
  guides(color = "none")

df_bdoor_iv_alt_all
ggsave("v2.backdoor_iv_alternative_all.png", path =  paste0(prefix, "plots/"),
       width = 15, height = 15, units = "cm")


# FRONTDOOR + IV
# UNDER THE NULL
df_fdoor_iv_null_all <- df_fdoor_iv %>% filter(hypothesis == "N") %>% 
  mutate(nonzero_functionals = ifelse(frontdoor_true_functional == 1|iv_true_functional == 1, TRUE, FALSE)) %>% 
  ggplot(., aes(x = sample_size, y = size, color = model_status, linetype = nonzero_functionals)) + 
  geom_line() + 
  ggh4x::facet_nested("IV" + iv ~ "Frontdoor" + frontdoor) +
  geom_hline(aes(yintercept=0.05), linetype="dashed", color = "black") +
  labs(x = "Sample Size",
       y = "Type I Error Rate",
       title= "Frontdoor + IV, Under the Null (\u03b1 = 0.05)",
       linetype = "Non-Zero Identified Functional In Wrong Model",
       caption = "Note: colors represent violations of different assumptions in the wrong model") +
  scale_x_continuous(breaks=c(250, 500, 750, 1000)) + 
  theme(legend.position="bottom",
        plot.title = element_text(hjust = 0.5)) +
  guides(color = "none")

df_fdoor_iv_null_all
ggsave("v2.frontdoor_iv_null_all.png", path =  paste0(prefix, "plots/"),
       width = 15, height = 15, units = "cm")

# UNDER THE ALTERNATIVE
df_fdoor_iv_alt_all <- df_fdoor_iv %>% filter(hypothesis == "A") %>% 
  mutate(nonzero_functionals = ifelse(frontdoor_true_functional == 1 & iv_true_functional == 1, TRUE, FALSE)) %>% 
  ggplot(., aes(x = sample_size, y = power, color = model_status, linetype = nonzero_functionals)) + 
  geom_line() +
  ggh4x::facet_nested("IV" + iv ~ "Frontdoor" + frontdoor) +
  labs(x = "Sample Size", y = "Power",
       title= "Frontdoor + IV, Under the Alternative (\u03b1 = 0.05)",
       linetype = "Non-Zero Identified Functional In Wrong Model",
       caption = "Note: colors represent violations of different assumptions in the wrong model") +
  scale_x_continuous(breaks=c(250, 500, 750, 1000)) + 
  theme(legend.position="bottom",
        plot.title = element_text(hjust = 0.5)) +
  guides(color = "none")


df_fdoor_iv_alt_all
ggsave("v2.frontdoor_iv_alternative_all.png", path =  paste0(prefix, "plots/"),
       width = 15, height = 15, units = "cm")


