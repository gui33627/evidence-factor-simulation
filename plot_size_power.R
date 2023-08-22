
library(tidyverse)
prefix <- "/Users/junhui/Library/CloudStorage/GoogleDrive-junhuiyang@umass.edu/My Drive/UMASS/Courses/Ted_research/simulations.v8.hpc/results/"

df_backdoor_frontdoor_null_all <- read.csv(paste0(prefix, "df_backdoor_frontdoor_null.csv"))
df_backdoor_frontdoor_alt_all <- read.csv(paste0(prefix, "df_backdoor_frontdoor_alt.csv"))
df_backdoor_frontdoor_iv_null_all <- read.csv(paste0(prefix, "df_backdoor_frontdoor_iv_null.csv"))
df_backdoor_frontdoor_iv_alt_all <- read.csv(paste0(prefix, "df_backdoor_frontdoor_iv_alt.csv"))
df_bdoor_iv_null_all <- read.csv(paste0(prefix, "df_bdoor_iv_null.csv"))
df_bdoor_iv_alt_all <- read.csv(paste0(prefix, "df_bdoor_iv_alt.csv"))
df_fdoor_iv_null_all <- read.csv(paste0(prefix, "df_fdoor_iv_null.csv"))
df_fdoor_iv_alt_all <- read.csv(paste0(prefix, "df_fdoor_iv_alt.csv"))

################################################################################
# BACKDOOR + FRONTDOOR
# UNDER THE NULL
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


