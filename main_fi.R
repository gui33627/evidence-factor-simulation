# summary table of the test/power as a function of sample size

library(mgcv)
prefix <- "/home/junhuiyang/Documents/simulations.v7.hpc/"
prefix_output <- "/home/junhuiyang/Documents/Outputs_new/"
sapply(list.files(pattern=".R", path = paste0(prefix, "R"), full.names = TRUE), source)

# backdoor: TRUE-backdoor model is true; FALSE-backdoor model is false; NA-backdoor model is not in the union model
# front door: TRUE-frontdoor model is true; FALSE-front door model is false; NA-frontdoor model is not in the union model
# iv: TRUE-iv model is true; FALSE-iv model is false; NA-iv model is not in the union model
# backdoor_true_functional, frontdoor_true_functional, iv_true_functional: 0-is zero, 1-is non-zero, NA-is not in the union model
# hypothesis: 'N'-under the null (true null); 'A'-under the alternative (true alternative)
# sample_size: the sample size in the simulation
# value: size under the null, and power under the alternative

set.seed(123)
# simulation times
N <- 1000
# sample size
sample_size <- c(250, 500, 750, 1000)

for (n in sample_size) {
  
  source(paste0(prefix,"evidence_factors_frontdoor_iv.R"))
  assign(paste0("df_frontdoor_iv_", n), 
         cbind(backdoor = rep(NA, length(beta)), frontdoor = frontdoor_values, iv = iv_values,
               backdoor_true_functional = rep(NA, length(beta)), 
               frontdoor_true_functional = frontdoor_true_functional,
               iv_true_functional = iv_true_functional, 
               b1 = rep(NA, length(beta)), b2 = rep(NA, length(beta)), 
               f1 = f1, f2 = f2, f3 = f3, 
               i1 = i1, i2 = i2, i3 = i3, i4 = i4, 
               hypothesis = hypothesis, beta = beta, sample_size = rep(n, length(beta)), 
               size = size_values, power = power_values))
  write.csv(get(paste0("df_frontdoor_iv_", n)), 
            file = paste0(prefix_output, "df_frontdoor_iv_", n, ".csv"))
  
}









