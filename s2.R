library(dplyr)
library(ggplot2)
# Sample size
n <- 50

# Number of replications
k <- 5000

true_mean <- 30
true_var <- 25

list_norm <- replicate(k, 
                       data_frame(x=rnorm(n, mean = true_mean, 
                                          sd = sqrt(true_var))))

s2_n <- sapply(list_norm, function(x) var(x)*(n-1)/n)
mean(s2_n)

s2_n_1 <- sapply(list_norm, function(x) var(x))
mean(s2_n_1)

