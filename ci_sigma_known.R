library(dplyr)
library(ggplot2)

ci_sigma_known <- function(samples, true_sd, confidence = 0.95) {
  size <- sapply(samples, length) # Vector of sample sizes n
  pm_val <- -qnorm((1 - confidence)/2) # Constant 1.96 approx
  means <- sapply(samples, mean) # Vector of sample averages
  err <- true_sd/sqrt(size)
  return(data_frame(sample_no = 1:length(samples),
                    sample_means = means,
                    ymin = means - pm_val*err,
                    ymax = means + pm_val*err))
}

k <- 10000
n <- 20
mu <- 10
sigma <- 5

norm_data <- replicate(k, data_frame(rnorm(n, mu, sigma)))

ci <- ci_sigma_known(norm_data, true_sd = sigma)

covered <- ci$ymin < mu & ci$ymax > mu

mean(covered)

ggplot(ci, aes(x=sample_no, y=sample_means, ymin=ymin, ymax=ymax)) +
  geom_pointrange(aes(color=factor(as.numeric(covered)))) +
  geom_hline(aes(yintercept=10)) + 
  coord_flip() +
  theme(legend.position="none")

coverage(ci, 10)
