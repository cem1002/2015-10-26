library(dplyr)
library(ggplot2)

ci_sigma_unknown_1 <- function(samples, confidence = 0.95) {
  size <- sapply(samples, length)
  z <- -qnorm((1 - confidence)/2)
  means <- sapply(samples, mean)
  sds <- sapply(samples, sd)
  err <- sds/sqrt(n)
  return(data_frame(sample_no = 1:length(samples),
                    sample_means = means,
                    ymin = means - z*err,
                    ymax = means + z*err))
}

k <- 100
n <- 20
mu <- 10
sigma <- 5

norm_data <- replicate(k, data_frame(rnorm(n, mu, sigma)))

ci <- ci_sigma_unknown_1(norm_data)

covered <- ci$ymin < mu & ci$ymax > mu

mean(covered)

ggplot(ci, aes(x=sample_no, y=sample_means, ymin=ymin, ymax=ymax)) +
  geom_pointrange(aes(color=factor(as.numeric(covered)))) +
  geom_hline(aes(yintercept=10)) + 
  coord_flip() +
  theme(legend.position="none")

# Everyhing done properly
ci_sigma_unknown_correct <- function(samples, confidence = 0.95) {
  size <- sapply(samples, length)
  pm_val <- -qt((1 - confidence)/2, size - 1)
  means <- sapply(samples, mean)
  sds <- sapply(samples, sd)
  err <- sds/sqrt(n)
  return(data_frame(sample_no = 1:length(samples),
                    sample_means = means,
                    ymin = means - pm_val*err,
                    ymax = means + pm_val*err))
}

ci <- ci_sigma_unknown_correct(norm_data)

covered <- ci$ymin < mu & ci$ymax > mu

mean(covered)

ggplot(ci, aes(x=sample_no, y=sample_means, ymin=ymin, ymax=ymax)) +
  geom_pointrange(aes(color=factor(as.numeric(covered)))) +
  geom_hline(aes(yintercept=10)) + 
  coord_flip() +
  theme(legend.position="none")


