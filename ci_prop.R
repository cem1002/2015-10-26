library(dplyr)
library(ggplot2)

ci_prop <- function(samples, confidence = 0.95) {
  size <- sapply(samples, length)
  z <- -qnorm((1 - confidence)/2)
  means <- sapply(samples, mean)
  sds <- sqrt(means*(1-means))
  err <- sds/sqrt(n)
  return(data_frame(sample_no = 1:length(samples),
                    sample_means = means,
                    ymin = means - z*err,
                    ymax = means + z*err))
}

k <- 10000
n <- 92

# 2spooky4me
# n <- 97

p <- 0.2

bern_data <- replicate(k, data_frame(sample(c(1,0), size = n, replace = TRUE, 
                                 prob = c(p, 1-p))))

ci <- ci_prop(bern_data)

covered <- covered <- ci$ymin < p & ci$ymax > p

mean(covered)


ggplot(ci, aes(x=sample_no, y=sample_means, ymin=ymin, ymax=ymax)) +
  geom_pointrange(aes(color=factor(as.numeric(covered)))) +
  geom_hline(aes(yintercept=p)) + 
  coord_flip() +
  theme(legend.position="none")
