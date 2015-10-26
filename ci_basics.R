library(dplyr)
library(ggplot2)

# Produces a data_frame with normal density values for plotting
norm_dens <- function(mu = 0, sigma = 1) {
  x <- seq(mu - 5*sigma, mu + 5*sigma, length.out = 1000)
  data_frame(x=x, density=dnorm(x, mu, sigma))
}

mu <- 20
sigma <- 5

(pop <- ggplot(norm_dens(mu, sigma), aes(x=x, y=density)) + geom_line())

pop + 
  geom_line(aes(x=x, y=density), data=norm_dens(mu, sigma/sqrt(30))) +
  geom_line(aes(x=x, y=density), data=norm_dens(mu, sigma/sqrt(120)))



