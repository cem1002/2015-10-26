norm_dens <- function(mu = 0, sigma = 1) {
  x <- seq(mu - 5*sigma, mu + 5*sigma, length.out = 1000)
  data_frame(x=x, density=dnorm(x, mu, sigma))
}

t_dens <- function(df) {
  x <- seq(-5, 5, length.out = 1000)
  data_frame(x=x, density=dt(x, df))
}

(n01 <- ggplot(aes(x=x, y=density), data=norm_dens(0,1)) + geom_line())

ggplot(aes(x=x, y=density), data=t_dens(5)) + geom_line()

n01 + 
  geom_line(aes(x=x, y=density), data=t_dens(5), color=2) +
  geom_line(aes(x=x, y=density), data=t_dens(10), color=3) +
  geom_line(aes(x=x, y=density), data=t_dens(30), color=5)
  

