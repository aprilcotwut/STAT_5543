# # # #
# Problem 1: Sampling from a gamma distribution...
#
# Author: A Walker
# # # #


# Since f(x) = exp{-x^6/10} and gamma is f(x) = c*exp{-lambda*x^{1/alpha}}...
lambda <- 1/10; alpha <- 1/6
N <- 1e5 # number of samples to generate
y <- rgamma(N, alpha, lambda)
z <- y^alpha


hist(z, breaks = 30, freq = F, col = rgb(0.75,0.4,0.1,0.5)) # z is your sample

target <- function(x){exp(-lambda*x^(1/alpha))/
integrate(function(x) exp(-lambda*x^(1/alpha)),0,Inf)$value}
curve(target,lwd=2,add=T)
