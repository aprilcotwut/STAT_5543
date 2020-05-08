# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# This script implement's a Gibb's sampler for Problem 2 of Exam 2            #
#                                                                             #
# Author: April Walker                                                        #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
library(invgamma)

#
# gibbs_sampler: adapted from Dr. Datta's normal_gibbs to handle gibbs samples
#   for the specific scenario discussed above
#
#   input--
#   x: inital 'x' samples for x~N(theta, var)
#   a: param for sigma^2~InvGamma(a,b)
#   b: param for sigma^2~InvGamma(a,b)
#   tau.sq: param for
#   niter: number of samples wanted
#   burn.in: number of samples to toss
#
#   output--
#   list$sig: the gibbs samples for sig
#   list$theta: the gibbs samples for theta
gibbs_sampler <- function(theta.0, x, a, b, tau.sq, niter, burn.in=0) {
  theta <- sig <- rep(0, niter+burn.in)
  n <- length(x)
  # declare first value based on expected value
  sig[1] <- var(x)
  theta[1] <- mean(x)

  for (i in 2:(niter+burn.in)) {
    # sample for theta
    mu.i <- ((sig[i-1]*theta.0) + (n*tau.sq*mean(x)))/(sig[i-1]+n*tau.sq)
    var.i <- (sig[i-1]*tau.sq)/(sig[i-1]+n*tau.sq)
    theta[i] <- rnorm(1, mean=mu.i, sd=sqrt(var.i))
    # sample for sigma^2
    sig[i] <- rinvgamma(1, shape= n/2 + a, rate = 0.5*sum((x-theta[i])^2) + b)
  }
  return(list(sig = sig[-(1:burn.in)],
              theta = theta[-(1:burn.in)]))
}

x <- c(91, 504, 557, 609, 693, 727, 764, 803, 857, 929, 970, 1043, 1089,
           1195, 1384, 1713)

m <- 1e5 # number of samples we want

a <- b <- 3
theta.0 <-
tau.sq <- 10

n <- length(x)

samp <- gibbs_sampler(theta.0, x, a, b, tau.sq, niter=m, burn.in=1000)

### Trace plots
jpeg("trace_sigmasq_exam2_alt.jpg")
plot(samp$sig, type = "s", main = "Trace Plot for Sigma^2", ylim = c(min(samp$sig)-1,max(samp$sig)+1))
dev.off()

jpeg("trace_theta_exam2_alt.jpg")
plot(samp$theta, type = "s", main = "Trace Plot for Theta", ylim = c(min(samp$theta)-0.1,max(samp$theta)+0.1))
dev.off()

### Histogram (alt kind)
jpeg("hist_sigmasq_exam2.jpg")
hist(na.omit(log(samp$sig)), breaks = 100,freq = F, col = "#3003FF", main = "Log(sigma^2) Gibbs Histogram")
dev.off()

jpeg("hist_theta_exam2.jpg")
hist(na.omit(log(samp$theta)), breaks = 100,freq = F, col = "#3003FF", main = "Log(theta) Gibbs Histogram")
dev.off()

# 90% Posterior probability internals
quantile(log(samp$sig), prob=c(0.05, 0.95), na.rm=T)
quantile(log(samp$theta), prob=c(0.05, 0.95), na.rm=T)
