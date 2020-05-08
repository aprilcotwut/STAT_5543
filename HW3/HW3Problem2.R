# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# This script implement's a Gibb's sampler like in HW3Problem1.R but treats n #
#   as an unknown parameter with a Poisson prior and conditional distrib      #
#                                                                             #
# Author: April Walker                                                        #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

#
# unknown_param gibbs: adapted from Dr. Datta's normal_gibbs to handle gibbs samples
#   for the specific scenario discussed above
#
#   input--
#   alpha.0: beta distrib's inital 'alpha' parameter
#   beta.0: beta distrib's inital 'beta' parameter
#   s.0: initial value for s~binom samples
#   lambda: param for n~pois(lambda)
#   niter: number of samples wanted
#   burn.in: number of samples to toss
#
#   output--
#   list$s: the gibbs samples for s
#   list$theta: the gibbs samples for theta
#   list$n: the gibbs samples for n
unknown_param_gibbs<- function(alpha.0, beta.0, s.0, lambda, niter, burn.in=0) {
  s <- theta <- n <- rep(0, niter+burn.in)
  # set first value as initial given value...
  s[1] <- s.0
  n[1] <- rpois(1, lambda)
  theta[1] <- s[1]/n[1]
  for (i in 2:(niter+burn.in)) {
    # as defined in background sample from s(i) and theta(i)
    s[i] = rbinom(1, n[i-1], theta[i-1])
    theta[i] = rbeta(1,alpha.0 + s[i], beta.0 + n[i-1] - s[i])
    n[i] = s[i] + rpois(1, (1 - theta[i]) * lambda)
  }
  return(list(s = s[-(1:burn.in)],
              theta = theta[-(1:burn.in)],
              n = n[-(1:burn.in)]))
}

m <- 1e5 # number of samples we want

s.0 <- 16 # taken from background observed data
alpha.0 <- 2.0 # as defined in background as our prior
beta.0 <- 6.4 # as defined in background as our prior
lambda <- 74 # as defined in problem 2

samp <- unknown_param_gibbs(alpha.0, beta.0, s.0, lambda, niter=m, burn.in=1000)

### Histogram (alt kind)
jpeg("hist_s_prob2.jpg")
plot(table(samp$s)/m, main = "PMF for S")
dev.off()

### Posterior Median
cat("Posterior median of theta:    ", median(samp$theta), "\n \n")
# out of curiosity...
cat("Posterior median of n:    ", median(samp$n), "\n")
cat("Posterior median of s:    ", median(samp$s), "\n")

### Trace plots
jpeg("trace_s_prob2.jpg")
plot(samp$s, type = "s", main = "Trace Plot for S", ylim = c(min(samp$s)-1,max(samp$s)+1))
dev.off()

jpeg("trace_theta_prob2.jpg")
plot(samp$theta, type = "s", main = "Trace Plot for Theta", ylim = c(min(samp$theta)-0.1,max(samp$theta)+0.1))
dev.off()

jpeg("trace_n_prob2.jpg")
plot(samp$n, type = "s", main = "Trace Plot for N", ylim = c(min(samp$n)-0.5,max(samp$n)+0.5))
dev.off()
