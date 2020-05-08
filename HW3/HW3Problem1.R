# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# This script implement's a Gibb's sampler for a bivariate samples from a     #
#   joint density (s, \theta) where s has a Binomial conditional distrib and  #
#   and \theta has a Beta conditional distrib
#                                                                             #
# Author: April Walker                                                        #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

#
# bivariate gibbs: adapted from Dr. Datta's normal_gibbs to handle bivariate
#   samples
#
#   input--
#   alpha.0: beta distrib's inital 'alpha' parameter
#   beta.0: beta distrib's inital 'beta' parameter
#   s.0: initial value for s~binom samples
#   theta.0: initial value for theta~beta samples
#   niter: number of samples wanted
#   burn.in: number of samples to toss
#
#   output--
#   list$s: the gibbs samples for s
#   list$theta: the gibbs samples for theta
bivariate_gibbs<- function(alpha.0, beta.0, s.0, theta.0, niter, burn.in=0) {
  s <- theta <- rep(0, niter+burn.in)
  n <- as.integer(round(s.0/theta.0))
  # set first value as initial given value...
  s[1] <- s.0
  theta[1] <- theta.0
  for (i in 2:(niter+burn.in)) {
    # as defined in background sample from s(i) and theta(i)
    s[i] = rbinom(1, n, theta[i-1])
    theta[i] = rbeta(1,alpha.0 + s[i], beta.0 + n - s[i])
  }
  return(list(s = s[-(1:burn.in)], theta = theta[-(1:burn.in)])) # bivariate samples
}

m <- 1e5 # number of samples we want

n <- 74 # taken from background observed data
s.0 <- 16 # taken from background observed data
theta.0 <- s.0/n # MLE for theta from observed data
alpha.0 <- 2.0 # as defined in background as our prior
beta.0 <- 6.4 # as defined in background as our prior

samp <- bivariate_gibbs(alpha.0, beta.0, s.0, theta.0, niter=m, burn.in=1000)

### Trace plots
jpeg("trace_s_prob1_alt.jpg")
plot(samp$s, type = "s", main = "Trace Plot for S", ylim = c(min(samp$s)-1,max(samp$s)+1))
dev.off()

jpeg("trace_theta_prob1_alt.jpg")
plot(samp$theta, type = "s", main = "Trace Plot for Theta", ylim = c(min(samp$theta)-0.1,max(samp$theta)+0.1))
dev.off()

### Histogram (alt kind)
jpeg("hist_s_prob1.jpg")
plot(table(samp$s)/m, main = "PMF for S")
dev.off()

### Posterior Median
cat("MLE of theta:                 ", s.0/n, "\n")
cat("Posterior median of theta:    ", median(samp$theta), "\n \n ")

### Test sensitivity with alternative samples...
samp2 <- bivariate_gibbs(alpha.0, beta.0, s.0, theta.0*.5, niter=m, burn.in=1000)
samp3 <- bivariate_gibbs(alpha.0, beta.0, s.0, theta.0*(2/74), niter=m, burn.in=1000)
samp4 <- bivariate_gibbs(alpha.0, beta.0, s.0, theta.0*2, niter=m, burn.in=1000)

cat("Posterior median of with theta.0 < MLE theta:     ", median(samp2$theta), "\n")
cat("Posterior median of with theta.0 << MLE theta:    ", median(samp3$theta), "\n")
cat("Posterior median of with theta.0 > MLE theta:     ", median(samp4$theta), "\n \n")

# out of curiosity...
cat("Posterior median of s:    ", median(samp$s), "\n")

### Trace plots inaccurate theta
samp <-samp3

jpeg("trace_s_prob1_alt.jpg")
plot(samp$s, type = "s", main = "Trace Plot for S Using Inaccurate Initial Theta", ylim = c(min(samp$s)-1,max(samp$s)+1))
dev.off()

jpeg("trace_theta_prob1_alt.jpg")
plot(samp$theta, type = "s", main = "Trace Plot for Theta Using Inaccurate Initial Theta", ylim = c(min(samp$theta)-0.1,max(samp$theta)+0.1))
dev.off()
