# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# This script implement's a Gibb's sampler like in HW3Problem1.R but treats n #
#   as an unknown parameter with a Poisson prior and conditional distrib      #
#                                                                             #
# Author: April Walker                                                        #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

#
# bivariate gibbs: adapted from Dr. Datta's normal_gibbs to handle bivariate
#   samples
#
#   input--
#   alpha.0: beta distrib's inital 'alpha' parameter
#   beta: beta distrib's inital 'beta' parameter
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
