# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# This script samples rv's from the PDF p(x)=sin(x), x in (0, pi/2)           #
#                                                                             #
# Author: April Walker                                                        #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

### Derivation of F^{-1}(x):
# F(x) = \int_0^x p(u) = \int_0^x sinu = 1 - cosx
# F^{-1}(x) = arccos(1-x) and for x in (0, 1), F^{-1} in (0, pi/2)
our_inverse_cdf <- function(x) { acos(1-x) }

#
# custom_fdc_rvgen: uses the inverse CDF transform to gen rvs (cdf^-1 = fdc, math)
#   input--
#   n: integer value corresponding to the number of rv samples wanted
#   inverse_cdf: a string corresponding to a function which contains the fdc
#
#   output--
#   x: an array size n of numerical rv samples
custom_fdc_rvgen <- function(n = 1, inverse_cdf = "our_inverse_cdf") {
  # draw u ~ unif(0,1)
  u <- runif(n)
  # draw x ~ fdc
  x <- get(inverse_cdf)(u)
  return(x)
}

#
# custom_rej_rvgen: uses rejection sampling method to gen rvs
#   input--
#   n: integer value corresponding to the number of rv samples wanted
#   a: numerical value indicating the lower bound of x
#   b: numerical value indicating the upper bound of x
#   M: a value with which to scale your distribution
#   pdf: a string corresponding to the pdf function
#
#   output--
#   x: an array size n of numerical rv samples
custom_rej_rvgen <- function(n = 1, a, b, M, pdf) {
  x <- c()
  N <- 0
  while(length(x) < n) {
    # draw u ~ unif(0,1)
    u <- runif(1)
    # draw canidate from unif distrib to be scaled to exceed p(x)
    x_i <- runif(1, min = a, max = b)
    # if M scaled uniform distrib < p(x_i), accept
    if (M*dunif(x_i, a, b)*u < get(pdf)(x_i)) {
      x <- append(x, x_i)
    }
    N <- N + 1
  }
  cat("Efficiency is ", 100*(n/N), " % \n")
  return(x)
}

# # #
# Inverse CDF Transformation RV Generation
# # #

# generate random samples using inverse CDF transformation
z <- custom_fdc_rvgen(1e5, "our_inverse_cdf")
# plot histogram
jpeg("fdc_hist.jpg")
hist(z, breaks = 50, freq = F, main="Histogram of X Using Inverse CDF Transformation",
     col = "#7B9E89")
# and plot the typical sin cruve on 0 to pi/2
curve(sin, 0, pi/2, type = "l", add = T)
dev.off()

# # #
# Rejection RV Sampling Method
# # #
M <- 1.6 # This works for our given x range
# generate random samples using rejection sampling
z <- custom_rej_rvgen(1e5, a = 0, b = pi/2, M, pdf = "sin")
# plot histogram
jpeg("rej_hist_p1.jpg")
hist(z, breaks = 50, freq = F, main="Histogram of X Using Rejection Sampling",
     col = "#7B9E89")
 # and plot the typical sin cruve on 0 to pi/2
 curve(sin, 0, pi/2, type = "l", add = T)
 dev.off()
