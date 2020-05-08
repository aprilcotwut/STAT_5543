# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# This script samples rv's from the PDF p(x) \propto exp(-x^4/12) x in reals  #
#                                                                             #
# Author: April Walker                                                        #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

lambda <- 1/12; alpha <- 1/4
our_pdf <- function(x){ exp(-lambda*x^(1/alpha))/
integrate(function(x) exp(-lambda*x^(1/alpha)),-Inf,Inf)$value }


#
# custom_rej_rvgen: uses rejection sampling method to gen rvs
#   input--
#   n: integer value corresponding to the number of rv samples wanted
#   mu: numerical value giving the mean of the outer normal distrib
#   sd: numerical value giving the standard deviation of the outer normal distrib
#   M: a value with which to scale your distribution
#   pdf: a string corresponding to the pdf function
#
#   output--
#   x: an array size n of numerical rv samples
custom_rej_rvgen <- function(n = 1, mu, sd, M, pdf) {
  x <- c()
  N <- 0
  while(length(x) < n) {
    # draw u ~ unif(0,1)
    u <- runif(1)
    # draw canidate from norm distrib to be scaled to exceed p(x)
    x_i <- rnorm(1, mu, sd)
    # if M scaled uniform distrib < p(x_i), accept
    if (M*dnorm(x_i, mu, sd)*u < get(pdf)(x_i)) {
      x <- append(x, x_i)
    }
    N <- N + 1
  }
  cat("Efficiency is ", 100*(n/N), " % \n")
  return(x)
}

# # #
# Rejection RV Sampling Method
# # #
M <- 1.3 # value found trial/error on Desmos
sd <- 1.4 # valye found trial/error on Desmos
# generate random samples using rejection sampling
z <- custom_rej_rvgen(5e4, mu = 0, sd, M, pdf = "our_pdf")
# plot histogram
jpeg("rej_hist_p2.jpg", width = 700, height = 700)
hist(z, breaks = 50, freq = F, main="Histogram of X Using Rejection Sampling",
     ylim = c(0, 0.5), col = "#7B9E89")
 # and plot the pdf
curve(our_pdf, -4, 4, type = "l", lwd = 2, add = T)
# also plot our outer bound from rejecton sampling
M_curve <- function(x) { M*dnorm(x, 0, sd) }
curve(M_curve, -4, 4, type = "l", lty = 2, lwd = 2, add = T)
legend('topleft',c("M*N(x)","p(x)"),cex=.9,col=c("black","black"),lty=c(2,1))
dev.off()
