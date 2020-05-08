# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# This script samples rv's from the PDF p(x) \propto f(x) where f(x) is the   #
#   function discussed in Edelman (1988),                                     #
#              f(x) = (1 + sqrt(y))/(2*sqrt(y)) exp(-(y/2+sqrt(y)))           #                                                                      #
# Author: April Walker                                                        #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
E_1988 <- -1.68788
# number of samples
n <- 1e6
# a: controls how unlikely your lower bound is, with P(Y < lower bound)*n = a
#    higher a is more prone to errors
a <- 0.5

### First define f(y) and the scaled distribution
f_y <- function(y) { ((1+sqrt(y))/(2*sqrt(y)))*(exp(-(sqrt(y)+y/2))) }
p_y <- function(y) { f_y(y)/integrate(f_y, 0, Inf)$value }
# theta derivation in paper st we can sample from ~ Expo(1)
theta_y <- function(y) { (y/2)+(sqrt(y)) }

### To get my lower bound, I decided to find the value of which there is a
#   a 1/1e6 chance of something that value or lower occuring. I wanted this
#   in terms of machine eps cause why not
M <- uniroot(function(x) {0.5 - integrate(p_y, 0, .Machine$double.eps^x)$value*n},
             lower = 0.5, upper = 1)$root

### Gives inverse, alternatively GoFKernel:: has a good inverse()
#   The lower = .Machine$double.eps^M guarentees no zero values
#   The tol = .Machine$double.eps^0.5 allows more small values in our sample
inverse <- function (f) {
  function (y) uniroot((function (x) { f(x) - y }), lower = .Machine$double.eps^M,
                        upper = 1e9, tol = .Machine$double.eps^0.5)[1]$root
}

### Get the inverse analytically
theta_inv = function(theta) { as.numeric(lapply(theta, inverse(theta_y))) }

#
# custom_fdc_rvgen: uses the inverse CDF transform to gen rvs (cdf^-1 = fdc, math)
#   input--
#   n: integer value corresponding to the number of rv samples wanted
#   inverse_cdf: a string corresponding to a function which contains the fdc
#
#   output--
#   x: an array size n of numerical rv samples
custom_fdc_rvgen <- function(n = 1, inverse_cdf = "theta_inv") {
  # draw u ~ expo(1)
  u <- rexp(n, rate=1)
  # draw x ~ fdc
  x <- get(inverse_cdf)(u)
  return(x)
}

### RV generation
s <- proc.time()
z <- custom_fdc_rvgen(n)
cat(proc.time() - s, "\n")

### Plot!
jpeg("hist_p3_alt_again.jpg")
hist(z, breaks = 150, freq = F, main="Histogram of Y", xlab = "Y",
     col = "#7B9E89", xlim = c(0,5))
curve(p_y, 0, 8, lwd = 2, add = T)
dev.off()

### Monte Carlo E(log(Y)) \approx 1/n sum_i=1^n log(y_i)
E_hat <- mean(log(z))
cat("Approx expectation is ", E_hat, "\n")
cat("Difference from Edelman is ", abs(E_hat - E_1988), "\n")
# Var(log(y)) \approx 1\(n-1) sum((E_hat - y_i)^2)
Var_hat <- var(log_y)
CI <- E_hat + c(-1,1)*(2.576*sqrt(Var_hat))/sqrt(length(log_y))
cat(CI, "\n")
