# # # #
# Problem 1: Stuff w/ a trucated distribution...
#
# Author: A Walker
# # # #
library(ggplot2)

# 1: How would you generate from an exponential trucated interval to the
#    interval [0,b] using accept-reject scheme? Try for lambda = 2, b = 5
N <- 1e3
u <- runif(N)
lambda <- 2; a <- 0; b <- 5

# Map to standard exponential distrib...
x <- (-1/lambda)*log(1-u*(1-exp(-lambda*b)))
x <- -1/lambda*log(u)
x <- x[x<=b] # Then truncate...

# Then plot
ggplot(data.frame(x))+geom_histogram(aes(x=x), bins=50)

# 2: Can you use the inverse CDF transformation here? (yes)
N <- 1e3
u <- runif(N)
lambda <- 2; b <- 5 # we don't need a, see math

# By math we can get the inverse with...
x <- (-1/lambda)*log(1-u*(1-exp(-lambda*b)))
ggplot(data.frame(x))+geom_histogram(aes(x=x), bins=50)
