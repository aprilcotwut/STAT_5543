# # # #
# Probelm 1
# # # #

diamond <- function(n){
  xi = rep(0,n)
  yi = rep(0,n)
  N = 0
  for (i in 1:n){
    x = runif(1,min = -1,max = 1)
    y = runif(1,min = -1, max = 1)
    if (abs(x) + abs(y) <= 1){
      xi[i] = x
      yi[i] = y
      N = N + 1
    }
  }

  cat("Efficiency is ", N/n)
  plot(xi,yi)
  title("Uniform on a Diamond")
}

# # # #
# Problem 2
# # # #
i <<- 0
f <- function(n) {
  if (n > 1) {
    # prints a line each time
    # cat("still going\n")
    i <<- i + 1
    # spits two subprograms with half with input size
    f(n/2)
    f(n/2)
  }
}
# Using Master Theorem, n^{log2(2)} = O(n)
# However only considering log2(n) being an integer
# We can count down the tree to get the # of runs
#              f(n)             @ n=2, 1
#             /    \
#         f(n/2)   f(n/2)       @ n=4, 1+2
#         /   \     /   \
#   f(n/4) f(n/4) f(n/4) f(n/4) @ n=8, 4+2+1
# Its n-1 in that case
# I think for n %% 2 = 0
# it's 2^ceiling(log2(n)) - 1
for (j in seq(2, 1000, 2)) {
  i <<- 0
  f(j)
  if (i != 2^ceiling(log2(j)) - 1) {
    print("wrong")
  }
}

# # # #
# Problem 3
# # # #

#f(x) is normal

#g(x) is cauchy
g_prop <- function(x) { 1/(1+x^2) }
# \int_{-infty}^x g(x) dx = arctan(x) + pi/2
G_prop <- function(x) { atan(x) + pi/2 }
# \int_{-infty}^{infty} g(x) dx = pi
g <- function(x) { g_prop(x)/pi }
G <- function(x) {G_prop(x)/pi}

p <- function(x) { (1/(2*pi))*exp((-1/2)*x^2) }

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

u <- 0.632 # assume this is uniform generated
x_i <- 1.36 # assume this x_i is drawn

M <- 1.52

if (M*g(x_i)*u < p(x_i)) {
  cat("Accepted!")
} else {
  cat("Rejected!")
}



# # # #
# Problem 4
# # # #

### Timing:
#                T(n)
#           /     |     \
#         T(n/3) T(n/3) T(n/3)
#           ...    ...    ...
# We are given 3 n/3 subproblems and combining
# them in linear time
#
# T(n) = 3T(n/3) + O(n)
#
# By the Master Theorem since log_3(3) = 1, T(n) = O(nlogn) like w/ standard
# merge sort... I believe technically since threeway is log_3(n) vs log_2(n) it
# seems faster, but because you have to make more comparisons... it's really
# closer to c_1*log_2(n) vs c2_log_3(n) where c1 < c2... so it's negligable



threeway_merge <- function(a, b, c) {
  m <- length(a); n <- length(b); o <- length(c)
  d <- rep(NA, m + n + o)

  it <- 1

  # a, b, c all avaliable
  i <- j <- k <- 1
  while ((i <= m) & (j <= n) & (k <= o)) {
    loc <- which.min(c(a[i], b[j], c[k]))
    if (loc == 1) {
      d[it] <- a[i]; i <- i + 1; it <- it + 1;
    } else if (loc == 2) {
      d[it] <- b[j]; j <- j + 1; it <- it + 1;
    } else {
      d[it] <- c[k]; k <- k + 1; it <- it + 1;
    }
  }

  print(d)

  # a is empty
  if (i > m) {
    while ((j <= n) & (k <= o)) {
      if (b[j] < c[k]) {
        d[it] <- b[j]; j <- j + 1; it <- it + 1;
      } else {
        d[it] <- c[k]; k <- k + 1; it <- it + 1;
      }
    }
  }

  # b is empty
  if (j > n) {
    while ((i <= m) & (k <= o)) {
      if (a[i] < c[k]) {
        d[it] <- a[i]; i <- i + 1;  it <- it + 1;
      } else {
        d[it] <- c[k]; k <- k + 1;  it <- it + 1;
      }
    }
  }

  # c is empty
  if (k > o) {
    while ((i <= m) & (j <= n)) {
      if (a[i] < b[j]) {
        d[it] <- a[i]; i <- i + 1;  it <- it + 1;
      } else {
        d[it] <- b[j]; j <- j + 1;  it <- it + 1;
      }
    }
  }

  print(d)

  # if a and b empty
  if ((i > m) & (j > n)) { while (k <= o) {
    d[it] <- c[k]; k <- k + 1;  it <- it + 1;
  } }

  # if a and c empty
  if ((i > m) & (k > o)) { while (j <= n) {
    d[it] <- b[j]; j <- j + 1;  it <- it + 1;
  } }

  # if b and c empty
  if ((j > n) & (k > o)) { while (i <= m) {
    d[it] <- a[i]; i <- i + 1;  it <- it + 1;
  } }

  return(d)
}
