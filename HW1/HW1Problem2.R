# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# This script calculates the sum of (1/2)^i for i = 1 to i = n three ways,    #
#   avoiding or exploring potential errors and rounding limits                #
#                                                                             #
# Author: April Walker                                                        #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
options(warn=-1)
`%notin%` <- Negate(`%in%`)

n <- as.numeric(readline("Enter a int value for n: ")) # reads user input for n
# the max int is much smmaler than the max numeric, if big set smol
if (!is.na(n)) { if (n > .Machine$integer.max) {
  print("Your n is too large, setting to max R integer value")
  n <- .Machine$integer.max
} }

if (is.na(n)) { while(is.na(n)) {
  n <- as.integer(readline("...An INT value: "))
} }

# summation()
#   option: string containing "for", "while", or "sane" for method selection
summation <- function(option) {
  if (option %notin% c("for", "while", "sane")) {
    print("your option was invalid, defaulting to the sane method" )
    return(sum((1/2)^(1:n)))
  }

  # if adding values smaller than machine epsilon, return sum convergance (1)
  if ((1/2)^n == 0) {
    return(1)
  }

  total <- 0 # # declare total as 0 before summing...

  # # for loop method
  if (option == "for") { for (i in 1:n) {
      total <- total + (1/2)^i
  } }

  # # while loop method (gross)
  if (option == "while") {
    i <- 1 # gross
    while (i <= n) {
      total <- total + (1/2)^i
      i = i + 1
  } }

  # # "analytic" method
  else total <- sum((1/2)^(1:n))

  # # return total
  return(total)
}

print(summation("for"), digits = 17)
print(summation("while"), digits = 17)
print(summation("sane"), digits = 17)

# #
# Error exploration
# #

# # this explores when 1/2^n is approximated as zero
n <- 1
while ((1/2)^n != 0) {
  n <- n + 1
}
print(paste0("Your just adding zeros when you get to ", n))

# # this explores when the sum of 1/2^i is approximated as one
n <- 1
total <- 0
while (total != 1) {
  total <- summation("sane")
  n <- n + 1
}
print(paste0("Your sum approximates to 1 when you get to ", n-1))
