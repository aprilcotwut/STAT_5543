# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# This script provides a minimal tutorial in numeric vector manipulation in R.#
# I provide alternative methods in comments for most steps of the problem.    #
#                                                                             #
# Author: April Walker                                                        #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

x <- 9:16 ### numeric vector assignement
# x <- c(9, 10, 11, 12, 13, 14, 15, 15) ### alternative assignement
print(tail(x, 3)) ### prints last 3 values
# print(x[(length(x)-2):length(x)]) ### alternative print
print(x[x %% 2 == 0]) ### prints all even vals in x
# print(x[lapply(x, "%%", 2) == 0]) ### overkill alternative
# for (v in x) { if (v %% 2 == 0) { print(v) } } ### unasthetic alternative
x <- x[x %% 2 == 0] ### deletes all even vals in x, also an alt of prior step
print(x)

# if you don't write
# silly comments in your script
# it will not run
#        ~ a 100% true haiku by A Walker

# fin #
