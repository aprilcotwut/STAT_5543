# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# This script uses merge sort to remove duplicates in a vector                #
#                                                                             #
# Author: April Walker                                                        #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


duplicateRemover <- function(x) {
  ### internal fucntions
  .merge <- function(r, l) {
    #declares vector size of combined len
    new <- numeric(length(r) + length(l))
    r_j <- l_j <- i <- 1;
    for (i in 1:length(new)) {
        if((r_j <= length(r) && r[r_j] < l[l_j]) || l_j>length(l)) {
          new[i] <- r[r_j]
          r_j <- r_j + 1
        } else {
          new[i] <- l[l_j]
          l_j <- l_j + 1
        }
    }
    return(new)
  }
  .mergeSort <- function(x) {
    # as long as you have more than a single element (important for recursion)
    if(length(x) > 1) {
      # find the midpoint
      cut <- ceiling(length(x)/2)
      # keep recursively dividin up the left and right until you have
      # a single element in each
      r <- .mergeSort(x[1:cut])
      l <- .mergeSort(x[(cut+1):length(x)])
      .merge(r, l)
    } else return(x)
  }

  # if empty or single element you can't have duplicates
  if (length(x) <= 1) return(x)

  # else sort the array using .mergeSort()
  x <- .mergeSort(x)
  print(x)

  # declare vector holding location of items up for deletion...
  x_new <- c()

  # going through each element in x
  for (i in 1:(length(x)-1)) {
    # if the current element isn't equal to the next one, sotre it
    if (x[i] != x[i+1]) {
      x_new <- append(x_new, x[i])
    }
  }
  # we should always save the last one, because if it was a duplicate it wasn't
  # saved anyways...
  x_new <- append(x_new, x[length(x)])
  return(x_new)
}
