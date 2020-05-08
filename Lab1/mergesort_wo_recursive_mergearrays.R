
## MergeSort implementation (without recursive array merge)
## This was implemented by a student who took the Stat 5443 course last year. 


MergeSort <- function(x){
  if(length(x) == 1) {
    res <- x
  }else{
    # split x in 2 and call mergesort on both parts
    i <- floor(length(x)/2)
    s1 <- c(MergeSort(x[1:i]), Inf)
    s2 <- c(MergeSort(x[-(1:i)]), Inf)
    res <- rep(NA_real_, length(x))
    # merge the responses in ascending order
    i1 <- i2 <- 1
    for (i3 in 1:length(res)){
      if (s1[i1] <= s2[i2]) {
        res[i3] <- s1[i1]
        i1 <- i1 + 1
      } else {
        res[i3] <- s2[i2]
        i2 <- i2 + 1
      }
    }      
  } 
  return(res)
}

par(mfrow=c(1,2))
x <- rnorm(100)
plot(x, main = 'rnorm(100)', ty = 'l', bty = 'n')
plot(MergeSort(x), main = 'MergeSort(x)', bty = 'n', ty = 'l', col = 'blue')