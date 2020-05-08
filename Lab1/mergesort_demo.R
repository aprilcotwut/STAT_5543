
rm(list=ls())

mergearrays <- function(x,y){
  m = length(x)
  n = length(y)
  if(m==0){
    return(z = y)
    }
  if(n==0){
    return(z = x)
  }
  if (x[1]<=y[1]){
    return(z = c(x[1],mergearrays(x[-1],y)))
  }else{
    return(z = c(y[1],mergearrays(x,y[-1])))
  }
}

x = c(1,2,3)
y = c(2.5,3.5,4.5)
mergearrays(x,y)

## Merge sort with recursive array merge 

mergesort <- function(x){
  n = length(x)
  mid = floor(n/2)
  if(n > 1){
    return(mergearrays(mergesort(x[1:mid]),mergesort(x[(mid+1):n])))
  }else{
    return(x)
  }
}
x = c(1,3,4,5,7,2)
mergesort(x)

x = rnorm(20) ## 20 numbers from N(0,1) distribution

mergesort(x)

## Sometimes this might throw an error as follows: that's completely fine. 
## Error: node stack overflow
## Error during wrapup: node stack overflow

## We can do merge sort without the recursive array merge operation 
## to avoid running into that error

## Time complexity

array.lengths = seq(100, 10000, by = 100)
mergesort.time = rep(0,length(array.lengths));


for(i in 1:length(mergesort.time)){
  #obtain the cpu time from the i-th random sample
  #and save the results as, say, t1 and t2
  y =  runif(array.lengths[i]) #generated sample of size specified by array.length
  ptm <- proc.time()
  MergeSort(y) #sorting algorithm 1
  t1 = proc.time()-ptm
  mergesort.time[i]=t1[["elapsed"]]
  
}

summary(mergesort.time)

plot(array.lengths, mergesort.time, type = "l")

## Implement Bubble Sort. 
## Compare Bubble Sort with Merge-sort for the same arrays of random numbers. 
