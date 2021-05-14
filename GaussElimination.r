A <- matrix(c(-8,1,-2,-20,2,-6,-1,-38,-3,-1,7,-34), 3, 4, byrow = T)
GaussForward <- function(A) {
  n <- nrow(A)
  m <- ncol(A)
  for(i in 1:n) {
    A[i,] <- A[i, ]/A[i,i]
    if (i==n) {
      break
    }
    for(j in i+1:(n-i)){
      A[j,i:m] <- A[j,i:m] - (A[j,i] * A[i, i:m])
    }
  }
  return(A)
}

GaussBackward <- function(A) {
  n <- nrow(A)
  m <- ncol(A)
  x <- vector()
  for(i in 1:n){
    c <- 0
    t <- 1
    for(j in (m-i):(m-1)){
      if((j) == (m-1)){
        break
      }
      c <- c + (x[i-t] * A[(n+1)-i,j+1])
      t <- t + 1
    }
    x[i] <- A[(n+1)-i,m] - c
  }
  print(A)
  for (i in n:1) {
    print(x[i])
  }
}
