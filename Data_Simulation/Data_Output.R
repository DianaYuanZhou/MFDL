## -------------- Output response data---------------------------
Data.Output.wide <- function(f1, f2, loc, index, inter = F, noise = 0.3){
  set.seed(index)
  force(f1)
  force(f2)
  if (is.vector(loc)) {
    Y1 <- f1(loc)
    Y2 <- f2(loc)

    p <- ncol(Y1)
    q <- 0
    if (p == 1){
      rawvars <- matrix(rnorm(length(Y1), 0, 1), nrow = nrow(Y1))
    } 
    else {
      sigma <- diag(1, p, p)
      for (i in 1:(p - 1)) {
        sigma[i, i + 1] <- 1/2
        sigma[i + 1, i] <- 1/2
      }
      Sigma <- q*sigma + (1 - q)*diag(1, p, p)
      rawvars <- mvrnorm(n = length(f1(0)), mu = rep(0, p), Sigma = Sigma)
    }
       Y1 <- scale(Y1)
       Y2 <- scale(Y2)

    if(inter == T){
      c = runif(1, -2, 2)
      Y <- Y1 + Y2 + c* Y1*Y2 
    }else{
     Y <- Y1 + Y2
    }
    Y <- Y + rawvars*noise
    Y <- as.matrix(Y)
  }
  return(Y)
}