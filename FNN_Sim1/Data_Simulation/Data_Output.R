## -------------- Output response data---------------------------
# Data.Output.wide <- function(f1, f2, loc, cf = c(1,1), Y.int, index, setting, noise = 0.3){
#   set.seed(index)
#   # force(f1)
#   # force(f2)
#   
#   if (is.vector(loc)) {
#     Y1 <- f1(x = loc)
#     Y2 <- f2(x = loc)
# 
#     p <- ncol(Y1)
#     q <- 0
#     if (p == 1){
#       rawvars <- matrix(rnorm(length(Y1), 0, 1), nrow = nrow(Y1))
#     } 
#     else {
#       sigma <- diag(1, p, p)
#       for (i in 1:(p - 1)) {
#         sigma[i, i + 1] <- 1/2
#         sigma[i + 1, i] <- 1/2
#       }
#       Sigma <- q*sigma + (1 - q)*diag(1, p, p)
#       rawvars <- mvrnorm(n = length(f1(x = 0)), mu = rep(0, p), Sigma = Sigma)
#     }
# 
#     if(setting$interaction == T){
#       #ifelse(setting$input[2] == 'scalar', Y <- Y1 + Y2 + Y.int, Y <- Y1 + Y2 + Y.int)
#       Y <- Y1 + Y2 + cf[1]*Y.int
#       #Y <- Y1 + Y2 + Y.int
#     }else{
#      Y <- Y1 + Y2
#     }
#     if(setting$sim_function == 'nonlinear') Y <- scale(Y)
#     
#     Y <- cf[2]*Y + rawvars*noise
#     Y <- as.matrix(Y)
#   }
#   return(mget(c('Y1','Y2','Y')))
#   #return(mget(c('Y')))
# }

Data.Output.wide <- function(f1, loc, cf = c(1,1), Y.int, index, setting, noise = 0.3){
  set.seed(index)
  # force(f1)
  # force(f2)
  
  if (is.vector(loc)) {
    Y1 <- f1(x = loc)
    #Y2 <- f2(x = loc)
    
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
      rawvars <- mvrnorm(n = length(f1(x = 0)), mu = rep(0, p), Sigma = Sigma)
    }
    
    if(setting$interaction == T){
      #ifelse(setting$input[2] == 'scalar', Y <- Y1 + Y2 + Y.int, Y <- Y1 + Y2 + Y.int)
      Y <- Y1 + Y2 + cf[1]*Y.int
      #Y <- Y1 + Y2 + Y.int
    }else{
      Y <- Y1
    }
    if(setting$sim_function == 'nonlinear') Y <- scale(Y)
    
    Y <- cf[2]*Y + rawvars*noise
    Y <- as.matrix(Y)
  }
  return(mget(c('Y1','Y')))
  #return(mget(c('Y')))
}