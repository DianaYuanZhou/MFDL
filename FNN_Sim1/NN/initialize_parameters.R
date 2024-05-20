## ----------Random generate parameters for simulation----------
initialize.parameters <- function(layers, para.ub = 0.05){
  para.lb <- -para.ub
  list2env(layers, envir = environment())
  if (length(layers) == 3) {
    # assign.list(c("nb", "ns", "nr"), layers)
    W1 <- matrix(runif(K1*K0, para.lb, para.ub), ncol = K1, nrow = K0)
    b1 <- runif(K1, para.lb, para.ub)
    W2 <- matrix(runif(K2*K1, para.lb, para.ub), ncol = K2, nrow = K1)
    b2 <- runif(K2, para.lb, para.ub)
  }
  if (length(layers) == 4) {
    W1 <- matrix(runif(K1*K0, para.lb, para.ub), ncol = K1, nrow = K0)
    b1 <- runif(K1, para.lb, para.ub)
    W2 <- matrix(runif(K1*K21*K22, para.lb, para.ub), nrow = K1)
    b2 <- runif(K21*K22, para.lb, para.ub)
  }
  result <- mget(c("W1", "b1", "W2", "b2"))
  return(result)
}