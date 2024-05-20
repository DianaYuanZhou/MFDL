FNN0 <- function(D0, Y, Bases, G.valid = NULL, Y.valid = NULL, lr = 0.1, 
                epoch = 3e3, lambda1 = 0, lambda2 = 0, activation = sigmoid, 
                valid.goal = 0, parameters = NULL, verbose = 10, p = 100,
                penmat = NULL){
  list2env(Bases, envir = environment())
  D1 <- as.matrix(D0 %*% B0 / nrow(B0) * ncol(B0))
  Bases0 <- mget(c("B1", "B2"))
 #------If input is matrix------
  if (is.null(parameters)) {
    K0 <- ncol(B0)
    K1 <- ncol(B1)
    if (!is.list(B2)) {
      K2 <- ncol(B2)
      layers <- mget(c("K0", "K1", "K2"))
    } else {
      K21 <- ncol(B2[[1]])
      K22 <- ncol(B2[[2]])
      layers <- mget(c("K0", "K1", "K21", "K22"))
    }
    parameters <- initialize.parameters(layers)
  }
  
  activation.prime <- Deriv(activation)
  cache <- forward.propagation(D1, parameters, B1, activation)
  if ((valid.goal == 0) && (!is.null(Y.valid))) {
    k <- 0
    Y.valid.hat <- pred(parameters, G.valid, Bases, activation) %*% t(B2)
    valid.error.best <- mse(Y.valid, Y.valid.hat)
    result.best <- list(parameters = parameters, j = 0, 
                        train.error = mse(Y, cache$C2 %*% t(B2)))
  } else {
    valid.error <- NULL
    k <- NULL
  }
  for (j in 1:epoch) {
    grads = back.propagation(Y, parameters, cache, D1, Bases0, 
                             activation, activation.prime, 
                             lambda1 = lambda1, lambda2 = lambda2, 
                             penmat = penmat)
    parameters <- update.parameters(parameters, grads, lr)
    cache <- forward.propagation(D1, parameters, B1, activation)
    if (j %% verbose == 0) {
      train.error <- mse(Y, cache$C2 %*% t(B2))
      if (!is.null(Y.valid)) {
        Y.valid.hat <- pred(parameters, G.valid, Bases, activation) %*% t(B2)
        valid.error <- mse(Y.valid, Y.valid.hat)
        if (valid.error < valid.goal) {
          return(mget(c('parameters', 'j', 'train.error')))
        }
        if (valid.goal == 0) {
          if (valid.error < valid.error.best) {
            valid.error.best <- valid.error
            result.best <- mget(c('parameters', 'j', 'train.error'))
            k <- 0
          } else k <- k + 1
        }
      }
      # cat(j, train.error, valid.error, k, "\n")
      if (!is.null(k)) {
        if (k == p) return(result.best)
      }
    }
  }
  return(mget(c('parameters', 'j', 'train.error')))
}