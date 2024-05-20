FNNdeep0 <- function(D0, Y, Bases, G.valid = NULL, Y.valid = NULL, lr = 0.1, 
                  epoch = 3e3, lambda1 = 0, lambda2 = 0, activation = sigmoid, activation2 = sigmoid, method = 'Continue',
                  valid.goal = 0, parameters = NULL, verbose = 10, p = 1000,
                  penmat = NULL, nhidden, ADADELTA = F){
  list2env(Bases, envir = environment())
  #NB0 <- ncol(B0)
  NB0 <- 1
  D1 <- as.matrix(D0 %*% B0 / NB0)
  Bases0 <- mget(c("B1", "B2"))
  #------If input is matrix------

    if (is.null(parameters)) {
      K1 <- list()
      for (i in 1: length(B1)){
        K0 <- ncol(B0)
        K1[[i]] <- ncol(B1[[i]])
      }
      if (!is.list(B2)) {
        K2 <- ncol(B2)
        layers <- mget(c("K0", "K1", "K2"))
       } else {
        K21 <- ncol(B2[[1]])
        K22 <- ncol(B2[[2]])
        layers <- mget(c("K0", "K1", "K21", "K22"))
    }
    parameters <- initialize.parameters2(layers,nhidden = nhidden)  
    }
  
  ## ----- ADADELTA ----------
  if(ADADELTA == T){
    #decay_rate <- 0.9 # Initialize rho
    #e <- 1e-8 # Initialize constant
    adadelta.para <- initialize.parameters.adadelta(layers, nhidden = nhidden) #Initialize g_sq and delta_x_sq 
  }
  
  activation.prime <- Deriv(activation)
  activation2.prime <- Deriv(activation2)
  
  cache <- forward.propagation2(D1, parameters, B1, activation, activation2, nhidden = nhidden)

  if ((valid.goal == 0) && (!is.null(Y.valid))) {
    k <- 0
    Y.valid.hat <- pred2(parameters, G.valid, Bases, activation, activation2, nhidden = nhidden) %*% t(B2)
    valid.error.best <- mse(Y.valid, Y.valid.hat)
    result.best <- list(parameters = parameters, j = 0, 
                        train.error = mse(Y, cache$C3 %*% t(B2)))
  } else {
    valid.error <- NULL
    k <- NULL
  }
  if (method == 'Continue'){
    valid.goal <- valid.error.best
  }else{valid.goal = 0}
  
  for (j in 1:epoch) {
    grads = back.propagation2(Y, parameters, cache, D1, B0, Bases0, 
                              activation, activation.prime,activation2, activation2.prime, 
                              lambda1 = lambda1, lambda2 = lambda2, 
                              penmat = penmat, nhidden)
    if(ADADELTA == T){
      parameters <- update.parameters.adadelta(parameters, grads, nhidden, adadelta.para, decay_rate = decay_rate, e = e)
    }else {parameters <- update.parameters2(parameters, grads, lr, nhidden)}
    
    cache <- forward.propagation2(D1, parameters, B1, activation, activation2, nhidden)

    if (j %% verbose == 0) {
      train.error <- mse(Y, cache$C3 %*% t(B2))
      
      if (!is.null(Y.valid)) {
        Y.valid.hat <- pred2(parameters, G.valid, Bases, activation, activation2, nhidden) %*% t(B2)
        valid.error <- mse(Y.valid, Y.valid.hat)
        
        if (valid.error < valid.error.best) {
          valid.error.best <- valid.error
          result.best <- mget(c('parameters', 'j', 'train.error'))
        }# Continue
        
        if (valid.goal == 0) {
          if (valid.error < valid.error.best) {
            valid.error.best <- valid.error
            result.best <- mget(c('parameters', 'j', 'train.error'))
            k <- 0
          } else k <- k + 1
        }# Early stopping
      }
      if (j %% 1000 == 0)
        cat(j, valid.error.best, valid.error, '\n')
      if (!is.null(k)) {
        if (k == p) return(result.best)
      }
    }
  }

  return(result.best)
}

FNNdeep <- function(D0, Y, Bases, G.valid = NULL, Y.valid = NULL, lr = 0.3, 
                epoch = 3e3, lambda1 = 0, lambda2 = 0, activation = sigmoid, activation2 = sigmoid, method = 'Continue',
                valid.goal = 0, parameters = NULL, verbose = 10, p = 1000,
                penmat = NULL, nhidden, ADADELTA = F){

    ### Use this part when no panelty
    valid.error <- rep(0, length(lambda1))
    results <- list()
    for (i in 1:length(lambda1)) {
      lambda <- lambda1[i]
      result <- FNNdeep0(D0 = D0, Y = Y, Bases = Bases, G.valid = G.valid,
                    Y.valid = Y.valid, lr = lr, epoch = epoch, 
                    lambda1 = lambda, activation = activation, activation2 = activation2,
                    valid.goal = valid.goal, parameters = parameters, 
                    verbose = verbose, p = p, penmat = penmat, nhidden = nhidden, ADADELTA = ADADELTA)
      Y.valid.hat <- pred2(result$parameters, G.valid,
                          Bases, activation, activation2, nhidden = nhidden) %*% t(Bases$B2)
      valid.error[i] <- mse(Y.valid, Y.valid.hat)
      results[[i]] <- result
    }
 
  
  i <- which.min(valid.error)
  print(valid.error)
  cat(i, "\n")
  return(results[[i]])
}