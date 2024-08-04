NN0 <- function(D0, Y, Bases, G.valid = NULL, Y.valid = NULL, lr = 0.1, 
                epoch = 3e3, lambda1 = 0, lambda2 = 0, activation = sigmoid, method = 'Continue',
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
    parameters <- initialize.parameters(layers)  
  }
  
  ## ----- ADADELTA ----------
  if(ADADELTA == T){
    #decay_rate <- 0.9 # Initialize rho
    #e <- 1e-8 # Initialize constant
    adadelta.para <- initialize.adadelta.nn(layers, nhidden = nhidden) #Initialize g_sq and delta_x_sq 
  }
  
  activation.prime <- Deriv(activation)
  #activation2.prime <- Deriv(activation2)
  
  cache <- forward.propagation(D1, parameters, B1, activation)
  
  if ((valid.goal == 0) && (!is.null(Y.valid))) {
    k <- 0
    Y.valid.hat <- pred(parameters, G.valid, Bases, activation) %*% t(B2)
    valid.error.best <- mse(Y.valid, Y.valid.hat)
    result.best <- list(parameters = parameters, j = 0, 
                        train.error = mse(Y, cache$C2 %*% t(B2)))
  } else {
    valid.error.best <- 0
    valid.error <- NULL
    k <- NULL
  }
  if (method == 'Continue'){
    valid.goal <- valid.error.best
  }else{valid.goal = 0}
  
  for (j in 1:epoch) {
    grads = back.propagation(Y, parameters, cache, D1, Bases0, 
                             activation, activation.prime, 
                             lambda1 = lambda1, lambda2 = lambda2, 
                             penmat = penmat)
    if(ADADELTA == T){
      parameters <- update.adadelta.nn(parameters, grads, nhidden, adadelta.para, decay_rate = decay_rate, e = e)
    }else {parameters <- update.parameters(parameters, grads, lr)}
    
    cache <- forward.propagation(D1, parameters, B1, activation)
    
    if (j %% verbose == 0) {
      train.error <- mse(Y, cache$C2 %*% t(B2))
      
      if (!is.null(Y.valid)) {
        Y.valid.hat <- pred(parameters, G.valid, Bases, activation) %*% t(B2)
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
      }else{result.best <- mget(c('parameters', 'j', 'train.error'))}
      
      if (j %% 1000 == 0)
        cat(j, train.error, valid.error.best, valid.error, '\n')
      if (!is.null(k)) {
        if (k == p) return(result.best)
      }
    }
  }
  
  return(result.best)
}

NN <- function(D0, Y, Bases, G.valid = NULL, Y.valid = NULL, lr = 0.3, 
               epoch = 3e3, lambda1 = 0, lambda2 = 0, activation = sigmoid, method = 'Continue',
               valid.goal = 0, parameters = NULL, verbose = 10, p = 1000,
               penmat = NULL, nhidden, ADADELTA = F){
  
  ### Use this part when no panelty
  valid.error <- rep(0, length(lambda1))
  results <- list()
  for (i in 1:length(lambda1)) {
    lambda <- lambda1[i]
    result <- NN0(D0 = D0, Y = Y, Bases = Bases, G.valid = G.valid,
                  Y.valid = Y.valid, lr = lr, epoch = epoch, 
                  lambda1 = lambda, activation = activation, valid.goal = valid.goal, parameters = parameters, 
                  verbose = verbose, p = p, penmat = penmat, nhidden = nhidden, ADADELTA = ADADELTA)
    if(!is.null(Y.valid)){
      Y.valid.hat <- pred(result$parameters, G.valid, Bases, activation) %*% t(Bases$B2)
      valid.error[i] <- mse(Y.valid, Y.valid.hat)
      
    }else{
      Y.valid.hat <- pred(result$parameters, D0, Bases, activation) %*% t(Bases$B2)
      valid.error[i] <- mse(Y, Y.valid.hat)        
    }
    results[[i]] <- result
  }
  
  
  i <- which.min(valid.error)
  print(valid.error)
  cat(i, "\n")
  return(results[[i]])
}