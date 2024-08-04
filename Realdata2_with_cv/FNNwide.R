FNNwide0 <- function(D0, Y.wide, Bases.pre, Bases.wide, G.valid = NULL, Y.valid = NULL, lr.wide, 
                  epoch = 3e3, lambda1 = 0, lambda2 = 0, activation = sigmoid, activation2 = sigmoid, method = 'Continue', 
                  valid.goal = 0, parameters = NULL,parameters.pre = NULL, verbose = 5, p = 1000,
                  penmat = NULL, lr.pre, ninput, nhidden.pre, nhidden.wide, ADADELTA = F){
  list2env(Bases.pre, envir = environment())
  list2env(Bases.wide, envir = environment())
  
  B2 <- cb2(Bases.wide, Y.wide, D0)
  
  D1.input <- list()
  #NB0.pre <- ncol(B0.pre[[i]])
  NB0.pre <- 1
  for (i in 1:ninput){
      D1.input[[i]] <- as.matrix(D0[[i]] %*% B0.pre[[i]] / NB0.pre)

  }
  #Bases0.pre <- mget(c("B0.pre","B1.pre"))
  Bases0 <- mget(c("B1.wide","B11.wide", "B2"))
  
  #------If input is matrix------
  K0.pre <- list()
  K11.pre <- list()
  
  if (is.null(parameters)) {  
    for (i in 1:ninput){
      K0.pre[[i]] <- ncol(B0.pre[[i]])
      K11.pre[[i]] <- ncol(B1.pre[[i]])
    }
    K1.wide <- ncol(B1.wide)
    K11.wide <- ncol (B11.wide)
    
    if (!is.list(B2)) {
      K2 <- ncol(B2)
      layers <- mget(c("K1.wide","K11.wide","K2"))
      layers.pre <- mget(c("K0.pre", "K11.pre", "K2"))
      
    } 
    
	parameters.pre <- initialize.pre(layers.pre, ninput = ninput, nhidden.pre = nhidden.pre) #list including W0 and b0
	parameters <- initialize.wide(layers.pre = layers.pre, layers = layers, nhidden = nhidden.wide)
  #list including W, b and matrix W3, b3
	
	if(ADADELTA == T){
	  adadelta.para.pre <- initialize.pre.adadelta(layers.pre, ninput = ninput, nhidden.pre = nhidden.pre)
	  adadelta.para.wide <- initialize.wide.adadelta(layers.pre = layers.pre, layers = layers, nhidden = nhidden.wide)
	}
	}
  
  activation.prime <- Deriv(activation)
  activation2.prime <- Deriv(activation2)
  
  cache.pre<-list()
  cache.pre <- FP.pre(D1.input, parameters.pre, B1 = B11.pre, activation, activation2, ninput = ninput, nhidden.pre)
  
  H3.pre<-list()
  H3 <- list()
  H3[[1]] <-0
  H3[[1]] <- activation2(cache.pre$H2.pre[[1]][[nhidden.pre]])
  if (ninput >=2 ){
     for (i in 2:ninput){
     H3.pre[[i]] <- 0  
     H3.pre[[i]]<- activation2(cache.pre$H2.pre[[i]][[nhidden.pre]])
     H3[[1]] <- cbind(H3[[1]], H3.pre[[i]])
  }
  }
  #H3[[1]] <- scale(H3[[1]])
  
  cache <- FP.wide(H1 = H3[[1]], parameters, B1.wide, B11.wide, activation, activation2, nhidden.wide)


  #-------BP and training part----------------- 
  if ((valid.goal == 0) && (!is.null(Y.valid))) {
    k <- 0
#     Y.valid.hat <- pred.wide(parameters, D0 = G.valid, Bases0, Bases0.pre, activation, activation2, nhidden.wide,
# 	                      parameters.pre, ninput, nhidden.pre) %*% t(B2)
    Y.valid.hat <- pred.wide(parameters, Y = Y.valid, D0 = G.valid, Bases.wide, Bases.pre, activation, activation2, nhidden.wide,
   	                      parameters.pre, ninput, nhidden.pre)

    valid.error.best <- mse(Y.valid[,1], Y.valid.hat)
    
    Y.train.hat <- rowSums(cache$C3[as.numeric(rownames(B2)), , drop = FALSE] * B2)
    result.best <- list(parameters = parameters, parameters.pre = parameters.pre, j = 0, 
                        train.error = mse(Y.wide[,1], Y.train.hat))
  } else {
    valid.error.best <- 0
    valid.error <- NULL
    k <- NULL
  }
  
  if (method == 'Continue'){
    valid.goal <- valid.error.best
  } else {valid.goal = 0}
  
  grads.pre<-list()
  for (j in 1:epoch) {

    grads = BP.wide(Y.wide, parameters, cache, D1 = D1.input, Bases0, 
                              activation, activation.prime, activation2, activation2.prime, 
                              lambda1 = lambda1, lambda2 = lambda2, 
                              penmat = penmat, nhidden = nhidden.wide)
    if(ADADELTA == T){
      parameters <- update.adadelta.wide(parameters, grads, nhidden.wide, adadelta.para.wide, decay_rate = decay_rate, e = e)
      #print(parameters$delta_W2)
      }else {
      parameters <- update.wide(parameters, grads, lr.wide = lr.wide, nhidden = nhidden.wide)
    }

    grads.pre = BP.pre(Y.wide, parameters.pre, cache.pre, D1.input, B1 = B11.pre, Bases.pre, parameters, cache, Bases0,
                              activation, activation.prime, activation2, activation2.prime, 
                              lambda1 = 0, lambda2 = 0, 
                              penmat = penmat, ninput, nhidden.pre = nhidden.pre)
    if(ADADELTA == T){
      parameters.pre <- update.adadelta.pre(parameters.pre, grads.pre, ninput, nhidden.pre = nhidden.pre, adadelta.para.pre, decay_rate = decay_rate, e = e)
      #print(parameters.pre$delta_W1.pre)
      }else {
      parameters.pre<- update.pre(parameters.pre, grads.pre, lr.pre = lr.pre, ninput, nhidden.pre = nhidden.pre)
    }
		cache.pre <- FP.pre(D1.input, parameters.pre, B1=B11.pre, activation, activation2, ninput, nhidden.pre = nhidden.pre)
		
		H3[[1]] <- activation2(cache.pre$H2.pre[[1]][[nhidden.pre]])
		if (ninput >=2){
		for (i in 2:ninput){
		  H3[[1]]<-cbind(H3[[1]], activation2(cache.pre$H2.pre[[i]][[nhidden.pre]]))
		}
		}
		#H3[[1]] <- scale(H3[[1]])

	  cache <- FP.wide(H1 = H3[[1]], parameters, B1.wide, B11.wide, activation, activation2, nhidden.wide)
	
    if (j %% verbose == 0) {
      Y.train.hat <- rowSums(cache$C3[as.numeric(rownames(B2)), , drop = FALSE] * B2)
      train.error <- mse(Y.wide[,1], Y.train.hat)

      if (!is.null(Y.valid)) {
        # Y.valid.hat <- C3 %*% t(B2)
        Y.valid.hat <- pred.wide(parameters,Y.valid, G.valid, Bases.wide, Bases.pre, activation, activation2, nhidden.wide,
                                 parameters.pre,  ninput, nhidden.pre)
        valid.error <- mse(Y.valid[,1], Y.valid.hat)
        
      if (valid.error < valid.error.best) {
          valid.error.best <- valid.error
          result.best <- mget(c('parameters','parameters.pre', 'j', 'train.error'))
        } # Continue
        
        if (valid.goal == 0) {
          if (valid.error < valid.error.best) {
            valid.error.best <- valid.error
            result.best <- mget(c('parameters','parameters.pre', 'j', 'train.error'))
            k <- 0
          } else k <- k + 1
        } # Early stopping
      }else{result.best <- mget(c('parameters','parameters.pre', 'j', 'train.error'))}
      
      if (j %% 2000 == 0)
         cat(j, train.error, valid.error.best, valid.error, "\n")
      
      if (!is.null(k)) {
        if (k == p) return(result.best)
      }
    }
  }
  
  return(result.best)
}


FNN.wide <- function(D0, Y.wide, Bases.pre, Bases.wide, G.valid = NULL, Y.valid = NULL, lr.wide, 
                epoch = 3e3, lambda1 = 0, lambda2 = 0, activation = sigmoid, activation2 = sigmoid, method = method,
                valid.goal = 0, parameters = NULL,parameters.pre = NULL,verbose = 10, p = 1000,
                penmat = NULL,lr.pre, ninput, nhidden.pre, nhidden.wide, ADADELTA = T){

  valid.error <- rep(0, length(lambda1))
  results <- list()
  for (i in 1:length(lambda1)) {
    lambda <- lambda1[i]
    result <- FNNwide0(D0 = D0, Y.wide = Y.wide, Bases.pre = Bases.pre, Bases.wide = Bases.wide, G.valid = G.valid,
                       Y.valid = Y.valid, lr.wide = lr.wide, epoch = epoch, method = method,
                       lambda1 = lambda, lambda2 = lambda2, activation = activation, activation2 = activation2,
                       valid.goal = valid.goal, parameters = parameters, parameters.pre = parameters.pre,
                       verbose = verbose, p = p, penmat = penmat, 
                       lr.pre = lr.pre, ninput = ninput, nhidden.pre = nhidden.pre, nhidden.wide = nhidden.wide, ADADELTA = ADADELTA)
    
    #list2env(Bases.pre, envir = environment())
    #list2env(Bases.wide, envir = environment())
    # Bases0.pre <- mget(c("B0.pre","B1.pre"))
    # Bases0 <- mget(c("B1.wide","B11.wide", "B2"))
    
    if (!is.null(Y.valid)){
      Y.valid.hat <- pred.wide(result$parameters, Y.valid,G.valid,
                               Bases.wide, Bases.pre,activation, activation2, nhidden.wide,
                               result$parameters.pre,  ninput, nhidden.pre)
      
      valid.error[i] <- mse(Y.valid[,1], Y.valid.hat)
      
    }else{
      Y.train.hat <- pred.wide(result$parameters, Y.wide, D0,
                               Bases.wide, Bases.pre,activation, activation2, nhidden.wide,
                               result$parameters.pre,  ninput, nhidden.pre)
      
      valid.error[i] <- mse(Y.wide[,1], Y.train.hat)
    }
    results[[i]] <- result
  }
  
  
  i <- which.min(valid.error)
  print(valid.error)
  cat(i, "\n")
  return(results[[i]])
}