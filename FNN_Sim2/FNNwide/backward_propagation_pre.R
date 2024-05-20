# back.propagation <- function(x, ...) UseMethod("back.propagation")
BP.pre <- function(Y, parameters.pre, cache.pre, D1.input, Bases0.pre, parameters, cache,
								activation,activation.prime,
                lambda1 = 0, lambda2 = 0, penmat = 0, ninput, nhidden.pre){
  #N <- dim(D1.input[[1]])[1]
  N <- dim(D1.input[[1]])[1]*dim(D1.input[[1]])[2]
  list2env(parameters.pre, envir = environment())
  list2env(cache.pre, envir = environment())
  list2env(Bases0.pre, envir = environment())
  list2env(parameters, envir = environment())
  list2env(cache, envir = environment())

  if (!is.list(B2)) {
    ## add another layer predict using C3
    #Y.hat <- C3 %*% t(B2)
    #M3 <- (Y.hat - Y) %*% B2 / nrow(B2) * ncol(B2)
    #M3 <- M3 + lambda2 * C3 %*% penmat
	  #M3.pre <- M[[1]]
		
	M1.pre <- list()
	M2.pre <- list()
	dW1.pre <- list()
	db1.pre <- list()	
	dW2.pre <- list()
	db2.pre <- list()

	for (i in 1:ninput){
	   
	   M2.pre[[i]] <- list()
	   a <- min(ncol(B1.pre[[i]]),ncol(B1.pre[[1]])) 
	   
	if (nhidden.pre >= 2){

	   #M2.pre[[i]][[nhidden.pre]] <- ((M2[[1]] %*% t(W2[[1]][((i-1)*100+1):(i*100),]) %*% t(B11)) * relu.prime(H2.pre[[i]][[nhidden.pre]])) %*% B11 / nrow(B11) * ncol(B11)
	  M2.pre[[i]][[nhidden.pre]] <- ((M2[[1]] %*% t(W2[[1]][((i-1)*ncol(B1.pre[[1]])+1):((i-1)*ncol(B1.pre[[1]])+a),]) %*% t(B1.pre[[i]])) * relu.prime(H2.pre[[i]][[nhidden.pre]])) %*% B1.pre[[i]] / nrow(B1.pre[[i]]) * ncol(B1.pre[[i]])
	   
	   for (j in 1:(nhidden.pre-1)){
       ## Default activation function: tanh
		   #M[[nhidden-i]] <- ((M[[nhidden-i+1]] %*% t(W[[nhidden-i+1]]) %*% t(B1)) * activation.prime(H[[nhidden-i]])) %*% B1 / nrow(B1) * ncol(B1)
	     ## Activation function: ReLU
		  M2.pre[[i]][[nhidden.pre-j]] <- ((M2.pre[[i]][[nhidden.pre-j+1]] %*% t(W2.pre[[i]][[nhidden.pre-j+1]]) %*% t(B1.pre[[i]])) * relu.prime(H2.pre[[i]][[nhidden.pre-j]])) %*% B1.pre[[i]] / nrow(B1.pre[[i]]) * ncol(B1.pre[[i]])
		}
	}
	#M1 <- ((M2 %*% t(W2) %*% t(B1)) * activation.prime(H1)) %*% B1 / nrow(B1) * ncol(B1)
	#M1 - First hidden layer
	   else if(ncol(B1.pre[[2]]) == 1){
	     M2.pre[[1]] <- list()
	     M2.pre[[2]] <- list()
	     a<- ncol(B1.pre[[1]])
	     M2.pre[[1]][[1]] <- ((M2[[1]] %*% t(as.matrix(W2[[1]][1:a,])) %*% t(B1.pre[[1]])) * relu.prime(H2.pre[[1]][[nhidden.pre]])) %*% B1.pre[[1]] / nrow(B1.pre[[1]]) * ncol(B1.pre[[1]])
	     M2.pre[[2]][[1]] <- ((M2[[1]] %*% as.matrix(W2[[1]][((ncol(B1.pre[[1]])+1):(ncol(B1.pre[[1]])+1)),])) %*% t(B1.pre[[2]])) * relu.prime(H2.pre[[2]][[nhidden.pre]]) %*% B1.pre[[2]] / nrow(B1.pre[[2]]) * ncol(B1.pre[[2]])
	   }else {
	     M2.pre[[i]][[1]] <- ((M2[[1]] %*% t(W2[[1]][((i-1)*ncol(B1.pre[[1]])+1):((i-1)*ncol(B1.pre[[1]])+a),]) %*% t(B1.pre[[i]])) * relu.prime(H2.pre[[i]][[nhidden.pre]])) %*% B1.pre[[i]] / nrow(B1.pre[[i]]) * ncol(B1.pre[[i]])
	   }
	   M1.pre[[i]] <- 0  
	   M1.pre[[i]] <- ((M2.pre[[i]][[1]] %*% t(W2.pre[[i]][[1]]) %*% t(B1.pre[[i]])) * relu.prime(H1.pre[[i]])) %*% B1.pre[[i]] / nrow(B1.pre[[i]]) * ncol(B1.pre[[i]])
	   
	   dW2.pre[[i]] <- list()
     db2.pre[[i]] <- list()
    
	  for (j in 1:nhidden.pre){

    dW.temp <- t(D2.pre[[i]][[j]]) %*% M2.pre[[i]][[j]] + lambda1 * W2.pre[[i]][[j]]
    db.temp <- colSums(M2.pre[[i]][[j]])
    
    dW2.pre[[i]][[j]] <- dW.temp
    db2.pre[[i]][[j]] <- db.temp
	
    #dW2 <- t(D2) %*% M2/prod(dim(Y)) + lambda1/N * W2
    #db2 <- colSums(M2)/prod(dim(Y))
	}
	
	  dW1.pre[[i]] <- t(D1.input[[i]]) %*% M1.pre[[i]]
	  db1.pre[[i]] <- colSums(M1.pre[[i]])
  } 
	# else {
  #   list2env(B2, envir = environment())
  #   Y.residual <- (Y.hat - Y) %*% (B21 %x% B22)
  #   Mid <- ((Y.residual %*% t(W2) %*% t(B1)) * activation.prime(D2)) %*% B1
  #   dW1 <- t(D1) %*% Mid/prod(dim(Y)) + lambda/N * W1
  #   db1 <- colSums(Mid)/prod(dim(Y))
  #   dW2 <- t(H2) %*% Y.residual/prod(dim(Y)) + lambda/N * W2
  #   db2 <- colSums(Y.residual)/prod(dim(Y))
  # }
  # }
    
  }	
  grads.pre <- mget(c("dW1.pre","db1.pre","dW2.pre","db2.pre"))
  #grads <- mget(c(dW1", "db1", "dW2", "db2", "dW3" ,"db3"))
  return(grads.pre)
}