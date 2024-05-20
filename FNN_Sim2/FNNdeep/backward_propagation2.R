# back.propagation <- function(x, ...) UseMethod("back.propagation")
back.propagation2 <- function(Y, parameters, cache, D1, Bases0,
                             activation, activation.prime, 
                             lambda1 = 0, lambda2 = 0, penmat = 0, nhidden){
  #N <- dim(D1)[1]
  #N <- dim(D1)[1]*dim(D1)[2]
  list2env(parameters, envir = environment())
  list2env(cache, envir = environment())
  list2env(Bases0, envir = environment())
  if (!is.list(B2)) {
    ## add another layer predict using C3
    Y.hat <- C3 %*% t(B2)
    M3 <- (Y.hat - Y) %*% B2 / nrow(B2) * ncol(B2)
    # M3 <- M3 + lambda2 * C3 %*% penmat
	
	M <- list()
	dW <-list()
	db <-list()
	
	n <- length(nhidden)
	if (n >= 2){
	    M[[n]] <- ((M3 %*% t(W3) %*% t(B1[[n]])) * activation.prime(H[[n]])) %*% B1[[n]] / nrow(B1[[n]]) * ncol(B1[[n]])
	    for (i in 1:(n-1)){
     ## Default activation function: tanh
		 #M[[nhidden-i]] <- ((M[[nhidden-i+1]] %*% t(W[[nhidden-i+1]]) %*% t(B1)) * activation.prime(H[[nhidden-i]])) %*% B1 / nrow(B1) * ncol(B1)
	     ## Activation function: ReLU
		 M[[n-i]] <- ((M[[n-i+1]] %*% t(W[[n-i+1]]) %*% t(B1[[n-i]])) * relu.prime(H[[n-i]])) %*% B1[[n-i]] / nrow(B1[[n-i]]) * ncol(B1[[n-i]])
		}
	}
	#M1 <- ((M2 %*% t(W2) %*% t(B1)) * activation.prime(H1)) %*% B1 / nrow(B1) * ncol(B1)
	#M1 - First hidden layer
	else {
	M[[1]] <- ((M3 %*% t(W3) %*% t(B1[[1]])) * activation.prime(H[[1]])) %*% B1[[1]] / nrow(B1[[1]]) * ncol(B1[[1]])
	}
	
	for (i in 1:n){
    dW[[i]] <- t(D[[i]]) %*% M[[i]] + lambda1 * W[[i]]
    db[[i]] <- colSums(M[[i]])
	
    #dW2 <- t(D2) %*% M2/prod(dim(Y)) + lambda1/N * W2
    #db2 <- colSums(M2)/prod(dim(Y))
	}
	
	dW3 <- t(D3) %*% M3 + lambda1 * W3
	db3 <- colSums(M3)
	
  } # else {
  #   list2env(B2, envir = environment())
  #   Y.residual <- (Y.hat - Y) %*% (B21 %x% B22)
  #   Mid <- ((Y.residual %*% t(W2) %*% t(B1)) * activation.prime(D2)) %*% B1
  #   dW1 <- t(D1) %*% Mid/prod(dim(Y)) + lambda/N * W1
  #   db1 <- colSums(Mid)/prod(dim(Y))
  #   dW2 <- t(H2) %*% Y.residual/prod(dim(Y)) + lambda/N * W2
  #   db2 <- colSums(Y.residual)/prod(dim(Y))
  # }
  
    
  grads <- mget(c("dW","db", "dW3" ,"db3"))
  #grads <- mget(c(dW1", "db1", "dW2", "db2", "dW3" ,"db3"))
  return(grads)
}