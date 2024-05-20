# back.propagation <- function(x, ...) UseMethod("back.propagation")
back.propagation2 <- function(Y, parameters, cache, D1, B0, Bases0,
                             activation, activation.prime, activation2, activation2.prime,
                             lambda1 = 0, lambda2 = 0, penmat = 0, nhidden){
  list2env(parameters, envir = environment())
  list2env(cache, envir = environment())
  list2env(Bases0, envir = environment())
  if (!is.list(B2)) {
    ## add another layer predict using C3
    Y.hat <- C3 %*% t(B2)
    M3 <- 2*(Y.hat - Y) %*% B2

	M <- list()
	dW <-list()
	db <-list()
	
	n <- length(nhidden)
	if (n >= 2){
	    M[[n]] <- ((M3 %*% t(W3) %*% t(B1[[n]])) * activation.prime(H[[n]])) %*% B1[[n]]
	    for (i in 1:(n-1)){
		 M[[n-i]] <- ((M[[n-i+1]] %*% t(W[[n-i+1]]) %*% t(B1[[n-i]])) * activation2.prime(H[[n-i]])) %*% B1[[n-i]]
		}
	}
	else {
	M[[1]] <- ((M3 %*% t(W3) %*% t(B1[[1]])) * activation.prime(H[[1]])) %*% B1[[1]]
	}
  
	#NB0 <- ncol(B0)
	NB0 <- 1
	dW[[1]] <- t(D[[1]]) %*% M[[1]]/NB0 + lambda1 * W[[1]]
	db[[1]] <- colSums(M[[1]])	
	if(n >= 2){
  	for (i in 2:n){
  	    #NB1 <- ncol(B1[[i]])
  	    NB1 <- 1
        dW[[i]] <- t(D[[i]]) %*% M[[i]]/NB1 + lambda1 * W[[i]]
        db[[i]] <- colSums(M[[i]])
	  }	  
	}

	#NB1 <- ncol(B1[[n]])
	NB1 <- 1
	dW3 <- t(D3) %*% M3/NB1 + lambda1 * W3
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