# back.propagation <- function(x, ...) UseMethod("back.propagation")
BP.wide <- function(Y, parameters, cache, D1, Bases0,
                             activation, activation.prime, 
                             lambda1 = 0, lambda2 = 0, penmat = 0, nhidden){
  #N <- dim(D1[[1]])[1]
  N <- dim(D1[[1]])[1]*dim(D1[[1]])[2]
  list2env(parameters, envir = environment())
  list2env(cache, envir = environment())
  list2env(Bases0, envir = environment())
  if (!is.list(B2)) {

    Y.hat <- C3 %*% t(B2)
    M3 <- (Y.hat - Y) %*% B2 / nrow(B2) * ncol(B2)
    # M3 <- M3 + lambda2 * C3 %*% penmat
	
	  M2 <- list()
	  dW2 <-list()
	  db2 <-list()
	
	if (nhidden >= 3){
	    for (i in 1:(nhidden-1)){
     M2[[nhidden]] <- ((M3 %*% t(W3) %*% t(B11.wide)) * activation.prime(H2[[nhidden]])) %*% B11.wide / nrow(B11.wide) * ncol(B11.wide)
	     ## Default activation function: tanh
		 #M[[nhidden-i]] <- ((M[[nhidden-i+1]] %*% t(W[[nhidden-i+1]]) %*% t(B1)) * activation.prime(H[[nhidden-i]])) %*% B1 / nrow(B1) * ncol(B1)
	     ## Activation function: ReLU
		 M2[[nhidden-i]] <- ((M2[[nhidden-i+1]] %*% t(W2[[nhidden-i+1]]) %*% t(B11.wide)) * relu.prime(H2[[nhidden-i]])) %*% B11.wide / nrow(B11.wide) * ncol(B11.wide)
		 M2[[1]] <- ((M2[[2]] %*% t(W2[[2]]) %*% t(B11.wide)) * relu.prime(H2[[1]])) %*% B11.wide / nrow(B11.wide) * ncol(B11.wide)
		}
	}
	#M1 <- ((M2 %*% t(W2) %*% t(B1)) * activation.prime(H1)) %*% B1 / nrow(B1) * ncol(B1)
	#M1 - First hidden layer
	else if (nhidden == 2){
	  M2[[2]] <- ((M3 %*% t(W3) %*% t(B11.wide)) * activation.prime(H2[[2]])) %*% B11.wide / nrow(B11.wide) * ncol(B11.wide)
	  #M2[[1]] <- ((M2[[2]] %*% t(W2[[2]]) %*% t(B1.wide)) * relu.prime(H2[[1]])) %*% B1.wide / nrow(B1.wide) * ncol(B1.wide)
	  M2[[1]] <- ((M2[[2]] %*% t(W2[[2]]) %*% t(B11.wide)) * relu.prime(H2[[1]])) %*% B11.wide / nrow(B11.wide) * ncol(B11.wide)
	}
	else if(nhidden == 1){
	  M2[[1]] <- ((M3 %*% t(W3) %*% t(B11.wide)) * activation.prime(H2[[1]])) %*% B11.wide / nrow(B11.wide) * ncol(B11.wide)
	}
	
	for (i in 1:nhidden){
    dW2[[i]] <- t(D2[[i]]) %*% M2[[i]]
    db2[[i]] <- colSums(M2[[i]])
	
    #dW2 <- t(D2) %*% M2/prod(dim(Y)) + lambda1/N * W2
    #db2 <- colSums(M2)/prod(dim(Y))
	}
	
	dW3 <- t(D3) %*% M3 + lambda1* W3 + lambda2 * W3  %*% penmat
	db3 <- colSums(M3) + lambda2 * penmat %*% b3
	
	
  } # else {
  #   list2env(B2, envir = environment())
  #   Y.residual <- (Y.hat - Y) %*% (B21 %x% B22)
  #   Mid <- ((Y.residual %*% t(W2) %*% t(B1)) * activation.prime(D2)) %*% B1
  #   dW1 <- t(D1) %*% Mid/prod(dim(Y)) + lambda/N * W1
  #   db1 <- colSums(Mid)/prod(dim(Y))
  #   dW2 <- t(H2) %*% Y.residual/prod(dim(Y)) + lambda/N * W2
  #   db2 <- colSums(Y.residual)/prod(dim(Y))
  # }

    
  grads <- mget(c("M2","M3","dW2","db2", "dW3" ,"db3"))
  #grads <- mget(c(dW1", "db1", "dW2", "db2", "dW3" ,"db3"))
  return(grads)
}