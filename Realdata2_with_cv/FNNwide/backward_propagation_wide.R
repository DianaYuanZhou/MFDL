# back.propagation <- function(x, ...) UseMethod("back.propagation")
BP.wide <- function(Y, parameters, cache, D1, Bases.wide,
                             activation, activation.prime, activation2, activation2.prime,
                             lambda1 = 0, lambda2 = 0, penmat = 0, nhidden){

  list2env(parameters, envir = environment())
  list2env(cache, envir = environment())
  list2env(Bases.wide, envir = environment())
  if (!is.list(B2)) {

    # Y.hat <- C3 %*% t(B2)
    # M3 <- 2*(Y.hat - Y) %*% B2
    Y.hat <- rowSums(cache$C3[as.numeric(rownames(B2)), , drop = FALSE] * B2)
    M3 <- 2*(Y.hat - Y[,1])*B2
    id <- as.numeric(rownames(B2))
    M3. <- outer(1:max(id), id, "==") * 1
    M3 <- M3. %*% M3

	  M2 <- list()
	  dW2 <-list()
	  db2 <-list()
	
	if (nhidden >= 3){
	    for (i in 1:(nhidden-1)){
     M2[[nhidden]] <- ((M3 %*% t(W3) %*% t(B11.wide)) * activation.prime(H2[[nhidden]])) %*% B11.wide
     M2[[nhidden-i]] <- ((M2[[nhidden-i+1]] %*% t(W2[[nhidden-i+1]]) %*% t(B11.wide)) * activation2.prime(H2[[nhidden-i]])) %*% B11.wide
     M2[[1]] <- ((M2[[2]] %*% t(W2[[2]]) %*% t(B11.wide)) * activation2.prime(H2[[1]])) %*% B11.wide
     
		}
	}
	else if (nhidden == 2){
	  M2[[2]] <- ((M3 %*% t(W3) %*% t(B11.wide)) * activation.prime(H2[[2]])) %*% B11.wide
	  M2[[1]] <- ((M2[[2]] %*% t(W2[[2]]) %*% t(B11.wide)) * activation2.prime(H2[[1]])) %*% B11.wide
	  
	  }
	else if(nhidden == 1){
	  M2[[1]] <- ((M3 %*% t(W3) %*% t(B11.wide)) * activation.prime(H2[[1]])) %*% B11.wide
	}
	
	for (i in 1:nhidden){
	  #NB11.wide <- ncol(B11.wide)
	  NB11.wide <- 1
    dW2[[i]] <- t(D2[[i]]) %*% M2[[i]]/NB11.wide + lambda1* W2[[i]]
    db2[[i]] <- colSums(M2[[i]])
	}
	
	#NB11.wide <- ncol(B11.wide)
	NB11.wide <- 1
	dW3 <- t(D3) %*% M3/NB11.wide + lambda1* W3 + lambda2* W3 %*% penmat
	db3 <- colSums(M3) + lambda2* b3 %*% penmat
	#db3 <- colSums(M3)
	}
    
  grads <- mget(c("M2","M3","dW2","db2", "dW3" ,"db3"))

  return(grads)
}