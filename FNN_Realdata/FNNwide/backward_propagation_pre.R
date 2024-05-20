# back.propagation <- function(x, ...) UseMethod("back.propagation")
BP.pre <- function(Y, parameters.pre, cache.pre, D1.input, B1, Bases.pre, parameters, cache, Bases.wide,
								activation, activation.prime, activation2, activation2.prime,
                lambda1 = 0, lambda2 = 0, penmat = 0, ninput, nhidden.pre){

  list2env(parameters.pre, envir = environment())
  list2env(cache.pre, envir = environment())
  list2env(Bases.pre, envir = environment())
  list2env(parameters, envir = environment())
  list2env(cache, envir = environment())
  list2env(Bases.wide, envir = environment())

  if (!is.list(B2)) {

	M1.pre <- list()
	M2.pre <- list()
	dW1.pre <- list()
	db1.pre <- list()	
	dW2.pre <- list()
	db2.pre <- list()
	
  p1 <- ncol(H2.pre[[1]][[nhidden.pre]])
  p2 <- ncol(H2.pre[[2]][[nhidden.pre]])
	I <- list()
  I[[1]] <- diag(1, p1, p1+p2)
  I[[2]] <- cbind(diag(0, p2, p1), diag(1, p2, p2))
  
  # H3.pre<-list()
  # H3 <- list()
  # H3[[1]] <-0
  # H3[[1]] <- cache.pre$H2.pre[[1]][[nhidden.pre]]
  # if (ninput >=2 ){
  #   for (i in 2:ninput){
  #     H3.pre[[i]] <- 0  
  #     H3.pre[[i]]<- cache.pre$H2.pre[[i]][[nhidden.pre]]
  #     H3[[1]] <- cbind(H3[[1]], H3.pre[[i]])
  #   }
  # }

	for (i in 1:ninput){
	   
	   M2.pre[[i]] <- list()

	if (nhidden.pre >= 2){
     M2.pre[[i]][[nhidden.pre]] <- ((M2[[1]] %*% t(W2[[1]]) %*% t(B1.wide) %*% t(I[[i]])) * 
                                      activation2.prime(H2.pre[[i]][[nhidden.pre]])) %*% B1.pre[[i]]
     
     for (j in 1:(nhidden.pre-1)){
  	 M2.pre[[i]][[nhidden.pre-j]] <- ((M2.pre[[i]][[nhidden.pre-j+1]] %*% t(W2.pre[[i]][[nhidden.pre-j+1]]) %*% t(B1.pre[[i]])) * 
  	                                    activation2.prime(H2.pre[[i]][[nhidden.pre-j]])) %*% B1.pre[[i]]
  	 
  	 }
	}
     else {
	     M2.pre[[i]][[1]] <- ((M2[[1]] %*% t(W2[[1]]) %*% t(B1.wide) %*% t(I[[i]])) * 
	                            activation2.prime(H2.pre[[i]][[nhidden.pre]])) %*% B1.pre[[i]]
	     
	   }
	   M1.pre[[i]] <- 0  
	   M1.pre[[i]] <- ((M2.pre[[i]][[1]] %*% t(W2.pre[[i]][[1]]) %*% t(B1.pre[[i]])) * 
	                     activation2.prime(H1.pre[[i]])) %*% B1.pre[[i]]
	   
	   dW2.pre[[i]] <- list()
     db2.pre[[i]] <- list()
    
	  for (j in 1:nhidden.pre){
    #NB1 <- ncol(B1[[i]])
	    NB1 <- 1
    dW.temp <- t(D2.pre[[i]][[j]]) %*% M2.pre[[i]][[j]]/NB1
    db.temp <- colSums(M2.pre[[i]][[j]])
    
    dW2.pre[[i]][[j]] <- dW.temp + lambda1 * W2.pre[[i]][[j]]
    db2.pre[[i]][[j]] <- db.temp

	}
	  #NB0.pre <- ncol(B0.pre[[i]])
     NB0.pre <- 1
	  dW1.pre[[i]] <- t(D1.input[[i]]) %*% M1.pre[[i]]/NB0.pre  + lambda1 * W1.pre[[i]]
	  db1.pre[[i]] <- colSums(M1.pre[[i]])

  } 
	
  }	
  grads.pre <- mget(c("dW1.pre","db1.pre","dW2.pre","db2.pre"))
  #grads <- mget(c(dW1", "db1", "dW2", "db2", "dW3" ,"db3"))
  return(grads.pre)
}