forward.propagation2 <- function(D1, parameters, B1, activation, activation2,nhidden){
  N <- nrow(D1)
  list2env(parameters, envir = environment())
  C1 <- D1 %*% W[[1]] + matrix(rep(b[[1]], N), nrow = N, byrow = T)
  H1 <- C1 %*% t(B1[[1]])
  
  D <- list()
  C <- list()
  H <- list()
  D[[1]] <- D1
  C[[1]] <- C1
  H[[1]] <- H1
  
  n <- length(nhidden)
  if (n >= 2){
  
     for (i in 2:n){
	 # Defualt activation function: tanh
	 #D[[i]] <- activation(H[[i-1]]) %*% B1 / nrow(B1) * ncol(B1)
     #NB <- ncol(B1[[i-1]])
      NB <- 1
	   D[[i]] <- activation2(H[[i-1]]) %*% B1[[i-1]]/NB
     C[[i]] <- D[[i]] %*% W[[i]] + matrix(rep(b[[i]], N), nrow = N, byrow = T)
     H[[i]] <- C[[i]] %*% t(B1[[i]])
     }
  }
  
  #NB <- ncol(B1[[n]])
  NB <- 1
  D3 <- activation(H[[n]]) %*% B1[[n]]/ NB
  C3 <- D3 %*% W3 + matrix(rep(b3, N), nrow = N, byrow = T)
  
  cache <- mget(c("D","C","H","D3","C3"))
  #cache <- mget(c(namelist, "D3" ,"C3"),-c("d[[1]]","C[[1]]"))
  return(cache)
}