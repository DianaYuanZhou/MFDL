FP.wide <- function(H1, parameters, B1.wide, B11.wide, activation, nhidden){
  N <- nrow(H1)
  list2env(parameters, envir = environment())

  D2 <- list()
  C2 <- list()
  H2 <- list()
  
  D2[[1]] <- relu(H1) %*% B1.wide / nrow(B1.wide) * ncol(B1.wide)
  C2[[1]] <- D2[[1]] %*% W2[[1]] + matrix(rep(b2[[1]], N), nrow = N, byrow = T)
  H2[[1]] <- C2[[1]] %*% t(B11.wide)
  
  if (nhidden >= 2){
  
     for (i in 2:nhidden){
	 # Defualt activation function: tanh
	 #D[[i]] <- activation(H[[i-1]]) %*% B1 / nrow(B1) * ncol(B1)
	   D2[[i]] <- relu(H2[[i-1]]) %*% B11.wide / nrow(B11.wide) * ncol(B11.wide)
     C2[[i]] <- D2[[i]] %*% W2[[i]] + matrix(rep(b2[[i]], N), nrow = N, byrow = T)
     H2[[i]] <- C2[[i]] %*% t(B11.wide)
     }
  }
  
  D3 <- activation(H2[[nhidden]]) %*% B11.wide / nrow(B11.wide) * ncol(B11.wide)
  C3 <- D3 %*% W3 + matrix(rep(b3, N), nrow = N, byrow = T)
  

  
  # if (!is.list(B2)) {
  #   Y.hat <- (M2 %*% W2 + matrix(rep(b2, N), nrow = N, byrow = T)) %*% t(B2)
  # } else {
  #   list2env(B2, envir = environment())
  #   Y.hat <- sweep(M2 %*% W2 / ncol(M2), 1, b2, "+") %*% t(B21 %x% B22)
  # }
  
  cache <- mget(c("H1","D2","C2","H2","D3","C3"))
  #cache <- mget(c(namelist, "D3" ,"C3"),-c("d[[1]]","C[[1]]"))
  return(cache)
}