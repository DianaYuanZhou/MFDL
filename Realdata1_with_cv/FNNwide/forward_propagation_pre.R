FP.pre <- function(D1.input, parameters.pre,
                    B1, activation, activation2, ninput, nhidden.pre){
  N <- nrow(D1.input[[1]])
  list2env(parameters.pre, envir = environment())
  
  D1.pre <- list()
  C1.pre <- list()
  H1.pre <-list()
  D2.temp <- list()
  C2.temp <- list()
  H2.temp <- list()
  D2.pre <- list()
  C2.pre <- list()
  H2.pre <- list()  

  for (i in 1:ninput){
    D1.pre[[i]] <- 0
    C1.pre[[i]] <- 0
    H1.pre[[i]] <- 0
    
    D1.pre[[i]] <- D1.input[[i]]
    C1.pre[[i]] <- D1.pre[[i]] %*% W1.pre[[i]] + matrix(rep(b1.pre[[i]], N), nrow = N, byrow = T)
    H1.pre[[i]] <- C1.pre[[i]] %*% t(B1[[i]])

    D2.pre[[i]] <- list()
    C2.pre[[i]] <- list()
    H2.pre[[i]] <- list()      
    
    D2.pre[[i]][[1]] <- 0
    C2.pre[[i]][[1]] <- 0
    H2.pre[[i]][[1]] <- 0
    
    #D2.pre[[i]][[1]] <- relu(H1.pre[[i]]) %*% B1[[i]]
    #NB1 <- ncol(B1[[i]])
    NB1 <-1 
    D2.pre[[i]][[1]] <- activation2(H1.pre[[i]]) %*% B1[[i]]/NB1
    C2.pre[[i]][[1]] <- D2.pre[[i]][[1]] %*% W2.pre[[i]][[1]] + matrix(rep(b2.pre[[i]][[1]], N), nrow = N, byrow = T)
    H2.pre[[i]][[1]] <- C2.pre[[i]][[1]] %*% t(B1[[i]]) 
    
  
    if (nhidden.pre >=2 ){

      for (j in 2:nhidden.pre){
      D2.pre[[i]][[j]] <- 0
      C2.pre[[i]][[j]] <- 0
      H2.pre[[i]][[j]] <- 0      
        
      #D2.temp <- relu(H2.pre[[i]][[j-1]]) %*% B1[[i]]
      #NB1 <- ncol(B1[[i]])
      NB1 <-1 
      D2.temp <- activation2(H2.pre[[i]][[j-1]]) %*% B1[[i]]/NB1
      D2.pre[[i]][[j]] <- D2.temp
      
      C2.temp <- D2.pre[[i]][[j]] %*% W2.pre[[i]][[j]] + matrix(rep(b2.pre[[i]][[j]], N), nrow = N, byrow = T)
      C2.pre[[i]][[j]] <- C2.temp
      
      H2.temp <- C2.pre[[i]][[j]] %*% t(B1[[i]])
      H2.pre[[i]][[j]] <- H2.temp
      }
      
    }

  }

  cache.pre <- mget(c("D1.pre","C1.pre","H1.pre","D2.pre","C2.pre","H2.pre"))
  return(cache.pre)
}
