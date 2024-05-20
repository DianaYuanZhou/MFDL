pred.wide <- function(parameters, D0, Bases0, Bases0.pre, activation, activation2, nhidden.wide,
                   parameters.pre, ninput, nhidden.pre){
  list2env(Bases0, envir = environment())
  list2env(Bases0.pre, envir = environment())
  list2env(parameters.pre, envir = environment())
  
  D1.input <- list()
  #NB0.pre <- ncol(B0.pre[[i]]))
  NB0.pre <- 1
  for (i in 1:ninput){
    D1.input[[i]] <- as.matrix(D0[[i]] %*% B0.pre[[i]] / NB0.pre)
  }
  
  cache.pre <- FP.pre(D1.input, parameters.pre, B1.pre, activation, activation2, ninput, nhidden.pre)
  
  H3 <-list()
  H3.pre <- list()
  #H3[[1]] <- relu(cache.pre$H2.pre[[1]][[nhidden.pre]])
  H3[[1]] <- activation2(cache.pre$H2.pre[[1]][[nhidden.pre]])  
  
  if (ninput >= 2){
  for (i in 2:ninput){
  H3.pre[[i]] <- 0
  #H3.pre[[i]] <- relu(cache.pre$H2.pre[[i]][[nhidden.pre]])
  H3.pre[[i]] <- activation2(cache.pre$H2.pre[[i]][[nhidden.pre]])
  
  H3[[1]] <-cbind(H3[[1]],H3.pre[[i]])
  }
  }
  
  C3 <- FP.wide(H3[[1]], parameters, B1.wide, B11.wide, activation, activation2, nhidden = nhidden.wide)$C3
  return(C3)
}