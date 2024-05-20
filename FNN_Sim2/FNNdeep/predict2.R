pred2 <- function(parameters, D0, Bases, activation, nhidden){
  list2env(Bases, envir = environment())
  D1 <- as.matrix(D0 %*% B0 / nrow(B0) * ncol(B0))
  C3 <- forward.propagation2(D1, parameters, B1, activation, nhidden = nhidden)$C3
  return(C3)
}