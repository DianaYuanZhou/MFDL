pred2 <- function(parameters,Y, D0, Bases, activation, activation2, nhidden){
  list2env(Bases, envir = environment())
  #N <- ncol(B0)
  B2 <- cb2(Bases, Y, D0)
  N <- 1
  D1 <- as.matrix(D0 %*% B0 / N)
  C3 <- forward.propagation2(D1, parameters, B1, activation, activation2, nhidden = nhidden)$C3
  Y.pred <- rowSums(C3[as.numeric(rownames(B2)), , drop = FALSE] * B2)
  return(Y.pred)
}