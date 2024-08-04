pred <- function(parameters, D0, Bases, activation){
  list2env(Bases, envir = environment())
  D1 <- as.matrix(D0 %*% B0 / nrow(B0) * ncol(B0))
  C2 <- forward.propagation(D1, parameters, B1, activation)$C2
  return(C2)
}