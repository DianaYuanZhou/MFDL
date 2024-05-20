forward.propagation <- function(D1, parameters, B1, activation){
  N <- nrow(D1)
  list2env(parameters, envir = environment())
  C1 <- D1 %*% W1 + matrix(rep(b1, N), nrow = N, byrow = T)
  H1 <- C1 %*% t(B1)
  D2 <- activation(H1) %*% B1 / nrow(B1) * ncol(B1)
  C2 <- D2 %*% W2 + matrix(rep(b2, N), nrow = N, byrow = T)
  # if (!is.list(B2)) {
  #   Y.hat <- (M2 %*% W2 + matrix(rep(b2, N), nrow = N, byrow = T)) %*% t(B2)
  # } else {
  #   list2env(B2, envir = environment())
  #   Y.hat <- sweep(M2 %*% W2 / ncol(M2), 1, b2, "+") %*% t(B21 %x% B22)
  # }
  cache <- mget(c("H1", "D2", "C2"))
  return(cache)
}