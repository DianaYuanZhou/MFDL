# back.propagation <- function(x, ...) UseMethod("back.propagation")
back.propagation <- function(Y, parameters, cache, D1, Bases0,
                             activation, activation.prime, 
                             lambda1 = 0, lambda2 = 0, penmat = 0){
  N <- dim(D1)[1];
  list2env(parameters, envir = environment())
  list2env(cache, envir = environment())
  list2env(Bases0, envir = environment())
  if (!is.list(B2)) {
    Y.hat <- C2 %*% t(B2)
    M2 <- (Y.hat - Y) %*% B2 / nrow(B2) * ncol(B2)
    # M2 <- M2 + lambda2 * C2 %*% penmat
    M1 <- ((M2 %*% t(W2) %*% t(B1)) * activation.prime(H1)) %*% B1 / nrow(B1) * ncol(B1)
    dW1 <- t(D1) %*% M1/prod(dim(Y)) + lambda1/N * W1
    db1 <- colSums(M1)/prod(dim(Y))
    dW2 <- t(D2) %*% M2/prod(dim(Y)) + lambda1/N * W2 + lambda2 * W2  %*% penmat 
    db2 <- colSums(M2)/prod(dim(Y)) + lambda2 * penmat %*% b2
  } # else {
  #   list2env(B2, envir = environment())
  #   Y.residual <- (Y.hat - Y) %*% (B21 %x% B22)
  #   Mid <- ((Y.residual %*% t(W2) %*% t(B1)) * activation.prime(D2)) %*% B1
  #   dW1 <- t(D1) %*% Mid/prod(dim(Y)) + lambda/N * W1
  #   db1 <- colSums(Mid)/prod(dim(Y))
  #   dW2 <- t(H2) %*% Y.residual/prod(dim(Y)) + lambda/N * W2
  #   db2 <- colSums(Y.residual)/prod(dim(Y))
  # }
  grads <- mget(c("dW1", "db1", "dW2", "db2"))
  return(grads)
}