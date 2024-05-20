# back.propagation <- function(x, ...) UseMethod("back.propagation")
back.propagation <- function(Y, parameters, cache, D1, Bases0,
                             activation, activation.prime, 
                             lambda1 = 0, lambda2 = 0, penmat = 0){

  list2env(parameters, envir = environment())
  list2env(cache, envir = environment())
  list2env(Bases0, envir = environment())
  if (!is.list(B2)) {
    Y.hat <- C2 %*% t(B2)
    M2 <- (Y.hat - Y) %*% B2
    # M2 <- M2 + lambda2 * C2 %*% penmat
    M1 <- ((M2 %*% t(W2) %*% t(B1[[1]])) * activation.prime(H1)) %*% B1[[1]]
    dW1 <- t(D1) %*% M1 + lambda1* W1
    db1 <- colSums(M1)
    dW2 <- t(D2) %*% M2 + lambda1 * W2 + lambda2 * W2  %*% penmat 
    db2 <- colSums(M2) + lambda2 * penmat %*% b2
  } 
  
  grads <- mget(c("dW1", "db1", "dW2", "db2"))
  return(grads)
}