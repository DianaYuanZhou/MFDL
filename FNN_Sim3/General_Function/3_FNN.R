FNN <- function(D0, Y, Bases, G.valid = NULL, Y.valid = NULL, lr = 0.3, 
                epoch = 3e3, lambda1 = 0, lambda2 = 0, activation = sigmoid, 
                valid.goal = 0, parameters = NULL, verbose = 10, p = 100,
                penmat = NULL){
  if (is.null(penmat))
    penmat <- diag(nrow = ncol(Bases$B2), ncol = ncol(Bases$B2))
  if (length(lambda1) == 1) {
    if (length(lambda2) == 1) {
      return(FNN0(D0 = D0, Y = Y, Bases = Bases, G.valid = G.valid, 
                  Y.valid = Y.valid, lr = lr, epoch = epoch, 
                  lambda1 = lambda1, lambda2 = lambda2, 
                  activation = activation, valid.goal = valid.goal,
                  parameters = parameters, verbose = verbose, p = p,
                  penmat = penmat))
    } else {
      valid.error <- rep(0, length(lambda2))
      results <- list()
      for (i in 1:length(lambda2)) {
        lambda <- lambda2[i]
        result <- FNN0(D0 = D0, Y = Y, Bases = Bases, G.valid = G.valid,
                      Y.valid = Y.valid, lr = lr, epoch = epoch, 
                      lambda2 = lambda, activation = activation, 
                      valid.goal = valid.goal, parameters = parameters, 
                      verbose = verbose, p = p, penmat = penmat)
        Y.valid.hat <- pred(result$parameters, G.valid,
                            Bases, activation) %*% t(Bases$B2)
        valid.error[i] <- mse(Y.valid, Y.valid.hat)
        results[[i]] <- result
      }
    }
  } else {
    valid.error <- rep(0, length(lambda1))
    results <- list()
    for (i in 1:length(lambda1)) {
      lambda <- lambda1[i]
      result <- FNN0(D0 = D0, Y = Y, Bases = Bases, G.valid = G.valid,
                    Y.valid = Y.valid, lr = lr, epoch = epoch, 
                    lambda1 = lambda, activation = activation, 
                    valid.goal = valid.goal, parameters = parameters, 
                    verbose = verbose, p = p, penmat = penmat)
      Y.valid.hat <- pred(result$parameters, G.valid,
                          Bases, activation) %*% t(Bases$B2)
      valid.error[i] <- mse(Y.valid, Y.valid.hat)
      results[[i]] <- result
    }
  }
  i <- which.min(valid.error)
  print(valid.error)
  cat(i, "\n")
  return(results[[i]])
}