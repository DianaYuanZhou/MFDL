FNN2 <- function(D0, Y, Bases, G.valid = NULL, Y.valid = NULL, lr = 0.3, 
                epoch = 3e3, lambda1 = 0, lambda2 = 0, activation = sigmoid,
                valid.goal = 0, parameters = NULL, verbose = 10, p = 100,
                penmat = NULL, nhidden, ADADELTA = F){

    ### Use this part when no panelty
    valid.error <- rep(0, length(lambda1))
    results <- list()
    for (i in 1:length(lambda1)) {
      lambda <- lambda1[i]
      result <- FNN20(D0 = D0, Y = Y, Bases = Bases, G.valid = G.valid,
                    Y.valid = Y.valid, lr = lr, epoch = epoch, 
                    lambda1 = lambda, activation = activation,
                    valid.goal = valid.goal, parameters = parameters, 
                    verbose = verbose, p = p, penmat = penmat, nhidden = nhidden, ADADELTA = ADADELTA)
      Y.valid.hat <- pred2(result$parameters, G.valid,
                          Bases, activation, nhidden = nhidden) %*% t(Bases$B2)
      valid.error[i] <- mse(Y.valid, Y.valid.hat)
      results[[i]] <- result
    }
 
  
  i <- which.min(valid.error)
  print(valid.error)
  cat(i, "\n")
  return(results[[i]])
}