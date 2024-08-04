MAE.null <- function(Y.train, Y.test, valid = T){
  if (length(dim(Y.train)) == 2) {
    Y.mean <- colMeans(Y.train)
    train.mae.null <- mae(Y.train, rep.row(Y.mean, nrow(Y.train)))
    test.mae.null <- mae(Y.test, rep.row(Y.mean, nrow(Y.test)))
  }
  if (length(dim(Y.train)) == 3) {
    Y.mean <- apply(Y.train, c(2, 3), mean)
    train.mae.null <- mae(Y.train, aperm(replicate(dim(Y.train)[1], Y.mean)
                                           , perm = c(3, 1, 2)))
    test.mae.null <- mae(Y.test, aperm(replicate(dim(Y.test)[1], Y.mean)
                                         , perm = c(3, 1, 2)))
  }
  if (!valid)  return(c(train.mae.null, test.mae.null, 0))
  else return(c(train.mae.null, train.mae.null, test.mae.null, 0))
}

MAE.index <- function(Y, Y.hat, index){
  result <- rep(0, length(index))
  for (i in 1:length(index)) {
    #result[i] <- mse(subs(Y, index[[i]]), subs(Y.hat, index[[i]]))
    result[i] <- mae(Y[rownames(Y) %in% index[[i]]], Y.hat[rownames(Y.hat) %in% index[[i]]])
  }
  return(result)
}

MAE.flm <- function(index, lambda1, G.train, Y.train, G.test, Y.test,
                     Bases, valid = T){
  set.seed(index)
  
  X.train <- as.matrix(G.train %*% Bases$B0)
  X.train <- cbind(X.train,1)
  X.test <- as.matrix(G.test %*% Bases$B0)
  X.test <- cbind(X.test,1)
  icoef <- ginv(t(X.train) %*% X.train + lambda1 * diag(1, ncol(X.train), ncol(X.train))) %*% t(X.train)%*% Y.train
  train.mae <- mae(Y.train, X.train %*% icoef)
  test.mae <- mae(Y.test, X.test %*% icoef)
  
  if (!valid)
    return(c(train.mae, test.mae, 0))
  else
    return(c(train.mae, train.mae, test.mae, 0))
  
}


# MAE.nn <- function(parameters, G, Y, index, Bases, activation){
#   Y.hat <- pred(parameters, G, Bases, activation) %*% t(Bases$B2)
#   mae.nn <- MAE.index(Y, Y.hat, index)
#   return(mae.nn)
# }

MAE.nn <- function(parameters, G.train, Y.train, G.test, Y.test, Bases, activation){
  Y.hat.train <- pred(parameters, G.train, Bases, activation) %*% t(Bases$B2)
  mae.train <- mae(Y.train[,1], Y.hat.train)
  Y.hat.test <- pred(parameters, G.test, Bases, activation) %*% t(Bases$B2)
  mae.test <- mae(Y.test[,1], Y.hat.test)  
  mae.nn <- c(mae.train, mae.test)
  return(mae.nn)
}

MAE.index1 <- function(Y, Y.hat, index){
  result <- rep(0, length(index))
  for (i in 1:length(index)) {
    result[i] <- mae(Y[names(Y) %in% index[[i]]], Y.hat[names(Y.hat) %in% index[[i]]])
  }
  return(result)
}


# MAE.fnn2 <- function(parameters, G, Y, index, Bases, activation, activation2, nhidden){
#   #Y.hat <- pred2(parameters, G, Bases, activation, activation2, nhidden =nhidden) %*% t(Bases$B2)
#   Y.hat <- pred2(parameters, Y,  G, Bases, activation, activation2, nhidden =nhidden)
#   mae.fnn2 <- MAE.index1(Y[,1], Y.hat, index)
#   return(mae.fnn2)
# }

MAE.fnn2 <- function(parameters, G.train, Y.train,G.test, Y.test, Bases, activation, activation2, nhidden){
  #Y.hat <- pred2(parameters, G, Bases, activation, activation2, nhidden =nhidden) %*% t(Bases$B2)
  Y.hat.train <- pred2(parameters, Y.train,  G.train, Bases, activation, activation2, nhidden =nhidden)
  mae.train <- mae(Y.train[,1], Y.hat.train)
  
  Y.hat.test <- pred2(parameters, Y.test,  G.test, Bases, activation, activation2, nhidden =nhidden)
  mae.test <- mae(Y.test[,1], Y.hat.test)
  mae.fnn2 <- c(mae.train, mae.test)
  return(mae.fnn2)
}

## FNN wide
# MAE.FNNwide <- function(parameters, G.wide, Y, index, Bases.wide, activation, activation2, nhidden,
#                            parameters.pre, Bases.pre,ninput, nhidden.pre){
#   
#   list2env(Bases.pre, envir = environment())
#   list2env(Bases.wide, envir = environment())
#   # Bases0.pre <- mget(c("B0.pre","B1.pre"))
#   # Bases0 <- mget(c("B1.wide","B11.wide", "B2"))
#   
#   Y.hat <- pred.wide(parameters, Y, G.wide, Bases.wide, Bases.pre, activation, activation2, nhidden = nhidden,
#                   parameters.pre = parameters.pre,  ninput = ninput, nhidden.pre = nhidden.pre)
#   error.FNNwide <- MAE.index1(Y[,1], Y.hat, index)
#   return(error.FNNwide)
# }

MAE.FNNwide <- function(parameters, G.wide.train, Y.train, G.wide.test, Y.test, Bases.wide, activation, activation2, nhidden,
                          parameters.pre, Bases.pre,ninput, nhidden.pre){
  
  list2env(Bases.pre, envir = environment())
  list2env(Bases.wide, envir = environment())
  
  Y.hat.train <- pred.wide(parameters, Y.train, G.wide.train, Bases.wide, Bases.pre, activation, activation2, nhidden = nhidden,
                           parameters.pre = parameters.pre,  ninput = ninput, nhidden.pre = nhidden.pre)
  mae.train <- mae(Y.train[,1], Y.hat.train)
  Y.hat.test <- pred.wide(parameters, Y.test, G.wide.test, Bases.wide, Bases.pre, activation, activation2, nhidden = nhidden,
                          parameters.pre = parameters.pre,  ninput = ninput, nhidden.pre = nhidden.pre)
  mae.test <- mae(Y.test[,1], Y.hat.test)
  mae.FNNwide <- c(mae.train, mae.test)
  return(mae.FNNwide)
}
