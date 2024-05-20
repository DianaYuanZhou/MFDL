Error.null <- function(Y.train, Y.test, valid = T){
  if (length(dim(Y.train)) == 2) {
    Y.mean <- colMeans(Y.train)
    train.error.null <- mse(Y.train, rep.row(Y.mean, nrow(Y.train)))
    test.error.null <- mse(Y.test, rep.row(Y.mean, nrow(Y.test)))
  }
  if (length(dim(Y.train)) == 3) {
    Y.mean <- apply(Y.train, c(2, 3), mean)
    train.error.null <- mse(Y.train, aperm(replicate(dim(Y.train)[1], Y.mean)
                                           , perm = c(3, 1, 2)))
    test.error.null <- mse(Y.test, aperm(replicate(dim(Y.test)[1], Y.mean)
                                         , perm = c(3, 1, 2)))
  }
  if (!valid)  return(c(train.error.null, test.error.null, 0))
  else return(c(train.error.null, train.error.null, test.error.null, 0))
}

Error.index <- function(Y, Y.hat, index){
  result <- rep(0, length(index))
  for (i in 1:length(index)) {
    #result[i] <- mse(subs(Y, index[[i]]), subs(Y.hat, index[[i]]))
    result[i] <- mse(Y[rownames(Y) %in% index[[i]]], Y.hat[rownames(Y.hat) %in% index[[i]]])
  }
  return(result)
}

Pred.flm <-function(index, lambda1, G.train, Y.train, G.test, Y.test,
                    Bases, valid = T, ratio){
  set.seed(index)
  X.train <- as.matrix(G.train %*% Bases$B0)
  X.train <- cbind(X.train,1)
  X.test <- as.matrix(G.test %*% Bases$B0)
  X.test <- cbind(X.test,1)
  icoef <- ginv(t(X.train) %*% X.train + lambda1 * diag(1, ncol(X.train), ncol(X.train))) %*% t(X.train) %*% Y.train
  train.flm <- mse(Y.train, X.train %*% icoef)
  test.flm <- mse(Y.test, X.test %*% icoef)
  
  if (!valid)
    return(c(train.flm, test.flm, 1/ratio))
  else
    return(c(train.flm, train.flm, test.flm, 1/ratio))
}

Error.flm <- function(index, lambda1.flm, G.train, Y.train, G.test, Y.test,
                      Bases, valid = T, ratio){
  allindex <- sample(nrow(G.train))
  folds <- cut(allindex,breaks=5,labels=FALSE)
  
  test.mse <- seq(length(lambda1.flm))
  for(i in lambda1.flm){
    test.mse.cv <- seq(5)
    for(fold in 1:5){
      testindex <- which(folds == fold)
      result <- Pred.flm(index, lambda1 = i, G.train[-testindex,], Y.train[-testindex,], G.train[testindex,], Y.train[testindex,],
                         Bases = Bases, valid = valid, ratio = ratio)
      test.mse.cv[fold] <-  result[length(result)-1]
    }
    test.mse <- mean(test.mse.cv)
  }
  
  lambda1.best <- lambda1.flm[which.min(test.mse)]
  test.flm <- Pred.flm(index, lambda1.best, G.train, Y.train, G.test, Y.test,
                       Bases = Bases, valid = valid, ratio = ratio)
  return(c(test.flm, lambda1.best))
}


Error.nn <- function(parameters, G, Y, index, Bases, activation){
  Y.hat <- pred(parameters, G, Bases, activation) %*% t(Bases$B2)
  error.nn <- Error.index(Y, Y.hat, index)
  return(error.nn)
}

Error.index1 <- function(Y, Y.hat, index){
  result <- rep(0, length(index))
  for (i in 1:length(index)) {
    result[i] <- mse(Y[names(Y) %in% index[[i]]], Y.hat[names(Y.hat) %in% index[[i]]])
  }
  return(result)
}


Error.fnn2 <- function(parameters, G, Y, index, Bases, activation, activation2, nhidden){
  #Y.hat <- pred2(parameters, G, Bases, activation, activation2, nhidden =nhidden) %*% t(Bases$B2)
  Y.hat <- pred2(parameters, Y,  G, Bases, activation, activation2, nhidden =nhidden)
  error.fnn2 <- Error.index1(Y[,1], Y.hat, index)
  return(error.fnn2)
}

## FNN wide
Error.FNNwide <- function(parameters, G.wide, Y, index, Bases.wide, activation, activation2, nhidden,
                           parameters.pre, Bases.pre,ninput, nhidden.pre){
  
  list2env(Bases.pre, envir = environment())
  list2env(Bases.wide, envir = environment())
  # Bases0.pre <- mget(c("B0.pre","B1.pre"))
  # Bases0 <- mget(c("B1.wide","B11.wide", "B2"))
  
  Y.hat <- pred.wide(parameters, Y, G.wide, Bases.wide, Bases.pre, activation, activation2, nhidden = nhidden,
                  parameters.pre = parameters.pre,  ninput = ninput, nhidden.pre = nhidden.pre)
  error.FNNwide <- Error.index1(Y[,1], Y.hat, index)
  return(error.FNNwide)
}
