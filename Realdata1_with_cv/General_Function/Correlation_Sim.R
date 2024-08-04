# -------------- null ---------------------
Corr.null <- function(Y.train, Y.test, valid = T){
  if (length(dim(Y.train)) == 2) {
    Y.mean <- colMeans(Y.train)
    train.error.null <- mse(Y.train, rep.row(Y.mean, nrow(Y.train)))
    train.corr.null <- cor(Y.train, rep.row(Y.mean, nrow(Y.train)))
    test.error.null <- mse(Y.test, rep.row(Y.mean, nrow(Y.test)))
    test.corr.null <- cor(Y.test, rep.row(Y.mean, nrow(Y.test)))
  }
  if (length(dim(Y.train)) == 3) {
    Y.mean <- apply(Y.train, c(2, 3), mean)
    train.corr.null <- cor(Y.train, aperm(replicate(dim(Y.train)[1], Y.mean)
                                         , perm = c(3, 1, 2)))
    test.corr.null <- cor(Y.test, aperm(replicate(dim(Y.test)[1], Y.mean)
                                       , perm = c(3, 1, 2)))
  }
  if (!valid)  return(c(train.corr.null, test.corr.null, 0))
  else return(c(train.corr.null, train.corr.null, test.corr.null, 0))
}

Corr.index <- function(Y, Y.hat, index){
  result.corr <- rep(0, length(index))
  for (i in 1: length(index)) {
    #result.corr[i] <- cor(subs(Y, index[[i]]), subs(Y.hat, index[[i]]))
    result.corr[i] <- cor(Y[rownames(Y) %in% index[[i]]], Y.hat[rownames(Y.hat) %in% index[[i]]])
    
  }
  return(result.corr)
}

# ------------------ flm -------------------------
Corr.flm <- function(index, lambda1, G.train, Y.train, G.test, Y.test, Bases){
  set.seed(index)

  X.train <- as.matrix(G.train %*% Bases$B0)
  X.train <- cbind(X.train,1)
  X.test <- as.matrix(G.test %*% Bases$B0)
  X.test <- cbind(X.test,1)
  icoef <- ginv(t(X.train) %*% X.train + lambda1 * diag(1, ncol(X.train), ncol(X.train))) %*% t(X.train)%*% Y.train
  train.corr <- cor(Y.train, X.train %*% icoef)
  test.corr <- cor(Y.test, X.test %*% icoef)
  return(c(train.corr, test.corr))
  #return(c(train.corr, train.corr, test.corr))

}

# Corr.nn <- function(parameters, G, Y, index, Bases, activation){
#   Y.hat <- pred(parameters, G, Bases, activation) %*% t(Bases$B2)
#   corr.nn <- Corr.index(Y, Y.hat, index)
#   return(corr.nn)
#  #return(Y.hat)
# }

Corr.nn <- function(parameters, G.train, Y.train, G.test, Y.test, Bases, activation){
  Y.hat.train <- pred(parameters, G.train, Bases, activation) %*% t(Bases$B2)
  corr.train <- cor(Y.train[,1], Y.hat.train)
  Y.hat.test <- pred(parameters, G.test, Bases, activation) %*% t(Bases$B2)
  corr.test <- cor(Y.test[,1], Y.hat.test)  
  corr.nn <- c(corr.train, corr.test)
  return(corr.nn)
}

Corr.index1 <- function(Y, Y.hat, index){
  result.corr <- rep(0, length(index))
  for (i in 1: length(index)) {
    result.corr[i] <- cor(Y[names(Y) %in% index[[i]]], Y.hat[names(Y.hat) %in% index[[i]]])
    
  }
  return(result.corr)
}

# ## add another layer
# Corr.fnn2 <- function(parameters, G, Y, index, Bases, activation, activation2, nhidden){
#   Y.hat <- pred2(parameters, Y,  G, Bases, activation, activation2, nhidden =nhidden)
#   corr.fnn2 <- Corr.index1(Y[,1], Y.hat, index)
#   return(corr.fnn2)
# }

Corr.fnn2 <- function(parameters, G.train, Y.train,G.test, Y.test, Bases, activation, activation2, nhidden){
  #Y.hat <- pred2(parameters, G, Bases, activation, activation2, nhidden =nhidden) %*% t(Bases$B2)
  Y.hat.train <- pred2(parameters, Y.train,  G.train, Bases, activation, activation2, nhidden =nhidden)
  corr.train <- cor(Y.train[,1], Y.hat.train)
  
  Y.hat.test <- pred2(parameters, Y.test,  G.test, Bases, activation, activation2, nhidden =nhidden)
  corr.test <- cor(Y.test[,1], Y.hat.test)
  corr.fnn2 <- c(corr.train, corr.test)
  return(corr.fnn2)
}

## FNN wide
Corr.FNNwide <- function(parameters, G.wide.train, Y.train, G.wide.test, Y.test, Bases.wide, activation, activation2, nhidden,
                           parameters.pre, Bases.pre,ninput, nhidden.pre){
  
  list2env(Bases.pre, envir = environment())
  list2env(Bases.wide, envir = environment())
  
  Y.hat.train <- pred.wide(parameters, Y.train, G.wide.train, Bases.wide, Bases.pre, activation, activation2, nhidden = nhidden,
                           parameters.pre = parameters.pre,  ninput = ninput, nhidden.pre = nhidden.pre)
  corr.train <- cor(Y.train[,1], Y.hat.train)
  Y.hat.test <- pred.wide(parameters, Y.test, G.wide.test, Bases.wide, Bases.pre, activation, activation2, nhidden = nhidden,
                          parameters.pre = parameters.pre,  ninput = ninput, nhidden.pre = nhidden.pre)
  corr.test <- cor(Y.test[,1], Y.hat.test)
  corr.FNNwide <- c(corr.train, corr.test)
  return(corr.FNNwide)
}
