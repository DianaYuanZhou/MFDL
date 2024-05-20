# -------------- null ---------------------
Corr.null <- function(Y.train, Y.test, valid = T){
  if (length(dim(Y.train)) == 2) {
    Y.mean <- colMeans(Y.train)
    train.error.null <- mse(Y.train, rep.row(Y.mean, nrow(Y.train)))
    train.corr.null <- RV(Y.train, rep.row(Y.mean, nrow(Y.train)))
    test.error.null <- mse(Y.test, rep.row(Y.mean, nrow(Y.test)))
    test.corr.null <- RV(Y.test, rep.row(Y.mean, nrow(Y.test)))
  }
  if (length(dim(Y.train)) == 3) {
    Y.mean <- apply(Y.train, c(2, 3), mean)
    train.corr.null <- RV(Y.train, aperm(replicate(dim(Y.train)[1], Y.mean)
                                         , perm = c(3, 1, 2)))
    test.corr.null <- RV(Y.test, aperm(replicate(dim(Y.test)[1], Y.mean)
                                       , perm = c(3, 1, 2)))
  }
  if (!valid)  return(c(train.corr.null, test.corr.null))
  else return(c(train.corr.null, train.corr.null, test.corr.null))
}

# ----------------- lm -----------------------
# Error.lm <- function(G.train, Y.train, G.test, Y.test, pos){
#   X.train <- cbind(G.train, 1)
#   X.test <- cbind(G.test, 1)
#   icoef <- ginv(t(X.train) %*% X.train) %*% t(X.train)%*% Y.train
#   train.lm <- mse(Y.train, X.train %*% icoef)
#   test.lm <- mse(Y.test, X.test %*% icoef)
#   MSE <- t(Y.test - X.test %*% icoef)%*%(Y.test -X.test %*% icoef)
#   var <- as.matrix(diag(ginv(t(X.test) %*% X.test))) %*% t(as.matrix(diag(MSE)))
#   MSE <- t(Y.train - X.train %*% icoef)%*%(Y.train -X.train %*% icoef)
#   var <- as.matrix(diag(ginv(t(X.train) %*% X.train))) %*% t(as.matrix(diag(MSE)))
#   return(c(train.lm, train.lm, test.lm, length(pos)))
# }

# ------------------ flm -------------------------
Corr.flm <- function(G1.train, G2.train, Y.train, G1.test, G2.test, Y.test, pos1, pos2, ratio, valid = T){
  nbasis1 <- ceiling(length(pos1)*ratio)
  bbasis1 <- create.bspline.basis(norder = 5, nbasis = nbasis1)
  
  if(length(pos2) < 5){
    B.flm <- bdiag(eval.basis(pos1, bbasis1), diag(1, 1, 1))
    G.train <- cbind(G1.train, G2.train)
    X.train <- as.matrix(G.train %*% B.flm)
    X.train <- cbind(X.train, 1)
    G.test <- cbind(G1.test, G2.test)
    X.test <- as.matrix(G.test %*% B.flm)
    X.test <- cbind(X.test, 1)
    icoef <- ginv(t(X.train) %*% X.train) %*% t(X.train)%*% Y.train
    train.corr <- RV(Y.train, X.train %*% icoef)
    test.corr <- RV(Y.test, X.test %*% icoef)
  }  else {
    B1.flm <- eval.basis(pos1, bbasis1)
    #nbasis2 <- ceiling(length(pos2)*ratio)
    nbasis2 <- length(pos2)
    bbasis2 <- create.bspline.basis(norder = 5, nbasis = nbasis2, rangeval = range(pos2))
    B2.flm <- eval.basis(pos2, bbasis2)
    
    X1.train <- as.matrix(G1.train %*% B1.flm)
    X2.train <- as.matrix(G2.train %*% B2.flm)
    X.train <- cbind(X1.train, X2.train, 1)
    X1.test <- as.matrix(G1.test %*% B1.flm)
    X2.test <- as.matrix(G2.test %*% B2.flm)
    X.test <- cbind(X1.test, X2.test, 1)
    icoef <- ginv(t(X.train) %*% X.train) %*% t(X.train)%*% Y.train
    train.corr <- RV(Y.train, X.train %*% icoef)
    test.corr <- RV(Y.test, X.test %*% icoef)
  }
  
  if (!valid) 
    return(c(train,corr, test.corr))
  else
    return(c(train.corr, train.corr, test.corr))
}

# ------------------ fnn --------------------------
Corr.index <- function(Y, Y.hat, index){
  result.corr <- rep(0, length(index))
  for (i in 1: length(index)) {
      result.corr[i] <- RV(subs(Y, index[[i]]), subs(Y.hat, index[[i]]))
    
  }
  return(result.corr)
}

Corr.fnn <- function(parameters, G, Y, index, Bases, activation){
  Y.hat <- pred(parameters, G, Bases, activation) %*% t(Bases$B2)
  corr.fnn <- Corr.index(Y, Y.hat, index)
  return(corr.fnn)
}


## add another layer
Corr.fnn2 <- function(parameters, G, Y, index, Bases, activation, nhidden){
  Y.hat <- pred2(parameters, G, Bases, activation, nhidden =nhidden) %*% t(Bases$B2)
  corr.fnn2 <- Corr.index(Y, Y.hat, index)
  return(corr.fnn2)
}

## FNN wide
Corr.FNNwide <- function(parameters, G, Y, index, Bases.wide, activation, nhidden,
                           parameters.pre, Bases.pre,ninput, nhidden.pre){
  
  list2env(Bases.pre, envir = environment())
  list2env(Bases.wide, envir = environment())
  Bases0.pre <- mget(c("B0.pre","B1.pre","B2"))
  Bases0 <- mget(c("B1.wide","B11.wide", "B2"))
  
  Y.hat <- pred.wide(parameters, G, Bases0 = Bases0, activation, nhidden = nhidden,
                  parameters.pre = parameters.pre, Bases0.pre = Bases0.pre, ninput = ninput, nhidden.pre = nhidden.pre) %*% t(Bases.wide$B2)
  corr.FNNwide <- Corr.index(Y, Y.hat, index)
  return(corr.FNNwide)
}
