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

# Pred.flm <-function(index, lambda1, G1.train, G2.train, Y.train, G1.test, G2.test, Y.test, 
#                     pos1, pos2, ratio = 0.1, ratio2 = 0.1, valid = T){
#   set.seed(index)
#   nbasis1 <- ceiling(length(pos1)*ratio)
#   bbasis1 <- create.bspline.basis(norder = 5, nbasis = nbasis1)
#   
#   if(length(pos2) < 10){
#     B.flm <- bdiag(eval.basis(pos1, bbasis1), diag(1, 1, 1))
#     G.train <- cbind(G1.train, G2.train)
#     X.train <- as.matrix(G.train %*% B.flm)
#     X.train <- cbind(X.train, 1)
#     G.test <- cbind(G1.test, G2.test)
#     X.test <- as.matrix(G.test %*% B.flm)
#     X.test <- cbind(X.test, 1)
#     icoef <- ginv(t(X.train) %*% X.train + lambda1 * diag(1, ncol(X.train), ncol(X.train))) %*% t(X.train) %*% Y.train
#     train.flm <- mse(Y.train, X.train %*% icoef)
#     test.flm <- mse(Y.test, X.test %*% icoef)
#   }  else {
#     B1.flm <- eval.basis(pos1, bbasis1)
#     nbasis2 <- ceiling(length(pos2)*ratio2)
#     bbasis2 <- create.bspline.basis(norder = 5, nbasis = nbasis2)
#     B2.flm <- eval.basis(pos2, bbasis2)
#     
#     X1.train <- as.matrix(G1.train %*% B1.flm)
#     X2.train <- as.matrix(G2.train %*% B2.flm)
#     X.train <- cbind(X1.train, X2.train, 1)
#     X1.test <- as.matrix(G1.test %*% B1.flm)
#     X2.test <- as.matrix(G2.test %*% B2.flm)
#     X.test <- cbind(X1.test, X2.test, 1)
#     icoef <- ginv(t(X.train) %*% X.train + lambda1 * diag(1, ncol(X.train), ncol(X.train))) %*% t(X.train)%*% Y.train
#     train.flm <- mse(Y.train, X.train %*% icoef)
#     test.flm <- mse(Y.test, X.test %*% icoef)
#   }
#   if (!valid) 
#     return(c(train.flm, test.flm, 1/ratio))
#   else
#     return(c(train.flm, train.flm, test.flm, 1/ratio))
# }
# 
# Error.flm <- function(index, lambda1, G1.train, G2.train, Y.train, G1.test, G2.test, Y.test, 
#                       pos1, pos2, ratio = 0.1, ratio2 = 0.1, valid = T){
#   allindex <- sample(nrow(G1.train))
#   folds <- cut(allindex,breaks=10,labels=FALSE)
#   
#   test.mse <- seq(length(lambda1))
#   for(i in lambda1){
#     test.mse.cv <- seq(10)
#     for(fold in 1:10){
#       testindex <- which(folds == fold)
#        result <- Pred.flm(index, lambda1 = i, G1.train[-testindex,], G2.train[-testindex,], Y.train[-testindex,],
#                                G1.train[testindex,], G2.train[testindex,], Y.train[testindex,],
#                                pos1, pos2, ratio = ratio, ratio2 = ratio2, valid = valid)
#        test.mse.cv[fold] <-  result[length(result)-1]
#     }
#     test.mse <- mean(test.mse.cv)
#   }
#   
#   lambda1.best <- lambda1[which.min(test.mse)]
#   test.flm <- Pred.flm(index, lambda1.best, G1.train, G2.train, Y.train, G1.test, G2.test, Y.test,
#                        pos1, pos2, ratio = ratio, ratio2 = ratio2, valid = valid)
#   return(c(test.flm, lambda1.best))
# }

Pred.flm <-function(index, lambda1, G1.train, Y.train, G1.test, Y.test, 
                    pos1, ratio = 0.1, valid = T){
  set.seed(index)
  nbasis1 <- ceiling(length(pos1)*ratio)
  bbasis1 <- create.bspline.basis(norder = 5, nbasis = nbasis1)
  
    B.flm <- eval.basis(pos1, bbasis1)
    G.train <- G1.train
    X.train <- as.matrix(G.train %*% B.flm)
    X.train <- cbind(X.train, 1)
    G.test <- G1.test
    X.test <- as.matrix(G.test %*% B.flm)
    X.test <- cbind(X.test, 1)
    icoef <- ginv(t(X.train) %*% X.train + lambda1 * diag(1, ncol(X.train), ncol(X.train))) %*% t(X.train) %*% Y.train
    train.flm <- mse(Y.train, X.train %*% icoef)
    test.flm <- mse(Y.test, X.test %*% icoef)

  if (!valid) 
    return(c(train.flm, test.flm, 1/ratio))
  else
    return(c(train.flm, train.flm, test.flm, 1/ratio))
}

Error.flm <- function(index, lambda1, G1.train, Y.train, G1.test, Y.test, 
                      pos1, ratio = 0.1, valid = T){
  allindex <- sample(nrow(G1.train))
  folds <- cut(allindex,breaks=10,labels=FALSE)
  
  test.mse <- seq(length(lambda1))
  for(i in lambda1){
    test.mse.cv <- seq(10)
    for(fold in 1:10){
      testindex <- which(folds == fold)
      result <- Pred.flm(index, lambda1 = i, G1.train[-testindex,], Y.train[-testindex,],
                         G1.train[testindex,], Y.train[testindex,],
                         pos1, ratio = ratio, valid = valid)
      test.mse.cv[fold] <-  result[length(result)-1]
    }
    test.mse <- mean(test.mse.cv)
  }
  
  lambda1.best <- lambda1[which.min(test.mse)]
  test.flm <- Pred.flm(index, lambda1.best, G1.train, Y.train, G1.test, Y.test,
                       pos1, ratio = ratio, valid = valid)
  return(c(test.flm, lambda1.best))
}

Error.fnn <- function(parameters, G, Y, index, Bases, activation){
  Y.hat <- pred(parameters, G, Bases, activation) %*% t(Bases$B2)
  error.fnn <- Error.index(Y, Y.hat, index)
  return(error.fnn)
}
Error.index <- function(Y, Y.hat, index){
  result <- rep(0, length(index))
  for (i in 1:length(index)) {
    result[i] <- mse(subs(Y, index[[i]]), subs(Y.hat, index[[i]]))
  }
  return(result)
}

## add another layer
Error.fnn2 <- function(parameters, G, Y, index, Bases, activation, activation2, nhidden){
  Y.hat <- pred2(parameters, G, Bases, activation, activation2, nhidden =nhidden) %*% t(Bases$B2)
  error.fnn2 <- Error.index(Y, Y.hat, index)
  return(error.fnn2)
}

## FNN wide
Error.FNNwide <- function(parameters, G.wide, Y, index, Bases.wide, activation, activation2, nhidden,
                           parameters.pre, Bases.pre,ninput, nhidden.pre){
  
  list2env(Bases.pre, envir = environment())
  list2env(Bases.wide, envir = environment())
  Bases0.pre <- mget(c("B0.pre","B1.pre","B2"))
  Bases0 <- mget(c("B1.wide","B11.wide", "B2"))
  
  Y.hat <- pred.wide(parameters, G.wide, Bases0 = Bases0, activation, activation2, nhidden = nhidden,
                  parameters.pre = parameters.pre, Bases0.pre = Bases0.pre, ninput = ninput, nhidden.pre = nhidden.pre) %*% t(Bases.wide$B2)
  error.FNNwide <- Error.index(Y, Y.hat, index)
  return(error.FNNwide)
}
