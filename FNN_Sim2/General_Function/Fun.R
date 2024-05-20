set.seed(31)
source("source.R")
## ----------Get Simulation Data----------
list2env(Data.Input(n = 200, p = 500), envir = environment())
loc <- sort(runif(100))
# loc <- t(apply(matrix(runif(15*500), ncol = 15), 1, sort))
f <- Fun.Output(G, pos)
Y <- Data.Output(f, loc, 0.1)
# ----------divide data----------
list2env(split(mget(c("G", "Y")), 0.8, "train", "test"), 
         envir = environment())
list2env(split(list(G = G.train, Y = Y.train), 0.75, "subtrain", "valid"), 
         envir = environment())
## ----------Basis----------
B0 <- diag(nrow = length(pos), ncol = length(pos))
B1 <- diag(nrow = 100, ncol = 100)
B2.nn <- diag(nrow = length(loc), ncol = length(loc))
bb2 <- create.bspline.basis(norder = 5, nbasis = ceiling(length(loc)*0.4))
penmat <- bsplinepen(bb2)
B2.fn <- eval.basis(loc, bb2)
## 
Fun.Bspline <- function(W, B){
  f <- function(x){
    output <- eval.basis(x, B)
    y <- c(W %*% t(output))
    return(y)
  }
  return(f)
}
## ----------nn----------
Bases.nn <- list(B0 = B0, B1 = B1, B2 = B2.nn)
session.nn <- FNN(G.subtrain, Y.subtrain, Bases.nn, G.valid, Y.valid, lr = 1,
                  epoch = 6e3, lambda1 = 0, activation = sigmoid)
## ----------fnn----------
Bases.fn <- list(B0 = B0, B1 = B1, B2 = B2.fn)
session.fn <- FNN(G.subtrain, Y.subtrain, Bases.fn, G.valid, Y.valid, lr = 1,
                  epoch = 6e3, activation = sigmoid, penmat = penmat/penmat[1, 1],
                  lambda2 = 0.1)
index <- test[19]
# fp <- Fun.Output(G, pos, index)
f.select <- function(fs, index){
  f <- function(x){
    y <- fs(x)[index, ]
    return(y)
  }
  return(f)
}
fp <- f.select(f, index)

C2 <- pred(session.fn$parameters, G[index, ], Bases.fn, sigmoid)
fq <- Fun.Bspline(C2, bb2)

Y.f <- fp(loc)
Y.true <- Y[index, ]
Y.nn <- c(pred(session.nn$parameters, G[index, ], Bases.nn, sigmoid))
Y.fn <- fq(loc)
Y.fn2 <- C2 %*% t(Bases.fn$B2)
print(mse(Y.nn, Y.true))
print(mse(Y.fn, Y.true))
print(mse(Y.fn2, Y.true))

ggplot() + stat_function(data.frame(x = c(0, 1)), mapping = aes(x),
                         fun = fp, color = "red") +
  stat_function(data.frame(x = c(0, 1)), mapping = aes(x),
                   fun = fq, color = "blue") +
  geom_point(mapping = aes(x = loc, y = Y.true), color = "black") +
  geom_point(mapping = aes(x = loc, y = c(Y.nn)), color = "green")
