dY <- apply(Y, 1, diff)
dX <- diff(loc)
dX2 <- diff(loc, lag = 2)
dYX <- dY/dX
dYYX <- diff(dY/dX) * 2
dYXX <- dY[-1, ]*dX[-length(dX)] + dY[-dim(dY)[1], ]*dX[-1]
d2YX <- dYYX/dYXX
cor(t(d2YX))
