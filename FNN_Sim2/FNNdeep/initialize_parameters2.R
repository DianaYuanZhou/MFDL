## ----------Random generate parameters for simulation----------
initialize.parameters2 <- function(layers, para.ub = 5e-2 ,nhidden){
  para.lb <- -para.ub
  list2env(layers, envir = environment())
  if (length(layers) == 3) {
    # assign.list(c("nb", "ns", "nr"), layers)
	W1 <- matrix(runif(K1[[1]]*K0, para.lb, para.ub), ncol = K1[[1]], nrow = K0) # K1*K0
  b1 <- runif(K1[[1]], para.lb, para.ub)
	
	W <- list()
	b <- list()
	W[[1]] <- W1
	b[[1]] <- b1
	
	n <- length(nhidden)
	if (n >= 2){
	
	   for(i in 2:n){
	   W[[i]] <- matrix(runif(K1[[i]]*K1[[i-1]], para.lb, para.ub), ncol = K1[[i]], nrow = K1[[i-1]]) #K1*K1
     b[[i]] <- runif(K1[[i]], para.lb, para.ub)
	   # add another layer setup same with previous layer
       }
	}
	
	W3 <- matrix(runif(K2*K1[[n]], para.lb, para.ub), ncol = K2, nrow = K1[[n]]) #K2*K1
    b3 <- runif(K2, para.lb, para.ub)
  }
  # if (length(layers) == 4) {
  #   W1 <- matrix(runif(K1*K0, para.lb, para.ub), ncol = K1, nrow = K0)
  #   b1 <- runif(K1, para.lb, para.ub)
  #   W2 <- matrix(runif(K1*K21*K22, para.lb, para.ub), nrow = K1)
  #   b2 <- runif(K21*K22, para.lb, para.ub)
  # }

  result <- mget(c("W","b","W3" ,"b3"))
  #result <- mget(c("W1", "b1", "W2", "b2" ,"W3" ,"b3"))
  return(result)
}