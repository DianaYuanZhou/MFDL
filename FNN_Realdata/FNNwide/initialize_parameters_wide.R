## ----------Random generate parameters for simulation----------
initialize.wide <- function(layers.pre,layers, para.ub = 5e-2 ,nhidden){
  para.lb <- -para.ub
  list2env(layers.pre, envir = environment())
  list2env(layers, envir = environment())
  
  if (length(layers.pre) == 3) {
    # assign.list(c("nb", "ns", "nr"), layers)
	W2 <- list()
	b2 <- list()
	W2[[1]] <- matrix(runif(K1.wide*K11.wide, para.lb, para.ub), ncol = K11.wide, nrow = K1.wide)
	b2[[1]] <- runif(K11.wide, para.lb, para.ub)
	
	if (nhidden >= 2){
	
	   for(i in 2:nhidden){
	     W2[[i]] <- matrix(runif(K11.wide*K11.wide, para.lb, para.ub), ncol = K11.wide, nrow = K11.wide) #K1*K1
       b2[[i]] <- runif(K11.wide, para.lb, para.ub)
	   # add another layer setup same with previous layer
       }
	}
	
	W3 <- matrix(runif(K2*K11.wide, para.lb, para.ub), ncol = K2, nrow = K11.wide) #K2*K1
  b3 <- runif(K2, para.lb, para.ub)
  }
  # if (length(layers) == 4) {
    # W1 <- matrix(runif(K1*K0, para.lb, para.ub), ncol = K1, nrow = K0)
    # b1 <- runif(K1, para.lb, para.ub)
    # W2 <- matrix(runif(K1*K21*K22, para.lb, para.ub), nrow = K1)
    # b2 <- runif(K21*K22, para.lb, para.ub)
  # }
  

  result <- mget(c("W2","b2", "W3" ,"b3"))
  return(result)
}