## ----------Random generate parameters for simulation----------
initialize.adadelta.nn <- function(layers, para.ub = 0, nhidden = 1){
  para.lb <- 0
  list2env(layers, envir = environment())
  
  g_sq <- list('W1', 'b1', 'W2', 'b2')
  delta_x_sq <- list('W1', 'b1', 'W2', 'b2')

  if (length(layers) == 3) {
	W1 <- matrix(runif(K1[[1]]*K0, para.lb, para.ub), ncol = K1[[1]], nrow = K0) # K1*K0
  b1 <- runif(K1[[1]], para.lb, para.ub)
	
	g_sq$W1 <- W1
	g_sq$b1 <- b1
	
	delta_x_sq$W1 <- W1
	delta_x_sq$b1 <- b1
	
	g_sq$W2 <- matrix(runif(K2*K1[[1]], para.lb, para.ub), ncol = K2, nrow = K1[[1]]) #K2*K1
	g_sq$b2 <- runif(K2, para.lb, para.ub)
	delta_x_sq$W2 <- matrix(runif(K2*K1[[1]], para.lb, para.ub), ncol = K2, nrow = K1[[1]]) #K2*K1
	delta_x_sq$b2 <- runif(K2, para.lb, para.ub)
  }

  result <- mget(c('g_sq','delta_x_sq'))
  return(result)
}