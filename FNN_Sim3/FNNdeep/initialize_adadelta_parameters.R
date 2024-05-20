## ----------Random generate parameters for simulation----------
initialize.parameters.adadelta <- function(layers, para.ub = 0, nhidden){
  para.lb <- 0
  list2env(layers, envir = environment())
  
  g_sq <- list('W', 'b', 'W3', 'b3')
  delta_x_sq <- list('W', 'b', 'W3', 'b3')

  if (length(layers) == 3) {
	W1 <- matrix(runif(K1[[1]]*K0, para.lb, para.ub), ncol = K1[[1]], nrow = K0) # K1*K0
  b1 <- runif(K1[[1]], para.lb, para.ub)
	
	g_sq$W <- list()
	g_sq$b <- list()
	g_sq$W[[1]] <- W1
	g_sq$b[[1]] <- b1
	
	delta_x_sq$W <- list()
	delta_x_sq$b <- list()
	delta_x_sq$W[[1]] <- W1
	delta_x_sq$b[[1]] <- b1
	
	n <- length(nhidden)
	if (n >= 2){
	   for(i in 2:n){
	   g_sq$W[[i]] <- matrix(runif(K1[[i]]*K1[[i-1]], para.lb, para.ub), ncol = K1[[i]], nrow = K1[[i-1]]) #K1*K1
	   g_sq$b[[i]] <- runif(K1[[i]], para.lb, para.ub)
	   delta_x_sq$W[[i]] <- matrix(runif(K1[[i]]*K1[[i-1]], para.lb, para.ub), ncol = K1[[i]], nrow = K1[[i-1]]) #K1*K1
	   delta_x_sq$b[[i]] <- runif(K1[[i]], para.lb, para.ub)	   
     }
	}
	
	g_sq$W3 <- matrix(runif(K2*K1[[n]], para.lb, para.ub), ncol = K2, nrow = K1[[n]]) #K2*K1
	g_sq$b3 <- runif(K2, para.lb, para.ub)
	delta_x_sq$W3 <- matrix(runif(K2*K1[[n]], para.lb, para.ub), ncol = K2, nrow = K1[[n]]) #K2*K1
	delta_x_sq$b3 <- runif(K2, para.lb, para.ub)
  }

  result <- mget(c('g_sq','delta_x_sq'))
  return(result)
}