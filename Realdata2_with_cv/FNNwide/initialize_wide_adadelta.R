## ----------Random generate parameters for simulation----------
initialize.wide.adadelta <- function(layers.pre,layers, para.ub = 0 ,nhidden){
  para.lb <- 0
  list2env(layers.pre, envir = environment())
  list2env(layers, envir = environment())
  
  g_sq.wide <- list('W2', 'b2', 'W3', 'b3')
  delta_x_sq.wide <- list('W2', 'b2', 'W3', 'b3')
  
  if (length(layers.pre) == 3) {
    # assign.list(c("nb", "ns", "nr"), layers)
  g_sq.wide$W2 <- list()
  g_sq.wide$b2 <- list()
  g_sq.wide$W2[[1]] <- matrix(runif(K1.wide*K11.wide, para.lb, para.ub), ncol = K11.wide, nrow = K1.wide)
  g_sq.wide$b2[[1]] <- runif(K11.wide, para.lb, para.ub)
  
  delta_x_sq.wide$W2 <- list()
  delta_x_sq.wide$b2 <- list()
  delta_x_sq.wide$W2[[1]] <- matrix(runif(K1.wide*K11.wide, para.lb, para.ub), ncol = K11.wide, nrow = K1.wide)
  delta_x_sq.wide$b2[[1]] <- runif(K11.wide, para.lb, para.ub)
	
	if (nhidden >= 2){
	
	   for(i in 2:nhidden){
	     g_sq.wide$W2[[i]] <- matrix(runif(K11.wide*K11.wide, para.lb, para.ub), ncol = K11.wide, nrow = K11.wide) #K1*K1
	     g_sq.wide$b2[[i]] <- runif(K11.wide, para.lb, para.ub)
	     
	     delta_x_sq.wide$W2[[i]] <- matrix(runif(K11.wide*K11.wide, para.lb, para.ub), ncol = K11.wide, nrow = K11.wide) #K1*K1
	     delta_x_sq.wide$b2[[i]] <- runif(K11.wide, para.lb, para.ub)
       }
	}
	
  g_sq.wide$W3 <- matrix(runif(K2*K11.wide, para.lb, para.ub), ncol = K2, nrow = K11.wide) #K2*K1
  g_sq.wide$b3 <- runif(K2, para.lb, para.ub)
  
  delta_x_sq.wide$W3 <- matrix(runif(K2*K11.wide, para.lb, para.ub), ncol = K2, nrow = K11.wide) #K2*K1
  delta_x_sq.wide$b3 <- runif(K2, para.lb, para.ub)
  }
  
  result <- mget(c('g_sq.wide','delta_x_sq.wide'))
  #result <- mget(c("W2","b2", "W3" ,"b3"))
  return(result)
}