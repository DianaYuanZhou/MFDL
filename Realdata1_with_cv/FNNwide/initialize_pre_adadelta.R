## ----------Random generate parameters for simulation----------
initialize.pre.adadelta <- function(layers.pre, para.ub0 = NULL, ninput, nhidden.pre){
  if(is.null(para.ub0)) para.ub0 <- rep(0, ninput)
  para.lb0 <- rep(0, ninput)
  list2env(layers.pre, envir = environment())
  
  g_sq.pre <- list('W1.pre', 'b1.pre', 'W2.pre', 'b2.pre')
  delta_x_sq.pre <- list('W1.pre', 'b1.pre', 'W2.pre', 'b2.pre')
  
  if (length(layers.pre) == 3) {
    # assign.list(c("nb", "ns", "nr"), layers)
    g_sq.pre$W1.pre <-list()
    g_sq.pre$b1.pre <-list()
    g_sq.pre$W2.pre <-list()
    g_sq.pre$b2.pre <-list()

    delta_x_sq.pre$W1.pre <-list()
    delta_x_sq.pre$b1.pre <-list()
    delta_x_sq.pre$W2.pre <-list()
    delta_x_sq.pre$b2.pre <-list()

    for(i in 1:ninput){
      #Weight of first layer of each input
      g_sq.pre$W1.pre[[i]] <- 0
      g_sq.pre$b1.pre[[i]] <- 0
      g_sq.pre$W1.pre[[i]] <- matrix(runif(K11.pre[[i]]*K0.pre[[i]], para.lb0[i], para.ub0[i]), ncol = K11.pre[[i]], nrow = K0.pre[[i]]) # K1*K0
      g_sq.pre$b1.pre[[i]] <- runif(K11.pre[[i]], para.lb0[i], para.ub0[i])
      g_sq.pre$W2.pre[[i]] <- list()
      g_sq.pre$b2.pre[[i]] <- list()
      
      delta_x_sq.pre$W1.pre[[i]] <- 0
      delta_x_sq.pre$b1.pre[[i]] <- 0
      delta_x_sq.pre$W1.pre[[i]] <- matrix(runif(K11.pre[[i]]*K0.pre[[i]], para.lb0[i], para.ub0[i]), ncol = K11.pre[[i]], nrow = K0.pre[[i]]) # K1*K0
      delta_x_sq.pre$b1.pre[[i]] <- runif(K11.pre[[i]], para.lb0[i], para.ub0[i])
      delta_x_sq.pre$W2.pre[[i]] <- list()
      delta_x_sq.pre$b2.pre[[i]] <- list()
	      
	      for(j in 1:nhidden.pre){
	        g_sq.pre$W2.pre[[i]][[j]] <- 0
	        g_sq.pre$b2.pre[[i]][[j]] <- 0
	        W.temp <- matrix(runif(K11.pre[[i]]*K11.pre[[i]], para.lb0[i], para.ub0[i]), ncol = K11.pre[[i]], nrow = K11.pre[[i]])
	        g_sq.pre$W2.pre[[i]][[j]] <- W.temp
	        b.temp <- runif(K11.pre[[i]], para.lb0[i], para.ub0[i])
	        g_sq.pre$b2.pre[[i]][[j]] <- b.temp

	        delta_x_sq.pre$W2.pre[[i]][[j]] <- 0
	        delta_x_sq.pre$b2.pre[[i]][[j]] <- 0
	        W.temp <- matrix(runif(K11.pre[[i]]*K11.pre[[i]], para.lb0[i], para.ub0[i]), ncol = K11.pre[[i]], nrow = K11.pre[[i]])
	        delta_x_sq.pre$W2.pre[[i]][[j]] <- W.temp
	        b.temp <- runif(K11.pre[[i]], para.lb0[i], para.ub0[i])
	        delta_x_sq.pre$b2.pre[[i]][[j]] <- b.temp 
	       }

    }
}
  result.pre <- mget(c('g_sq.pre','delta_x_sq.pre'))
  #result.pre <- mget(c("W1.pre","b1.pre","W2.pre","b2.pre"))
  return(result.pre)
}