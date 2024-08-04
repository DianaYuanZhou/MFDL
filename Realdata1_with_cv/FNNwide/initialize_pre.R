## ----------Random generate parameters for simulation----------
initialize.pre <- function(layers.pre, para.ub0 = NULL, ninput, nhidden.pre){
  if(is.null(para.ub0)) para.ub0 <- rep(5e-2, ninput)
  para.lb0 <- -para.ub0
  list2env(layers.pre, envir = environment())
  if (length(layers.pre) == 3) {
    # assign.list(c("nb", "ns", "nr"), layers)
    W1.pre <-list()
    b1.pre <-list()
    W2.pre <-list()
    b2.pre <-list()
	  W3.pre <- list()
	  b3.pre <- list()
    
    for(i in 1:ninput){
      #Weight of first layer of each input
        W1.pre[[i]] <- 0
        b1.pre[[i]] <- 0
	      W1.pre[[i]] <- matrix(runif(K11.pre[[i]]*K0.pre[[i]], para.lb0[i], para.ub0[i]), ncol = K11.pre[[i]], nrow = K0.pre[[i]]) # K1*K0
        b1.pre[[i]] <- runif(K11.pre[[i]], para.lb0[i], para.ub0[i])
	      W2.pre[[i]] <- list()
	      b2.pre[[i]] <- list()
	      
	      for(j in 1:nhidden.pre){

	       W2.pre[[i]][[j]] <- 0
	       b2.pre[[i]][[j]] <- 0
	       W.temp <- matrix(runif(K11.pre[[i]]*K11.pre[[i]], para.lb0[i], para.ub0[i]), ncol = K11.pre[[i]], nrow = K11.pre[[i]])
	       W2.pre[[i]][[j]] <- W.temp
	       b.temp <- runif(K11.pre[[i]], para.lb0[i], para.ub0[i])
         b2.pre[[i]][[j]] <- b.temp 
	       }
        
        #W0[[i]] <- W2.pre
        #b0[[i]] <- b2.pre
	      
	      #Weight of last layer of each input
	      #W3.pre[[i]] <- matrix(runif(K1*K1, para.lb0, para.ub0), ncol = K1, nrow = K1) #K2*K1
        #b3.pre[[i]] <- runif(K1, para.lb0, para.ub0)

    }
}
  # if (length(layers) == 4) {
    # W1 <- matrix(runif(K1*K0, para.lb0, para.ub0), ncol = K1, nrow = K0)
    # b1 <- runif(K1, para.lb0, para.ub0)
    # W2 <- matrix(runif(K1*K21*K22, para.lb0, para.ub0), nrow = K1)
    # b2 <- runif(K21*K22, para.lb0, para.ub0)
  # }
  
  result.pre <- mget(c("W1.pre","b1.pre","W2.pre","b2.pre"))
  return(result.pre)
}