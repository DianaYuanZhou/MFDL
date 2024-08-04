# Update parameters Using gradient descent
update.adadelta.pre <- function(parameters.pre, grads.pre, ninput, nhidden.pre, adadelta.para.pre, decay_rate = 0.95, e = 1e-6){
  list2env(parameters.pre, envir = environment())
  list2env(grads.pre, envir = environment())
  list2env(adadelta.para.pre, envir = environment())
  
  delta_W1.pre <- list()
  delta_b1.pre <- list()
  delta_W2.pre <- list()
  delta_b2.pre <- list()
  
  
  for (i in 1:ninput){
    delta_W2.pre[[i]] <- list()
    delta_b2.pre[[i]] <- list()
    
  for (j in 1:nhidden.pre){
    g_sq.pre$W2.pre[[i]][[j]] <- decay_rate * g_sq.pre$W2.pre[[i]][[j]] + (1-decay_rate) * (dW2.pre[[i]][[j]]^2) #Accumulating gradient
    delta_W2.pre[[i]][[j]] <- dW2.pre[[i]][[j]] * (sqrt(delta_x_sq.pre$W2.pre[[i]][[j]] + e) / sqrt(g_sq.pre$W2.pre[[i]][[j]] + e)) #Compuing update
    delta_x_sq.pre$W2.pre[[i]][[j]] <- decay_rate * delta_x_sq.pre$W2.pre[[i]][[j]] + (1-decay_rate) * (delta_W2.pre[[i]][[j]]^2) #Accumulating update
    
    g_sq.pre$b2.pre[[i]][[j]] <- decay_rate * g_sq.pre$b2.pre[[i]][[j]] + (1-decay_rate) * (db2.pre[[i]][[j]]^2) #Accumulating gradient
    delta_b2.pre[[i]][[j]] <- db2.pre[[i]][[j]] * (sqrt(delta_x_sq.pre$b2.pre[[i]][[j]] + e) / sqrt(g_sq.pre$b2.pre[[i]][[j]] + e)) #Compuing update
    delta_x_sq.pre$b2.pre[[i]][[j]] <- decay_rate * delta_x_sq.pre$b2.pre[[i]][[j]] + (1-decay_rate) * (delta_b2.pre[[i]][[j]]^2) #Accumulating update
    
     W2.pre[[i]][[j]] <- W2.pre[[i]][[j]] - delta_W2.pre[[i]][[j]];
     b2.pre[[i]][[j]] <- b2.pre[[i]][[j]] - delta_b2.pre[[i]][[j]];
  }
    g_sq.pre$W1.pre[[i]] <- decay_rate * g_sq.pre$W1.pre[[i]] + (1-decay_rate) * (dW1.pre[[i]]^2) #Accumulating gradient
    delta_W1.pre[[i]] <- dW1.pre[[i]] * (sqrt(delta_x_sq.pre$W1.pre[[i]] + e) / sqrt(g_sq.pre$W1.pre[[i]] + e)) #Compuing update
    delta_x_sq.pre$W1.pre[[i]] <- decay_rate * delta_x_sq.pre$W1.pre[[i]]+ (1-decay_rate) * (delta_W1.pre[[i]]^2) #Accumulating update
    
    g_sq.pre$b1.pre[[i]] <- decay_rate * g_sq.pre$b1.pre[[i]] + (1-decay_rate) * (db1.pre[[i]]^2) #Accumulating gradient
    delta_b1.pre[[i]] <- db1.pre[[i]] * (sqrt(delta_x_sq.pre$b1.pre[[i]] + e) / sqrt(g_sq.pre$b1.pre[[i]] + e)) #Compuing update
    delta_x_sq.pre$b1.pre[[i]] <- decay_rate * delta_x_sq.pre$b1.pre[[i]] + (1-decay_rate) * (delta_b1.pre[[i]]^2) #Accumulating update
    
	  W1.pre[[i]] <- W1.pre[[i]] - delta_W1.pre[[i]]
	  b1.pre[[i]] <- b1.pre[[i]] - delta_b1.pre[[i]]

  }
  updated_parameters_pre <- mget(c("W1.pre","b1.pre","W2.pre","b2.pre", 
                                   "delta_W2.pre", "delta_b2.pre", "delta_W1.pre", "delta_b1.pre"))
  return(updated_parameters_pre)
}