# Update parameters Using gradient descent
update.adadelta.wide <- function(parameters, grads, nhidden, adadelta.para.wide, decay_rate = 0.95, e = 1e-6){
  list2env(parameters, envir = environment())
  list2env(grads, envir = environment())
  list2env(adadelta.para.wide, envir = environment())
  
  delta_W2 <- list()
  delta_b2 <- list()
  delta_W3 <- list()
  delta_b3 <- list()
  
  for (i in 1:nhidden){
    g_sq.wide$W2[[i]] <- decay_rate * g_sq.wide$W2[[i]] + (1-decay_rate) * (dW2[[i]]^2) #Accumulating gradient
    delta_W2[[i]] <- dW2[[i]] * (sqrt(delta_x_sq.wide$W2[[i]] + e) / sqrt(g_sq.wide$W2[[i]] + e)) #Compuing update
    delta_x_sq.wide$W2[[i]] <- decay_rate * delta_x_sq.wide$W2[[i]] + (1-decay_rate) * (delta_W2[[i]]^2) #Accumulating update
    
    g_sq.wide$b2[[i]] <- decay_rate * g_sq.wide$b2[[i]] + (1-decay_rate) * (db2[[i]]^2) #Accumulating gradient
    delta_b2[[i]] <- db2[[i]] * (sqrt(delta_x_sq.wide$b2[[i]] + e) / sqrt(g_sq.wide$b2[[i]] + e)) #Compuing update
    delta_x_sq.wide$b2[[i]] <- decay_rate * delta_x_sq.wide$b2[[i]] + (1-decay_rate) * (delta_b2[[i]]^2) #Accumulating update
    
     W2[[i]] <- W2[[i]] - delta_W2[[i]];
     b2[[i]] <- b2[[i]] - delta_b2[[i]];
     }
	
  g_sq.wide$W3 <- decay_rate * g_sq.wide$W3 + (1-decay_rate) * (dW3^2) #Accumulating gradient
  delta_W3 <- dW3 * (sqrt(delta_x_sq.wide$W3 + e) / sqrt(g_sq.wide$W3 + e)) #Compuing update
  delta_x_sq.wide$W3 <- decay_rate * delta_x_sq.wide$W3 + (1-decay_rate) * (delta_W3^2) #Accumulating update
  
  g_sq.wide$b3 <- decay_rate * g_sq.wide$b3 + (1-decay_rate) * (db3^2) #Accumulating gradient
  delta_b3 <- db3 * (sqrt(delta_x_sq.wide$b3 + e) / sqrt(g_sq.wide$b3 + e)) #Compuing update
  delta_x_sq.wide$b3 <- decay_rate * delta_x_sq.wide$b3 + (1-decay_rate) * (delta_b3^2) #Accumulating update
  
  W3 <- W3 - delta_W3;
  b3 <- b3 - delta_b3;

  
  updated_parameters <- mget(c("M2","W2","b2" ,"W3" ,"b3", "delta_W2", "delta_b2", "delta_W3", "delta_b3"))
  return(updated_parameters)
}