# Update parameters Using gradient descent
update.parameters.adadelta <- function(parameters, grads, nhidden, adadelta.para, decay_rate = 0.95, e = 1e-6){
  list2env(parameters, envir = environment())
  list2env(grads, envir = environment())
  list2env(adadelta.para, envir = environment())
  
  delta_W <- list()
  delta_b <- list()
  delta_W3 <- list()
  delta_b3 <- list()
  
  n <- length(nhidden)
  for (i in 1:n){
     g_sq$W[[i]] <- decay_rate * g_sq$W[[i]] + (1-decay_rate) * (dW[[i]]^2) #Accumulating gradient
     delta_W[[i]] <- dW[[i]] * (sqrt(delta_x_sq$W[[i]] + e) / sqrt(g_sq$W[[i]] + e)) #Compuing update
     delta_x_sq$W[[i]] <- decay_rate * delta_x_sq$W[[i]] + (1-decay_rate) * (delta_W[[i]]^2) #Accumulating update

     g_sq$b[[i]] <- decay_rate * g_sq$b[[i]] + (1-decay_rate) * (db[[i]]^2) #Accumulating gradient
     delta_b[[i]] <- db[[i]] * (sqrt(delta_x_sq$b[[i]] + e) / sqrt(g_sq$b[[i]] + e)) #Compuing update
     delta_x_sq$b[[i]] <- decay_rate * delta_x_sq$b[[i]] + (1-decay_rate) * (delta_b[[i]]^2) #Accumulating update
     
     W[[i]] <- W[[i]] - delta_W[[i]]
     b[[i]] <- b[[i]] - delta_b[[i]];
     }
	
  g_sq$W3 <- decay_rate * g_sq$W3 + (1-decay_rate) * (dW3^2) #Accumulating gradient
  delta_W3 <- dW3 * (sqrt(delta_x_sq$W3 + e) / sqrt(g_sq$W3 + e)) #Compuing update
  delta_x_sq$W3 <- decay_rate * delta_x_sq$W3 + (1-decay_rate) * (delta_W3^2) #Accumulating update
  
  g_sq$b3 <- decay_rate * g_sq$b3 + (1-decay_rate) * (db3^2) #Accumulating gradient
  delta_b3 <- db3 * (sqrt(delta_x_sq$b3 + e) / sqrt(g_sq$b3 + e)) #Compuing update
  delta_x_sq$b3 <- decay_rate * delta_x_sq$b3 + (1-decay_rate) * (delta_b3^2) #Accumulating update
  
  W3 <- W3 - delta_W3;
  b3 <- b3 - delta_b3;

  
  updated_parameters <- mget(c("W","b" ,"W3" ,"b3"))
  return(updated_parameters)
}