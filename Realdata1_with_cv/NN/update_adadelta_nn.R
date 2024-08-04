# Update parameters Using gradient descent
update.adadelta.nn <- function(parameters, grads, nhidden = 1, adadelta.para, decay_rate = 0.95, e = 1e-6){
  list2env(parameters, envir = environment())
  list2env(grads, envir = environment())
  list2env(adadelta.para, envir = environment())
  
  g_sq$W1 <- decay_rate * g_sq$W1 + (1-decay_rate) * (dW1^2) #Accumulating gradient
  delta_W1 <- dW1 * (sqrt(delta_x_sq$W1 + e) / sqrt(g_sq$W1 + e)) #Compuing update
  delta_x_sq$W1 <- decay_rate * delta_x_sq$W1 + (1-decay_rate) * (delta_W1^2) #Accumulating update

  g_sq$b1 <- decay_rate * g_sq$b1 + (1-decay_rate) * (db1^2) #Accumulating gradient
  delta_b1 <- db1 * (sqrt(delta_x_sq$b1 + e) / sqrt(g_sq$b1 + e)) #Compuing update
  delta_x_sq$b1 <- decay_rate * delta_x_sq$b1 + (1-decay_rate) * (delta_b1^2) #Accumulating update
     
  W1 <- W1 - delta_W1
  b1 <- b1 - delta_b1;
     
	
  g_sq$W2 <- decay_rate * g_sq$W2 + (1-decay_rate) * (dW2^2) #Accumulating gradient
  delta_W2 <- dW2 * (sqrt(delta_x_sq$W2 + e) / sqrt(g_sq$W2 + e)) #Compuing update
  delta_x_sq$W2 <- decay_rate * delta_x_sq$W2 + (1-decay_rate) * (delta_W2^2) #Accumulating update
  
  g_sq$b2 <- decay_rate * g_sq$b2 + (1-decay_rate) * (db2^2) #Accumulating gradient
  delta_b2 <- db2 * (sqrt(delta_x_sq$b2 + e) / sqrt(g_sq$b2 + e)) #Compuing update
  delta_x_sq$b2 <- decay_rate * delta_x_sq$b2 + (1-decay_rate) * (delta_b2^2) #Accumulating update
  
  W2 <- W2 - delta_W2;
  b2 <- b2 - delta_b2;

  
  updated_parameters <- mget(c("W1","b1" ,"W2" ,"b2"))
  return(updated_parameters)
}