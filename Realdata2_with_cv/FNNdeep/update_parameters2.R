# Update parameters Using gradient descent
update.parameters2 <- function(parameters, grads, learning_rate = 0.12, nhidden){
  list2env(parameters, envir = environment())
  list2env(grads, envir = environment())
  
  n <- length(nhidden)

  for (i in 1:n){
     W[[i]] <- W[[i]] - learning_rate[i] * dW[[i]];
     b[[i]] <- b[[i]] - learning_rate[i] * db[[i]];
     }
	 
  W3 <- W3 - learning_rate[[n]] * dW3;
  b3 <- b3 - learning_rate[[n]] * db3;

  
  updated_parameters <- mget(c("W","b" ,"W3" ,"b3"))
  return(updated_parameters)
}