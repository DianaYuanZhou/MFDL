# Update parameters Using gradient descent
update.parameters <- function(parameters, grads, learning_rate = 0.12){
  list2env(parameters, envir = environment())
  list2env(grads, envir = environment())
  # Update rule for each parameter
  W1 <- W1 - learning_rate * dW1;
  b1 <- b1 - learning_rate * db1;
  W2 <- W2 - learning_rate * dW2;
  b2 <- b2 - learning_rate * db2;
  
  updated_parameters <- mget(c("W1", "b1", "W2", "b2"))
  return(updated_parameters)
}