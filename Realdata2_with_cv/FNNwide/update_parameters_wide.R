# Update parameters Using gradient descent
update.wide <- function(parameters, grads, lr.wide = 0.12, nhidden){
  list2env(parameters, envir = environment())
  list2env(grads, envir = environment())
  
  for (i in 1:nhidden){
     W2[[i]] <- W2[[i]] - lr.wide * dW2[[i]];
     b2[[i]] <- b2[[i]] - lr.wide * db2[[i]];
     }
	 
  W3 <- W3 - lr.wide * dW3;
  b3 <- b3 - lr.wide * db3;

  
  updated_parameters <- mget(c("M2","W2","b2" ,"W3" ,"b3"))
  return(updated_parameters)
}