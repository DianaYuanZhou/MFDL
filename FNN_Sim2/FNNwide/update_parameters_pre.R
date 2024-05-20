# Update parameters Using gradient descent
update.pre <- function(parameters.pre, grads.pre, lr.pre = 0.12, ninput, nhidden.pre){
  list2env(parameters.pre, envir = environment())
  list2env(grads.pre, envir = environment())
  
  for (i in 1:ninput){
  for (j in 1:nhidden.pre){
     W2.pre[[i]][[j]] <- W2.pre[[i]][[j]] - lr.pre[i] * dW2.pre[[i]][[j]];
     b2.pre[[i]][[j]] <- b2.pre[[i]][[j]] - lr.pre[i] * db2.pre[[i]][[j]];
     }
	W1.pre[[i]] <- W1.pre[[i]] - lr.pre[i] * dW1.pre[[i]][[j]]
	b1.pre[[i]] <- b1.pre[[i]] - lr.pre[i] * db1.pre[[i]][[j]]
  #W3 <- W3 - learning_rate * dW3;
  #b3 <- b3 - learning_rate * db3;

  }
  updated_parameters_pre <- mget(c("W1.pre","b1.pre","W2.pre","b2.pre"))
  return(updated_parameters_pre)
}