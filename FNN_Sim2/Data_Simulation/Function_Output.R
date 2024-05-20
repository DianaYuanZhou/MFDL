## ---------- Construct simulation functions ---------------------
Fun.Output.linear <- function(G, pos, index = NULL, B1 = NULL, B2 = NULL){
  set.seed(index)
  cf <- runif(1, -2, 4)
  f <- function(x) {
    if(is.null(B1)){
      B1 <- runif(length(pos), -2, 2)
    }    
    if(is.null(B2)){
      #B2 <- runif(length(x), -2, 2)
      nrow.B2 <- ifelse(is.vector(B1), 1, ncol(B1))
      B2 <- matrix(runif(nrow.B2*length(x), -2, 2), nrow = nrow.B2, ncol = length(x))
    }
    n <- nrow(G)
    p <- length(pos)
       
    y <- cf*((G %*% B1) %*% B2) / sqrt(p)
    
    force(y)
    return(y)
  }
  force(f)
  return(f)
}

Fun.Output.nonlinear <- function(G, pos, index = NULL, B1 = NULL, B2 = NULL){
  set.seed(index)
  ef <- runif(3, 1/3, 2)
  # cf <- runif(6, -6, 6)
  # c <- runif(4, -pi, pi)
  cf <- list('1' = runif(3, -3, 3),
             '2' = runif(3, -3, 3),
             '3' = runif(3, -3, 3))
  c <- list('1' = runif(2, -pi, pi),
             '2' =  runif(2, -pi, pi),
             '3' =  runif(2, -pi, pi))
  
  f <- function(x) {
    if(is.null(B1)){
      B1 <- runif(length(pos), -2, 2)
    }    
    if(is.null(B2)){
      #B2 <- runif(length(x), -2, 2)
      nrow.B2 <- ifelse(is.vector(B1), 1, ncol(B1))
      B2 <- matrix(runif(nrow.B2*length(x), -2, 2), nrow = nrow.B2, ncol = length(x))
    }
    n <- nrow(G)
    p <- length(pos)    
    
    y <- 0
    for (i in 1:length(ef)){
       y <- y + (cf[[i]][1] * (G^ef[i] %*% cos(cf[[i]][2]*pos + c[[i]][1])) %*% sin(cf[[i]][3]*x + c[[i]][2]))
    }
    y <- y/sqrt(p) 
    
    force(y)
    return(y)
  }
  force(f)
  return(f)
}

Function.Output <- function(setting, index, G1, G2, pos1, pos2, p.loc = 50, ratio1 = 0.2, ratio2 = 0.2){
  set.seed(index)
  if (setting$response == 'scalar'){
    loc <- runif(1, 0, 1)
    # loc2 <- runif(1, 0, 1)
  }else if (setting$response == 'vector'){ 
    loc <- sort(runif(p.loc, 0, 1))
    # loc2 <- sort(runif(p.loc, 0, 1))
  }
      # Define f1
      if(setting$input[1] == 'vector'){
        ifelse(setting$sim_function == 'linear', f1 <- Fun.Output.linear(G1, pos1, index), f1 <- Fun.Output.nonlinear(G1, pos1, index))
      }else if (setting$input[1] == 'function'){
        bb1.sim1 <- create.bspline.basis(norder = 5, nbasis = ceiling(length(pos1)*ratio1)) #function input - linear
        B1.sim1 <- eval.basis(pos1, bb1.sim1)
        ifelse(setting$sim_function == 'linear', f1 <- Fun.Output.linear(G1, pos1, index, B1 = B1.sim1), f1 <- Fun.Output.nonlinear(G1, pos1, index, B1 = B1.sim1))
      }    
  

      # Define f2
      if (setting$input[2] == 'scalar'){
        ifelse(setting$sim_function == 'linear', f2 <- Fun.Output.linear(G2, pos2, index), f2 <- Fun.Output.nonlinear(G2, pos2, index))
      } else if (setting$input[2] == 'function'){
        bb1.sim2 <- create.bspline.basis(norder = 5, nbasis = ceiling(length(pos2)*ratio2))
        B1.sim2 <- eval.basis(pos2, bb1.sim2)
        ifelse(setting$sim_function == 'linear', f2 <- Fun.Output.linear(G2, pos2, index, B1 = B1.sim2), f2 <- Fun.Output.nonlinear(G2, pos2, index, B1 = B1.sim2))
      }
  
  return(mget(c("loc", "f1", "f2")))
}