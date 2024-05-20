## ---------- Construct simulation functions ---------------------
Fun.Output.linear <- function(G, pos, index = NULL, B1 = NULL, B2 = NULL){
  set.seed(index)
  #cf <- runif(1, -3, 3)
  f <- function(Gt = G, t = pos, x) {
    if(is.null(B1)){
      B1 <- runif(length(t), -1, 1)
    }    
    if(is.null(B2)){
      #B2 <- runif(length(x), -2, 2)
      nrow.B2 <- ifelse(is.vector(B1), 1, ncol(B1))
      B2 <- matrix(runif(nrow.B2*length(x), -1, 1), nrow = nrow.B2, ncol = length(x))
    }
    n <- nrow(Gt)
    p <- length(t)
    
    #Nt <- ncol(Gt)
    Nt <- 1   
    y <- ((Gt %*% B1) %*% B2)/Nt
    #y <- scale(y, scale = F)
    y <- scale(y)
    
    force(y)
    return(y)
  }
  force(f)
  return(f)
}

Fun.Output.nonlinear <- function(G, pos, index = NULL, B1 = NULL, B2 = NULL){
  set.seed(index)
  # ef <- runif(3, 1/3, 2)
  ef <- c(1/3, 3/2, 3)
  # cf <- runif(6, -6, 6)
  # c <- runif(4, -pi, pi)
  cf <- list('1' = c(2/3, -2, 2),
             '2' = c(2/3, -2, 2),
             '3' = c(2/3, -2, 2))
  c <- list('1' = runif(2, -pi, pi),
             '2' =  runif(2, -pi, pi),
             '3' =  runif(2, -pi, pi))
  
  f <- function(Gt = G, t = pos, x) {
#    if(is.null(B1)){
#      B1 <- runif(length(t), -1, 1)
#    }    
#    if(is.null(B2)){
#      #B2 <- runif(length(x), -2, 2)
#      nrow.B2 <- ifelse(is.vector(B1), 1, ncol(B1))
#      B2 <- matrix(runif(nrow.B2*length(x), -1, 1), nrow = nrow.B2, ncol = length(x))
#    }
      
      n <- nrow(Gt)
      p <- length(t)    
    
    y <- 0
    for (i in 1:length(ef)){
       y <- y + (cf[[i]][1] * (Gt^ef[i] %*% cos(cf[[i]][2]*t + c[[i]][1])) %*% sin(cf[[i]][3]*x + c[[i]][2]))

    }
    
    #Nt <- ncol(Gt)
    Nt <- 1
    y <- y/Nt
    y <- scale(y,scale = F)
    #y <- scale(y)
    
    force(y)
    return(y)
  }
  force(f)
  return(f)
}


# Function.Output <- function(setting, index, G1, G2, pos1, pos2, p.loc = 50, ratio1 = 0.2, ratio2 = 0.2){
#   set.seed(index)
#   if (setting$response == 'scalar'){
#     loc <- runif(1, 0, 1)
#   }else if (setting$response == 'vector'){ 
#     loc <- sort(runif(p.loc, 0, 1))
#   }
#       # Define f1
#       if(setting$input[1] == 'vector'){
#         ifelse(setting$sim_function == 'linear', f1 <- Fun.Output.linear(G1, pos1, index), f1 <- Fun.Output.nonlinear(G1, pos1, index))
#       }else if (setting$input[1] == 'function'){
#         bb1.sim1 <- create.bspline.basis(norder = 5, nbasis = ceiling(length(pos1)*ratio1)) #function input - linear
#         B1.sim1 <- eval.basis(pos1, bb1.sim1)
#         ifelse(setting$sim_function == 'linear', f1 <- Fun.Output.linear(G1, pos1, index, B1 = B1.sim1), f1 <- Fun.Output.nonlinear(G1, pos1, index, B1 = B1.sim1))
#       }    
#   
#       # Define f2
#       if (setting$input[2] == 'scalar'){
#         ifelse(setting$sim_function == 'linear', f2 <- Fun.Output.linear(G2, pos2, index), f2 <- Fun.Output.nonlinear(G2, pos2, index))
#       } else if (setting$input[2] == 'function'){
#         bb1.sim2 <- create.bspline.basis(norder = 5, nbasis = ceiling(length(pos2)*ratio2))
#         B1.sim2 <- eval.basis(pos2, bb1.sim2)
#         ifelse(setting$sim_function == 'linear', f2 <- Fun.Output.linear(G2, pos2, index, B1 = B1.sim2), f2 <- Fun.Output.nonlinear(G2, pos2, index, B1 = B1.sim2))
#       }
#   
#   return(mget(c("loc", "f1", "f2")))
# }

Function.Output <- function(setting, index, G1, pos1, p.loc = 50, ratio1 = 0.2){
  set.seed(index)
  if (setting$response == 'scalar'){
    loc <- runif(1, 0, 1)
  }else if (setting$response == 'vector'){ 
    loc <- sort(runif(p.loc, 0, 1))
  }
  # Define f1
  if(setting$input[1] == 'vector'){
    ifelse(setting$sim_function == 'linear', f1 <- Fun.Output.linear(G1, pos1, index), f1 <- Fun.Output.nonlinear(G1, pos1, index))
  }else if (setting$input[1] == 'function'){
    bb1.sim1 <- create.bspline.basis(norder = 5, nbasis = ceiling(length(pos1)*ratio1)) #function input - linear
    B1.sim1 <- eval.basis(pos1, bb1.sim1)
    ifelse(setting$sim_function == 'linear', f1 <- Fun.Output.linear(G1, pos1, index, B1 = B1.sim1), f1 <- Fun.Output.nonlinear(G1, pos1, index, B1 = B1.sim1))
  }    
  
  return(mget(c("loc", "f1")))
}

Function.Output.int <- function(index, setting, G1, G2, pos1, pos2, loc, ratio1 = 0.2, ratio2 = 0.2){
  set.seed(index)
  
  # Define G1.int
  if(setting$input[1] == 'vector'){
    B1.sim1 <- runif(length(pos1), -2, 2)
    #B1.sim1 <- rep(1, length(pos1))
  } else {
    bb1.sim1 <- create.bspline.basis(norder = 5, nbasis = ceiling(length(pos1)*ratio1))
    B1.sim1 <- eval.basis(pos1, bb1.sim1)
  }
  
  nrow.B2.sim1 <- ifelse(is.vector(B1.sim1), 1, ncol(B1.sim1))
  B2.sim1 <- matrix(runif(nrow.B2.sim1*length(loc), -2, 2), nrow = nrow.B2.sim1, ncol = length(loc))

  # Define G2.int
  if(setting$input[2] == 'scalar'){
    B1.sim2 <- runif(length(pos2), -2, 2)
    #B1.sim2 <- rep(1,length(pos2))
  } else {
    bb1.sim2 <- create.bspline.basis(norder = 5, nbasis = ceiling(length(pos2)*ratio2)) 
    B1.sim2 <- eval.basis(pos2, bb1.sim2)
  }
  
  nrow.B2.sim2 <- ifelse(is.vector(B1.sim2), 1, ncol(B1.sim2))
  B2.sim2 <- matrix(runif(nrow.B2.sim2*length(loc), -2, 2), nrow = nrow.B2.sim2, ncol = length(loc))

  #ifelse(setting$input[1] == 'function', cf <- matrix(runif(nrow(G1)*length(loc), -3, 3), nrow = nrow(G1), ncol = length(loc)), 
  #      cf <- matrix(runif(nrow(G1)*length(loc), -3, 3), nrow = nrow(G1), ncol = length(loc)))
  cf <- 1
  
  # N1 <- ncol(G1)
  # N2 <- ncol(G2)
  N1 <- 1
  N2 <- 1
  
  Y.int1 <- ((G1 %*% B1.sim1) %*% B2.sim1)/N1
  Y.int2 <- ((G2 %*% B1.sim2) %*% B2.sim2)/N2
  Y.int1 <- scale(Y.int1)
  Y.int2 <- scale(Y.int2)
  Y.int <- cf * Y.int1 * Y.int2
  #Y.int <- scale(Y.int,scale = F)
  #Y.int <- scale(Y.int)
   
  return(mget(c('Y.int','Y.int1','Y.int2')))
} 
