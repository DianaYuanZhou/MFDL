## ---------- Second Input Data ----------
Data.Input2 <- function(setting, n = 200, p = 50, index){
  set.seed(index)
  if (setting$input[2] == 'scalar'){
     G2 <- as.matrix(rnorm(n, mean = 0, sd = 0.5)) #Scalar input, default (0.2, 0.5)
     pos2 <- runif(1, 0, 1)
     
  }else {
    G2 <- mvrnorm(n, mu = rep(0,p), Sigma = diag(0.5, p, p),tol = 1e-6, empirical = FALSE) #default (0.2, 0.5)
    pos2 <- sort(runif(p, 0, 1))
    
    # Old simultion
    #list2env(Data.Input.wide(n = 200, p = 50, seg.pos1 = seg.pos), envir = environment())
    #f2 <- Fun.Output.wide(G2, pos2)
  }
  return(mget(c("G2", "pos2")))
}