source("source.R")
source("Model_parameters.R")

main.wide <- function(index, sim.index = 1, activation = tanh, activation2 = tanh, method = "Continue", name.folder = F){
  
  set.seed(index)
  
  setting.input <- list('Sim1' = list(c('vector', 'scalar'), c('function', 'function')),
                        'Sim2' = list(c('function', 'scalar')),
                        'Sim3' = list(c('function', 'scalar'), c('function', 'function'))
  )
  setting.response <- list('Sim1' = c('scalar'),
                           'Sim2' = c('scalar', 'vector'),
                           'Sim3' = c('scalar')
  ) 
  
  setting <- list('input' = setting.input[[sim.index]][[1]],
                  'response' = setting.response[[sim.index]][1],
                  'sim_function' = 'linear',
                  'interaction' = T,
                  'noise' = 0.3)
  
  # ------------- Get Simulation Data for 2 input model ----------
  # Simulation 1:
  # input: vector + scalar, functional + functional
  # response: scalar
  # simulation function: linear, linear + interaction, non-linear
  # noise: 0.3
  
  # Simulation 2:
  # input: functional + scalor
  # response: scalar, vector
  # simulation function: linear, linear + interaction, non-linear
  # noise: 0.3
  
  # Simulation 3:
  # input: functional + scalor, functional + functional
  # response: scalar
  # simulation function: linear + interaction
  # noise: 0.1, 0.3, 1
  

  # First input (SNP)
  list2env(Data.Input(n = 200, p = 100, index = index), envir = environment())

  # Second input (Gxp)
  list2env(Data.Input2(setting, n = 200, p = 50, index = index), envir = environment())
  G2 <- abs(G2)
  
  # Get phenotype data
  list2env(Function.Output(setting, index, G1, G2, pos1, pos2, ratio1 = 0.2, ratio2 = 0.2), 
           envir = environment())
  
  if(setting$interaction == T){
    list2env(Function.Output.int(index, setting, G1, G2, pos1, pos2, loc, ratio1 = 0.2, ratio2 = 0.2), envir = environment())
  }
  
  Y <- Data.Output.wide(f1, f2, loc, Y.int, index, noise = noise)

  # ------------------Devide datasets------------------------
  list2env(split(mget(c("G1", "G2", "Y")), 0.8, "train", "test"),
           envir = environment())
  list2env(split(list(G1 = G1.train, G2 = G2.train, Y = Y.train), 0.75, "subtrain", "valid"),
           envir = environment())

  # -------------- NULL --------------------------------
  starttime <- Sys.time()
  error.index <- list(train[subtrain], train[valid], test)
  endtime <- Sys.time()
  result.mse <- c(Error.null(Y.train, Y.test),endtime - starttime)
  result.corr <- Corr.null(Y.train, Y.test)
  
  ## ----------FLM----------
  starttime <- Sys.time()
  FLM <- Error.flm(G1.train,G2.train, Y.train, G1.test, G2.test, Y.test, pos1, pos2, ratio = 1/10)
  endtime <- Sys.time()
  FLM <- c(FLM, endtime - starttime)
  result.mse <- rbind(result.mse, FLM)
  FLM <- Corr.flm(G1.train,G2.train, Y.train, G1.test, G2.test, Y.test, pos1, pos2,  ratio = 1/10)
  result.corr <- rbind(result.corr, FLM)
  
  # ----------------------------------------
  G.train <- cbind(G1.train, G2.train)
  G.test <- cbind(G1.test, G2.test)
  
  G.subtrain <- cbind(G1.subtrain, G2.subtrain)
  G.valid <- cbind(G1.valid, G2.valid)
  
  ## -----------FNN deep ------------
      set.seed(index)
      for(i in 1:length(ns.fnn)){
      # ----------FNN Basis----------
          if(length(pos2) < 5){
          bb0 <- create.bspline.basis(norder = 5, nbasis = ceiling(length(pos1)*0.1))
          B0.fn <- eval.basis(pos1, bb0)
          B0.fn <- bdiag(B0.fn, diag(1, length(pos2), length(pos2)))
        } else{
          bb0_1 <- create.bspline.basis(norder = 5, nbasis = ceiling((length(pos1))*0.1))
          bb0_2 <- create.bspline.basis(norder = 5, nbasis = ceiling((length(pos2))*0.1))
          B0.fn <- bdiag(eval.basis(pos1, bb0_1), eval.basis(pos2, bb0_2))
        }
        if (length(loc) == 1){
          B2.fn <- diag(1, 1, 1)
          penmat <- diag(1, 1, 1) # Scalar response
        }
        else{
          # bb2 <- create.bspline.basis(norder = 5, nbasis = ceiling(length(loc)*0.2))
          # B2.fn <- eval.basis(loc, bb2)
          # penmat <- bsplinepen(bb2)
          B2.fn <- diag(length(loc))
          penmat <- diag(length(loc)) # Vector response
        }

        B1 <- list()
        for (j in 1:length(ns.fnn[[i]]) ){
          B1[[j]] <- diag(nrow = ns.fnn[[i]][j], ncol = ns.fnn[[i]][j])
        }
        Bases.fn2 <- list(B0 = B0.fn, B1 = B1, B11 = B1, B2 = B2.fn)

      starttime <- Sys.time()
      session.fn2 <- FNNdeep(G.subtrain, Y.subtrain, Bases.fn2, G.valid, Y.valid, lr = lr.fnn[[i]],
                        epoch = epoch[1], lambda1 = lambda1, activation = activation, activation2 = activation2, method = method,
                        penmat = penmat, nhidden = ns.fnn[[i]], ADADELTA = ADADELTA)
      FN2 <- c(Error.fnn2(session.fn2$parameters, cbind(G1,G2), Y, error.index,
                          Bases.fn2, activation, activation2, nhidden = ns.fnn[[i]]), session.fn2$j)
      endtime <- Sys.time()
      FN2 <- c(FN2, endtime - starttime)
      result.mse <- rbind(result.mse, FN2)
      rownames(result.mse)[nrow(result.mse)] <- paste("FNN with", i ,"hiddenlayer")
      FN2 <- c(Corr.fnn2(session.fn2$parameters, cbind(G1,G2), Y, error.index,
                         Bases.fn2, activation, activation2, nhidden = ns.fnn[[i]]))
      result.corr <- rbind(result.corr, FN2)
      rownames(result.corr)[nrow(result.corr)] <- paste("FNN with", i ,"hiddenlayer")
       }

  #-----------fnn wide ------------
  set.seed(index)
  starttime <- Sys.time()
         #------------------ Two inputs basis -----------------
  bb0.pre1 <- create.bspline.basis(norder = 5, nbasis = ceiling(length(pos1)*0.1))
  B0.pre1 <- eval.basis(pos1, bb0.pre1)
  if (length(pos2) < 5){
    B0.pre <- list('1st' = B0.pre1,
                 '2nd' = diag(nrow = ncol(G2), ncol = ncol(G2)))
    B1.pre <- list('1st' = diag(nrow = ns.wide[[1]][1], ncol = ns.wide[[1]][1]),
                 '2nd' = diag(nrow = ncol(G2), ncol = ncol(G2)))
    
    B1.wide <- diag(nrow = ns.wide[[1]][1]+ncol(G2), ncol = ns.wide[[1]][1]+ncol(G2))
    #B11.wide <- diag(nrow = ns.wide[[1]][1]+ncol(G2), ncol = ns.wide[[2]][1])
    B11.wide <- diag(nrow = ns.wide[[2]][1], ncol = ns.wide[[2]][1])

  } else{
    bb0.pre2 <- create.bspline.basis(norder = 5, nbasis = ceiling((length(pos2))*0.1))
    B0.pre2 <- eval.basis(pos2, bb0.pre2)
    B0.pre <- list('1st' = B0.pre1,
                   '2nd' = B0.pre2)
    B1.pre <- list('1st' = diag(nrow = ns.wide[[1]][1], ncol = ns.wide[[1]][1]),
                   '2nd' = diag(nrow = ns.wide[[1]][2], ncol = ns.wide[[1]][2]))
    # B1.wide <- diag(nrow = ns.wide[[1]][1]+ns.wide[[1]][2], ncol = ns.wide[[1]][1]+ncol(G2))
    # B11.wide <- diag(nrow = ns.wide[[1]][1]+ns.wide[[1]][2], ncol = ns.wide[[2]][1])
    B1.wide <- diag(nrow = ns.wide[[1]][1]+ns.wide[[1]][2], ncol = ns.wide[[1]][1]+ns.wide[[1]][2])
    B11.wide <- diag(nrow = ns.wide[[2]][1], ncol = ns.wide[[2]][1])
    
    # pos.wide <- seq(0, 1, len = ns.wide[[1]][1]+ns.wide[[1]][2])
    # bb0.wide <- create.bspline.basis(norder = 4, nbasis = ceiling(length(pos.wide)*0.1))
    # B1.wide <- eval.basis(pos.wide, bb0.wide)
    # B11.wide <- diag(nrow = length(pos.wide), ncol = ns.wide[[2]][1])

  }

  Bases.pre <-  list(B0.pre = B0.pre, B1.pre = B1.pre, B11.pre = B1.pre)
  Bases.wide <- list(B1.wide = B1.wide, B11.wide = B11.wide, B2 = B2.fn )

  G.subtrain.wide <- list(G1.subtrain, G2.subtrain)
  G.valid.wide <- list(G1.valid, G2.valid)

  session.fnnwide <- FNN.wide(D0 = G.subtrain.wide, Y.wide = Y.subtrain, Bases.pre = Bases.pre, Bases.wide = Bases.wide, G.valid = G.valid.wide, Y.valid = Y.valid, lr.wide = lr.wide,
                            epoch = epoch[2], lambda1 = lambda1.wide, lambda2 = lambda2.wide, activation = activation, activation2 = activation2, method = method,
                            penmat = penmat, lr.pre = lr.pre, ninput = ninput, nhidden.wide = nhidden.wide, nhidden.pre = nhidden.pre, ADADELTA = ADADELTA)
  G.wide <- list(G1, G2)
  FNNwide <- c(Error.FNNwide(parameters = session.fnnwide$parameters, G.wide, Y, error.index,
                            Bases.wide = Bases.wide, activation, activation2, nhidden = nhidden.wide,
                            parameters.pre = session.fnnwide$parameters.pre, Bases.pre = Bases.pre, ninput = ninput, nhidden.pre = nhidden.pre), session.fnnwide$j)
  endtime <- Sys.time()
  FNNwide <- c(FNNwide, endtime - starttime)

  result.mse <- rbind(result.mse, FNNwide)
  rownames(result.mse)[nrow(result.mse)] <- paste("FNN with", ninput ,"input")
  FNNwide <- c(Corr.FNNwide(session.fnnwide$parameters, G.wide, Y, error.index,
                           Bases.wide = Bases.wide, activation, activation2, nhidden = nhidden.wide,
                           session.fnnwide$parameters.pre,Bases.pre = Bases.pre, ninput = ninput, nhidden.pre = nhidden.pre))
  result.corr <- rbind(result.corr, FNNwide)
  rownames(result.corr)[nrow(result.corr)] <- paste("FNN with", ninput ,"input")


  ## ----------result----------
   rownames(result.mse)[1] <- 'NUL'
   colnames(result.mse) <- c("subtrain.mse","valid.mse","test.mse", "para", "running time")
   result.mse <- rownames_to_column(as.data.frame(result.mse), "method")
   rownames(result.corr)[1] <- 'NUL'
   colnames(result.corr) <- c("subtrain.corr", "valid.corr","test.corr")
   result.corr <- rownames_to_column(as.data.frame(result.corr), "method")
   #print(c(lr.pre[2],lr.wide[2]))
   print(result.mse)
   #print(result.corr)
   
   if (is.character(dir.new(name.folder))) {
     write.table(result.mse, file = paste0("../", name.folder, "/MSE/", index, ".txt"))
     write.table(result.corr, file = paste0("../", name.folder, "/Corr/", index, ".txt"))
   }
}
