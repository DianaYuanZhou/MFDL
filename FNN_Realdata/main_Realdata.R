source("source.R")
source("Model_parameters.R")

Cov1 <- c('AGE', 'PTGENDER', 'PTEDUCAT', 'APOE4')
#Cov.cog <- c('ADAS11_bl','ADAS13_bl','CDRSB_bl','MMSE_bl')
#Cov.bio <- c('FDG_bl','ABETA_bl','TAU_bl','PTAU_bl')

main.wide <- function(index, sim.index = c('APOE', 'ACE_QC4'), covariates = NULL, activation = tanh, activation2 = tanh, method = "Continue", name.folder = F){
  
  set.seed(index)

  # ------------------ Import Real Data -----------------------
  list2env(Data.Output.Realdata1(index, sim.index, covariates = covariates), envir = environment())
  sample.index <- rownames(G1)
  #G2 <- G2[sample.index,, drop = F]
  # G2 <- scale(G2)
  
  list2env(Data.match(G1, G2, Y, Age, covariates), envir = environment())
 
  # ------------------ Devide datasets ------------------------
  list2env(split(mget(c("G1", "G2")), 0.8, "train", "test"),
           envir = environment())
  list2env(split(list(G1 = G1.train, G2 = G2.train), 0.75, "subtrain", "valid"),
           envir = environment())
  
  G.train <- cbind(G1.train, G2.train)
  G.test <- cbind(G1.test, G2.test)
  
  G.subtrain <- cbind(G1.subtrain, G2.subtrain)
  G.valid <- cbind(G1.valid, G2.valid)

  Y.train <- Y[row.names(Y) %in% row.names(G1.train),]
  Y.subtrain <- Y[row.names(Y) %in% row.names(G1.subtrain),]
  Y.valid <- Y[row.names(Y) %in% row.names(G1.valid),]
  Y.test <- Y[row.names(Y) %in% row.names(G1.test),]
  
  G1.nn.train <- G1.nn[row.names(Y) %in% row.names(G1.train),]
  G1.nn.subtrain <- G1.nn[row.names(Y) %in% row.names(G1.subtrain),]
  G1.nn.valid <- G1.nn[row.names(Y) %in% row.names(G1.valid),]
  G1.nn.test <- G1.nn[row.names(Y) %in% row.names(G1.test),]
  
  G2.nn.train <- G2.nn[row.names(Y) %in% row.names(G1.train),]
  G2.nn.subtrain <- G2.nn[row.names(Y) %in% row.names(G1.subtrain),]
  G2.nn.valid <- G2.nn[row.names(Y) %in% row.names(G1.valid),]
  G2.nn.test <- G2.nn[row.names(Y) %in% row.names(G1.test),]

  # -------------- NULL --------------------------------
  starttime <- Sys.time()
  #error.index <- list(train[subtrain], train[valid], test)
  error.index <- list(sample.index[train[subtrain]], sample.index[train[valid]], sample.index[test])
  endtime <- Sys.time()
  result.mse <- c(Error.null(Y.train[,1,drop = F], Y.test[,1, drop = F]),endtime - starttime)
  #result.corr <- Corr.null(Y.train[,1,drop = F], Y.test[,1,drop = F])

  ## ---------- NN ----------
  set.seed(index)

  B0_1 <- diag(1, ncol(G1.nn), ncol(G1.nn))
  B0_2 <- diag(1, ncol(G2.nn), ncol(G1.nn))
  B0.NN <- bdiag(B0_1, B0_2)
  B1.NN <- list(diag(nrow = ns.nn, ncol = ns.nn))

  B2.NN <- diag(1, 1, 1)
  penmat.NN <- diag(1, 1, 1) # Scalar response

  Bases.NN <- list(B0 = B0.NN, B1 = B1.NN, B11 = B1.NN, B2 = B2.NN)

  starttime <- Sys.time()
  session.nn <- NN(cbind(G1.nn.subtrain, G2.nn.subtrain), Y.subtrain[,1,drop = F], Bases.NN, cbind(G1.nn.valid, G2.nn.valid), Y.valid[,1,drop = F],
                   lr = lr.nn, epoch = epoch[1], lambda1 = lambda1, activation = activation, method = method,
                   penmat = penmat.NN, nhidden = 1, ADADELTA = ADADELTA)
  NN <- c(Error.nn(session.nn$parameters, cbind(G1.nn, G2.nn), Y[,1,drop = F], error.index,
                      Bases.NN, activation), session.nn$j)
  endtime <- Sys.time()
  NN <- c(NN, endtime - starttime)
  result.mse <- rbind(result.mse, NN)
  NN <- c(Corr.nn(session.nn$parameters, cbind(G1.nn, G2.nn), Y[,1,drop = F], error.index,
                     Bases.NN, activation))
  #result.corr <- rbind(result.corr, NN)
  result.corr <- NN
  
  # ---------- Basis ----------
    ratio <- 0.4
    # if(ncol(G2) < 5){
    #   #bb0_1 <- create.bspline.basis(norder = 4, nbasis = ceiling(length(pos1)*ratio))
    #   bb0_1 <- create.bspline.basis(norder = 4, nbasis = 15)
    #   B0_1 <- eval.basis(pos1, bb0_1)
    #   B0_2 <- diag(1, length(pos2), length(pos2))
    # } else{
    #   #bb0_1 <- create.bspline.basis(norder = 4, nbasis = ceiling((length(pos1))*ratio))
    #   bb0_1 <- create.bspline.basis(norder = 4, nbasis = 15)
    #   B0_1 <- eval.basis(pos1, bb0_1) 
    #   bb0_2 <- create.bspline.basis(norder = 4, nbasis = length(pos2))
    #   B0_2 <- eval.basis(pos2, bb0_2)
    # }
    
    bb0_1 <- create.bspline.basis(norder = 4, nbasis = 10)
    B0_1 <- eval.basis(pos1, bb0_1)
    bb0_2 <- create.bspline.basis(norder = 4, nbasis = 10)
    B0_2 <- eval.basis(pos2, bb0_2)
    #B0_2 <- diag(1, length(pos2), length(pos2))
    B0 <- bdiag(B0_1, B0_2)
    B0.flm <- bdiag(B0_1, B0_2, 1)
    
    if (!is.null(loc)&&length(loc) == 1){
      B2 <- diag(1, 1, 1)
      penmat <- diag(1, 1, 1) # Scalar response
    } else{
      bb2 <- cbb(norder = 4, c(Y[,'loc'], Y[,'loc0']), 10)
      penmat <- bsplinepen(bb2)

    }
    for(i in 1:length(ns.fnn)){
      B1 <- list()
      for (j in 1:length(ns.fnn[[i]]) ){
        B1[[j]] <- diag(nrow = ns.fnn[[i]][j], ncol = ns.fnn[[i]][j])
      }
    }
    Bases <- list(B0 = B0, B1 = B1, B11 = B1, B2 = bb2)
    Bases.flm <- list(B0 = B0.flm, B1 = B1, B11 = B1, B2 = bb2)
    

  
  ## ----------FLM----------
  starttime <- Sys.time()
  FLM <- Error.flm(index, lambda1.flm, cbind(G1.nn.train, G2.nn.train), Y.train[,1,drop = F], 
                   cbind(G1.nn.test, G2.nn.test), Y.test[,1,drop = F], Bases = Bases.flm, ratio = ratio)
  lambda1.best <- FLM[length(FLM)]
  print(lambda1.best)
  endtime <- Sys.time()
  FLM <- c(FLM[-length(FLM)], endtime - starttime)
  result.mse <- rbind(result.mse, FLM)
  FLM <- Corr.flm(index, lambda1.best, cbind(G1.nn.train, G2.nn.train), Y.train[,1,drop = F],
                  cbind(G1.nn.test, G2.nn.test), Y.test[,1,drop = F], Bases.flm)
  result.corr <- rbind(result.corr, FLM)

  # -----------FNN deep ------------
  for(i in 1:length(ns.fnn)){

      starttime <- Sys.time()
      session.fn2 <- FNNdeep(G.subtrain, Y.subtrain, Bases, G.valid, Y.valid, lr = lr.fnn[[i]],
                        epoch = epoch[1], lambda1 = lambda1, activation = activation, activation2 = activation2, method = method,
                        penmat = penmat, nhidden = ns.fnn[[i]], ADADELTA = ADADELTA)
      FN2 <- c(Error.fnn2(session.fn2$parameters, cbind(G1,G2), Y, error.index,
                          Bases, activation, activation2, nhidden = ns.fnn[[i]]), session.fn2$j)
      endtime <- Sys.time()
      FN2 <- c(FN2, endtime - starttime)
      result.mse <- rbind(result.mse, FN2)
      rownames(result.mse)[nrow(result.mse)] <- paste("FNN with", i ,"hiddenlayer")
      FN2 <- c(Corr.fnn2(session.fn2$parameters, cbind(G1,G2), Y, error.index,
                     Bases, activation, activation2, nhidden = ns.fnn[[i]]))
      result.corr <- rbind(result.corr, FN2)
      rownames(result.corr)[nrow(result.corr)] <- paste("FNN with", i ,"hiddenlayer")
   }

  ## -----------fnn wide ------------
  starttime <- Sys.time()

    ##------------------ Two inputs basis -----------------
    B0.pre <- list('1st' = B0_1,
                 '2nd' = B0_2)
  if (length(pos2) < 5){
    B1.pre <- list('1st' = diag(nrow = ns.wide[[1]][1], ncol = ns.wide[[1]][1]),
                 '2nd' = diag(nrow = ncol(G2), ncol = ncol(G2)))

    B1.wide <- diag(nrow = ns.wide[[1]][1]+ncol(G2), ncol = ns.wide[[1]][1]+ncol(G2))
    B11.wide <- diag(nrow = ns.wide[[2]][1], ncol = ns.wide[[2]][1])
  } else{
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
  Bases.wide <- list(B1.wide = B1.wide, B11.wide = B11.wide, B2 = Bases$B2 )

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
   rownames(result.corr)[1] <- 'NN'
   colnames(result.corr) <- c("subtrain.corr", "valid.corr","test.corr")
   result.corr <- rownames_to_column(as.data.frame(result.corr), "method")
   #print(c(lr.pre[2],lr.wide[2]))
   print(result.mse)
   print(result.corr)

   if (is.character(dir.new(name.folder))) {
     write.table(result.mse, file = paste0("../", name.folder, "/MSE/", index, ".txt"))
     write.table(result.corr, file = paste0("../", name.folder, "/Corr/", index, ".txt"))
   }
}
