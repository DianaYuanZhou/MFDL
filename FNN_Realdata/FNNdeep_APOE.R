source("source.R")

learning_rate <- list("1hl" = 1,
                      "2hl" = c(1,1),
                      "3hl" = c(1, 1, 1))
hidden_units <- list("1hl" = 100,
               "2hl" = c(100, 50),
               "3hl" = c(100, 50, 30))


main.wide <- function(index, lr.nn = 1, ns.nn = 50, lr.fnn = learning_rate, ns.fnn = hidden_units, nhidden= 3,
                      epoch = c(1e4, 1e4), lambda1 = c(0.1,0.3,1,3,10,30), lambda2 = 0, noise = 0.1,name.folder = F, activation = tanh,  
                      lr.pre = c(1, 1), lr.wide = c(1, 1), ns.wide = 50, lambda1.wide = c(0.1,0.3,1,3,10,30), ninput = 2, nhidden.pre = 1, nhidden.wide = 1){

  set.seed(index)
  ## ---------- Real Data ------------------
  list2env(readRDS(file = '../ADNI_Data/Data_UCSF/APOE1.rds'), envir = environment())
  X$PTGENDER <- as.numeric(X$PTGENDER)
  X$APOE4 <- as.numeric(X$APOE4)
  #G2 <- cbind(Gxp, X$AGE, X$PTGENDER, X$PTEDUCAT)
  G2 <- as.matrix(Gxp)
  G1 <- G0
  G <- G0
  #G <- cbind(G0, Gxp, X$AGE, X$PTGENDER, X$PTEDUCAT)
  Y <- as.matrix(log(Y))
  Y <- scale(Y)

  # ----------divide data for wide model----------
  list2env(split(mget(c("G","G1", "G2", "Y")), 0.9, "train", "test"),
           envir = environment())
  list2env(split(list(G = G.train, G1 = G1.train, G2 = G2.train, Y = Y.train, Y = Y.train), 8/9, "subtrain", "valid"),
           envir = environment())
  

          # Defualt divide:
          # list2env(split(mget(c("G", "Y")), 0.8, "train", "test"), envir = environment())
          # list2env(split(list(G = G.train, Y = Y.train), 0.75, "subtrain", "valid"), envir = environment())
  
  
  # -----------null----------
  starttime <- Sys.time()
  error.index <- list(train[subtrain], train[valid], test)
  endtime <- Sys.time()
  result.mse <- c(Error.null(Y.train, Y.test),endtime - starttime)
  result.corr <- Corr.null(Y.train, Y.test)
  
  ## ----------lm-----------
  # starttime <- Sys.time()
  # LM <- Error.lm(G.train, Y.train, G.test, Y.test, pos)
  # endtime <- Sys.time()
  # LM <- c(LM, endtime - starttime)
  # 
  # result <- rbind(result, LM)
  
  ## ----------flm----------
  starttime <- Sys.time()
  FLM <- Error.flm(G, G.train, Y.train, G.test, Y.test, pos, 1/20)
  endtime <- Sys.time()
  FLM <- c(FLM, endtime - starttime)
  result.mse <- rbind(result.mse, FLM)
  FLM <- Corr.flm(G, G.train, Y.train, G.test, Y.test, pos, 1/20)
  result.corr <- rbind(result.corr, FLM)
  
  ## ----------Real Data Basis ------------
  loc <- sort(runif(ncol(Y)))
  B0.nn <- diag(nrow = length(pos), ncol = length(pos))
  B1.nn <- diag(nrow = ns.nn, ncol = ns.nn)
  B2.nn <- diag(nrow = ncol(Y), ncol = ncol(Y))
  
  bb0 <- create.bspline.basis(norder = 5, nbasis = ceiling(length(pos)*0.75))
  B0.fn <- eval.basis(pos, bb0)
  bb2 <- create.bspline.basis(norder = 5, nbasis = ncol(Y))
  #B2.fn <- diag(1,ncol(Y), ncol(Y))
  B2.fn <- eval.basis(loc, bb2)
  penmat <- diag(1,ncol(Y), ncol(Y))
  if(ncol(G) > length(pos)){
    B0.nn <- bdiag(B0.nn, diag(1,(ncol(G)-length(pos)), (ncol(G)-length(pos))))
    B0.fn <- bdiag(B0.fn, diag(1,(ncol(G)-length(pos)), (ncol(G)-length(pos))))
  }
  
  Bases.nn <- list(B0 = B0.nn, B1 = B1.nn, B2 = B2.nn)


   ## ----------nn----------
     set.seed(index)
     starttime <- Sys.time()
     
     session.nn <- FNN(G.subtrain, Y.subtrain, Bases.nn, G.valid, Y.valid, lr =lr.nn,
                       epoch = epoch[1], lambda1 = lambda1, activation = activation)
     NN <- c(Error.fnn(session.nn$parameters, G, Y, error.index,
                       Bases.nn, activation), session.nn$j)
     endtime <- Sys.time()
     NN <- c(NN, endtime - starttime)
     result.mse <- rbind(result.mse, NN)
     NN <- Corr.fnn(session.nn$parameters, G, Y, error.index,
                            Bases.nn, activation)
     
     
     result.corr <- rbind(result.corr, NN)

  #-----------fnn deep ------------
  set.seed(index)
  for(i in 1:nhidden){

  B1.fn <- diag(nrow = ns.fnn[[i]][1], ncol = ns.fnn[[i]][1])
  Bases.fn2 <- list(B0 = B0.fn, B1 = B1.fn, B11 = B1.fn, B2 = B2.fn)

  starttime <- Sys.time()
  session.fn2 <- FNN2(G.subtrain, Y.subtrain, Bases.fn2, G.valid, Y.valid, lr = lr.fnn[[i]],
                    epoch = epoch[1], lambda1 = lambda1, activation = activation,
                    penmat = penmat, nhidden = i)
  FN2 <- c(Error.fnn2(session.fn2$parameters, G, Y, error.index,
                      Bases.fn2, activation, nhidden = i), session.fn2$j)
  endtime <- Sys.time()
  FN2 <- c(FN2, endtime - starttime)
  result.mse <- rbind(result.mse, FN2)
  rownames(result.mse)[nrow(result.mse)] <- paste("FNN with", i ,"hiddenlayer")
  FN2 <- Corr.fnn2(session.fn2$parameters, G, Y, error.index,
                           Bases.fn2, activation, nhidden = i)
  result.corr <- rbind(result.corr, FN2)
  rownames(result.corr)[nrow(result.corr)] <- paste("FNN with", i ,"hiddenlayer")
  }

  #-----------fnn wide ------------
    # set.seed(index)
    # for(i in 2:ninput){
    #   starttime <- Sys.time()
    # 
    #   #------------------ Two inputs basis -----------------
    #   bb0.pre <- create.bspline.basis(norder = 5, nbasis = ceiling(length(pos)*0.4))
    #   B0.wide <- eval.basis(pos, bb0.pre)
    # 
    #   B0.pre <- list('1st' = B0.wide,
    #                  '2nd' = diag(nrow = ncol(G2), ncol = ncol(G2)))
    #   B1.pre <- list('1st' = diag(nrow = ns.wide, ncol = ns.wide),
    #                  '2nd' = diag(nrow = ncol(G2), ncol = ncol(G2)))
    # 
    #   Bases.pre <-  list(B0.pre = B0.pre, B1.pre = B1.pre, B11.pre = B1.pre)
    # 
    #   B1.wide <- diag(nrow = ncol(B1.pre[[1]])+ncol(B1.pre[[2]]), ncol = ncol(B1.pre[[1]])+ncol(B1.pre[[2]]))
    #   Bases.wide <- list(B0.wide = B0.wide, B1.wide = B1.wide, B11.wide = B1.wide, B2 = B2.fn )
    # 
    #   G.subtrain.wide <- list(G1.subtrain, G2.subtrain)
    #   G.valid.wide <- list(G1.valid, G2.valid)
    # 
    #   session.fnnwide <- FNN.wide(G.subtrain.wide, Y.subtrain, Bases.pre = Bases.pre, Bases.wide = Bases.wide, G.valid.wide, Y.valid, lr.wide = lr.wide[i],
    #                               epoch = epoch[2], lambda1 = lambda1.wide, activation = activation, method = "Earlystopping",
    #                               penmat = penmat/penmat[1, 1],lr.pre = lr.pre[i], ninput = i, nhidden = nhidden.wide, nhidden.pre = nhidden.pre)
    #   G.wide <- list(G1, G2)
    #   FNNwide <- c(Error.FNNwide(session.fnnwide$parameters, G.wide, Y, error.index,
    #                              Bases.wide = Bases.wide, activation, nhidden = nhidden.wide,
    #                              session.fnnwide$parameters.pre,Bases.pre = Bases.pre, ninput = i, nhidden.pre = nhidden.pre), session.fnnwide$j)
    #   endtime <- Sys.time()
    #   FNNwide <- c(FNNwide, endtime - starttime)
    #   result.mse <- rbind(result.mse, FNNwide)
    #   rownames(result.mse)[nrow(result.mse)] <- paste("FNN with", i ,"input")
    #   
    #   FNNwide <- c(Corr.FNNwide(session.fnnwide$parameters, G.wide, Y, error.index,
    #                              Bases.wide = Bases.wide, activation, nhidden = nhidden.wide,
    #                              session.fnnwide$parameters.pre,Bases.pre = Bases.pre, ninput = i, nhidden.pre = nhidden.pre))
    #   result.corr <- rbind(result.corr, FNNwide)
    #   rownames(result.corr)[nrow(result.corr)] <- paste("FNN with", i ,"input")
    # }

  ## ----------result----------
  rownames(result.mse)[1] <- 'NUL'
  colnames(result.mse) <- c("subtrain.mse","valid.mse","test.mse", "para", "running time")
  result.mse <- rownames_to_column(as.data.frame(result.mse), "method")
  rownames(result.corr)[1] <- 'NUL'
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
