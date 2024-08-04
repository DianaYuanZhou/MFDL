source("source.R")
source("Model_parameters.R")
library(Matrix)
library(splitTools)

#Cov1 <- c('AGE', 'PTGENDER', 'PTEDUCAT', 'APOE4')
#Cov.cog <- c('ADAS11_bl','ADAS13_bl','CDRSB_bl','MMSE_bl')
Cov.bio <- c('ABETA_bl','TAU_bl','PTAU_bl')

main.wide <- function(index, sim.index = 'APOE', covariates = Cov.bio[1], ninput = 3, K_fold = 3, activation = tanh, activation2 = tanh, method = "Continue", name.folder = F){
  
  set.seed(index)

  # ------------------ Import Real Data -----------------------
  list2env(Data.Output.Realdata(index, sim.index, covariates = covariates), envir = environment())
  #sample.index <- rownames(G1)
  #G2 <- G2[sample.index,, drop = F]
  
  if (sum(is.na(G2)) != 0){
    G2 <- na.omit(G2)
    sample.index <- rownames(G2)
    G1 <- G1[rownames(G1) %in% sample.index,,drop = F]
    Y <- Y[rownames(Y) %in% sample.index,,drop = F]
    Age <- Age[names(Age) %in% sample.index]
  }
  G2[,2] <- (G2[,2]-mean(G2[,2]))/sd(G2[,2])
  
  list2env(Data.match(G1, G2, Y, Age, covariates), envir = environment())
  
  # ------------------ Stratified cross-validation ------------------------
  # Age_bl <- data.frame(Age, sample_index = names(Age)) %>% group_by(sample_index) %>% mutate(Age_bl = min(Age)) %>%
  #   dplyr::select(sample_index, Age_bl) %>% unique()
  # ## 3-fold
  # strat <- partition(Age_bl$Age_bl, p = c(subtrain1 = 0.25, subtrain2 = 0.25, subtrain3 = 0.25, test = 0.25), seed = index)
  # sample.index <- Age_bl$sample_index
  ## 5-fold
  #strat <- partition(Age_bl$Age_bl, p = c(subtrain1 = 0.16, subtrain2 = 0.16, subtrain3 = 0.16, subtrain4 = 0.16, suntrain5 = 0.16, test = 0.2), seed =index)
   
  Y_bl <- data.frame(Y1 = Y[,1], sample_index = rownames(Y), loc = Y[,2]) %>% group_by(sample_index) %>% mutate(Y_bl = Y1[which.min(loc)]) %>%
    dplyr::select(sample_index, Y_bl) %>% unique()
  strat <- partition(Y_bl$sample_index, p = c(subtrain1 = 0.25, subtrain2 = 0.25, subtrain3 = 0.25, test = 0.25), seed = index)
  sample.index <- Y_bl$sample_index
  
  
  test_sample_idx <- sample.index[strat$test]
  train_sample_idx <- setdiff(sample.index, test_sample_idx)
  
  
  G1.train <- G1[rownames(G1) %in% train_sample_idx,]
  G1.test <- G1[rownames(G1) %in% test_sample_idx,]
  
  G2.train <- G2[rownames(G2) %in% train_sample_idx,]
  G2.test <- G2[rownames(G2) %in% test_sample_idx,]
  
  folds <- list()
  for (i in 1:K_fold){
    folds[[i]] <- sample.index[strat[[i]]]
  }
  #folds <- create_folds(train_sample_idx, k = K_fold, seed = index)
  
  # ------------------ Devide datasets ------------------------
  G.train <- cbind(G1.train, G2.train)
  G.test <- cbind(G1.test, G2.test)
  
  Y.train <- Y[row.names(Y) %in% train_sample_idx,]
  Y.test <- Y[row.names(Y) %in% test_sample_idx,]
  
  G1.nn.train <- G1.nn[row.names(Y) %in% train_sample_idx,]
  G1.nn.test <- G1.nn[row.names(Y) %in% test_sample_idx,]
  
  G2.nn.train <- G2.nn[row.names(Y) %in% train_sample_idx,]
  G2.nn.test <- G2.nn[row.names(Y) %in% test_sample_idx,]
  
  Y.subtrain.list <- list()
  Y.valid.list <- list()
  
  G1.subtrain.list <- list()
  G1.valid.list <- list()
  
  G2.subtrain.list <- list()
  G2.valid.list <- list()
  
  G1.nn.subtrain.list <- list()
  G1.nn.valid.list <- list()
  
  G2.nn.subtrain.list <- list()
  G2.nn.valid.list <- list()
  
  error.index.list <- list()
  
  for (i in 1:K_fold){
    valid_sample_idx <- folds[[i]]
    subtrain_sample_idx <- setdiff(train_sample_idx, valid_sample_idx)
    #valid_sample_idx <- train_sample_idx[-folds[[i]]]
    
    Y.subtrain.list[[i]] <- Y.train[row.names(Y.train) %in% subtrain_sample_idx,]
    Y.valid.list[[i]] <- Y.train[row.names(Y.train) %in% valid_sample_idx,]
    
    G1.subtrain.list[[i]] <- G1.train[row.names(G1.train)%in% subtrain_sample_idx,]
    G1.valid.list[[i]] <- G1.train[row.names(G1.train)%in% valid_sample_idx,]
    
    G2.subtrain.list[[i]] <- G2.train[row.names(G2.train)%in% subtrain_sample_idx,]
    G2.valid.list[[i]] <- G2.train[row.names(G2.train)%in% valid_sample_idx,]
    
    G1.nn.subtrain.list[[i]] <- G1.nn.train[row.names(G1.nn.train) %in% subtrain_sample_idx,]
    G1.nn.valid.list[[i]] <- G1.nn.train[row.names(G1.nn.train) %in% valid_sample_idx,]
    
    G2.nn.subtrain.list[[i]] <- G2.nn.train[row.names(G2.nn.train) %in% subtrain_sample_idx,]
    G2.nn.valid.list[[i]] <- G2.nn.train[row.names(G2.nn.train) %in% valid_sample_idx,]
    
    error.index.list[[i]] <- list(subtrain_sample_idx, valid_sample_idx)
  }
  
  error.index.test <- list(train_sample_idx, test_sample_idx)

  # -------------- NULL --------------------------------
  #error.index <- list(train[subtrain], train[valid], test)
  #error.index <- list(sample.index[train[subtrain]], sample.index[train[valid]], sample.index[test])
  result.mse <- Error.null(Y.train[,1,drop = F], Y.test[,1, drop = F], valid = FALSE)
  result.mae <- MAE.null(Y.train[,1,drop = F], Y.test[,1,drop = F], valid = FALSE)

  ## ---------- NN ----------
  set.seed(index)

  B0_1 <- diag(1, ncol(G1.nn), ncol(G1.nn))
  B0_2 <- diag(1, ncol(G2.nn), ncol(G1.nn))
  B0.NN <- bdiag(B0_1, B0_2)
  B1.NN <- list(diag(nrow = ns.nn, ncol = ns.nn))

  B2.NN <- diag(1, 1, 1)
  penmat.NN <- diag(1, 1, 1) # Scalar response

  Bases.NN <- list(B0 = B0.NN, B1 = B1.NN, B11 = B1.NN, B2 = B2.NN)
  
  print('Training NN...')
  valid_loss_avg <- rep(0, length(lambda1))
  for (k in 1:length(lambda1)){

    valid_loss_per_fold <- rep(0, K_fold)
    for(i in 1:K_fold){
      error.index.train <- error.index.list[[i]]

      G1.nn.subtrain <- G1.nn.subtrain.list[[i]]
      G1.nn.valid <- G1.nn.valid.list[[i]]

      G2.nn.subtrain <- G2.nn.subtrain.list[[i]]
      G2.nn.valid <- G2.nn.valid.list[[i]]

      Y.subtrain <- Y.subtrain.list[[i]]
      Y.valid <- Y.valid.list[[i]]

      session.nn <- NN(cbind(G1.nn.subtrain, G2.nn.subtrain), Y.subtrain[,1,drop = F], Bases.NN, cbind(G1.nn.valid, G2.nn.valid), Y.valid[,1,drop = F],
                     lr = lr.nn, epoch = epoch[1], lambda1 = lambda1[k], activation = activation, method = method,
                     penmat = penmat.NN, nhidden = 1, ADADELTA = ADADELTA)
      valid_loss_per_fold[i] <- c(Error.nn(session.nn$parameters, cbind(G1.nn.subtrain, G2.nn.subtrain), Y.subtrain, cbind(G1.nn.valid, G2.nn.valid), Y.valid,
                            Bases.NN, activation), session.nn$j)[2]

    }
    valid_loss_avg[k] <- mean(valid_loss_per_fold)

  }
  lambda.best.nn <- lambda1[which.min(valid_loss_avg)]
  session.nn.best <- NN(cbind(G1.nn.train, G2.nn.train), Y.train[,1,drop = F], Bases.NN, NULL, NULL,
                   lr = lr.nn, epoch = epoch[1], lambda1 = c(lambda.best.nn), activation = activation, method = method,
                   penmat = penmat.NN, nhidden = 1, ADADELTA = ADADELTA)
  NN.mse <- c(Error.nn(session.nn.best$parameters, cbind(G1.nn.train, G2.nn.train), Y.train, cbind(G1.nn.test, G2.nn.test), Y.test,
                                       Bases.NN, activation), session.nn.best$j)

  result.mse <- rbind(result.mse, NN.mse)
  rownames(result.mse)[nrow(result.mse)] <- "DL"

  NN.corr <- c(Corr.nn(session.nn.best$parameters, cbind(G1.nn.train, G2.nn.train), Y.train, cbind(G1.nn.test, G2.nn.test), Y.test,
                     Bases.NN, activation))
  result.corr <- NN.corr

  NN.mae <- c(MAE.nn(session.nn.best$parameters, cbind(G1.nn.train, G2.nn.train), Y.train, cbind(G1.nn.test, G2.nn.test), Y.test,
                       Bases.NN, activation), session.nn.best$j)
  result.mae <- rbind(result.mae, NN.mae)
  rownames(result.mae)[nrow(result.mae)] <- "DL"

  best.lambda.vec <- c(lambda.best.nn)
  # ---------- Basis ----------
    ratio <- 0.4
    if(ncol(G2) < 5){
      #bb0_1 <- create.bspline.basis(norder = 4, nbasis = ceiling(length(pos1)*ratio))
      bb0_1 <- create.bspline.basis(norder = 4, nbasis = 15)
      B0_1 <- eval.basis(pos1, bb0_1)
      B0_2 <- diag(1, length(pos2), length(pos2))
    } else{
      #bb0_1 <- create.bspline.basis(norder = 4, nbasis = ceiling((length(pos1))*ratio))
      bb0_1 <- create.bspline.basis(norder = 4, nbasis = 15)
      B0_1 <- eval.basis(pos1, bb0_1) 
      bb0_2 <- create.bspline.basis(norder = 4, nbasis = length(pos2))
      B0_2 <- eval.basis(pos2, bb0_2)
    }
    B0 <- bdiag(B0_1, B0_2)
    
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
    Bases.flm <- list(B0 = B0.NN, B1 = B1, B11 = B1, B2 = bb2)
    

  
  ## ----------FLM----------
  print('Training FLM...')
  FLM.mse <- Error.flm(index, lambda1.flm, K_fold = K_fold, cbind(G1.nn.train, G2.nn.train), Y.train[,1,drop = F], 
                   cbind(G1.nn.test, G2.nn.test), Y.test[,1,drop = F], Bases = Bases.flm, valid = FALSE, ratio = ratio)
  lambda1.best <- FLM.mse[length(FLM.mse)]

  FLM.mse <- c(FLM.mse[-length(FLM.mse)])
  result.mse <- rbind(result.mse, FLM.mse)
  rownames(result.mse)[nrow(result.mse)] <- "FLM"
  
  FLM.corr <- Corr.flm(index, lambda1.best, cbind(G1.nn.train, G2.nn.train), Y.train[,1,drop = F],
                  cbind(G1.nn.test, G2.nn.test), Y.test[,1,drop = F], Bases.flm)
  result.corr <- rbind(result.corr, FLM.corr)
  rownames(result.corr)[nrow(result.corr)] <- "FLM"
  
  FLM.mae <- MAE.flm(index, lambda1.best, cbind(G1.nn.train, G2.nn.train), Y.train[,1,drop = F],
                       cbind(G1.nn.test, G2.nn.test), Y.test[,1,drop = F], Bases.flm, valid = FALSE)
  result.mae <- rbind(result.mae, FLM.mae)
  rownames(result.mae)[nrow(result.mae)] <- "FLM"
  best.lambda.vec <- c(best.lambda.vec, lambda1.best)

  # -----------FNN deep ------------
  print('Training DFNN...')
  for(ns in 1:length(ns.fnn)){
    valid_loss_avg <- rep(0, length(lambda1))

    for (k in 1:length(lambda1)){

      valid_loss_per_fold <- rep(0, K_fold)
      for (i in 1:K_fold){
        error.index.train <- error.index.list[[i]]

        G1.subtrain <- G1.subtrain.list[[i]]
        G1.valid <- G1.valid.list[[i]]

        G2.subtrain <- G2.subtrain.list[[i]]
        G2.valid <- G2.valid.list[[i]]

        Y.subtrain <- Y.subtrain.list[[i]]
        Y.valid <- Y.valid.list[[i]]

        session.fn2 <- FNNdeep(cbind(G1.subtrain, G2.subtrain), Y.subtrain, Bases, cbind(G1.valid, G2.valid), Y.valid, lr = lr.fnn[[ns]],
                               epoch = epoch[1], lambda1 = lambda1[k], activation = activation, activation2 = activation2, method = method,
                               penmat = penmat, nhidden = ns.fnn[[ns]], ADADELTA = ADADELTA)
        valid_loss_per_fold[i] <- Error.fnn2(session.fn2$parameters, cbind(G1.subtrain,G2.subtrain), Y.subtrain, cbind(G1.valid, G2.valid), Y.valid,
                                Bases, activation, activation2, nhidden = ns.fnn[[ns]])[2]
      }
      valid_loss_avg[k] <- mean(valid_loss_per_fold)

    }
    lambda.best.fn <- lambda1[which.min(valid_loss_avg)]
    session.fn2.best <- FNNdeep(cbind(G1.train, G2.train), Y.train, Bases, NULL, NULL, lr = lr.fnn[[ns]],
                           epoch = epoch[1], lambda1 = lambda.best.fn, activation = activation, activation2 = activation2, method = method,
                           penmat = penmat, nhidden = ns.fnn[[ns]], ADADELTA = ADADELTA)
    FN2.mse <- c(Error.fnn2(session.fn2.best$parameters, cbind(G1.train,G2.train), Y.train, cbind(G1.test, G2.test), Y.test,
                                           Bases, activation, activation2, nhidden = ns.fnn[[ns]]), session.fn2.best$j)

    result.mse <- rbind(result.mse, FN2.mse)
    rownames(result.mse)[nrow(result.mse)] <- paste0("FNN-", ns ,"HL")

    FN2.corr <- c(Corr.fnn2(session.fn2.best$parameters, cbind(G1.train,G2.train), Y.train, cbind(G1.test, G2.test), Y.test,
                            Bases, activation, activation2, nhidden = ns.fnn[[ns]]))
    result.corr <- rbind(result.corr, FN2.corr)
    rownames(result.corr)[nrow(result.corr)] <- paste0("FNN-", ns ,"HL")

    FN2.mae <- c(MAE.fnn2(session.fn2.best$parameters, cbind(G1.train,G2.train), Y.train, cbind(G1.test, G2.test), Y.test,
                          Bases, activation, activation2, nhidden = ns.fnn[[ns]]), session.fn2.best$j)
    result.mae <- rbind(result.mae, FN2.mae)
    rownames(result.mae)[nrow(result.mae)] <- paste0("FNN-", ns ,"HL")
    best.lambda.vec <- c(best.lambda.vec, lambda.best.fn)
   }

  ## -----------fnn wide ------------
  print('Training MFDL...')

    ##------------------ Two inputs basis -----------------
  B0_2 <- diag(1, 1, 1)
  B0.pre <- list('1st' = B0_1, '2nd' = B0_2, '3rd' = B0_2)
  if (length(pos2) < 5){
    B1.pre <- list('1st' = diag(nrow = ns.wide[[1]][1], ncol = ns.wide[[1]][1]),
                 '2nd' = diag(nrow = 1, ncol = 1),
                 '3rd' = diag(nrow = 1, ncol = 1))

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
  
  valid_loss_avg <- rep(0, length(lambda1.wide))
  for (k in 1:length(lambda1.wide)){
    valid_loss_per_fold <- rep(0, K_fold)
    for (i in 1:K_fold){
      error.index.train <- error.index.list[[i]]
      
      G1.subtrain <- G1.subtrain.list[[i]]
      G1.valid <- G1.valid.list[[i]]
      
      G2.subtrain <- G2.subtrain.list[[i]]
      G2.valid <- G2.valid.list[[i]]
      
      Y.subtrain <- Y.subtrain.list[[i]]
      Y.valid <- Y.valid.list[[i]]
      
      G.subtrain.wide <- list(G1.subtrain, G2.subtrain[,1,drop = F], G2.subtrain[,2,drop = F])
      G.valid.wide <- list(G1.valid, G2.valid[,1,drop = F], G2.valid[,2,drop = F])
      
      session.fnnwide <- FNN.wide(D0 = G.subtrain.wide, Y.wide = Y.subtrain, Bases.pre = Bases.pre, Bases.wide = Bases.wide, G.valid = G.valid.wide, Y.valid = Y.valid, lr.wide = lr.wide,
                                  epoch = epoch[2], lambda1 = lambda1.wide[k], lambda2 = lambda2.wide, activation = activation, activation2 = activation2, method = method,
                                  penmat = penmat, lr.pre = lr.pre, ninput = ninput, nhidden.wide = nhidden.wide, nhidden.pre = nhidden.pre, ADADELTA = ADADELTA)
      G.train.wide <- list(G1.train, G2.train[,1,drop = F], G2.train[,2,drop = F])
      valid_loss_per_fold[i] <- Error.FNNwide(parameters = session.fnnwide$parameters, G.subtrain.wide, Y.subtrain, G.valid.wide, Y.valid,
                                                Bases.wide = Bases.wide, activation, activation2, nhidden = nhidden.wide,
                                                parameters.pre = session.fnnwide$parameters.pre, Bases.pre = Bases.pre, ninput = ninput, nhidden.pre = nhidden.pre)[2]
      
    }
    valid_loss_avg[k] <- mean(valid_loss_per_fold) 
  }

  lambda1.wide.best <- lambda1.wide[which.min(valid_loss_avg)]
  session.fnnwide.best <- FNN.wide(D0 = G.train.wide, Y.wide = Y.train, Bases.pre = Bases.pre, Bases.wide = Bases.wide, G.valid = NULL, Y.valid = NULL, lr.wide = lr.wide,
                            epoch = epoch[2], lambda1 = c(lambda1.wide.best), lambda2 = lambda2.wide, activation = activation, activation2 = activation2, method = method,
                            penmat = penmat, lr.pre = lr.pre, ninput = ninput, nhidden.wide = nhidden.wide, nhidden.pre = nhidden.pre, ADADELTA = ADADELTA)

  G.wide <- list(G1, G2[,1,drop = F], G2[,2,drop = F])
  G.test.wide <- list(G1.test, G2.test[,1,drop = F], G2.test[,2,drop = F])
  FNNwide.mse <- c(Error.FNNwide(parameters = session.fnnwide.best$parameters, G.train.wide, Y.train, G.test.wide, Y.test,
                            Bases.wide = Bases.wide, activation, activation2, nhidden = nhidden.wide,
                            parameters.pre = session.fnnwide.best$parameters.pre, Bases.pre = Bases.pre, ninput = ninput, nhidden.pre = nhidden.pre), session.fnnwide.best$j)
  
  result.mse <- rbind(result.mse, FNNwide.mse)
  rownames(result.mse)[nrow(result.mse)] <- "MFDL"
  
  FNNwide.corr <- c(Corr.FNNwide(session.fnnwide.best$parameters, G.train.wide, Y.train, G.test.wide, Y.test,
                           Bases.wide = Bases.wide, activation, activation2, nhidden = nhidden.wide,
                           session.fnnwide.best$parameters.pre,Bases.pre = Bases.pre, ninput = ninput, nhidden.pre = nhidden.pre))
  result.corr <- rbind(result.corr, FNNwide.corr)
  rownames(result.corr)[nrow(result.corr)] <- "MFDL"
  
  FNNwide.mae <- c(MAE.FNNwide(session.fnnwide.best$parameters,G.train.wide, Y.train, G.test.wide, Y.test,
                                 Bases.wide = Bases.wide, activation, activation2, nhidden = nhidden.wide,
                                 session.fnnwide.best$parameters.pre,Bases.pre = Bases.pre, ninput = ninput, nhidden.pre = nhidden.pre), session.fnnwide.best$j)
  result.mae <- rbind(result.mae, FNNwide.mae)
  rownames(result.mae)[nrow(result.mae)] <- "MFDL"
  best.lambda.vec <- c(best.lambda.vec, lambda1.wide.best)

  ## ----------result----------
   rownames(result.mse)[1] <- 'NULL'
   colnames(result.mse) <- c("Train.mse","Test.mse", "para")
   result.mse <- rownames_to_column(as.data.frame(result.mse), "method")
   rownames(result.corr)[1] <- 'DL'
   colnames(result.corr) <- c("Train.corr", "Test.corr")
   result.corr <- rownames_to_column(as.data.frame(result.corr), "method")
   rownames(result.mae)[1] <- 'NULL'
   colnames(result.mae) <- c("Train.mae", "Test.mae", "para")
   result.mae <- rownames_to_column(as.data.frame(result.mae), "method")
   
   print(result.mse)
   print(result.corr)
   print(result.mae)
   print(best.lambda.vec)

   if (is.character(dir.new(name.folder))) {
     write.table(result.mse, file = paste0("./", name.folder, "/MSE/", index, ".txt"))
     write.table(result.corr, file = paste0("./", name.folder, "/Corr/", index, ".txt"))
     write.table(result.mae, file = paste0("./", name.folder, "/MAE/", index, ".txt"))
   }
}
