## ----------Data Substraction----------
Data.Input <- function(n, p){
  # ----------Substract data given N and p----------
  maf.interval <- c(0.005,0.5)
  
  variant <- function(x){length(unique(x)) > 1}
  
  ### Get data for use
  ped <- read.table("../FNN_Data/ped.ped", sep = "\t")
  info <- read.table("../FNN_Data/info.info", header = T)
  # ped: 1092 obs. of 12735 variables
  # info: 12735 obs. of 4 variables
  # n = 1092, p = 12735 high dimensional question
  
  ## N samples chosen among 1092 objects for simulation
  smp.idx <- sample(1:nrow(ped), n) # smp as sample order
  ## maf interval SNP index
  maf.idx <- (info$maf > maf.interval[1] & info$maf < maf.interval[2])
  geno <- ped[smp.idx, maf.idx]
  pos <- info$pos
  loc <- pos[maf.idx]
  
  ## Delete void data
  vrt <- apply(geno,2,variant)  # see variability with a genomic region
  loc <- loc[vrt]
  geno <- as.matrix(geno[,vrt]); # get rid of individuals with no variability
  
  ## Truncated SNP index
  seg.pos <- sample(1:(length(loc) - p + 1), 1)
  idx.trun <- seg.pos:(seg.pos + p - 1)
  geno <- geno[, idx.trun]
  loc <- loc[idx.trun]
  
  G <- as.matrix(geno) # get rid of individuals with no variability
  pos <- (loc - loc[1])/(loc[length(loc)] - loc[1])
  
  return(mget(c("G", "pos", "seg.pos")))
}

Data.Input.wide <- function(n, p, seg.pos1){
  # ----------Substract data given N and p----------
  maf.interval <- c(0.005,0.5)
  
  variant <- function(x){length(unique(x)) > 1}
  
  ### Get data for use
  ped <- read.table("../FNN_Data/ped.ped", sep = "\t")
  info <- read.table("../FNN_Data/info.info", header = T)
  # ped: 1092 obs. of 12735 variables
  # info: 12735 obs. of 4 variables
  # n = 1092, p = 12735 high dimensional question
  
  ## N samples chosen among 1092 objects for simulation
  smp.idx <- sample(1:nrow(ped), n) # smp as sample order
  ## maf interval SNP index
  maf.idx <- (info$maf > maf.interval[1] & info$maf < maf.interval[2])
  geno <- ped[smp.idx, maf.idx]
  pos <- info$pos
  loc <- pos[maf.idx]
  
  ## Delete void data
  vrt <- apply(geno,2,variant)  # see variability with a genomic region
  loc <- loc[vrt]
  geno <- as.matrix(geno[,vrt]); # get rid of individuals with no variability
  
  ## Truncated SNP index
  idxpool <- 1:(length(loc) - p + 1)
  if(seg.pos1 > 500){
    seg.pos2 <- sample(idxpool[-c((seg.pos1 - p):(seg.pos1 + p - 1))], 1)
  } else {seg.pos2 <- sample(idxpool[-c(1:(seg.pos1 + p - 1))], 1)}
  idx.trun2 <- seg.pos2:(seg.pos2 + p - 1)
  geno2 <- geno[, idx.trun2]
  loc2 <- loc[idx.trun2]
  G2 <- as.matrix(geno2) # get rid of individuals with no variability
  pos2 <- (loc2 - loc2[1])/(loc2[length(loc2)] - loc2[1])
  
  return(mget(c("G2", "pos2")))
}

##----- Options for y -------------------------------------------------
# Fun.Output <- function(G, pos, index = NULL){
#   cf <- runif(13, -12, 12)
#   f <- function(x) {
#     n <- nrow(G)
#     p <- length(pos)
# 
# 	#Use y2 and y3 to complicate input		 
#     y2 <- (cf[9] * (G %*% cos(cf[1]*pos)) %*% sin(cf[2]*x) +
#             cf[10] * (G %*% sin(cf[3]*pos)) %*% cos(cf[4]*x) +
#             cf[11] * (G^2 %*% sin(cf[5]*pos)) %*% cos(cf[6]*x) +
#             cf[12] * (sqrt(G) %*% cos(cf[7]*pos)) %*% sin(cf[8]*x) +
#             cf[13]) / sqrt(p)/4
# 
#     y3 <- (cf[9] * (G %*% cos(cf[1]*pos)) %*% sin(cf[2]*(x-1)) +
#             cf[10] * (G %*% sin(cf[3]*pos)) %*% cos(cf[4]*(x-1)) +
#             cf[11] * (G^2 %*% sin(cf[5]*pos)) %*% cos(cf[6]*(x-1)) +
#             cf[12] * (sqrt(G) %*% cos(cf[7]*pos)) %*% sin(cf[8]*(x-1)) +
#             cf[13]) / sqrt(p)/4	
#     y <- y2 * y3
# 	
#     if (!is.null(index))
#       y <- y[index, ]
#     return(y)
#   }
#   return(f)
# }

Fun.Output.linear <- function(G, pos, B1 = NULL, B2 = NULL, index = NULL){
  cf <- runif(1, -2, 2)
  f <- function(x, B1 = NULL, B2 = NULL) {
    if(is.null(B1)){
      B1 <- runif(length(pos), -2, 2)
    }
    if(is.null(B2)){
      B2 <- runif(length(x), -2, 2)
    }
    n <- nrow(G)
    p <- length(pos)
    y <- cf*((G %*% B1) %*% B2) / sqrt(p)

    if (!is.null(index))
      y <- y[index, ]
    return(y)
  }
  return(f)
}

Fun.Output.nonlinear <- function(G, pos, B1 = NULL, B2 = NULL, index = NULL){
  f <- function(x, B1 = NULL, B2 = NULL) {
    cf <- runif(1, -2, 4)
    ef <- runif(1, 1/3, 3)
    if(is.null(B1)){
      B1 <- runif(length(pos), -2, 2)
    }
    if(is.null(B2)){
      B2 <- runif(length(x), -2, 2)
    }
    n <- nrow(G)
    p <- length(pos)
    y <- ((cf*G^ef %*% B1) %*% B2) / sqrt(p)
    #y <- cf*((cos(G) %*% B1) %*% B2) / sqrt(p)
    
    if (!is.null(index))
      y <- y[index, ]
    return(y)
  }
  return(f)
}


##------------------------------------------------------------------

# Data.Output <- function(f, loc, noise = 0.3){
#   if (is.vector(loc)) {
#     Y <- f(loc)
#     p <- ncol(Y)
#     q <- 0
#     if (p == 1)
#       rawvars <- matrix(rnorm(length(Y), 0, 1), nrow = nrow(Y))
#     else {
#       sigma <- diag(1, p, p)
#       for (i in 1:(p - 1)) {
#         sigma[i, i + 1] <- 1/2
#         sigma[i + 1, i] <- 1/2
#       }
#       Sigma <- q*sigma + (1 - q)*diag(1, p, p)
#       rawvars <- mvrnorm(n = length(f(0)), mu = rep(0, p), Sigma = Sigma)
#     }
#     if (length(loc) == 1)
#       Y <- scale(Y)
# 
#     Y <- Y + rawvars*noise
#     Y <- as.matrix(Y)
#   }
#   return(Y)
# }

Data.Output.wide <- function(f1, f2, loc, inter = F, noise = 0.3){
  if (is.vector(loc)) {
    Y1 <- f1(loc)
    Y2 <- f2(loc)
    p <- ncol(Y1)
    q <- 0
    if (p == 1) 
      rawvars <- matrix(rnorm(length(Y1), 0, 1), nrow = nrow(Y1))
    else {
      sigma <- diag(1, p, p)
      for (i in 1:(p - 1)) {
        sigma[i, i + 1] <- 1/2
        sigma[i + 1, i] <- 1/2
      }
      Sigma <- q*sigma + (1 - q)*diag(1, p, p)
      rawvars <- mvrnorm(n = length(f1(0)), mu = rep(0, p), Sigma = Sigma)
    }
 
#      Y1 <- scale(Y1)
#      Y2 <- scale(Y2)

    if(inter == T){
      c = runif(1, -2, 2)
      Y <- Y1 + Y2 + rawvars*noise + c* Y1 * Y2
    }else{
     Y <- Y1 + Y2 + rawvars*noise
    }
    Y <- as.matrix(Y)
    Y <- scale(Y)
  }
  return(Y)
}
