## ---------- First Input Data ----------
Data.Input <- function(n, p, index){
  set.seed(index)
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
  
  G1 <- as.matrix(geno) # get rid of individuals with no variability
  pos1 <- (loc - loc[1])/(loc[length(loc)] - loc[1])
  
  return(mget(c("G1", "pos1", "seg.pos")))
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

