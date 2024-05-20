# Sigmoid activation
sigmoid <- function(x){
  (1 + exp(-x))^(-1)
}
# Linear activation
linear <- function(x){
  x
}

# Relu activation
relu <- function(x){
  (abs(x) + x) / 2
}
relu.prime <- Deriv(relu)


rep.row <- function(x,n){
  matrix(rep(x,each = n),nrow = n)
}
rep.col <- function(x,n){
  matrix(rep(x,each = n), ncol = n, byrow = TRUE)
}
subs <- function(objects, rows){
  if (is.matrix(objects)) return(objects[rows, , drop = FALSE])
  if (is.array(objects)) return(objects[rows, , ])
}
split <- function(objects, ratio, name1, name2){
  n <- nrow(objects[[1]])
  idx <- sample(1:n)
  assign(name1, idx[1:ceiling(ratio*n)])
  assign(name2, idx[(ceiling(ratio*n) + 1):n])
  names.in.list <- mget(c(name1, name2))
  for (name in names(objects)) {
    list2env(objects, envir = environment())
    assign(paste0(name, ".", name1), subs(get(name), get(name1)))
    assign(paste0(name, ".", name2), subs(get(name), get(name2)))
    names.in.list.new <- mget(c(paste0(name, ".", name1), 
                                paste0(name, ".", name2)))
    names.in.list <- c(names.in.list, names.in.list.new)
  }
  return(names.in.list)
}
# create output file folder name
dir.new <- function(name){
  if (is.character(name)) {
    dir.new <- paste0("../", name)
    if (dir.exists(dir.new) == FALSE)
      dir.create(dir.new)
    return(dir.new)
  }
  else 
    return(FALSE)
}

#MSE for vector response
mse.vector <- function(y, y.hat){
  error <- y - y.hat
  square.error <- diag(tcrossprod(error))
  mse <- mean(square.error)/ncol(y)
  return(mse)
}
