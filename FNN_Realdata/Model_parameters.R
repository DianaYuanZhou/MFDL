#noise = 0.3
ADADELTA = T
if(ADADELTA == T){
  decay_rate <- 0.9 # Initialize rho
  e <- 1e-8 # Initialize constant
}
epoch = c(1e4, 1e4)
lambda1.flm <- c(1,3,10,30,100)

## --------------------- Parameters of NN --------------------------
lr.nn = 0.1
ns.nn = 10

## --------------------- Parameters of FNN deep --------------------------
 # lr.fnn <- list("1hl" = 1e-3,
 #                "2hl" = c(5e-2,1e-3),
 #                "3hl" = c(5e-2, 5e-2, 1e-3)
 #                       )
 # ns.fnn <- list("1hl" = 10,
 #               "2hl" = c(10, 10),
 #               "3hl" = c(10, 10, 10)
 #                   )
lr.fnn <- list("1hl" = 1e-3)
ns.fnn <- list("1hl" = 10)
lambda1 <-c(1,3,10,30,100)
lambda2 <- 0
         ### Regularization parameters:
            # lambda1.wide = c(0.1,0.3,1,3,10)

## --------------------- Parameters of FNN wide --------------------------
lr.pre <- c(1e-5, 1e-5)
lr.wide <- 1e-5
ns.wide <- list('pre' = c(5, 5), 'wide' = 5)
lambda1.wide <- c(1,3,10,30,100)
lambda2.wide <- 0
ninput <- 2
nhidden.pre <- 1
nhidden.wide <- 1