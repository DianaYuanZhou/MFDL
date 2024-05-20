noise = 0.3
ADADELTA = TRUE
if(ADADELTA == T){
  decay_rate <- 0.9 # Initialize rho
  e <- 1e-8 # Initialize constant
}
epoch = c(3e4, 3e4)

## --------------------- Parameters of NN --------------------------
lr.nn = 0.1

## --------------------- Parameters of FNN deep --------------------------
lr.fnn <- list("1hl" = 1e-3,
               "2hl" = c(5e-2,1e-3),
               "3hl" = c(5e-2, 5e-2, 1e-3)
                      )
ns.fnn <- list("1hl" = 50,
              "2hl" = c(50, 30),
              "3hl" = c(50, 30, 20)
                    )
lambda1 <- c(0,0.1,0.3,1,3)
lambda2 <- 0
         ### Regularization parameters:
            # lambda1.wide = c(0.1,0.3,1,3,10)

## --------------------- Parameters of FNN wide --------------------------
lr.pre <- c(5e-2, 5e-2)
lr.wide <- 5e-2
ns.wide <- list('pre' = c(50, 10), 'wide' = 50)
lambda1.wide <- c(0,0.1,0.3,1,3)
ninput <- 2
nhidden.pre <- 1
nhidden.wide <- 1