## Priors
config$prior_type$alpha <- "uniform"
config$prior_type$lag <- "uniform"
config$prior_type$epsilon <- "uniform"
config$prior_type$beta0_0 <- "uniform"
config$prior_type$beta0_1 <- "uniform"
config$prior_type$phi0 <- "uniform"
config$prior_type$phi1 <- "uniform"

config$prior_min$alpha <- 0
config$prior_min$lag <- 0
config$prior_min$phi0 <- 0
config$prior_min$phi1 <- 0
config$prior_min$beta0_0 <- 0.001
config$prior_min$beta0_1 <- 0.001
config$prior_min$epsilon <- 0

config$prior_max$alpha <- 0.5
config$prior_max$lag <- 360
config$prior_max$phi0 <- 1
config$prior_max$phi1 <- 1
config$prior_max$beta0_0 <- 0.05
config$prior_max$beta0_1 <- 0.05
config$prior_max$epsilon <- 1

## Seeds
seed1 <- list(alpha=0.1, lag=100, epsilon=0.3, beta0_0=0.01,  beta0_1=0.01,  phi0=0.3, phi1=0.8)
seed2 <- list(alpha=0.3, lag=60,  epsilon=0.5, beta0_0=0.015, beta0_1=0.015, phi0=0.2, phi1=0.5)
seed3 <- list(alpha=0.2, lag=80,  epsilon=0.7, beta0_0=0.02,  beta0_1=0.02,  phi0=0.1, phi1=0.4)
seeds <- list(seed1, seed2, seed3)