## Data
config$day_start <- "2014-04-01"
config$day_end <- "2020-03-31"
config$malakit_start <- "2018-04-01"
config$llin_start <- "2019-06-01"

data_surv <- read_csv("data/data_surv_monthly.csv")
data_meteo <- read_csv("data/data_meteo_daily.csv")
config$data_rows <- which(data_meteo$day >= config$day_start & data_meteo$day <= config$day_end)

K <- data_meteo$TX - mean(data_meteo$TX, na.rm = T)

## Fixed parameters
config$fixed_param$N <- 10000
config$fixed_param$pIS0 <- 0.50
config$fixed_param$T_T <- 7
config$fixed_param$rho <- 0.69 

## Config species
if(config$species == "pf"){
  config$surv <- data_surv$obs_pf
  config$prev$years <- c(2015, 2018, 2019)
  config$prev$val <- c(.223*.6, .155*0.2625, .053*.15)
  config$prev$sample_size <- c(421,1188,380)
  config$fixed_param$kappa <- 3
} else if(config$species == "pv"){
  config$surv <- data_surv$obs_pv
  config$prev$years <- c(2015, 2018, 2019)
  config$prev$val <- c(.223*.5, .155*.7625, .053*(1-.15))
  config$prev$sample_size <- c(421,1188,380)
  config$fixed_param$kappa <- 10
  config$fixed_param$T_HI <- 53
}