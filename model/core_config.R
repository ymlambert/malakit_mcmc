## Prob functions
config$getLogLikelihood <- function(theta, log = T) {
  tr <- config$simulate(theta)
  
  # p(prev_data|theta)
  prev <- c(mean(tr[which(tr[["year"]]==2015),]$I), mean(tr[which(tr[["year"]]==2018),]$I), mean(tr[which(tr[["year"]]==2019),]$I))
  d_prev <- sum(dbinom(x = round(config$prev$val * config$prev$sample_size,0), size = config$prev$sample_size, prob = prev/config$fixed_param$N, log = T), na.rm = T)
  
  # p(surv_data|theta)
  surv <- tr %>% group_by(month) %>% summarise(rD = sum(rD))
  d_surv <- sum(dpois(x = config$surv, lambda = surv[["rD"]], log = T))
  
  dens <- d_surv + d_prev
  
  if(!log) dens <- exp(dens)
  
  return(dens)
}

config$getLogPrior <- function(theta, log = T){
  log.prior <- c()
  for(param_name in names(config$prior_type)) {
    if(config$prior_type[[param_name]] == "uniform"){
      ## Uniform law between 0-1
      log.prior[param_name] <-
        dunif((theta[[param_name]]-config$prior_min[[param_name]])/(config$prior_max[[param_name]] - config$prior_min[[param_name]]), min = 0, max = 1, log = log)
    }
    else if(config$prior_type[[param_name]] == "normal"){
      ## Reduced normal law
      log.prior[param_name] <-
        dnorm((theta[[param_name]]-config$prior_mean[[param_name]])/config$prior_sd[[param_name]], mean = 0, sd = 1, log = log)
    }
  }
  log.sum <- sum(log.prior)
  return(ifelse(log, log.sum, exp(log.sum)))
}

config$getLogPosterior <- function(theta, log=T){
  log_prior <- config$getLogPrior(theta, log)
  log_likelihood <- config$getLogLikelihood(theta, log)
  return(log_likelihood + log_prior)
}


## Prepare seed
config$theta_seed <- c()
config$proposal$limits <- list(lower = c(), upper = c())

for(param_name in names(config$prior_type)) {
  if((config$prior_type[[param_name]]) != "fixed") {
    seed_k <- config$seed[[param_name]]
    names(seed_k) <- param_name
    config$theta_seed  <- c(config$theta_seed, seed_k)
    
    lower_limit_k <- config$prior_min[[param_name]]
    names(lower_limit_k) <- param_name
    config$proposal$limits$lower <- c(config$proposal$limits$lower, lower_limit_k)
    
    upper_limit_k <- config$prior_max[[param_name]]
    names(upper_limit_k) <- param_name
    config$proposal$limits$upper <- c(config$proposal$limits$upper, upper_limit_k)
  }
}

# Trace template
config$tr_template <- tibble(day =  seq(from = as.Date(config$day_start), to = as.Date(config$day_end), by = 1))
config$tr_template$month <- format.Date(config$tr_template$day, "%Y-%m")
config$tr_template$time <- seq_along(config$tr_template$day)
config$tr_template$period_llin <- ifelse(config$tr_template$day >= config$llin_start,2,1)
config$tr_template$period_malakit <- ifelse(config$tr_template$day >= config$malakit_start,2,1)
