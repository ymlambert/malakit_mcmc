# Model
config$simulate <- function(theta){
  
  times <- config$tr_template$time
  
  ## alpha
  alpha <- theta[["alpha"]]
  
  ## lag
  K_lag <- K[config$data_rows-round(theta[["lag"]],0)]
  
  ## epsilon
  epsilon <- theta[["epsilon"]]
  
  ## mu
  mu_NT <- 1/config$fixed_param$T_NT
  mu_T <- 1/config$fixed_param$T_T
  
  ## beta
  beta0 <- theta[["beta0"]]
  beta <- beta0*exp(alpha*K_lag)
  
  ## kappa
  kappa <- config$fixed_param$kappa
  
  ## phi
  phi0 <- theta[["phi0"]]
  phi1 <- theta[["phi1"]]

  phi <- ifelse(config$tr_template$period_malakit == 2, phi1, phi0)
  
  ## m
  m <- (phi-phi0)
  
  ## rho
  rho <- config$fixed_param$rho  
  
  
  ## Initial state
  N <- config$fixed_param$N
  I0 <- config$fixed_param$I0
  IS0 <- I0*config$fixed_param$pIS0
  IA0 <- I0 - IS0
  
  init_state <- c("S" = N-I0, "IA" = IA0, "IS" = IS0, "IT" = 0, "IM" = 0) 
  
  
  mk_ode <- function(time, state, parameters) {
    
    ## states
    S <- state[["S"]]
    IS <- state[["IS"]]
    IA <- state[["IA"]]
    IT <- state[["IT"]]
    IM <- state[["IM"]]
    
    lambda <- beta[time]*(IA/kappa + IS + IT + IM)/N
    
    dS <- -lambda*S + mu_NT*(IA+IS) + mu_T*(IT + IM)
    dIA <- (1-epsilon)*lambda*S - mu_NT*IA
    dIS <- epsilon*(1-phi[time])*lambda*S - mu_NT*IS
    dIT <- epsilon*(phi[time]-m[time])*lambda*S - mu_T*IT
    dIM <- epsilon*m[time]*lambda*S - mu_T*IM
    
    return(list(c(dS, dIA, dIS, dIT, dIM)))
  }
  
  tr <- data.frame(ode(y = init_state,
                       times = times,
                       func = mk_ode,
                       parms = theta,
                       method = "euler"))
  
  
  ## Incidence rates
  tr$rIS <- c(diff(tr$IS),0) + tr$IS*mu_NT
  tr$rIA <- c(diff(tr$IA),0) + tr$IA*mu_NT
  tr$rIT <- c(diff(tr$IT),0) + tr$IT*mu_T
  tr$rIM <- c(diff(tr$IM),0) + tr$IM*mu_T
  
  tr$rD <- rho*tr$rIT
  tr$I <- tr$IS + tr$IA + tr$IT + tr$IM
  
  tr <- merge(tr, config$tr_template, by="time", all.x = T)
  
  return(tr)
}

config$getMonthlyTraj <- function(theta){
  tr <- config$simulate(theta)
  
  tr$rI <- tr$rIA + tr$rIS + tr$rIT + tr$rIM
  tr$IST <- tr$IS + tr$IT + tr$IM
  tr$rIST <- tr$rIS + tr$rIT + tr$rIM
  tr$ITM <- tr$IT + tr$IM
  tr$rITM <- tr$rIT + tr$rIM
  
  tr <- tr %>% ungroup() %>% group_by(month) %>%
    summarise(year = first(year),
              rD = sum(rD), rI = sum(rI), rIS = sum(rIS), rIA = sum(rIA), rIT = sum(rIT), rIM = sum(rIM), rIST = sum(rIST), rITM = sum(rITM),
              S = mean(S), I = mean(I), IS = mean(IS), IA=mean(IA), IT=mean(IT), IM=mean(IM), IST = mean(IST), ITM=mean(ITM))
  tr$time <- 1:nrow(tr)
  return(tr)
}
