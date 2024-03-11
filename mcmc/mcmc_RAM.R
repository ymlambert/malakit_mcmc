## Publication : 
## Haario 2001: https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.52.3205&rep=rep1&type=pdf
## Vihola 2012: https://link.springer.com/article/10.1007/s11222-011-9269-5

## From fitR
## https://sbfnk.github.io/fitR//index.html
printNamedVector <- function (x, fmt = "%.2f", sep = " | ") {
  paste(paste(names(x), sprintf(fmt, x), sep = " = "), collapse = sep)
}

## From CRAN
## https://cran.r-project.org/web/packages/ramcmc/vignettes/ramcmc.html
mcmc_RAM <- function(theta0, target, theta_min=NULL, theta_max=NULL, S=NULL, n_iter, n_burnin, print_info_every = n_iter/100, adapt = T) {
  
  p <- length(theta0)
  
  if(is.null(S)) S <- diag(theta0/10)
  if(is.null(theta_min)) {
    theta_min <- theta0
    theta_min[] <- -Inf
  }
  if(is.null(theta_max)){
    theta_max <- theta0
    theta_max[] <- Inf
  } 
  
  theta <- matrix(NA, n_iter, p)
  colnames(theta) <- names(theta0) 
  theta[1, ] <- theta0
  
  accept <- numeric(n_iter)
  
  posterior <- numeric(n_iter)
  posterior[1] <- target(theta0)
  
  if (!is.null(print_info_every)) {
    message(Sys.time(), ", Init: ", printNamedVector(theta0), ", logdensity: ", posterior[1])
  }
  
  for (i in 2:n_iter){
    u <- rnorm(p)
    theta_prop <- c(theta[i - 1, ] + S %*% u)
    names(theta_prop) <- names(theta0)
    
    if (any(theta_prop < theta_min) || any(theta_prop > theta_max)) {
      posterior[i] <- posterior[i - 1]
      theta[i, ] <- theta[i - 1, ]
      acceptance_prob <- 0
    } else {
      posterior_prop <- target(theta_prop)
      acceptance_prob <- min(1, exp(posterior_prop - posterior[i-1]))
      if (runif(1) < acceptance_prob) {
        accept[i] <- 1
        theta[i, ] <- theta_prop
        posterior[i] <- posterior_prop
      } else {
        posterior[i] <- posterior[i - 1]
        theta[i, ] <- theta[i - 1, ]
      }
    } 
    
    if(adapt & i <= n_burnin) {
      S <- ramcmc::adapt_S(S, u, acceptance_prob, i - 1)
    }
    
    acceptance_rate = sum(accept)/i
    
    if (i%%ceiling(print_info_every) == 0) {
      message(Sys.time(), ", Iteration: ", i, "/", n_iter, ", acceptance rate: ", sprintf("%.3f", acceptance_rate), appendLF = FALSE)
      message(", state: ", (printNamedVector(theta[i,])))
      message(", logdensity: ", posterior[i])
    }
    
  }
  
  trace <- as_tibble(theta[1:n_iter, ])
  trace$log.density <- posterior
  
  list(trace = trace, 
       S = S,
       acceptance_rate = sum(accept[(n_burnin + 1):n_iter]) / (n_iter - n_burnin)
       )
}
