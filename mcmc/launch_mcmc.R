source("mcmc/mcmc_RAM.R")

config$script_priors <- paste0("model/",config$model_name,"/priors.R")
config$script_model <- paste0("model/",config$model_name,"/model_",config$species,".R")

source(config$script_priors)
source(config$script_model)

## MCMC
for(seed_n in 1:length(seeds)){
  config$seed <- seeds[[seed_n]]
  
  if(length(config$seed) > 1){
    
    ## Make Model
    source("model/core_config.R")
    
    result <- NULL
    attempt <- 1
    while(is.null(result) && attempt <= 3) {
      attempt <- attempt + 1
      try(
        result <- mcmc_RAM(target = config$getLogPosterior,
                           theta0 = config$theta_seed,
                           theta_min=config$proposal$limits$lower,
                           theta_max=config$proposal$limits$upper,
                           n_iter = config$n_iterations,
                           n_burnin = config$adapt_shape_stop)
      )
    }
    
    ## Write output
    folder_name <- paste0(config$species, "_", config$model_name, "__", config$run_name)
    config$file_name <- paste0("output/", folder_name, "/", folder_name,
                               "__seed", seed_n, 
                               "__", format.Date(Sys.time(), "%y%m%d_%H%M"), ".rds")
    folder_name <- paste0("output/",folder_name)
    if(!dir.exists(folder_name)) dir.create(folder_name)
    
    if(!is.null(result)) write_rds(result, config$file_name)
  }
}