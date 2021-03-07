simulate_a_single_disaster <- function(n_simulations, month = NULL, region = NULL, pa_projects = NULL, ia_registrations = NULL, reference_model_names = c('dec_counts_model' , 'ia_dec_given_pa_proj_model' , 'ia_dec_model' , 'ia_reg_given_pa_proj_model' , 'ia_reg_model' , 'pa_dec_given_ia_reg_model' , 'pa_dec_model' , 'pa_proj_given_ia_reg_model' , 'pa_proj_model')) {

    # n_simulations = 2000; month = NULL; region = NULL; pa_projects = NULL; ia_registrations = NULL;
    # reference_model_names = c('dec_counts_model' , 'ia_dec_given_pa_proj_model' , 'ia_dec_model' ,
    #                           'ia_reg_given_pa_proj_model' , 'ia_reg_model' , 'pa_dec_given_ia_reg_model' ,
    #                           'pa_dec_model' , 'pa_proj_given_ia_reg_model' , 'pa_proj_model')

    if(length(n_simulations) > 1 | length(month) > 1 | length(region) > 1 | length(pa_projects) > 1 | length(ia_registrations) > 1) {stop('Only arguments of length 1 are allowed.')}
    
    if((!is.null(pa_projects) && pa_projects < 0) || (!is.null(ia_registrations) && ia_registrations < 0)) {stop('pa_projects and ia_registrations must be non-negative integers or NULL')}
    
    require(rethinking)
    
    require(tidyverse)
    
    source('functions/simulate_month_and_region.R')
    
    models_to_load <- reference_model_names[!reference_model_names %in% ls(envir = globalenv())]
    
    if(length(models_to_load) > 0) {
        
        # map(paste0('models/', models_to_load, '.RData'), function(x) {
        #     
        #     print(x)
        #     
        #     load(x, envir = globalenv())
        #     
        #     })
        
        map(paste0('models/', models_to_load, '.RData'), load, envir = globalenv())
        
    }
    
    my_data <- simulate_month_and_region(n_simulations = n_simulations, month = month, region = region) 
    
    if(is.null(pa_projects) & is.null(ia_registrations)) {
        
        nonzero_pa_projects <- sim(pa_dec_model, n = 2, data = my_data)[1, ]
        
        my_data <- c(my_data, nonzero_pa_projects = list(nonzero_pa_projects))
        
        pa_projects <- if_else(nonzero_pa_projects == 1, ceiling(sim(pa_proj_model, n = 2, data = my_data)[1,]), 0)
        
        my_data <- c(my_data, pa_projects = list(pa_projects))
        
        nonzero_ia_registrations = if_else(nonzero_pa_projects == 1, sim(ia_dec_given_pa_proj_model, n = 2, data = my_data)[1,], 1L)
        
        my_data <- c(my_data, nonzero_ia_registrations = list(nonzero_ia_registrations))
        
        ia_registrations = if_else(nonzero_ia_registrations == 1, ceiling(sim(ia_reg_given_pa_proj_model, n = 2, data = my_data)[1,]), 0)
        
        my_data <- c(my_data, ia_registrations = list(ia_registrations))
        
    } else if(!is.null(pa_projects) & is.null(ia_registrations)) {
        
        nonzero_pa_projects <- rep(as.numeric(pa_projects > 0), n_simulations)
        
        pa_projects <- rep(pa_projects, n_simulations)
        
        my_data <- c(my_data, nonzero_pa_projects = list(nonzero_pa_projects), pa_projects = list(pa_projects))
        
        nonzero_ia_registrations = if_else(nonzero_pa_projects == 1, sim(ia_dec_given_pa_proj_model, n = 2, data = my_data)[1,], 1L)
        
        my_data <- c(my_data, nonzero_ia_registrations = list(nonzero_ia_registrations))
        
        ia_registrations = if_else(nonzero_ia_registrations == 1, ceiling(sim(ia_reg_given_pa_proj_model, n = 2, data = my_data)[1,]), 0)
        
        my_data <- c(my_data, ia_registrations = list(ia_registrations))
        
    } else if(is.null(pa_projects) & !is.null(ia_registrations)) {
        
        nonzero_ia_registrations <- rep(as.numeric(ia_registrations > 0), n_simulations)
        
        ia_registrations <- rep(ia_registrations, n_simulations) 
        
        my_data <- c(my_data, nonzero_ia_registrations = list(nonzero_ia_registrations), ia_registrations = list(ia_registrations))
        nonzero_pa_projects <-  if_else(nonzero_ia_registrations == 1, sim(pa_dec_given_ia_reg_model, n = 2, data = my_data)[1, ], 1L)
        
        my_data <- c(my_data, nonzero_pa_projects = list(nonzero_pa_projects))
        
        pa_projects <- if_else(nonzero_pa_projects == 1, ceiling(sim(pa_proj_given_ia_reg_model, n = 2, data = my_data)[1,]), 0)
        
        my_data <- c(my_data, pa_projects = list(pa_projects))
        
        
    } else if(!is.null(pa_projects) & !is.null(ia_registrations)) {
        
        nonzero_pa_projects <- rep(as.numeric(pa_projects > 0), n_simulations)
        
        pa_projects <- rep(pa_projects, n_simulations)
        
        nonzero_ia_registrations <- rep(as.numeric(ia_registrations > 0), n_simulations)
        
        ia_registrations <- rep(ia_registrations, n_simulations) 
        
        my_data <- c(my_data, nonzero_pa_projects = list(nonzero_pa_projects), pa_projects = list(pa_projects), nonzero_ia_registrations = list(nonzero_ia_registrations), ia_registrations = list(ia_registrations))
    }
    
    as_tibble(my_data)
    
}