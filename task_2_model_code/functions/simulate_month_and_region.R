simulate_month_and_region <- function(n_simulations, month = NULL, region = NULL) {
    
    # n_simulations = 2000; month = NULL; region = NULL
    
    require(LaplacesDemon)
    
    require(tidyverse)
    
    if(!all(c('conditional_probability_of_month_given_region_alpha','conditional_probability_of_region_given_month_alpha','marginal_probability_of_month_alpha','marginal_probability_of_region_alpha') %in% ls(envir = globalenv()))) {
        
        load('models/month_and_region_freq_model.RData', envir = globalenv())
        
        }
    
    
    
    if(is.null(month) & is.null(region)) {
        
        month_p <- rdirichlet(n = n_simulations, alpha = marginal_probability_of_month_alpha)
        
        month <- map_dbl(1:nrow(month_p), function(r){
            
            rmultinom(n = 1, size = 1, p = month_p[r,]) %>%
                as.numeric() %>%
                `==`(1) %>%
                which()
            
        })
        
        
        
        region_p <- rdirichlet(n = n_simulations, alpha = conditional_probability_of_region_given_month_alpha[month,])
        
        
        region <- map_dbl(1:nrow(region_p), function(r){
            
            rmultinom(n = 1, size = 1, p = region_p[r,]) %>%
                as.numeric() %>%
                `==`(1) %>%
                which()
            
        })
        
    } else if(!is.null(month) & is.null(region)) {
        
        month <- rep(month, n_simulations)
        
        region_p <- rdirichlet(n = n_simulations, alpha = conditional_probability_of_region_given_month_alpha[month,])
        
        
        region <- map_dbl(1:nrow(region_p), function(r){
            
            rmultinom(n = 1, size = 1, p = region_p[r,]) %>%
                as.numeric() %>%
                `==`(1) %>%
                which()
            
        })
        
    } else if(is.null(month) & !is.null(region)) {
        
        region <- rep(region, n_simulations)
        
        month_p <- rdirichlet(n = n_simulations, alpha = conditional_probability_of_month_given_region_alpha[region,])
        
        month <- map_dbl(1:nrow(month_p), function(r){
            
            rmultinom(n = 1, size = 1, p = month_p[r,]) %>%
                as.numeric() %>%
                `==`(1) %>%
                which()
            
        })
        
    } else {
        
        month <- rep(month, n_simulations)
        
        region <- rep(region, n_simulations)
        
    }
    
    list(month = month, region = region)
    
}