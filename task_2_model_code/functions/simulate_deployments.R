simulate_deployments <- function(n_simulations, position_code, month = NULL, region = NULL, pa_projects = NULL, ia_registrations = NULL, include_ne0 = TRUE, zero_trunc = FALSE){
    
    require(magrittr)
    
    require(rethinking)
    
    require(tidyverse)
    
    source('functions/simulate_a_single_disaster.R')
    
    source('functions/sample_position_billets.R')
    
    simulate_a_single_disaster(n_simulations = n_simulations, month = month, region = region, pa_projects = pa_projects, ia_registrations = ia_registrations) %$% 
        sample_position_billets(position_code = position_code, m = n_simulations, pa_projects = pa_projects, ia_registrations = ia_registrations, include_ne0 = include_ne0, zero_trunc = zero_trunc)
    
}