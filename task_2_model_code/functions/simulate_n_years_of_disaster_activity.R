simulate_n_years_of_disaster_activity <- function(n_years_of_disaster_activity, reference_model_names = c('dec_counts_model','pa_dec_model', 'pa_proj_model','ia_dec_given_pa_proj_model','ia_reg_given_pa_proj_model')) {
    
    require(rethinking)
    
    require(tidyverse)
    
    models_to_load <- reference_model_names[!reference_model_names %in% ls(envir = globalenv())]
    
    if(length(models_to_load) > 0) {
        
        map(paste0('models/', models_to_load, '.RData'), load, envir = globalenv())
        
        }
    
    expand_grid(month = 1:12, region = 1:10) %>%
        cbind(sim(dec_counts_model, n = n_years_of_disaster_activity, data = list(month = .$month, region = .$region)) %>%
                  as.matrix() %>%
                  t()) %>%
        pivot_longer(cols = names(.)[str_detect(names(.), '^[0-9]+$')], names_to = 'simulation', values_to = 'declarations') %>%
        mutate(simulation = as.numeric(simulation)) %>%
        filter(declarations > 0) %>%
        .[rep(1:nrow(.), .$declarations),] %>%
        select(-declarations) %>%
        mutate(nonzero_pa_projects = sim(pa_dec_model, n = 2, data = .)[1,],
               pa_projects = if_else(nonzero_pa_projects == 1, ceiling(sim(pa_proj_model, n = 2, data = .)[1,]), 0)) %>%
        mutate(nonzero_ia_registrations = if_else(nonzero_pa_projects == 1, sim(ia_dec_given_pa_proj_model, n = 2, data = .)[1,], 1L),
               ia_registrations = if_else(nonzero_ia_registrations == 1, ceiling(sim(ia_reg_given_pa_proj_model, n = 2, data = .)[1,]), 0))
    
}
