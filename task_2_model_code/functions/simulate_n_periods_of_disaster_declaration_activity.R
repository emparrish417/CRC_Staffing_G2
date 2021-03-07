

simulate_n_periods_of_disaster_declaration_activity <- function(n_periods, period_start_date = Sys.Date(), period_duration = 364, period_end_date = period_start_date + period_duration, regions_to_include = 1:10, reference_model_names = c('dec_counts_model' , 'ia_dec_given_pa_proj_model' , 'ia_dec_model' , 'ia_reg_given_pa_proj_model' , 'ia_reg_model' , 'pa_dec_given_ia_reg_model' , 'pa_dec_model' , 'pa_proj_given_ia_reg_model' , 'pa_proj_model')) {

    require(rethinking)
    
    require(tidyverse)
    
    source('functions/simulate_month_and_region.R')
    
    models_to_load <- reference_model_names[!reference_model_names %in% ls(envir = globalenv())]
    
    if(length(models_to_load) > 0) {
        
        map(paste0('models/', models_to_load, '.RData'), load, envir = globalenv())
        
    }
    
    seq.Date(period_start_date, period_end_date, by = 1) %>%
    tibble::tibble(date = .) %>%
        dplyr::mutate(year = lubridate::year(date), month = lubridate::month(date)) %>%
        dplyr::select(year, month) %>%
        unique() %>%
        dplyr::mutate(month_and_year_id = seq_along(year)) %>%
        .[rep(1:nrow(.), each = length(regions_to_include)),] %>%
        dplyr::group_by(month_and_year_id) %>%
        dplyr::mutate(region = regions_to_include) %>%
        dplyr::ungroup() %>%
        dplyr::select(-month_and_year_id) %>%
        cbind(rethinking::sim(dec_counts_model, n = n_periods, data = list(month = .$month, region = .$region)) %>%
                  as.matrix() %>%
                  t()) %>%
        tidyr::pivot_longer(cols = names(.)[stringr::str_detect(names(.), '^[0-9]+$')], names_to = 'simulation', values_to = 'declarations') %>%
        dplyr::mutate(simulation = as.numeric(simulation)) %>%
        dplyr::filter(declarations > 0) %>%
        .[rep(1:nrow(.), .$declarations),] %>%
        dplyr::select(-declarations) %>%
        # dplyr::mutate(nonzero_pa_projects = rethinking::sim(pa_dec_model, n = 2, data = .)[1,],
        #               pa_projects = dplyr::if_else(nonzero_pa_projects == 1, ceiling(rethinking::sim(pa_proj_model, n = 2, data = .)[1,]), 0)) %>%
        # dplyr::mutate(nonzero_ia_registrations = dplyr::if_else(nonzero_pa_projects == 1, rethinking::sim(ia_dec_given_pa_proj_model, n = 2, data = .)[1,], 1L),
        #               ia_registrations = dplyr::if_else(nonzero_ia_registrations == 1, ceiling(rethinking::sim(ia_reg_given_pa_proj_model, n = 2, data = .)[1,]), 0)) %>%
        dplyr::mutate(nonzero_ia_registrations = rethinking::sim(ia_dec_model, n = 2, data = .)[1,],
                      ia_registrations = dplyr::if_else(nonzero_ia_registrations == 1, ceiling(rethinking::sim(ia_reg_model, n = 2, data = .)[1,]), 0)) %>%
        dplyr::mutate(nonzero_pa_projects = dplyr::if_else(nonzero_ia_registrations == 1, rethinking::sim(pa_dec_given_ia_reg_model, n = 2, data = .)[1,], 1L),
                      pa_projects = dplyr::if_else(nonzero_pa_projects == 1, ceiling(rethinking::sim(pa_proj_given_ia_reg_model, n = 2, data = .)[1,]), 0)) %>%
        dplyr::mutate(day = runif(nrow(.), 0, lubridate::days_in_month(month)) %>% ceiling(),
                      date = as.Date(paste(year, month, day, sep = '-'))) %>%
        dplyr::filter(dplyr::between(date, period_start_date, period_end_date)) %>%
        dplyr::arrange(simulation, date, region, pa_projects, ia_registrations) %>%
        dplyr::mutate(simulated_disaster= seq_along(simulation)) %>%
        dplyr::select(simulated_period = simulation, simulated_disaster, year, month, day, date, region, pa_projects, ia_registrations)


}