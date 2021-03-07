# vector of packages to load
packages_to_load <- c('dplyr', 'readr', 'rethinking') 

# using the map_lgl function to load each element of the aforementioned vector.
purrr::map_lgl(packages_to_load, library, character.only = TRUE, logical.return = TRUE)

# fit the model
pa_dec_given_ia_reg_model <- quap(
    
    alist(
        
        nonzero_pa_projects ~ dbern(prob = inv_logit(logit_prob)),
        
        logit_prob <- alpha + mu[month] + rho[region] + beta * log(1 + ia_registrations),
        
        alpha ~ dlogis(location = 0, scale = 1),
        
        mu[month] ~ dlogis(location = 0, scale = 1),
        
        rho[region]  ~ dlogis(location = 0, scale = 1),
        
        beta ~ dlogis(location = 0, scale = 1)
    ),
    
    data = read_csv('data/clean/clean_major_disaster_declarations.csv', guess_max = 5E3) %>%
        filter(!is.na(pa_projects) & !is.na(month) & !is.na(region) & !is.na(ia_registrations) & ia_registrations > 0),
    
    start = list(alpha = 0, mu = 0, rho = 0, beta = 0)
    
)

# write to file
save(pa_dec_given_ia_reg_model, file = 'models/pa_dec_given_ia_reg_model.RData')
