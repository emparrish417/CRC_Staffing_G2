# vector of packages to load
packages_to_load <- c('dplyr', 'readr', 'rethinking') 

# using the map_lgl function to load each element of the aforementioned vector.
purrr::map_lgl(packages_to_load, library, character.only = TRUE, logical.return = TRUE)

# fit the model
pa_proj_given_ia_reg_model <- quap(
    
    alist(
        
        pa_projects ~ dlnorm(meanlog = eta, sdlog = exp(log_sdlog)),
        
        eta <- alpha + mu[month] + rho[region] + beta * log(1 + ia_registrations),
        
        alpha ~ dnorm(mean = 0, sd = 5),
        
        mu[month] ~ dnorm(mean = 0, sd = 5),
        
        rho[region]  ~ dnorm(mean = 0, sd = 5),
        
        beta ~ dnorm(mean = 0, sd = 5),
        
        log_sdlog ~ dnorm(mean = 0, sd = 5)
        
    ),
    
    data = read_csv('data/clean/clean_major_disaster_declarations.csv', guess_max = 4000) %>%
        filter(!is.na(pa_projects) & !is.na(month) & !is.na(region) & pa_projects > 0 & !is.na(ia_registrations)), 
    
    start = list(alpha = 0, mu = 0, rho = 0, log_sdlog = 0)
    
)

# write to file
save(pa_proj_given_ia_reg_model, file = 'models/pa_proj_given_ia_reg_model.RData')
