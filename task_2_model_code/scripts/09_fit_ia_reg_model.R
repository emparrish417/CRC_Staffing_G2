# vector of packages to load
packages_to_load <- c('dplyr', 'readr', 'rethinking') 

# using the map_lgl function to load each element of the aforementioned vector.
purrr::map_lgl(packages_to_load, library, character.only = TRUE, logical.return = TRUE)

# fit the model
ia_reg_model <- quap(
    
    alist(
        
        ia_registrations ~ dlnorm(meanlog = eta, sdlog = exp(log_sdlog)),
        
        eta <- alpha + mu[month] + rho[region],
        
        alpha ~ dnorm(mean = 0, sd = 5),
        
        mu[month] ~ dnorm(mean = 0, sd = 5),
        
        rho[region]  ~ dnorm(mean = 0, sd = 5),
        
        log_sdlog ~ dnorm(mean = 0, sd = 5)
        
    ),
    
    data =  read_csv('data/clean/clean_major_disaster_declarations.csv', guess_max = 4000) %>%
        filter(!is.na(ia_registrations) & !is.na(month) & !is.na(region) & ia_registrations > 0),
    
    start = list(alpha = 0, mu = 0, rho = 0, log_sdlog = 0)
    
)

# write to file
save(ia_reg_model, file = 'models/ia_reg_model.RData')
