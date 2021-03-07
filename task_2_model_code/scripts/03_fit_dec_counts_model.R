# vector of packages to load
packages_to_load <- c('dplyr', 'readr', 'rethinking') 

# using the map_lgl function to load each element of the aforementioned vector.
purrr::map_lgl(packages_to_load, library, character.only = TRUE, logical.return = TRUE)

# fit the model
dec_counts_model <- quap(
    
    alist(
        
        k_declarations ~ dgeom(prob = inv_logit(logit_prob)),
        
        logit_prob <- mu[month] + rho[region],
        
        mu[month] ~ dlogis(location = 0, scale = 1),
        
        rho[region]  ~ dlogis(location = 0, scale = 1)
        
    ),
    
    data = read_csv('data/clean/clean_counts_of_declarations.csv')
    
)

# write to file
save(dec_counts_model, file = 'models/dec_counts_model.RData')
