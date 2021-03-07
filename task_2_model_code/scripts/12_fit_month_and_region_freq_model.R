library(tidyverse)

declarations <- read_csv('data/clean/clean_major_disaster_declarations.csv', guess_max = 5e3)

marginal_probability_of_region_alpha <- map_dbl(1:10, function(region){
    
    .5 + sum(declarations$region == region)
    
}) 


marginal_probability_of_month_alpha <- map_dbl(1:12, function(month){
    
    .5 + sum(declarations$month == month)
    
})

conditional_probability_of_region_given_month_alpha <- vapply(X = 1:12, FUN.VALUE = numeric(10), FUN = function(month){
    
    map_dbl(1:10, function(region){
        
        .5 + sum(declarations$region[declarations$month == month] == region)
        
    })
    
}) %>% t()

conditional_probability_of_month_given_region_alpha <- vapply(X = 1:10, FUN.VALUE = numeric(12), FUN = function(region){
    
    map_dbl(1:12, function(month){
        
        .5 + sum(declarations$month[declarations$region == region] == month)
        
    })
    
}) %>% t()

save(conditional_probability_of_month_given_region_alpha, file = 'models/conditional_probability_of_month_given_region_alpha.RData')

save(conditional_probability_of_region_given_month_alpha, file = 'models/conditional_probability_of_region_given_month_alpha.RData')

save(marginal_probability_of_month_alpha, file = 'models/marginal_probability_of_month_alpha.RData')

save(marginal_probability_of_region_alpha, file = 'models/marginal_probability_of_region_alpha.RData')





save(conditional_probability_of_month_given_region_alpha, conditional_probability_of_region_given_month_alpha, marginal_probability_of_month_alpha, marginal_probability_of_region_alpha, file = 'models/month_and_region_freq_model.RData')
