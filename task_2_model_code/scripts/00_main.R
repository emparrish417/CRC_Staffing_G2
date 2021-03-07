source('functions/simulate_a_single_disaster.R')
source('functions/simulate_n_periods_of_disaster_declaration_activity.R')

potential_disasters <- simulate_a_single_disaster(1000)
    
potential_disaster_years <- simulate_n_periods_of_disaster_declaration_activity(1000)

write_csv(potential_disasters, 'data/clean/simulated_potential_disasters.csv')

write_csv(potential_disaster_years, 'data/clean/simulated_potential_disaster_years.csv')

