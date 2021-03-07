packages_to_load <- c('readr','dplyr','tidyr') 

purrr::map_lgl(packages_to_load, library, character.only = TRUE, logical.return = TRUE)

source('functions/coerce_missing_number_to_zero.R')

read_csv('data/clean/clean_major_disaster_declarations.csv', guess_max = 5000) %>%
    filter(year >= 2004 & year < 2021) %>%
    group_by(year, month, region) %>%
    summarize(k_declarations = n()) %>%
    left_join(expand.grid(year = min(.$year):max(.$year), month = 1:12, region = 1:10), ., by = c('year','month','region')) %>%
    mutate(k_declarations = coerce_missing_number_to_zero(k_declarations)) %>%
    write_csv('data/clean/clean_counts_of_declarations.csv')
