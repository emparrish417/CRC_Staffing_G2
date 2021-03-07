# load packages, and put Rtools in the system path
packages_to_load <- c('LaplacesDemon', 'snakecase','devtools','magrittr','tidyverse','lubridate','scales','readxl')
purrr::map_lgl(packages_to_load, library, character.only = TRUE, logical.return = TRUE)

# load states and regions, accent replacements for localities, and census data
states_and_regions <- read_csv('data/raw/raw_states_and_regions.csv')

# !!! accent replacements must be properly addressed !!! for reference: https://stackoverflow.com/questions/20495598/replace-accented-characters-in-r-with-non-accented-counterpart-utf-8-encoding
accent_replacements <- list(    'Š'='S', 'š'='s', 'Ž'='Z', 'ž'='z', 'À'='A', 'Á'='A', 'Â'='A', 'Ã'='A', 'Ä'='A', 'Å'='A', 'Æ'='A', 'Ç'='C', 'È'='E', 'É'='E',
                                'Ê'='E', 'Ë'='E', 'Ì'='I', 'Í'='I', 'Î'='I', 'Ï'='I', 'Ñ'='N', 'Ò'='O', 'Ó'='O', 'Ô'='O', 'Õ'='O', 'Ö'='O', 'Ø'='O', 'Ù'='U',
                                'Ú'='U', 'Û'='U', 'Ü'='U', 'Ý'='Y', 'Þ'='B', 'ß'='Ss', 'à'='a', 'á'='a', 'â'='a', 'ã'='a', 'ä'='a', 'å'='a', 'æ'='a', 'ç'='c',
                                'è'='e', 'é'='e', 'ê'='e', 'ë'='e', 'ì'='i', 'í'='i', 'î'='i', 'ï'='i', 'ð'='o', 'ñ'='n', 'ò'='o', 'ó'='o', 'ô'='o', 'õ'='o',
                                'ö'='o', 'ø'='o', 'ù'='u', 'ú'='u', 'û'='u', 'ý'='y', 'ý'='y', 'þ'='b', 'ÿ'='y' )

county_census <- read.csv('data/raw/raw_2010_census_data.csv', skip = 1, encoding = 'Latin-1') %>%
    rename_all(to_any_case, case = 'snake') %>%
    select(geographic_area, population, housing_units, area_in_square_miles_total_area, area_in_square_miles_land_area, area_in_square_miles_water_area) %>%
    filter(geographic_area != 'United States') %>%
    mutate(geographic_area = str_remove(geographic_area, 'United States - ')) %>%
    separate(geographic_area, into = c('sctt','locality'), sep = ' - ') %>%
    filter(!is.na(locality)) %>%
    mutate_at(.vars = vars(population, housing_units), str_remove, '\\([r0-9]+\\)') %>%
    mutate_at(.vars = vars(population, housing_units), as.numeric) %>%
    left_join(states_and_regions %>% select(state_name, state), by = c('sctt'='state_name')) %>%
    select(-sctt) %>%
    mutate(locality = chartr(paste(names(accent_replacements), collapse=''),
                             paste(accent_replacements, collapse=''),
                             locality))

# load ia, pa, and hm program data
# to refersh data
read_csv('https://www.fema.gov/api/open/v1/RegistrationIntakeIndividualsHouseholdPrograms.csv') %>%
    write_csv('data/raw/raw_ia_registrations.csv')

ia_registrations <- read_csv('data/raw/raw_ia_registrations.csv') %>%
    rename_all(to_any_case, case = 'snake') %>%
    select(-county, -city, -zip_code, -hash, -last_refresh, -id) %>%
    filter(!is.na(disaster_number)) %>%
    group_by(disaster_number, state) %>%
    summarize_all(sum) %>%
    ungroup() %>%
    rename_at(.vars = names(.) %>% `[`(., !. %in% c('disaster_number','state')), .funs = list(~paste0('ia_',.))) %>%
    mutate(ia_amount = ia_ihp_amount + ia_ha_amount + ia_ona_amount)

# to refersh data
read_csv('https://www.fema.gov/api/open/v1/PublicAssistanceFundedProjectsDetails.csv') %>%
    write_csv('data/raw/raw_pa_projects.csv')

pa_projects <- read_csv('data/raw/raw_pa_projects.csv') %>%
    rename_all(to_any_case, case = 'snake') %>%
    group_by(disaster_number, state = state_code) %>%
    summarize(pa_projects = n(), pa_applicants = n_distinct(applicant_id), pa_amount = sum(project_amount), pa_federal_share_obligated = sum(federal_share_obligated), pa_total_obligated = sum(total_obligated)) %>%
    ungroup()

# to refresh data
read_csv('https://www.fema.gov/api/open/v1/HazardMitigationAssistanceProjects.csv') %>% write_csv('data/raw/raw_hm_projects.csv')

hm_projects <- read_csv('data/raw/raw_hm_projects.csv') %>%
    rename_all(to_any_case, case = 'snake') %>%
    filter(!is.na(disaster_number)) %>%
    mutate(projects = 1) %>%
    group_by(disaster_number, state_name = state) %>%
    summarize_at(.vars = vars(number_of_properties, projects, number_of_final_properties, project_amount), sum) %>%
    ungroup() %>%
    filter(!is.na(project_amount)) %>%
    left_join(states_and_regions %>%
                  select(-region), by = 'state_name') %>%
    select(-state_name) %>%
    rename_at(.vars = names(.) %>% `[`(., !. %in% c('disaster_number','state')), .funs = list(~ paste0('hm_',.)))


# combine data into one dataframe
# to refresh the data
read_csv('https://www.fema.gov/api/open/v1/DisasterDeclarationsSummaries.csv') %>%
    write_csv('data/raw/raw_declarations.csv')

source('functions/coerce_missing_number_to_zero.R')

read_csv('data/raw/raw_declarations.csv') %>%
    rename_all(to_any_case, case = 'snake') %>%
    mutate_at(.vars = vars(ih_program_declared, ia_program_declared, pa_program_declared, hm_program_declared), as.logical) %>%
    mutate_at(.vars = vars(declaration_date, incident_begin_date, incident_end_date, disaster_close_out_date), as.Date) %>%
    left_join(states_and_regions %>% select(-state_name), by = c('state')) %>%
    select(-fy_declared, -title, -hash, -last_refresh, -place_code) %>%
    mutate(declared_county_area = str_remove_all(declared_county_area, '\\(|\\)')) %>%
    filter(disaster_type == 'DR') %>%
    left_join(county_census, by = c('declared_county_area' = 'locality', 'state')) %>%
    select(-c(population, housing_units)) %>%
    mutate_at(.vars = vars(state, region, disaster_type, incident_type), fct_infreq) %>%
    group_by(disaster_number, region, state) %>%
    summarize(area_declarations = n(),
              ih_program_declarations = sum(ih_program_declared),
              ia_program_declarations = sum(ia_program_declared),
              ia_or_ih_program_declarations = sum(ih_program_declared | ia_program_declared),
              pa_program_declarations = sum(pa_program_declared),
              hm_program_declarations = sum(hm_program_declared),
              ih_declared = ih_program_declarations > 0,
              ia_declared = ia_program_declarations > 0,
              ia_or_ih_declared = ia_declared | ih_declared,
              pa_declared = pa_program_declarations > 0,
              hm_declared = hm_program_declarations > 0,
              min_declaration_date = min(declaration_date),
              max_close_out_date = max(disaster_close_out_date),
              square_miles_in_declared_area = sum(area_in_square_miles_total_area, na.rm = FALSE),
              censored_sqaure_miles = any(is.na(area_in_square_miles_total_area))) %>%
    ungroup() %>%
    left_join(pa_projects %>% select(-state), by = 'disaster_number') %>%
    mutate_at(.vars = names(.) %>% `[`(., str_detect(., 'pa_') & . != 'pa_declared'), .funs = list(~ if_else(pa_declared, as.numeric(.), 0))) %>%
    left_join(ia_registrations %>% select(-state), by = 'disaster_number') %>%
    mutate_at(.vars = names(.) %>% `[`(., str_detect(., 'ia_') & !. %in% c('ia_declared', 'ia_or_ih_declared')), .funs = list(~ if_else(ia_or_ih_declared, as.numeric(.), 0))) %>%
    left_join(hm_projects %>% select(-state), by = 'disaster_number') %>%
    mutate_at(.vars = names(.) %>% `[`(., str_detect(., 'hm_') & .!= 'hm_declared'), .funs = list(~ if_else(hm_declared, as.numeric(.), 0))) %>%
    filter(region != 'HQ') %>%
    mutate(region = str_remove(region, 'Region ') %>% as.roman() %>% as.numeric(),
           total_amount = coerce_missing_number_to_zero(ia_amount) + coerce_missing_number_to_zero(pa_amount) + coerce_missing_number_to_zero(hm_project_amount),
           total_amount = if_else(total_amount == 0, NA_real_, total_amount),
           censored_total_amount = is.na(ia_amount) | is.na(pa_amount) | is.na(hm_project_amount),
           log_total_amount = log(total_amount),
           scaled_log_total_amount = scale(log_total_amount, center = mean(log_total_amount, na.rm = TRUE), scale = sd(log_total_amount, na.rm = TRUE)) %>% as.numeric()) %>%
    rename(right_censored_total_amount = censored_total_amount,
           ia_registrations = ia_total_valid_registrations) %>%
    mutate(year = year(min_declaration_date),
           month = month(min_declaration_date)) %>%
    mutate(nonzero_pa_projects = as.numeric(pa_projects > 0),
           nonzero_ia_registrations = as.numeric(ia_registrations > 0)) %>% 
    write_csv('data/clean/clean_major_disaster_declarations.csv')

