require(tidyverse)
require(lubridate)

#load previously wrangled pa project data
#this includes:
#1) projects that are in the CRC (Phase 3) and 
#2) projects that exist "out in the wild" that haven't made it to the crc yet (Phase 2)
existing_pa_projects <- read_csv('combined_existing_projects.csv')

#load fixed simulation results of 1000 possible years of disaster activity
#period start date = 1-20-2021, the timestamp of our initial pa project dataset
simulated_disaster_years <- read_csv('simulated_potential_disaster_years.csv')

# rank order disaster years by percentiles (1% intervals, this can be changed)
grouped_disaster_years <- simulated_disaster_years %>%
    group_by(simulated_period) %>%
    summarize(n_projects = sum(pa_projects)) %>%
    mutate(percent_rank = ntile(.$n_projects, 100))

# example, look at the 50th percentile bucket of simulated disasters
# we can let the user select this option using a slider or input box
example_percentile_filter <- grouped_disaster_years %>%
    filter(percent_rank == 50)

# choose a random disaster from the 50th percentile bucket
#uncomment the line below for reproducibility when calling this operation within the app
#set_seed(42)
chosen_disaster_year <- sample(example_percentile_filter$simulated_period, size = 1)

# get the projects corresponding simulated disaster year
# generating a project id for simulated projects for convenience
new_pa_projects <- simulated_disaster_years %>% 
    filter(simulated_period == chosen_disaster_year) %>%
    slice(rep(1:n(), times = .$pa_projects)) %>%
    mutate(new_project_id = 1000000 + seq.int(nrow(.))) %>% 
    select(-pa_projects)

# simulate percentages of small / medium / large projects
# simulate each project's arrival date to the CRC 
# the rate parameter in rexp is determined by the mean total lag time (precomputed in another script)
# the small, large, and very large probabilities are given by simulating outcomes from a lognormal distribution based on historical data (precomputed in another script)
# small = <131k, large = <1mil, very large = > 1mil.
time_start<- Sys.time()

#uncomment the line below for reproducibility when calling this operation within the app
#set_seed(42)
new_pa_projects_2 <- map_df(1:nrow(new_pa_projects), function(i) {
    
    print(i)                      
    
    tibble(project_id = new_pa_projects$new_project_id[i],
           project_lag = as.integer(rexp(1,1/456)),
           sent_to_crc_date = project_lag + new_pa_projects$date[i],
           project_size =   sample(c('Small','Large','Very Large'),
                                   size = 1,
                                   prob = c(.885, .0839, .0311)))
}) %>%
    select(-project_lag) %>%
    mutate(source = 'simulated_future_projects',
           status = 'Future')
print(Sys.time() - time_start)

#combine all projects in a single dataframe
all_pa_projects <- bind_rows(new_pa_projects_2, existing_pa_projects)

#save(all_pa_projects, file='all_pa_projects.Rdata')
#write_csv(all_pa_projects, 'all_pa_projects.csv')
