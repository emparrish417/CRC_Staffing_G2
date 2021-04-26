require(tidyverse)
require(lubridate)
require(propagate)
require(FAdist)

pa_project_demand <- function(id, extreme) {

  #load previously wrangled pa project data
  #this includes:
  #1) projects that are in the CRC (Phase 3) and 
  #2) projects that exist "out in the wild" that haven't made it to the crc yet (Phase 2)
  load('final_demand_models/simulated_potential_disaster_years.Rdata')
  load('final_demand_models/crc_processing_time_params_weibull_beta.Rdata')
  load('final_demand_models/total_lag_params.Rdata')
  load('final_demand_models/pa_project_spawn_params.Rdata')
  load('final_demand_models/existing_project_workload.Rdata')
  
  #load fixed simulation results of 1000 possible years of disaster activity
  #period start date = 1-20-2021, the timestamp of our initial pa project dataset
  #simulated_disaster_years <- read_csv('simulated_potential_disaster_years.csv')
  
  # rank order disaster years by percentiles (1% intervals, this can be changed)
  grouped_disaster_years <- potential_disaster_years %>%
      group_by(simulated_period) %>%
      summarize(n_projects = sum(pa_projects)) %>%
      mutate(percent_rank = ntile(.$n_projects, 100))
  
  # example, look at the 50th percentile bucket of simulated disasters
  # we can let the user select this option using a slider or input box
  example_percentile_filter <- grouped_disaster_years %>%
      #filter(percent_rank == 50)
      filter(percent_rank == extreme)
  
  # choose a random disaster from the 50th percentile bucket
  #uncomment the line below for reproducibility when calling this operation within the app
  #set_seed(1)
  chosen_disaster_year <- sample(example_percentile_filter$simulated_period, size = 1)
  
  # get the projects corresponding simulated disaster year
  # generating a project id for simulated projects for convenience
  new_pa_projects <- potential_disaster_years %>% 
      filter(simulated_period == chosen_disaster_year) %>%
      slice(rep(1:n(), times = .$pa_projects)) %>%
      mutate(new_project_id = 1000000 + seq.int(nrow(.)))
  
  # simulate percentages of small / medium / large projects (based on the month of the disaster)
  # simulate each project's arrival date to the CRC 
  # simulate each project's processing time for each step
  
  time_start<- Sys.time()
  print(time_start)
  
  #uncomment the line below for reproducibility when calling this operation within the app
  #set_seed(1)
  
  new_pa_projects_2 <- map_df(1:nrow(new_pa_projects), function(i) {
      
      print(i)                      
      
      tibble(project_id = new_pa_projects$new_project_id[i],
             project_lag = as.integer(as.integer(FAdist::rweibull3(1, 
                                                                   thres = total_lag_params$location,
                                                                   shape = total_lag_params$shape,
                                                                   scale = total_lag_params$scale))),
             sent_to_crc_date = project_lag + new_pa_projects$date[i],
             project_size =   sample(c('Small','Large','Very Large'),
                                     size = 1,
                                     prob = c(pa_project_spawn_params[pa_project_spawn_params$month == new_pa_projects$month[i],]$small_percent[1], 
                                              pa_project_spawn_params[pa_project_spawn_params$month == new_pa_projects$month[i],]$large_percent[1], 
                                              pa_project_spawn_params[pa_project_spawn_params$month == new_pa_projects$month[i],]$very_large_percent[1])),
             lane = sample(c('Standard', 'Specialized', 'Work Completed / Fully Documented'),
                                                        size = 1,
                                                        prob = c(.452, .054, .494)),
             
             time_crc_project_development = FAdist::rweibull3(1, 
                                                              thres = crc_processing_time_params[crc_processing_time_params$step == 'Pending CRC Project Development' & crc_processing_time_params$route == lane & crc_processing_time_params$size == project_size,]$weibull_location[1],
                                                              shape = crc_processing_time_params[crc_processing_time_params$step == 'Pending CRC Project Development' & crc_processing_time_params$route == lane & crc_processing_time_params$size == project_size,]$weibull_shape[1],
                                                              scale = crc_processing_time_params[crc_processing_time_params$step == 'Pending CRC Project Development' & crc_processing_time_params$route == lane & crc_processing_time_params$size == project_size,]$weibull_scale[1]),
             
             time_peer_review = FAdist::rweibull3(1, 
                                                  thres = crc_processing_time_params[crc_processing_time_params$step == 'Pending Peer Review' & crc_processing_time_params$route == lane & crc_processing_time_params$size == project_size,]$weibull_location[1],
                                                  shape = crc_processing_time_params[crc_processing_time_params$step == 'Pending Peer Review' & crc_processing_time_params$route == lane & crc_processing_time_params$size == project_size,]$weibull_shape[1],
                                                  scale = crc_processing_time_params[crc_processing_time_params$step == 'Pending Peer Review' & crc_processing_time_params$route == lane & crc_processing_time_params$size == project_size,]$weibull_scale[1]),
             
             time_insurance_completion = propagate:::rbeta2(1, 
                                                            alpha1 = crc_processing_time_params[crc_processing_time_params$step == 'Pending Insurance Completion' & crc_processing_time_params$route == lane & crc_processing_time_params$size == project_size,]$beta_alpha1[1],
                                                            alpha2 = crc_processing_time_params[crc_processing_time_params$step == 'Pending Insurance Completion' & crc_processing_time_params$route == lane & crc_processing_time_params$size == project_size,]$beta_alpha2[1],
                                                            a = crc_processing_time_params[crc_processing_time_params$step == 'Pending Insurance Completion' & crc_processing_time_params$route == lane & crc_processing_time_params$size == project_size,]$beta_a[1],
                                                            b = crc_processing_time_params[crc_processing_time_params$step == 'Pending Insurance Completion' & crc_processing_time_params$route == lane & crc_processing_time_params$size == project_size,]$beta_b[1]),
             
             time_insurance_peer_review = propagate:::rbeta2(1, 
                                                             alpha1 = crc_processing_time_params[crc_processing_time_params$step == 'Pending Peer Review' & crc_processing_time_params$route == lane & crc_processing_time_params$size == project_size,]$beta_alpha1[1],
                                                             alpha2 = crc_processing_time_params[crc_processing_time_params$step == 'Pending Peer Review' & crc_processing_time_params$route == lane & crc_processing_time_params$size == project_size,]$beta_alpha2[1],
                                                             a = crc_processing_time_params[crc_processing_time_params$step == 'Pending Peer Review' & crc_processing_time_params$route == lane & crc_processing_time_params$size == project_size,]$beta_a[1],
                                                             b = crc_processing_time_params[crc_processing_time_params$step == 'Pending Peer Review' & crc_processing_time_params$route == lane & crc_processing_time_params$size == project_size,]$beta_b[1]),
             
             time_fema_406_hmp_completion = FAdist::rweibull3(1, 
                                                              thres = crc_processing_time_params[crc_processing_time_params$step == 'Pending FEMA 406 HMP Completion' & crc_processing_time_params$route == lane & crc_processing_time_params$size == project_size,]$weibull_location[1],
                                                              shape = crc_processing_time_params[crc_processing_time_params$step == 'Pending FEMA 406 HMP Completion' & crc_processing_time_params$route == lane & crc_processing_time_params$size == project_size,]$weibull_shape[1],
                                                              scale = crc_processing_time_params[crc_processing_time_params$step == 'Pending FEMA 406 HMP Completion' & crc_processing_time_params$route == lane & crc_processing_time_params$size == project_size,]$weibull_scale[1]),
             
             time_qa_review = FAdist::rweibull3(1, 
                                                thres = crc_processing_time_params[crc_processing_time_params$step == 'Pending QA Review' & crc_processing_time_params$route == lane & crc_processing_time_params$size == project_size,]$weibull_location[1],
                                                shape = crc_processing_time_params[crc_processing_time_params$step == 'Pending QA Review' & crc_processing_time_params$route == lane & crc_processing_time_params$size == project_size,]$weibull_shape[1],
                                                scale = crc_processing_time_params[crc_processing_time_params$step == 'Pending QA Review' & crc_processing_time_params$route == lane & crc_processing_time_params$size == project_size,]$weibull_scale[1]),
             
             time_emmie_submission = FAdist::rweibull3(1, 
                                                       thres = crc_processing_time_params[crc_processing_time_params$step == 'Pending EMMIE Submission' & crc_processing_time_params$route == lane & crc_processing_time_params$size == project_size,]$weibull_location[1],
                                                       shape = crc_processing_time_params[crc_processing_time_params$step == 'Pending EMMIE Submission' & crc_processing_time_params$route == lane & crc_processing_time_params$size == project_size,]$weibull_shape[1],
                                                       scale = crc_processing_time_params[crc_processing_time_params$step == 'Pending EMMIE Submission' & crc_processing_time_params$route == lane & crc_processing_time_params$size == project_size,]$weibull_scale[1]),
             
             total_time = time_qa_review +
                 time_fema_406_hmp_completion +
                 time_insurance_peer_review +
                 time_insurance_completion +
                 time_peer_review +
                 time_crc_project_development +
                 time_emmie_submission
             
             )
  }) %>%
      dplyr::select(-project_lag) %>%
      mutate(source = 'simulated_future_projects',
             status = 'Future')
  
  print(Sys.time() - time_start)
  
  #save(new_pa_projects_2, file = 'new_pa_projects_2.Rdata')
  #load('new_pa_projects_2.Rdata')
  #combine all projects in a single dataframe
  all_pa_projects <- bind_rows(new_pa_projects_2, existing_project_workload)
  
  all_pa_projects
}

projects <- pa_project_demand("project_demand", 50)
# write.csv(projects, file.path(getwd(), "all_pa_projects_50.csv"), row.names = FALSE)

