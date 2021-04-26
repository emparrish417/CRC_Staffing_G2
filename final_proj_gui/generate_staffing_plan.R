library(tidyverse)
library(lubridate)
#load in a single demand scenario from prior selection
# can be csv or rdata file, this is ultimately chosen by the user
# we can have a few examples pre-populated for the demo


# data <- read_csv('final_demand_models/all_pa_projects.csv') %>%
#   mutate(year = year(sent_to_crc_date),
#          month = month(sent_to_crc_date),
#          scoped_total_time = time_crc_project_development + time_peer_review) %>%
#   filter(year <= 2022)
# 
# pre_2021_workload <- data %>%
#   filter(year < 2021) %>%
#   mutate(month = 1, year = 2021) %>%
#   group_by(month, year, lane) %>%
#   summarize(old_workload = sum(scoped_total_time)) %>%
#   ungroup()
# 
# 
# new_2021_workload <- data %>%
#   filter(between(year, 2021, 2022)) %>%
#   group_by(month, year, lane) %>%
#   summarize(new_workload = sum(scoped_total_time)) %>%
#   ungroup() %>%
#   left_join(., pre_2021_workload, by = c('month','year','lane')) %>%
#   mutate(old_workload = if_else(is.na(old_workload), 0 ,old_workload),
#          total_workload = old_workload + new_workload)

# can come up with scenarios by altering the parameters in the function generate_staffing_plan()

# MUST CHOOSE A LANE!
# Valid options:
# "Standard"
# "Work Completed / Fully Documented"
# "Specialized"

generate_staffing_plan <- function(workload, 
                                   lane_setting = 'Standard',
                                   old_workload_proportion = 1,
                                   new_workload_proportion = 1,
                                   overtime_rate = 1,
                                   days = 30,
                                   fte_cost_param = 5000,
                                   surge_cost_param = 15000,
                                   admin_cost_param = 10500,
                                   surge_setting = 1,
                                   current_fte = 200, 
                                   current_surge = 0,
                                   fte_hard_cap = 1000,
                                   surge_hard_cap = 500,
                                   fte_hiring_capacity = 100,
                                   surge_hiring_capacity = 100,
                                   fte_month_blacklist = c(1:3),
                                   surge_month_blacklist = c(1:1)) 

{
  
  workload2 <- workload %>%
    filter(lane == lane_setting) %>%
    arrange(year, month) %>%
    mutate(month_n = seq_along(month),
           lane = lane_setting)
  
  balanced_output <- data.frame(month_n = 1:nrow(workload2),
                                # new_workload = workload2$new_workload * new_workload_proportion,
                                # old_workload = workload2$old_workload * old_workload_proportion,
                                new_monthly_workload = workload2$new_workload * new_workload_proportion + workload2$old_workload * old_workload_proportion,
                                rollover_work = 0,
                                total_monthly_workload = 0,
                                fte_work_capacity = 0,
                                surge_work_capacity = 0,
                                total_work_capacity = 0,
                                surplus_capacity = 0,
                                fte_work_completed = 0,
                                surge_work_completed = 0,
                                total_work_completed = 0,
                                total_remaining_work = 0,
                                avg_fte_staff_required = 0,
                                max_surge_staff_required = 0,
                                current_fte_staff = 0,
                                new_fte_staff = 0,
                                total_fte_staff = 0,
                                current_surge_staff = 0,
                                new_surge_staff = 0,
                                total_surge_staff = 0,
                                all_current_staff = 0,
                                fte_cost = 0,
                                surge_cost = 0,
                                management_cost = 0,
                                total_cost = 0) 
  
  current_rollover = 0
  
  for (k in 1:nrow(workload2)) {
    
    #update 
    balanced_output$current_fte_staff[[k]] = current_fte
    
    balanced_output$current_surge_staff[[k]] = if_else(surge_setting == 1, current_surge, 0)
    
    balanced_output$total_monthly_workload[[k]] = balanced_output$new_monthly_workload[[k]] + current_rollover
    
    #costs
    balanced_output$surge_cost[[k]] = balanced_output$current_surge_staff[[k]] * surge_cost_param
    
    balanced_output$fte_cost[[k]] = balanced_output$current_fte_staff[[k]] * fte_cost_param
    
    balanced_output$management_cost[[k]] = (balanced_output$current_surge_staff[[k]] + balanced_output$current_fte_staff[[k]]) / 7 * admin_cost_param
    
    balanced_output$total_cost[[k]] = balanced_output$management_cost[[k]] + balanced_output$fte_cost[[k]] + balanced_output$surge_cost[[k]]
    
    #capacity
    balanced_output$fte_work_capacity[[k]] = balanced_output$current_fte_staff[[k]] * days * overtime_rate
    
    balanced_output$surge_work_capacity[[k]] = balanced_output$current_surge_staff[[k]] * days * overtime_rate
    
    balanced_output$total_work_capacity[[k]] = balanced_output$fte_work_capacity[[k]] + balanced_output$surge_work_capacity[[k]]
    
    #completed
    
    balanced_output$fte_work_completed[[k]] = min(balanced_output$total_monthly_workload[[k]], balanced_output$fte_work_capacity[[k]])
    
    balanced_output$surge_work_completed[[k]] =  min(balanced_output$total_monthly_workload[[k]] - balanced_output$fte_work_completed[[k]], balanced_output$surge_work_capacity[[k]])
    
    balanced_output$total_work_completed[[k]] = balanced_output$fte_work_completed[[k]] + balanced_output$surge_work_completed[[k]]
    
    #surplus
    
    balanced_output$surplus_capacity[[k]] = max(0, balanced_output$total_work_capacity[[k]] - balanced_output$total_work_completed[[k]])
    
    #rollover / delays
    
    balanced_output$rollover_work[[k]] = max(0,balanced_output$total_monthly_workload[[k]] - balanced_output$total_work_completed[[k]])
    
    #remaining
    
    balanced_output$total_remaining_work[[k]] = max(0,sum(balanced_output$new_monthly_workload) - sum(balanced_output$total_work_completed[1:k]))
    
    #staff thresholds
    balanced_output$avg_fte_staff_required[[k]] = (balanced_output$total_remaining_work[[k]] - balanced_output$surge_work_capacity[[k]]) / days / overtime_rate / (nrow(workload2) -k)
    
    #hire / fire staff
    balanced_output$new_fte_staff[[k]] = if_else(balanced_output$month[[k]] %in% fte_month_blacklist,
                                                 0,
                                                 if_else(balanced_output$avg_fte_staff_required[[k]] > balanced_output$current_fte_staff[[k]],
                                                         min(min(fte_hiring_capacity, balanced_output$avg_fte_staff_required[[k]] - balanced_output$current_fte_staff[[k]]),
                                                             fte_hard_cap - balanced_output$current_fte_staff[[k]]),
                                                         0))
    
    balanced_output$total_fte_staff[[k]] = balanced_output$current_fte_staff[[k]] + balanced_output$new_fte_staff[[k]]
    
    current_fte = current_fte + balanced_output$new_fte_staff[[k]]
    
    
    
    balanced_output$max_surge_staff_required[[k]] = max(if_else(balanced_output$rollover_work[[k]] > 0,
                                                                ((balanced_output$rollover_work[[k]] + mean(balanced_output$new_monthly_workload[k:nrow(workload2)]))
                                                                 - current_fte * days * overtime_rate) / days / overtime_rate,
                                                                0)
                                                        ,0)
    
    balanced_output$new_surge_staff[[k]] = if_else(surge_setting == 1,
                                                   if_else(balanced_output$month[[k]] %in% surge_month_blacklist,
                                                           0,
                                                           if_else(balanced_output$max_surge_staff_required[[k]] >= balanced_output$current_surge_staff[[k]],
                                                                   min(min(surge_hiring_capacity, balanced_output$max_surge_staff_required[[k]] - balanced_output$current_surge_staff[[k]]),surge_hard_cap - balanced_output$current_surge_staff[[k]] ),
                                                                   balanced_output$max_surge_staff_required[[k]] - balanced_output$current_surge_staff[[k]])),
                                                   0)
    
    
    
    balanced_output$total_surge_staff[[k]] = balanced_output$current_surge_staff[[k]] + balanced_output$new_surge_staff[[k]]
    
    
    
    # store parameters
    
    current_surge = current_surge + balanced_output$new_surge_staff[[k]]
    current_rollover = balanced_output$rollover_work[[k]]
    
    balanced_output$all_current_staff[[k]] = current_fte + current_surge
  }
  
  return(balanced_output %>% 
           mutate(lane = lane_setting))
}
###
#output <- generate_staffing_plan()


# surge scenario: higher total cost, lower ratio of management costs to total costs, minor surplus / waste, way fewer project delays (rollovers from month to month)