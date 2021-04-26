library(shiny)
library(plotly)
library(ggplot2)
library(lubridate)
library(dplyr)

source("main.R")
source("generate_staffing_plan.R")

ui <- fluidPage(
  
  # Application title
  column(width = 12, tags$head(
    tags$img(src="banner.png", width = "100%", height="1%"),
    tags$script(
      HTML("
            $(document).ready(function(){
              // Mark columns we want to toggle
              $('body').find('div [class=col-sm-4]').addClass('sidebarPanel');
              $('body').find('div [class=col-sm-8]').addClass('mainPanel');
            })


            Shiny.addCustomMessageHandler ('resize',function (message) {
              $('.sidebarPanel').toggle();
              $('.mainPanel').toggleClass('col-sm-8 col-sm-12');
              $(window).trigger('resize')
            });

           ")
    )
  )
  ),
  sidebarLayout(
    sidebarPanel(width = 4,  
      h3("Control Panel", align = "center"),
      br(), selectInput("lane", "Choose Lane", c("Standard" = "Standard", "Work Completed / Fully Documented" = "Work Completed / Fully Documented","Specialized" = "Specialized"), selected = "Standard"), 
      br(),
      wellPanel(sliderInput("extreme", "Workload Demand Extreme", min = 0, max = 100, value = 50, step=25),
      actionButton("demand", "Run Demand Simulation", align="center")),
      wellPanel(h4("Scenario", align="center"),
      fluidRow(column(width = 6,
                      numericInput("existing", "Existing Work Covered (%)", value=100, min=0, max=100)),
      column(width = 6,
                      numericInput("new_work", "New Work Covered (%)", value=100, min=0, max=100)))),
      
      wellPanel(h4("Staffing Constraints", align="center"),
      fluidRow(column(width = 6,
                      numericInput("FTE_hc", "FTE Hiring Capacity (ppl/month)", value=100, min=0)),
               column(width = 6,
                      numericInput("FTE_cap", "FTE Hard Cap", value=1000, min=0))),
      fluidRow(column(width = 6,
                      numericInput("TAC_hc", "TAC Hiring Capacity (ppl/month)", value=100, min=0)),
               column(width = 6,
                      numericInput("TAC_cap", "TAC Hard Cap", value=500, min=0))), numericInput("overtime", "Percent Overtime (%)", value=0, min=0, max=100)),
      actionButton("update", "Update Scenario", align="center")
      # h5("User History"),
      # textOutput("history1"), textOutput("history2"), textOutput("history3"),
    ),
    mainPanel(
      tabsetPanel(
        type = "tabs",
        id = "layout",
    
    # Show a plot of the generated distribution
      
        tabPanel("User Information", value = "instructions", 
                 h3("Consolodated Resource Center Dynamic Staffing Tool", align = "center"),
                 p("One of the major projects FEMA faces today is optimal staffing of their newly created Consolidated
Resource Centers (CRC), which collectively process and approve grants through the FEMA’s Public
Assistance (PA) program in support of disaster events nationwide. Caseload is highly variable from year
to year, and different classifications of employees vary greatly in cost, on-board time, and efficiency.
This dynamic staffing tool serves as a  decision support system to help program leaders in the FEMA PA Program make adaptive
staffing decisions based on projected workload and a model of the CRC staffing process."),
                 br(),
                 h4("Framework", align = "center"),
                 img(src='framework.png', align = "center", width="90%"),
                 
        ),
        tabPanel("Workload Demand Model", value = "demand_out", 
                 h4("Demand and Workload Simulation Outputs"),
                 plotlyOutput("projectsPlot"), br(),
                 plotlyOutput("hoursPlot")
        ),
        tabPanel("Staffing Recommendations", value = "staffing_out",
                 h4("Staffing Recommendations"),
                 actionButton("plan", "Download Staffing Plan"),
                 fluidRow(plotlyOutput("staffingPlot"), br()), 
                 fluidRow(plotlyOutput("eatPlot"), br()), 
                 fluidRow(plotlyOutput("costPlot")),
                 fluidRow(br())
        ),
        tabPanel("Surge Comparisons", value = "summary",
                 br(), fluidRow(plotlyOutput("surgePlot")), br(),
                 h4("Surge Comparison Observations"),
                 htmlOutput("sum_stats"), br(),
                 h4("Surge Staff and Cost Comparisons Over All Months"),
                 fluidRow(
                   column(width = 12, dataTableOutput('sttable')
                   ))
                 
        )
    )
  )
  )
)
  
  

server <- function(input, output, session) {
  
  surge_compare <- function(id, lane_table) {
    old_work <<- (input$existing)/100
    new_work <<- (input$new_work)/100
    ot <<- 1 + ((input$overtime)/100)
    TAC_max <<- input$TAC_cap
    TAC_hiring <<- input$TAC_hc
    FTE_max <<- input$FTE_cap
    FTE_hiring <<- input$FTE_hc
    
    staff_surge <- generate_staffing_plan(
      workload = new_2021_workload, 
      lane_setting = lane_table, 
      old_workload_proportion = old_work, 
      new_workload_proportion = new_work, 
      overtime_rate = ot, 
      surge_setting = 1, 
      fte_hard_cap = FTE_max,
      surge_hard_cap = TAC_max,
      fte_hiring_capacity = FTE_hiring,
      surge_hiring_capacity = TAC_hiring)
    
    staff_nosurge <- generate_staffing_plan(
      workload = new_2021_workload, 
      lane_setting = lane, 
      old_workload_proportion = old_work, 
      new_workload_proportion = new_work, 
      overtime_rate = ot, 
      surge_setting = 0, 
      fte_hard_cap = FTE_max,
      surge_hard_cap = TAC_max,
      fte_hiring_capacity = FTE_hiring,
      surge_hiring_capacity = TAC_hiring)
    
    Month <- staff_surge[["month_n"]]
    Surge_Staff <- staff_surge[["all_current_staff"]]
    Surge_Work_Completed <- staff_surge[["total_work_completed"]]
    Surge_Cost <- staff_surge[["total_cost"]]
    No_Surge_Staff <- staff_nosurge[["all_current_staff"]]
    No_Surge_Work_Completed <- staff_nosurge[["total_work_completed"]]
    No_Surge_Cost <- staff_nosurge[["total_cost"]]
    out_table <- data.frame(Month, Surge_Work_Completed, No_Surge_Work_Completed, Surge_Cost, No_Surge_Cost) 
    
    ## compute summary metrics for comparing staffing plans
    sum(staff_nosurge$surplus_capacity) 
    sum(staff_surge$surplus_capacity) 
    
    sum(staff_nosurge$rollover_work) 
    sum(staff_surge$rollover_work) 
    
    sum(staff_nosurge$management_cost)
    sum(staff_surge$management_cost)
    
    sum(staff_nosurge$total_cost)
    sum(staff_surge$total_cost)
    
    a <- (((sum(staff_surge$rollover_work) - sum(staff_nosurge$rollover_work) ) / sum(staff_nosurge$rollover_work)) * -100)
    
    z <- (sum(staff_surge$management_cost) / sum(staff_surge$total_cost)) * 100
    
    x <- (sum(staff_nosurge$management_cost) / sum(staff_nosurge$total_cost)) * 100
    
    y <- ((sum(staff_surge$total_cost) - sum(staff_nosurge$total_cost)) / sum(staff_nosurge$total_cost)) * 100
    
    
    summary_stats <- list(
      paste("- There was a <b>", round(a,2), "%</b> decrease in project delays when compared to not using surge."),
      paste("- This surge scenario carries a <b>", round(y,2),"%</b> increase in costs over no surge.</h4>"),
      paste("- Surge is <b>", round(z,2), "%</b> management costs as a fraction of total costs"),
      paste("- No surge is <b>", round(x,2), "%</b> management costs as a fraction of total costs.")
      )
    
    list(out_table, HTML(paste(summary_stats, collapse="<br/>")))
    }
  
  
  update_scenario <- function(id) {
    lane <<- input$lane
    old_work <<- (input$existing)/100
    new_work <<- (input$new_work)/100
    ot <<- 1 + ((input$overtime)/100)
    TAC_max <<- input$TAC_cap
    TAC_hiring <<- input$TAC_hc
    FTE_max <<- input$FTE_cap
    FTE_hiring <<- input$FTE_hc
    
    staffing <<- generate_staffing_plan(
      workload = new_2021_workload, 
      lane_setting = lane, 
      old_workload_proportion = old_work, 
      new_workload_proportion = new_work, 
      overtime_rate = ot, 
      surge_setting = 1, 
      fte_hard_cap = FTE_max,
      surge_hard_cap = TAC_max,
      fte_hiring_capacity = FTE_hiring,
      surge_hiring_capacity = TAC_hiring)
    
    staffing_nosurge <<- generate_staffing_plan(
      workload = new_2021_workload, 
      lane_setting = lane, 
      old_workload_proportion = old_work, 
      new_workload_proportion = new_work, 
      overtime_rate = ot, 
      surge_setting = 0, 
      fte_hard_cap = FTE_max,
      surge_hard_cap = TAC_max,
      fte_hiring_capacity = FTE_hiring,
      surge_hiring_capacity = TAC_hiring)
    
    }
  
  import_demand <- function(id) {
    
    extreme <<- input$extreme
    if (extreme == 50) {
      showModal(modalDialog(
        h4("Default Workload Demand Loaded", align="center"))
      )
      demand_outputs <<- read.csv(file = 'all_pa_projects_50.csv')
      
    } else if (extreme == 75) {
      showModal(modalDialog(
        h4("Running Workload Demand...Please Wait", align="center"), footer=NULL)
      )
      demand_outputs <<- read.csv(file = 'all_pa_projects_75.csv')
      Sys.sleep(2)
      removeModal()
      
    } else {
      showModal(modalDialog(
        h4("Running Workload Demand...Please Wait", align="center"), footer=NULL)
      )
      demand_outputs <<- pa_project_demand("project_demand", extreme)
      removeModal()
    }
    
    inter_data <<- demand_outputs %>%
      mutate(year = year(sent_to_crc_date),
             month = month(sent_to_crc_date),
             scoped_total_time = time_crc_project_development + time_peer_review) %>%
      filter(year <= 2022)
    
    pre_2021_workload <- inter_data %>%
      filter(year < 2021) %>%
      mutate(month = 1, year = 2021) %>%
      group_by(month, year, lane) %>%
      summarize(old_workload = sum(scoped_total_time)) %>%
      ungroup()
    
    
    new_2021_workload <<- inter_data %>%
      filter(between(year, 2021, 2022)) %>%
      group_by(month, year, lane) %>%
      summarize(new_workload = sum(scoped_total_time)) %>%
      ungroup() %>%
      left_join(., pre_2021_workload, by = c('month','year','lane')) %>%
      mutate(old_workload = if_else(is.na(old_workload), 0 ,old_workload),
             total_workload = old_workload + new_workload)
    
  }
  
  plot_workload <- function() {
    
    output$projectsPlot <- renderPlotly({
  
      projects_bysize <- inter_data %>%
          filter(between(year, 2021, 2022)) %>%
          group_by(month, year, lane, project_size) %>%
          summarise(num_projects = n()) %>%
          ungroup()
      
      projects_bysize_small <- projects_bysize %>%
        filter(lane == input$lane) %>%
        filter(project_size == "Small") %>%
        arrange(year, month) %>%
        mutate(month_n = seq_along(month),
               lane = input$lane, project_size = "Small")
      
      projects_bysize_large <- projects_bysize %>%
        filter(lane == input$lane) %>%
        filter(project_size == "Large") %>%
        arrange(year, month) %>%
        mutate(month_n = seq_along(month),
               lane = input$lane, project_size = "Large")
      
      projects_bysize_vlarge <- projects_bysize %>%
        filter(lane == input$lane) %>%
        filter(project_size == "Very Large") %>%
        arrange(year, month) %>%
        mutate(month_n = seq_along(month),
               lane = input$lane, project_size = "Very Large")
      
      month_n = c(1:nrow(projects_bysize_large))
     
      
      Month <- projects_bysize_small[["month_n"]] 
      Small <- projects_bysize_small[["num_projects"]]
      Large <- projects_bysize_large[["num_projects"]]
      Very_Large <- projects_bysize_vlarge[["num_projects"]]
    
      data <- data.frame(Month, Small, Large, Very_Large)
      
      fig <- plot_ly(data, x = ~Month, y = ~Small, type = 'bar', name = 'Small')
      fig <- fig %>% add_trace(y = ~Large, name = 'Large')
      fig <- fig %>% add_trace(y = ~Very_Large, name = 'Very Large')
      fig <- fig %>% layout(yaxis = list(title = 'Number of Projects'), barmode = 'stack', title="Number of Projects Per Month")
      
      fig
      
    })
    
    output$hoursPlot <- renderPlotly({
      
      projects_bysize <- inter_data %>%
        filter(between(year, 2021, 2022)) %>%
        group_by(month, year, lane, project_size) %>%
        summarise(scoped_total_time = mean(scoped_total_time)) %>%
        ungroup()
      
      projects_bysize_small <- projects_bysize %>%
        filter(lane == input$lane) %>%
        filter(project_size == "Small") %>%
        arrange(year, month) %>%
        mutate(month_n = seq_along(month),
               lane = input$lane, project_size = "Small")
      
      projects_bysize_large <- projects_bysize %>%
        filter(lane == input$lane) %>%
        filter(project_size == "Large") %>%
        arrange(year, month) %>%
        mutate(month_n = seq_along(month),
               lane = input$lane, project_size = "Large")
      
      projects_bysize_vlarge <- projects_bysize %>%
        filter(lane == input$lane) %>%
        filter(project_size == "Very Large") %>%
        arrange(year, month) %>%
        mutate(month_n = seq_along(month),
               lane = input$lane, project_size = "Very Large")
      
      month_n = c(1:nrow(projects_bysize_large))
      
      Month <- projects_bysize_small[["month_n"]] 
      Small <- projects_bysize_small[["scoped_total_time"]]
      Large <- projects_bysize_large[["scoped_total_time"]]
      Very_Large <- projects_bysize_vlarge[["scoped_total_time"]]
      
      data <- data.frame(Month, Small, Large, Very_Large)
      
      fig <- plot_ly(data, x = ~Month, y = ~Small, type = 'scatter', name = 'Small', mode = 'lines+markers')
      fig <- fig %>% add_trace(y = ~Large, name = 'Large', mode = 'lines+markers')
      fig <- fig %>% add_trace(y = ~Very_Large, name = 'Very Large', mode = 'lines+markers')
      fig <- fig %>% layout(yaxis = list(title = 'Average Processing Time'), title="Average Processing Times Per Month")
      
      fig
      
    })
  }
  
  plot_staffing <- function() {
    standard_out <- surge_compare("st", input$lane)
    output$sum_stats <- renderText({standard_out[[2]]})
    output$sttable <- renderDataTable({standard_out[[1]]}, options = list(scrollX = TRUE, pageLength = 5))
    
    output$surgePlot <- renderPlotly({
      
      Month <- staffing[["month_n"]]
      # Total_Surge_Work <- staffing[["total_work_completed"]]
      # Total_No_Surge_Work <- staffing_nosurge[["total_work_completed"]]
      Total_Surge_Work <- staffing[["all_current_staff"]]
      Total_No_Surge_Work <- staffing_nosurge[["all_current_staff"]]
      
      data <- data.frame(Month, Total_Surge_Work, Total_No_Surge_Work)
      
      fig <- plot_ly(data, x = ~Month, y = ~Total_Surge_Work, name = 'With Surge', type = 'scatter', mode = 'lines')
      fig <- fig %>% layout(yaxis = list(title = 'Number of Staff'), title="Staff By Month: Surge v. No Surge" )
      fig <- fig %>% add_trace(y = ~Total_No_Surge_Work, name = 'Without Surge', mode = 'lines')
      
      fig
      
    })
    
    output$eatPlot <- renderPlotly({
      
      Month <- staffing[["month_n"]]
      Rollover <- staffing[["rollover_work"]]
      Total_Work_Completed <- staffing[["total_work_completed"]]
      Surplus_Hours <- (staffing[["surplus_capacity"]] * -1)
      
      data <- data.frame(Month, Rollover, Total_Work_Completed, Surplus_Hours)
      
      fig <- plot_ly(data, x = ~Month, y = ~Rollover, type = 'bar', name = 'Rollover')
      fig <- fig %>% add_trace(y = ~Total_Work_Completed, name = 'Total Work Completed')
      fig <- fig %>% add_trace(y = ~Surplus_Hours, name = 'Surplus Hours')
      fig <- fig %>% layout(yaxis = list(title = 'Days of Work'), barmode = 'relative', title="Workload and Work Completed")
      
      fig
      
    })
    
    output$workloadPlot <- renderPlotly({
      
      Month <- staffing[["month_n"]]
      FTEs <- staffing[["new_monthly_workload"]]
      
      data <- data.frame(Month, FTEs)
      
      fig <- plot_ly(data, x = ~Month, y = ~FTEs, type = 'bar', name = 'FTEs')
      fig <- fig %>% layout(yaxis = list(title = 'Number of Staff'), barmode = 'stack', title="Staffing Per Month")
      
      fig
      
    })
    
    output$staffingPlot <- renderPlotly({
  
      Month <- staffing[["month_n"]]
      FTEs <- staffing[["current_fte_staff"]]
      TACs <- staffing[["current_surge_staff"]]
      
      data <- data.frame(Month, FTEs, TACs)
      
      fig <- plot_ly(data, x = ~Month, y = ~FTEs, type = 'bar', name = 'FTEs')
      fig <- fig %>% add_trace(y = ~TACs, name = 'TACs')
      fig <- fig %>% layout(yaxis = list(title = 'Number of Staff'), barmode = 'stack', title="Staffing Per Month")
    
      fig
    
    })
    
    output$costPlot <- renderPlotly({
      
      Month <- staffing[["month_n"]]
      FTEs <- staffing[["fte_cost"]]
      TACs <- staffing[["surge_cost"]]
      Management <- staffing[["management_cost"]]
      Total <- staffing[["total_cost"]]
      
      data <- data.frame(Month, FTEs, TACs, Management, Total)
      
      fig <- plot_ly(data, x = ~Month, y = ~FTEs, name = 'FTEs', type = 'scatter', mode = 'lines+markers')
      fig <- fig %>% layout(yaxis = list(title = 'Cost'), title="Cost of Staff Per Month" )
      fig <- fig %>% add_trace(y = ~TACs, name = 'TACs', mode = 'lines+markers')
      fig <- fig %>% add_trace(y = ~Management, name = 'Management', mode = 'lines+markers')
      
      fig
      
    })
  }
  
  observeEvent(input$plan, {
  write.csv(demand_outputs, file.path(getwd(), paste("all_pa_projects_", input$extreme, ".csv")), row.names = FALSE)
  write.csv(staffing, file.path(getwd(), paste("staffing_plan", input$extreme, ".csv")), row.names = FALSE)
  showModal(modalDialog(
    h4("Staffing Plan Downloaded", align="center"))
  )
  })
  
  observeEvent(input$update, {
    update_scenario()
    plot_staffing()
  }, ignoreInit = TRUE)
  
  observeEvent(input$demand, {
    import_demand()
    update_scenario()
    plot_workload()
    plot_staffing()
  })
    
}

# Run the application 
shinyApp(ui = ui, server = server)