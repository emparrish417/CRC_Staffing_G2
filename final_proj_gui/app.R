#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(plotly)
library(DT)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("FEMA CRC Staffing Recommender System"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            h3("Control Panel"),
            p(),
            br(),
            dateRangeInput("dates", "Date range"),
            numericInput("num", "Number of Projects over Range", value = 500, min = 0, max = 5000),
            sliderInput("x2", "Ratio of Large Projects", min = 0, max = 1, value = 0.3),
            checkboxGroupInput("variable", "Include Project types", inline=TRUE,  c("Complete" = "com",
                                                            "Standard" = "stan",
                                                            "Specialized" = "spec"), selected = c("com","stan","spec")),
            sliderInput("x1", "Ratio of Desired Temporary Staff", min = 0, max = 1, value = 0.1),
            numericInput("num3", "Max Approximate Budget", value = 500000, min = 0,
                         max = 50000000),
            checkboxInput(inputId = "surge", label = "Surge event"),
            actionButton("apply_change","Apply Change")
            ),
    
        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                type = "tabs",
                id = "layout",
                tabPanel("User Instructions", value = "instructions", 
                    h3("Consolodated Resource Center Dynamic Staffing Tool"),
                    p("One of the major projects FEMA faces today is optimal staffing of their newly created Consolidated
Resource Centers (CRC), which collectively process and approve grants through the FEMA’s Public
Assistance (PA) program in support of disaster events nationwide. Caseload is highly variable from year
to year, and different classifications of employees vary greatly in cost, on-board time, and efficiency.
This dynamic staffing tool serves as a  decision support system to help program leaders in the FEMA PA Program make adaptive
staffing decisions based on projected workload and a model of the CRC staffing process."),
                    br(),
                    h4("Framework"),
                    img(src='myImage.png', align = "center", width="30%"),
                    h4("User Controls/Instructions"),
                    p("Lorem ipsum")
                         
                ),
                tabPanel("Workload Demand Model", value = "demand", h4("Workload Demand Model"), 
                         h4("Simulated Outputs"),
                         dataTableOutput('table'),
                         fluidRow(column(2,actionButton("download_dem", "Download Simulation Results")),
                                  column(2,actionButton("rerun", "Rerun Simulation")))),
                
                tabPanel("Staffing Recommendations", value = "staffing_out",
                         fluidRow(h4("Staffing Recommendations"),
                                  
                                  
                                  column(6, h4("Baseline Staff"),
                             
                         plotOutput("distPlot2")),
                         column(6, h4("Updated Staffing"),
                       
                                plotOutput("distPlot3"))),
                         tableOutput(outputId = "table2"),
                         
                         actionButton("download", "Download Staffing Plan")
                )
                         
                )
            )
        )
        
    )
sample_data <- read.csv("task_2_model_code//data//clean//simulated_potential_disasters.csv")
staff_data <- read.csv("task_2_model_code//staff_data.csv")
# Define server logic required to draw a histogram
server <- function(input, output, session) {
   
    output$table <- renderDataTable(sample_data)
    
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)
        
        

        # draw the histogram with the specified number of bins
        #hist(x, breaks = bins, col = 'darkgray', border = 'white')
        
        p2 <- plot_ly(x = sample_data["month"],
                      type = "histogram")
    })
    
    # datasetInput <- reactive({
    #     switch(sample_data)
    # })
    # observeEvent(input$x1, {
    #     updateSliderInput(inputId = "x2", value = 100 - input$x1)
    # })
    
    # Generate a summary of the dataset
    # output$summary <- renderPrint({
    #     cols <- sample_data[c("pa_projects", "ia_registrations")]
    #     summary(sample_data[cols])
    # })
    
    output$table2 <- renderTable(staff_data)
    
    output$distPlot2 <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        print(x)
        bins <- 12
        
        
        
        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white') 
        })
    
    output$distPlot3 <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        print(x)
        bins <- 12
        
        
        
        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white') 
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
