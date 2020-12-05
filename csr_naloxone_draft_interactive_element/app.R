# CSR Naloxone Standing Order Project
# Shiny Web App
#
# Displays various EDA plots and tables.
#

require(shiny)
require(shinythemes)
require(tidyverse)
require(ggformula)
require(mosaic)
require(fastR2)
require(s245)
require(pander)
require(DHARMa)
require(glmmTMB)
require(MuMIn)
require(car)
require(dplyr)
require(readr)
require(ggeffects)

options(max.print = 6000)

# Set dummy variable choices until user uploads a file.
dummy1 <- c("dumb1", "dumb2", "dumb3")
dummy2 <- c("dumber1", "dumber2", "dumber3")
dummy3 <- c("dumbest1", "dumbest2", "dumbest3")

data_dropped <- data.frame(dummy1, dummy2, dummy3)

##########################################################################################

ui <- navbarPage(title = "CDC Wonder Data - Underlying Causes of Deaths Dataset",
                 theme = shinytheme("darkly"),
                 tabPanel(title = "Main",
                          fluidPage(
                              sidebarLayout(
                                  sidebarPanel(
                                      fileInput(inputId = "data_file", 
                                                label = "Choose File",
                                                accept = c(
                                                    "text/csv/rds",
                                                    "text/comma-separated-values,text/plain",
                                                    ".csv", ".rds")
                                      ),
                                      
                                      tags$hr(),
                                      
                                      radioButtons(inputId = "file_type", 
                                                   label = "Choose File Type", 
                                                   choices = c("CSV", "RDS")),
                                      
                                      downloadButton( outputId = "download_hist",
                                                      label = 'Download Histogram'),
                                      downloadButton( outputId = "download_scatterbox",
                                                      label = 'Download Scatter/Box-Plot'),
                                      
                                      # Consider: https://shiny.rstudio.com/reference/shiny/1.4.0/varSelectInput.html
                                      varSelectInput(inputId = 'variable',
                                                  label = 'Histogram - Choose a variable to plot:',
                                                  data = data_dropped,
                                                  selected = 'deaths'),
                                      
                                      sliderInput(inputId = "Bins",
                                                  label = "Number of bins (currently non-functional):",
                                                  min = 1,
                                                  max = 50,
                                                  value = 30),
                                      
                                      textOutput(outputId = "Note1", 
                                                 container = span),
                                      
                                      varSelectInput(inputId = 'variable2',
                                                  label = 'Boxplot/Scatterplot - Choose a variable to plot:',
                                                  data = data_dropped,
                                                  selected = 'deaths'),
                                  ),
                                  mainPanel(
                                      # https://shiny.rstudio.com/reference/shiny/0.11/plotOutput.html
                                      tabsetPanel(
                                          tabPanel(title = "Histogram",
                                                   plotOutput(outputId = "histogram")
                                          ),
                                          tabPanel(title = "Boxplot/Scatterplot",
                                                   plotOutput(outputId = "boxplot_scatterplot")        
                                          ),
                                          tabPanel(title = "Under Construction")
                                      )
                                  )
                              )
                          )
                 ),
                 tabPanel(title = "Glimpse",
                          verbatimTextOutput("glimpse")
                 ),
                 tabPanel(title = "Head",
                          verbatimTextOutput("head")          
                 )
)

##########################################################################################

server <- function(input, output, session) {
    # creating the plot
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    # "Global" variables.
    # https://stackoverflow.com/questions/53241685/r-shiny-passing-user-input-as-global-string
    my_variable <- reactive({input$variable})
    my_variable2 <- reactive({input$variable2})
    
    ##########################################################################################
    
    my_data <- reactive({
        
        # Get the input object.
        input_file <- input$data_file
        
        if (is.null(input_file))
            return(NULL)
        
        # Determine parsing function to use based on file type.
        if (input$file_type == "RDS") {
            data <- readRDS(file = input_file$datapath)
            print("RDS file type chosen!")
        } 
        else {
            data <- read_csv2(input_file$datapath)
            print("CSV file type chosen!")
        }
        
        # Drop unecessary columns in our dataset.
        data_dropped <- select(data, -c("gender_code", "race_code", "year_code", 
                                            "ten_year_age_groups_code", "county_code", 
                                            "crude_rate", 
                                            "crude_rate_lower_95percent_confidence_interval", 
                                            "crude_rate_upper_95percent_confidence_interval", 
                                            "crude_rate_standard_error"))
        
        # Update the available column names of dataframe from uploaded dataset file.
        updateVarSelectInput(session = session, inputId = "variable", label = "columns", data = data_dropped)
        updateVarSelectInput(session = session, inputId = "variable2", label = "columns2", data = data_dropped)
        
        return(data_dropped)
    })
    
    ##########################################################################################
    
    # Print a note about choosing a variable.
    output$Note1 <- renderText({
        print("Categorical (boxplot)\nQuantitative(scatterplot)")
    })
    
    ##########################################################################################
    
    # Histogram.
    my_hist <- reactive({
        gf_bar(as.formula(paste("~", input$variable)), data = my_data(), alpha = 0.25, color = "#087EC6",
               xlab=input$variable,
               ylab="Number of Observations",
               title="CDC Wonder Underlying Cause of Death Dataset Queries",
               subtitle = "",
               caption = ""
        ) + coord_flip()
        
    })
    
    ##########################################################################################
    
    # Scatterplot or Boxplot.
    my_scatterbox <- reactive({
        
        # Get the input object.
        input_file <- input$data_file
        
        if (is.null(input_file))
            return(NULL)
        
        column_name <- input$variable2
        if (is.numeric(my_data()[[column_name]])) {
            
            my_plot <- gf_point(as.formula(paste("deaths", "~", input$variable2)), data = my_data(), 
                                alpha = 0.1, color = "#087EC6",
                                xlab=input$variable2,
                                ylab="Deaths",
                                title="CDC Wonder Underlying Cause of Death Dataset Queries",
                                subtitle = "",
                                caption = ""
            )
            return(my_plot)
        } else {
            my_plot <- gf_boxplot(as.formula(paste("deaths", "~", input$variable2)), data = my_data(), 
                                  alpha = 0.5, color = "#3F1006",
                                  xlab=input$variable2,
                                  ylab="Deaths",
                                  title="CDC Wonder Underlying Cause of Death Dataset Queries",
                                  subtitle = "",
                                  caption = ""
            ) %>% gf_jitter(alpha = 0.05, color = "#087EC6") + coord_flip()
            return(my_plot)
        }
    })
    
    ##########################################################################################
    
    # Display histogram in the app tab panel.
    output$histogram <- renderPlot({
        print(my_hist())
    })
    
    # Display scatterplot or boxplot in the app tab panel.
    output$boxplot_scatterplot <- renderPlot({
        print(my_scatterbox())
    })
    
    # Display glimpse() of the data.
    # https://stackoverflow.com/questions/24701806/r-shiny-output-summary-statistics
    output$glimpse <- renderPrint({
        glimpse(my_data())
    }, width = 800)
    
    # Display head() of the data.
    output$head <- renderPrint({
        head(my_data(), 200) %>% knitr::kable()
    }, width = 800)
    
    ##########################################################################################
    
    # Export plots.
    output$download_hist <- downloadHandler(
        filename = 'saved_plot.png' ,
        content = function(file) {
            ggplot2::ggsave(file, plot = my_hist(), device = "png")
        }
    )
    output$download_scatterbox <- downloadHandler(
        filename = 'saved_plot2.png' ,
        content = function(file) {
            ggplot2::ggsave(file, plot = my_scatterbox(), device = "png")
        }
    )
}

##########################################################################################

# Run the application 
shinyApp(ui = ui, server = server)