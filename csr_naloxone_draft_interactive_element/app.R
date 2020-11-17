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

##########################################################################################

ui <- navbarPage(title = "CDC Wonder Data - Underlying Causes of Deaths Dataset",
                 theme = shinytheme("darkly"),
                 tabPanel(title = "Main",
                          fluidPage(
                              sidebarLayout(
                                  sidebarPanel(
                                      fileInput(inputId = "data_file", 
                                                label = "Choose CSV File",
                                                accept = c(
                                                    "text/csv",
                                                    "text/comma-separated-values,text/plain",
                                                    ".csv")
                                      ),
                                      tags$hr(),
                                      checkboxInput(inputId = "header", 
                                                    label = "Header", 
                                                    value = TRUE),
                                      
                                      downloadButton( outputId = "download_hist",
                                                      label = 'Download Histogram'),
                                      downloadButton( outputId = "download_scatterbox",
                                                      label = 'Download Scatter/Box-Plot'),
                                      
                                      # Consider: https://shiny.rstudio.com/reference/shiny/1.4.0/varSelectInput.html
                                      selectInput(inputId = 'variable',
                                                  label = 'Histogram - Choose a variable to plot:',
                                                  choices = names(data),
                                                  selected = 'deaths'),
                                      
                                      sliderInput(inputId = "Bins",
                                                  label = "Number of bins:",
                                                  min = 1,
                                                  max = 50,
                                                  value = 30),
                                      
                                      textOutput(outputId = "Note1", 
                                                 container = span),
                                      
                                      selectInput(inputId = 'variable2',
                                                  label = 'Boxplot/Scatterplot - Choose a variable to plot:',
                                                  choices = names(data),
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

server <- function(input, output) {
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
    
    my_data <- reactive({
        input_file <- input$data_file
        
        if (is.null(input_file))
            return(NULL)
        
        # Ensure correct data types.
        data <- read_csv2(input_file$datapath)
        data <- data %>% mutate_if(is.character, as.factor)
        data <- data %>% mutate(county_code = as.factor(county_code))
        data <- data %>% mutate(year = as.factor(year))
        data <- data %>% mutate(year_code = as.factor(year_code))
        data <- data %>% mutate(percent_of_total_deaths = as.numeric(gsub("%", "", percent_of_total_deaths)))
        data <- data %>% mutate(deaths = as.integer(deaths))
        data <- data %>% mutate(population = as.integer(population))
        
        return(data)
    })
    
    # Print a note about choosing a variable.
    output$Note1 <- renderText({
        print("Plot type dependent on variable type
              (Categorical vs. Quantitative)
              (Note: Use crude_rate_added instead)")
    })
    
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
    
    # Scatterplot or Boxplot.
    my_scatterbox <- reactive({
        
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
    
    #############################################
    
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