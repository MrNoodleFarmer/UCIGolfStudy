# Load R packages
library(rmarkdown)
library(markdown)
library(shiny)
library(shinythemes)
library(knitr)

#convert rmd files into md files
rmdfiles <- c("GP_About.Rmd", "GP_DataOverview.Rmd", "GP_Visualization.Rmd", "GP_Evaluation1.Rmd", "GP_Evaluation2.Rmd", "GP_Evaluation3.Rmd", "GP_Evaluation4.Rmd", "GP_Evaluation4_2.Rmd", "GP_Evaluation4_3.Rmd")
sapply(rmdfiles, knit, quiet = T)
#when using md files in Shiny, do includeMarkdown("filename")

# Define UI
ui <- fluidPage(theme = shinytheme("lumen"),
                navbarPage(
                  "UCI Golf Research Project",
                  
                  tabPanel("The Project", 
                           navlistPanel(widths = c(2,10),
                             "About",
                             tabPanel("About", 
                                      includeMarkdown("GP_About.md")),
                             "The Data",
                             tabPanel("Data Overview",
                                      includeMarkdown("GP_DataOverview.md")),
                             tabPanel("Visualization",
                                      includeMarkdown("GP_Visualization.md"))
                           )),
                  
                  tabPanel("Player and Standardized Population",
                           includeMarkdown("GP_Evaluation1.md")),
                  
                  tabPanel("Median Household Income",
                           includeMarkdown("GP_Evaluation2.md")),
                  
                  tabPanel("Golf Courses Per State",
                           includeMarkdown("GP_Evaluation3.md")),
                  
                  tabPanel("Weather",
                           navlistPanel(widths = c(2,10),
                                        tabPanel("Temperature", 
                                                 includeMarkdown("GP_Evaluation4.md")),
                                        tabPanel("Precipitation",
                                                 includeMarkdown("GP_Evaluation4_2.md")),
                                        tabPanel("Humidity",
                                                 includeMarkdown("GP_Evaluation4_3.md")),
                           )
                  ),
                
                  navbarMenu("More",
                             tabPanel("Datasets",
                             sidebarLayout(
                               
                               # Sidebar panel for inputs ----
                               sidebarPanel(
                                 
                                 # Input: Selector for choosing dataset ----
                                 selectInput(inputId = "dataset",
                                             label = "Choose Dataset to Preview:",
                                             choices = c("JGS boys", "JGS girls", "State Median Household Income", "State Population", "Golf Courses", 'Weather')),
                                 
                                 # Input: Numeric entry for number of obs to view ----
                                 numericInput(inputId = "obs",
                                              label = "Number of observations to view:",
                                              value = 10,
                                              step = 5)
                               ),
                               
                               # Main panel for displaying outputs ----
                               mainPanel(
                                 
                                 # Output: Verbatim text for data summary ----
                                 verbatimTextOutput("summary"),
                                 
                                 # Output: HTML table with requested number of observations ----
                                 tableOutput("view")
                                 
                               )
                             )),
                             tabPanel("Future Work"))
                  
                ) # navbarPage
) # fluidPage


# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
  
  # Return the requested dataset ----
  datasetInput <- reactive({
    switch(input$dataset,
           "JGS boys" = boysUS,
           "JGS girls" = girlsUS,
           "State Med Household Income" = State_Income,
           "State Population" = State_Pop,
           "Golf Courses" = State_Courses,
           "weather" = State_Weather)
  })
  
  # Generate a summary of the dataset ----
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)
  })
  
  # Show the first "n" observations ----
  output$view <- renderTable({
    head(datasetInput(), n = input$obs)
  })
  
} # server


# Create Shiny object
shinyApp(ui = ui, server = server)