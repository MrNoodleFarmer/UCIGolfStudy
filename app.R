
# Load R packages
library(rmarkdown)
library(shiny)
library(shinythemes)
library(knitr)

#convert rmd files into md files
rmdfiles <- c("GP_About.rmd", "GP_DataOverview.rmd", "GP_Visualization.rmd")
sapply(rmdfiles, knit, quiet = T)
#when using md files in Shiny, do includeMarkdown("filename")

# Define UI
ui <- fluidPage(theme = shinytheme("slate"),
                navbarPage(
                  "Golf Project Name",
                  tabPanel("Junior Boys",
                           sidebarPanel(
                             tags$h3("Input:"),
                             textInput("txt1", "Given Name:", ""), #txt1 is given to server
                             textInput("txt2", "Surname:", ""), #txt2 given to server
                             
                           ), # sidebarPanel
                           mainPanel(
                             h1("Header 1"),
                             
                             h4("Output 1"),
                             verbatimTextOutput("txtout"), #txtout generates from server
                             
                           ) # mainPanel
                           
                  ), # Navbar 1, tabPanel
                  
                  tabPanel("Main", 
                           "An informational page that includes what this project is and how to navigate it",
                           navlistPanel(widths = c(2,10),
                             "The Project",
                             tabPanel("About", 
                                      includeMarkdown("GP_About.md")),
                             "The Data",
                             tabPanel("Data Overview",
                                      includeMarkdown("GP_DataOverview.md")),
                             tabPanel("Visualization",
                                      includeMarkdown("GP_Visualization.md")),
                             navbarMenu("Evaluations",
                                        tabPanel("E1",
                                                 includeMarkdown("")
                                                  ),
                                        tabPanel("E2",
                                                 includeMarkdown("")
                                                  ),
                                        tabPanel("E3",
                                                 includeMarkdown("")
                                                  ),
                                        tabPanel("E4",
                                                 includeMarkdown("")
                                                  ),
                                        tabPanel("E5",
                                                 includeMarkdown("")
                                                  ),
                                        tabPanel("E6",
                                                 includeMarkdown("")
                                                  ),
                                        ),
                             "The Manual",
                             tabPanel("Component 5")
                           )),
                  
                  tabPanel("Junior Boys", 
                           "show maybe two separate selections for all datamaps in relation to boys, so that the user can compare maps side by side"),
                  
                  tabPanel("Junior Girls", 
                           "show maybe two separate selections for all datamaps in relation to girls, so that the user can compare maps side by side"),
                  
                  tabPanel("Junior Players",
                           "show maybe two separate selections for all datamaps in relation to all players, so that the user can compare maps side by side."),
                  
                  navbarMenu("More",
                             tabPanel("Datasets"),
                             tabPanel("Future Work"))
                  
                ) # navbarPage
) # fluidPage


# Define server function  
server <- function(input, output) {
  
  output$txtout <- renderText({
    paste( input$txt1, input$txt2, sep = " " )
  })
} # server


# Create Shiny object
shinyApp(ui = ui, server = server)