#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)


library(shiny)

# Define UI for application that draws a histogram
ui <- navbarPage(
    "Milestone 4",
    tabPanel("Model",
             fluidPage(
                 titlePanel("Model Title"),
                 sidebarLayout(
                     sidebarPanel(
                         textInput("name", "What's your name?"),  
                         textOutput("greeting"),
                         sliderInput("year", label="When were you born?",   
                                     min = 1960, max = 2020, value = 2000),  
                         textOutput("AgeCalc"),
                         selectInput(
                             "plot_type",
                             "Plot Type",
                             c("Option A" = "a", "Option B" = "b")
                             
                         )),
                     mainPanel())
             )),
    tabPanel("Discussion",
             titlePanel("Discussion Title"),
             p("Tour of the modeling choices you made and 
              an explanation of why you made them")),
    tabPanel("About", 
             titlePanel("About"),
             h3("Project Background and Motivations"),
             p("Hello, this is my Milestone 4. In it I discuss the various sources 
               I am thinking of using for my project on the intersection of social media, 
               newspapers, and legislation on national public opinion towards Muslims, 
               with a potential impact measure on Arab and South Asian communities caught 
               in the stereotypes surrounding Muslims."),
             h3("About Me"),
             p("My name is Janna Ramadan and I study Government and Near Eastern Languages and Civilizations. 
             You can reach me at janna_ramadan@college.harvard.edu.")))

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$greeting<- renderText({paste0("Hello ", input$name)})
    
}

# Run the application 
shinyApp(ui = ui, server = server)


