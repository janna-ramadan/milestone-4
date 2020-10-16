#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(tidyverse)
library(dplyr)


# Define UI for application 
ui <- navbarPage(
    "Milestone 4",
    tabPanel("About", 
             titlePanel(strong("About")),
             h3("Project Background and Motivations"),
             p("My final project is tentatively going to focus on the impact of 
             groupthink at the national level in the United States as it relates
             to public opinion about Muslims. Anti-Muslim and anti-Islamic 
             sentiments tend to bleed into hate crimes and negative impacts
             on Arab and South Asian communities in the U.S., so those
             communities and public opinion towards them will also be included.
             I am studying the impact social media, news sources, and public 
             officials (through legislation) have on national sentiments toward
             Muslims, Arabs, and South Asians. 
             br()
             For this Milestone, I have compiled data on hate crimes from the
             FBI. I also gathered Google News search trend data and attempted 
             to learn how to pull data from Twitter API. The Twitter API data
             is a work in progress as the data is limited to recent searches
             and it's not clear what key words identify tweets with negative
             sentiments. 
             br()
             Data from Twitter will represent social media popular rhetoric,
             which I will have to measure in accordance with public interaction
             wil the tweets. Thus far, Google's trends on News searchs with the
             key words 'Muslim' and 'Terrorism' is serving as a proxy for the
             impact of news sources. I am still looking for sources and 
             previously gathered data that measures articles and topics for
             the NY Times, Washington Post, BBC, AlJazeera, Fox News, and NPR. 
             Information on public officials' speeches is difficult because
             it breaks it down by official. Considering the amount of 
             representatives at the federal level, it is inefficient to go 
             through them one by one. I am looking for previously collected
             data on the mention of words relating to Muslims in speeches from
             the President or Congress. I may have to make a function that can
             read through speeches and identify frequency of words and measure
             sentiment as positive or negative, but that is honestly a last 
             resort move because I don't know enough code to do that yet.
             br()
             The URL to my rep is https://github.com/janna-ramadan/milestone-4"),
             h3("About Me"),
             p("Hello, my name is Janna Ramadan and I am a sophomore at Harvard
             College studying Government and Near Eastern Languages and 
             Civilizations. This is my Milestone 4 for my final project in the 
             Gov50 Data Science class. 
             br()
             You can reach me at janna_ramadan@college.harvard.edu.")), 
    
    
    
    tabPanel("Model",
             p("To be filled in")),
    #fluidPage(
    # titlePanel("Model Title"),
    # sidebarLayout(
    # sidebarPanel(
    #  textInput("name", "What's your name?"),  
    #  textOutput("greeting"),
    # sliderInput("year", label="When were you born?",   
    #             min = 1960, max = 2020, value = 2000),  
    # textOutput("AgeCalc"),
    # selectInput(
    #    "plot_type",
    #    "Plot Type",
    #    c("Option A" = "a", "Option B" = "b")
    
    #   )),
    # mainPanel())
    # )),
    
    
    
    tabPanel("Public Opinion",
             titlePanel(strong("Public Opinion Towards Muslims, Arabs, and 
                               South Asians")),
             p("Public Opinion about Muslims, Arabs, and South Asians is shaped
             through the public discourse taking place via social media, the 
             news, and those in public offices. By shaping public opinion, the 
             popular rhetoric also impacts the experience of Muslims, Arabs,
             and South Asians in the U.S. and the frequency of hate crimes."), 
             h3("Hate Crimes"), 
             p("The FBI define hate crimes as crimes motivated by prejudice 
               based on race, religion, sexual orientation, or ethnicity. For 
               the purposes of this project, I have focused on data available 
               on hate crimes motivated by Anti-Islam and Anti-Arab sentiments.
               The FBI does not track hate crimes motivated by Anti-South Asian
               sentiments, which it groups into the broader Anti-Asian hate 
               crimes. As such, I am unable to measure the rate of hate 
               crimes towards South Asians."),
             br(),
             plotOutput("fbicompiled"), 
             
             ))

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    
    fbicompiled <- read_csv("fbicompiled.csv")
    
    output$fbicompiled <- renderPlot({
        
        ggplot(data = fbicompiled, mapping = aes(x = year, y = count, color = bias_motivation)) +
            geom_point(size = 2) +
            theme_grey() +
            labs(title = "2012-2018 Count of Hate Crimes Motivated By Anti-Muslim or Anti-Arab Sentiment",
                 subtitle = "Counts victims of hate crimes within the United States",
                 x = "Year",
                 y = "Victim Count",
                 caption = "Source: FBI Hate Crimes") +
            scale_color_manual(name = "Hate Crime Motivation",
                               labels = c("Anti-Arab Sentiment", "Anti-Muslim Sentiment"),
                               values = c("olivedrab3", "lightskyblue2")) +
            theme(axis.text.x = element_text(size = 10, angle = 45),
                  text = element_text(family = "Palatino"))
        
    })
    
}





# Run the application
shinyApp(ui = ui, server = server)

