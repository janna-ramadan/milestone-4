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
library(lubridate)
library(janitor)
library(gganimate)
library(glue)
library(gifski)

fbi_compiled_complete <- read_csv("fbi_compiled_complete.csv")
newstrends0820 <- read_csv("newstrends0820.csv")
tweetfile_clean <- read_csv("tweetfile_clean.csv")
tweetfile_clean_muslim <- read_csv("tweetfile_clean_muslim.csv")
tweetfile_clean_islam <- read_csv("tweetfile_clean_islam.csv")
tweetfile_clean_arab_sa <- read_csv("tweetfile_clean_arab_sa.csv")
tweetfile_clean_antidem <- read_csv("tweetfile_clean_antidem.csv")
tweetfile_clean_ethnic <- read_csv("tweetfile_clean_ethnic.csv")
tweet_muslim_sentiment <- read_csv("tweetfile_muslim_sentiment.csv")
tweet_islam_sentiment <- read_csv("tweetfile_islam_sentiment.csv")
tweet_arab_sa_sentiment <- read_csv("tweetfile_arab_sa_sentiment.csv")
tweet_antidem_sentiment <- read_csv("tweetfile_antidem_sentiment.csv")
tweet_ethnic_sentiment <- read_csv("tweetfile_ethnic_sentiment.csv")
tweet_aggregate_sentiment <- read_csv("tweetfile_aggregate_sentiment.csv")


# Define UI for application 
ui <- fluidPage(
titlePanel("Islamophobia in the United States:
               Examining the Relationship Between News, Social Media, 
               and Hate Crimes"),
    navbarPage("",
    tabPanel("About", 
             h3("Islamophobia in the United States"),
             p(" Islamophobia (noun): The dislike of or prejudice against
             Islam or Muslims, especially as a political force."),
             br(),
             p("Findings from a 2019 report by the Institute for Social Policy
             and Understanding found that fear of and discrimination against 
             Muslims is on the rise in the United States. The trend can be seen
             in the Patriot Act, which disproportionately impacted and 
             targetted Muslim, Arab, and South Asian Americans, the 2017 
             Muslim Ban and its three iterations since banned immigration
             from Muslim-majority nations, and the rise in hate crimes against
             Muslims and Arabs, a community commonly associated with
             the Muslim American population."),
             br(),
             plotOutput("fbicompiled"),
             br(),
             h3("Data Analysis Aim"),
             p("This projects looks to uncover the relationship between
               Islamophobic sentiments and the trends seen with news coverage
               and social media. Islamophobia directly impacts the Muslim
               American community, but common perceptions about Muslims
               as brown and ethnically Arab or South Asian have caused 
               anti-muslim sentiment and actions to target the Arab and 
               South Asian American communities as well. In analyzing the 
               relationship between hate crimes, news, and social media, 
               all three, now tangled, communities are assessed. Hate crimes 
               will be held as the measure of active islamophobia in 
               the United States and as reflective of national sentiments. 
               When hate crimes rise, it is assumed that anti-Muslim sentiment 
               is rising in parallel. News search trends are gathered at the
               aggregate level through Google News. Social media trends are
               scraped from Twitter, searching specifically for tweets 
               including the words 'Muslim', 'Islam', 'Arab', 'South Asian' 
               and 'Anti-Democratic'. The overarching question is: 
               How does the news and social media activity feed into and shape 
               U.S. public perception of Muslims and Islam?"),
             br(),
             h3("Why I Chose This Topic"),
             p("I decided to focus on Islamophobia, hate crimes, and
               media representation because of my experiences growing up as a
               Muslim American in a post-9/11 United States. Every time I 
               have flown, I have had my bags searched or been taken to the 
               side for an extra pat down, even during domestic flights. 
               While working in a local goverment office, the space given to me
               to pray in was a storage closet. A friend in high school 
               recommended I put 'The Bomb' as my senior year shirt nickname. 
               All this while not wearing a hijab, making me not obviously
               Muslim."),
             br(),
             p("News consumption and social media can easily turn into 
               bubbles due to the algorithms. As a result, 
               I am not actively exposing myself to anti-muslim rhetoric.
               Living in Boston, Massachusetts, in a fairly immigrant heavy 
               community also provides some shielding hateful actions and 
               language. But it is clear that it exists. 
               It has manifested into the Muslim Ban, France's legislation 
               against hijabs and burqas, and the New Zealand Christchurch
               mosque shooting. I hope that through this project, I can
               uncover some of the cycle that creates and perpetuates
               anti-Muslim hateful dialogue and actions in the U.S., which
               is harming Muslim Americans and the conflated Arab and South
               Asian communities as well."),
             br(),
             h3("About Me"),
             p("Hello, my name is Janna Ramadan and I am a sophomore at Harvard
             College studying Government and Near Eastern Languages and 
             Civilizations. This is my final project in the 
             Gov50 Data Science class. You can reach me at 
               janna_ramadan@college.harvard.edu."), 
            p("You can acces my GitHub repo is",
            tags$a(href = "https://github.com/janna-ramadan/milestone-4"
             , "here."))),
    
   
    tabPanel("Media",
             titlePanel(strong("Media")),
             h3("News Trends: Displaying Common Public Correlation Between 
                Muslims and Terrorism"), 
             p("Even without empirical data, it is obvious to those living in 
               America that a stereotype exists connecting Muslims with 
               terrorism. To assessing the news media's role in perpetuating 
               that, it is important to start with the trends of public 
               interest. The graph below shows the relative interest in the
               term 'Muslim' and 'Terrorism' from Google News search trends. 
               The values are relative, meaning that it measures the popularity
               of the term in consideration of the frequency of searches with 
               that term included in the past."),
            br(),
            imageOutput("newstrends0820")),
    
    
    tabPanel("Social Media",
             titlePanel(strong("Social Media Trends Via Twitter Analysis")),
             sidebarLayout(
                 sidebarPanel(
                     selectInput("word_association", 
                                 "Twitter Word Associations",
                                 choices = c("Muslim" = 
                                                 "tweetfile_clean_muslim", 
                                             "Islam" = "tweetfile_clean_islam",
                                             "Arab and South Asian" =
                                            "tweetfile_clean_arab_sa",
                                             "Anti-Democratic" = 
                                                "tweetfile_clean_antidem",
                                             "Aggregate of Muslim, Islam,
                                             Arab, and South Asian" = 
                                                "tweetfile_clean_ethnic"))),
                 mainPanel(
                     p("Explore social media activity relating to islamophobia
               through most frequent words and sentiment analysis of tweets
               including the words 'muslim', 'Islam', 'Arab', 'South Asian', 
               and 'Anti Democratic."),
                     br(),
                     h3("Top 20 Most Frequent Word Associations"),
                     plotOutput("aggregate_word_association")))),
             #HOW DO I LIST MULTIPLE DATA SETS?
                                 
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
               crimes towards South Asians. In the U.S. Census Arabs are 
               considered White, and it was not until 2014 that the FBI began
               to collect separate data on hate crimes motivated by Anti-Arab
               sentiment, which is why the data starts in 2014."),
             br(),
             plotOutput("fbicompiled"))))

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    
    output$fbicompiled <- renderPlot({
        
        ggplot(data = fbi_compiled_complete, mapping = aes(x = year, 
                                                           y = count, 
                                                color = bias_motivation)) +
            geom_point(size = 2) +
            geom_hline(yintercept = 84, 
                       col = "olivedrab3", 
                       lty = "dashed") +
            geom_hline(yintercept = 163.78, 
                       col = "lightskyblue2", 
                       lty = "dashed") +
            theme_bw() +
            labs(title = "1996-2018 Count of Hate Crimes Motivated By 
                 Anti-Muslim or Anti-Arab Sentiment",
                 subtitle = "Counts victims of hate crimes within the 
                 United States",
                 x = "Year",
                 y = "Victim Count",
                 caption = "Source: FBI Hate Crimes") +
            scale_color_manual(name = "Hate Crime Motivation",
                               labels = c("Anti-Arab \nSentiment", 
                                          "Anti-Muslim \nSentiment"),
                               values = c("olivedrab3", "lightskyblue2")) +
            theme(axis.text.x = element_text(size = 5, angle = 45),
                  text = element_text(family = "Palatino"))
        
    })
    
    output$newstrends0820 <- renderImage({
        outfile <- tempfile(fileext='.gif')
        newsplot <- ggplot(data = newstrends0820, mapping = aes(x = date,
                                                y = value,
                                                color = key_word)) +
        geom_line() +
        transition_reveal(date) +
        scale_x_date(date_labels = "%b/%Y", date_breaks = "year") +
        theme(axis.text.x = element_text(size = 5, angle = 90),
              text = element_text(family = "Palatino")) +
        scale_color_manual(values = c("olivedrab3", "brown2"),
                           labels = c("Muslim", "Terorrism"),
                           name = "Key Word Search Term") +
        labs(title = "Level of Search Interest in the Topic 'Muslim' and
                 Search Term 'Terrorist'",
             subtitle = "Values Based on Google News Search Trends Between
                 2008 and 2020",
             x = "Date",
             y = "Relative Interest (with respect to peak search
                 frequency)",
             caption = "Source: Google News Trends")
        animate(newsplot, nframes = 75, render = gifski_renderer("outfile.gif"))
        list(src = "outfile.gif", contentType = "image/gif")
    
    })
    
    output$aggregate_word_association <- renderPlot({
        
        tweetfile_clean %>%
            count(word, sort = TRUE) %>%
            top_n(20) %>%
            mutate(word = reorder(word, n)) %>%
            ggplot(mapping = aes(x = word, y = n)) +
            geom_col(fill = "slategray2") +
            xlab(NULL) +
            coord_flip() +
            theme_minimal() +
            labs(title = "Top 20 Most Frequent Words Across Tweets
                 Including \nthe Words 'Muslim, 'Islam', 'Arab', 
                 'South Asian', and 'Anti-Democratic'",
                 subtitle = "Tweets gathered Nov. 3 - Nov. 17, 2020",
                 caption = "Source: Twitter", 
                 x = "Count",
                 y = "Unique Words")
        
    })
    
    output$tweetfile_clean_muslim <- renderPlot({
        
        tweetfile_clean_muslim %>%
            count(word, sort = TRUE) %>%
            top_n(20) %>%
            mutate(word = reorder(word, n)) %>%
            ggplot(mapping = aes(x = word, y = n)) +
            geom_col(fill = "firebrick2") +
            xlab(NULL) +
            coord_flip() +
            theme_minimal() +
            labs(title = "Top 20 Most Frequent Words Across Tweets Including
                 \nthe Word 'Muslim",
                 subtitle = "Tweets gathered Nov. 3 - Nov. 17, 2020",
                 caption = "Source: Twitter", 
                 x = "Count",
                 y = "Unique Words")
        
    })
    
    output$tweetfile_clean_islam <- renderPlot({
        
        tweetfile_clean_islam %>%
            count(word, sort = TRUE) %>%
            top_n(20) %>%
            mutate(word = reorder(word, n)) %>%
            ggplot(mapping = aes(x = word, y = n)) +
            geom_col(fill = "firebrick2") +
            xlab(NULL) +
            coord_flip() +
            theme_minimal() +
            labs(title = "Top 20 Most Frequent Words Across Tweets
                 Including \nthe Word 'Islam'",
                 subtitle = "Tweets gathered Nov. 3 - Nov. 17, 2020",
                 caption = "Source: Twitter", 
                 x = "Count",
                 y = "Unique Words")
        
    })
    
    output$tweetfile_clean_arab_sa <- renderPlot({
        
        tweetfile_clean_arab_sa %>%
            count(word, sort = TRUE) %>%
            top_n(20) %>%
            mutate(word = reorder(word, n)) %>%
            ggplot(mapping = aes(x = word, y = n)) +
            geom_col(fill = "firebrick2") +
            xlab(NULL) +
            coord_flip() +
            theme_minimal() +
            labs(title = "Top 20 Most Frequent Words Across Tweets
                 Including \nthe Words 'Arab' and 'South Asian'",
                 subtitle = "Tweets gathered Nov. 3 - Nov. 17, 2020",
                 caption = "Source: Twitter", 
                 x = "Count",
                 y = "Unique Words")
        
    })
    
    output$tweetfile_clean_antidem <- renderPlot({
        
        tweetfile_clean_antidem %>%
            count(word, sort = TRUE) %>%
            top_n(20) %>%
            mutate(word = reorder(word, n)) %>%
            ggplot(mapping = aes(x = word, y = n)) +
            geom_col(fill = "firebrick2") +
            xlab(NULL) +
            coord_flip() +
            theme_minimal() +
            labs(title = "Top 20 Most Frequent Words Across Tweets
                 Including \nthe Word 'Anti-Democratic'",
                 subtitle = "Tweets gathered Nov. 3 - Nov. 17, 2020",
                 caption = "Source: Twitter", 
                 x = "Count",
                 y = "Unique Words")
        
    })
    
    output$tweetfile_clean_ethnic <- renderPlot({
        
        tweetfile_clean_ethnic %>%
            count(word, sort = TRUE) %>%
            top_n(20) %>%
            mutate(word = reorder(word, n)) %>%
            ggplot(mapping = aes(x = word, y = n)) +
            geom_col(fill = "firebrick2") +
            xlab(NULL) +
            coord_flip() +
            theme_minimal() +
            labs(title = "Top 20 Most Frequent Words Across Tweets
                 Including \nthe Words 'Muslim, 'Islam', 'Arab', 
                 and 'South Asian',",
                 subtitle = "Tweets gathered Nov. 3 - Nov. 17, 2020",
                 caption = "Source: Twitter", 
                 x = "Count",
                 y = "Unique Words")
        
    })
}


# Run the application
shinyApp(ui = ui, server = server)

