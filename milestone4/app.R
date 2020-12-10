#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# loading packages

library(shiny)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(lubridate)
library(janitor)
library(gganimate)
library(glue)
library(gifski)
library(shinythemes)
library(broom.mixed)
library(gtsummary)
library(gt)
library(readxl)
library(rtweet)
library(tidytext)
library(tidymodels)
library(rstanarm)
library(dbplyr)
library(readr)
library(textdata)
library(rsample)

# loading datasets

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

muslimwords <- read_csv("muslimwords.csv")
islamwords <- read_csv("islamwords.csv")
arabsawords <- read_csv("arabsawords.csv")
antidemwords <- read_csv("antidemwords.csv")
ethnicwords <- read_csv("ethnicwords.csv")
totalwords <- read_csv("totalwords.csv")

muslim_numerical <- read_csv("muslim_numerical.csv")
islam_numerical <- read_csv("islam_numerical.csv")
arabsa_numerical <- read_csv("arabsa_numerical.csv")
antidem_numerical <- read_csv("antidem_numerical.csv")
ethnic_numerical <- read_csv("ethnic_numerical.csv")
aggregate_numerical <- read_csv("aggregate_numerical.csv")

ci <- read_csv("ci.csv")
characteryearnews <- read_csv("characteryearnews.csv")

# loading code used in models
 
cinewshate <- characteryearnews %>%
    left_join(fbi_compiled_complete, by = "year") %>%
    drop_na() %>%
    stan_glm(data = ., 
             count ~ mean_value + key_word - 1,
             refresh = 0,
             family = gaussian) %>%
    tbl_regression(., intercept = TRUE) %>%
    as_gt() %>%
    tab_header(title = "Predictive Regression of Count of Hate Crimes",
               subtitle = "The Effect of News Searches
             Including Key Words 'Muslim' or 'Terrorism'")

# Define UI for application 
ui <- fluidPage(
titlePanel("Perpetuating Islamophobia in the United States:
               Examining the Relationship Between News, Social Media, 
               and Hate Crimes"),
    navbarPage("",
               theme = shinytheme("simplex"),
    tabPanel("About", 
             
# Page presents aim of the project, hate crimes motivated by Anti-Muslim
# and Anti-Arab sentiment as recorded by the FBI, and my information.

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
             the Muslim American population. Looking at the graph, note 
            increases in hate crimes in 2001 (9/11) and between 2015, 2016,
              and 2017 (the rise of the Trump campaign, election, and Muslim
               ban). The FBI only began collecting hate crimes done with an
               anti-Arab motivation in 2015, and until now, they do not count 
               anti-South Asian motivated hate crimes separate from anti-Asian
               hate crimes. Thus, data on anti-South Asian sentiment motivated 
               hate crimes is not available. The dashed line represents the
               average amount of annual hate crimes for each respective 
               motivation."),
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
               and 'Anti-Democratic'. These keywords are chosen to directly 
               target tweets about the Muslim community, communities 
               associated with the Muslim community due to ethnic stereotypes,
               and the assumption that Muslim Americans are 'others' and are
               anti-American or anti-Democratic. The overarching question is: 
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
    
  
# Media covers the trends between Google News Search trends from 2008-2020
# for searches including the terms 'Muslim' or 'Terrorism'

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
               that term included in the past. Note that when searches 
               including the term 'Muslim' spike, searches including the 
               term 'terrorism' also spike. This shows an association in the 
               public knowledge and searches between Muslims and terrorism."),
            imageOutput("newstrends0820")),
    
# This page shows the results of the twitter sentiment analysis. It covers
# the most frequent words, word sentiments, and unweighted twtter sentiment
# word association distribution. It uses ifelse functions to create the 
# interactive portion, allowing viewers to compare between the aggregate data
# and the individual key words or aggregate ethnic terms.

    tabPanel("Social Media",
             titlePanel(strong("Social Media Trends Via Twitter Analysis")),
             tabsetPanel(
                 tabPanel("Word Associations By Key Word",
             sidebarLayout(
                 sidebarPanel(
                     selectInput("word_association", 
                                 "Twitter Word Associations",
                                 choices = c("Muslim" = "muslim", 
                                             "Islam" = "islam",
                                             "Arab and South Asian" = "arabsa",
                                             "Anti-Democratic" = "antidem",
                                             "Aggregate of Muslim, Islam,
                                         Arab, and South Asian" = "ethnic"))),
                 mainPanel(
                     br(),
                     p("Explore social media activity relating to islamophobia
               through most frequent words and sentiment analysis of tweets
               including the words 'muslim', 'Islam', 'Arab', 'South Asian', 
               and 'Anti Democratic."),
                     br(),
                     p("The data was collected over the first two weeks of 
                     November 2020
            , during which the United States was preparing for and going 
            through a presidential election. The most frequent words for the
            'Arab', 'South Asian', and 'Anti-Democratic' key word tweets are
            strongly influenced by this. Note the frequency of the words 
            referring to Vice President Kamala Harris, electoral college wins, 
            and political parties.  The dataset I collected 
            includes over 51,000
            scraped tweets from the United States."),
                     br(),
                     p("The most frequent words for tweets with the selected 
                     key word
              'Muslim' and 'Islam' revolve around religion, referencing God,
              the Arabic word for God Allah, Christianity, and the Quran. 
              Though relatively less frequent, some words respond to political
              activity revolving around Muslims. 'Ban' alludes to the Muslim
              ban. 'Trump', 'Biden', and 'Obama' all reference presidents. 
              Perhaps most interesting is the frequency of the words 'Black' 
              and 'American'. According to a 2017 Pew Research report, Black
              Muslims make up only 20% of the Muslim American population. 
              Despite their smaller portion of the general Muslim population, 
              'they 'Black' is the 8th most frequent word present in tweets 
              including the word 'Muslim.'"),
                     h3("Top 20 Most Frequent Word Associations"),
                     plotOutput("aggregate_word_association"),
                     plotOutput("word_association")))),
             
             tabPanel("Word Associations Sentiment",
             sidebarLayout(
                 sidebarPanel(
                     selectInput("word_sentiment", 
                                 "Twitter Word Associations",
                                 choices = c("Muslim" = "muslim2", 
                                             "Islam" = "islam2",
                                            "Arab and South Asian" = "arabsa2",
                                             "Anti-Democratic" = "antidem2",
                                             "Aggregate of Muslim, Islam,
                                         Arab, and South Asian" = "ethnic2"))),
                 mainPanel(
                     h3("Tweet Associated Word Sentiment Ranges"),
                     p("Of all the key word tweet searches, the tweets 
                     including the words 'Arab' and 'South Asian' had the 
                     greatest amount of associated words with negative 
                       sentiments. However, it should be noted that the word 
                       vice is coded as negative, as the sentiment function
                       evaluates words individually. Vice, referring to drugs
                       and alcohol, is coded as a negative word. The word trump
                       is coded positively, likely due to the use of phrases
                       such as 'love trumps hate'. It does not present a 
                       personal opinion. The dataset I collected 
                       includes over 51,000
            scraped tweets from the United States."),
                     br(),
                     p("Considering the dates the data is collected overlap 
                      with the 2020 U.S. elections, it can be assumed that 
                      the use of the word vice more commonly was associated 
                      with the word vice president. The impact of the election
                      can be seen further in the recurrence of the words 
                      progressive, win, victory, support, and fraud."),
                     br(),
                     p("Look closely at the negative terms associated most 
                       frequently with the tweets including the words 'Muslim',
                       'Islam', 'Arab', and 'South Asian'. How many can you 
                       count that refer to terrorism and violence?"),
                     plotOutput("aggregate_word_sentiment"),
                     plotOutput("word_sentiment")))),
             
             tabPanel("Word Associations Sentiment Numerical Ranges",
             sidebarLayout(
                 sidebarPanel(
                     selectInput("sentiment_range", 
                                 "Twitter Word Associations",
                                 choices = c("Muslim" = "muslim3", 
                                             "Islam" = "islam3",
                                            "Arab and South Asian" = "arabsa3",
                                             "Anti-Democratic" = "antidem3",
                                             "Aggregate of Muslim, Islam,
                                         Arab, and South Asian" = "ethnic3"))),
                 mainPanel(
                     h3("Tweet Associated Word Numerical Sentiment Ranges"),
                     p("The below graphs show the general distribution of 
                       the words included in tweets filtered for the key words
                       selected. The dataset I collected includes over 51,000
            scraped tweets from the United States.
            0 is a neutral sentiment. Positive values 
                       denote positive sentiments, with a 5 being more positive 
                       than a 2. Negative values denote negative sentiments, 
                       with a -5 being more negative than a -2.5."),
                     br(),
                     p("Most associated words per key word filter
                       are distributed with a peak around -2.5. However, these
                       graphs depict an unweighted distribution. Look under 
                       the 'Models' tab for further discussion on the true,
                        weighted mean sentiment values per key word filter."),
                     plotOutput("aggregate_sentiment_range"),
                     plotOutput("sentiment_range")))))),
      
# Models covers the result of the twitter analysis and the media vs. hate 
# crimes analysis. I used bootstrap to weight the sentiment range for twitter
# analysis. I used stan_glm to compare search trends to spikes in hate crimes,
# after joining the fbi and news search data sets.

    tabPanel("Models",
             titlePanel(strong("Data Analysis")),
             h3("Twitter Analysis"),
             br(),
             tableOutput("results_tibble"),
             p("The table above displays the weighted means of words associated
               to tweets filtered by key word selected. The more negative the
               values, the more negative the words associated with tweets 
               including the key word. Of the key words tested in this study, 
               tweets including the term 'Anti-Democratic' are associated with
              the most negative sentiment words at -0.76. This should be taken 
              in the context of the November 2020 elections, when this data was
              collected.In second place are tweets including the key words
              'Muslim' and 'Islam', with average weighted sentiments of 
               -0.71."),
             br(),
             p( "This model uses over 51,000 tweets with key word searches for 
                'Muslim', 'Islam', 'Arab', 'South Asian', and 
                'Anti-Democratic'. After filtering for key word, I mutated the
                sentiment values to multiply by the number of repetitions. This
                allows for the sentiment distribution to reflect the reality of
                the distribution of sentiments and frequency. From there I 
                bootstrapped the data 100 times to estimate the distribution
                of the key word associated word sentiments. The upper and lower
                confidence intervals present a very tight range in values. The 
                bounds never differ by more than 0.02, which indicates a very
                high confidence in the estimated sentiment values of words 
                associated with tweets containing the studied key words."),
             h3("News Search Rates vs. Hate Crime Rates"), 
             p("This regression table describes the correlation between hate
               crimes, both anti-Muslim and anti-Arab, to online news searches
               including the terms 'Muslim' and 'Terrorism'. I joined the 
               datasets measuring FBI hatecrimes and Google News Searches
               Trends data, and ran a stan_glm on the dataset. My model 
               measures count of hate crimes regressed by the 
               mean value of searches and key word of search term. From the 
               findings, there is no consistent correlation between the two. 
               This can be noted in the range of the confidence intervals. 
               The range in confidence intervals is massive. The measure of 
               mean value of hate crimes ranges from -2.8 to 7.9. The impacts 
               of online searches including the word 'Muslim' predict 
               hate crime counts ranging from 6.1 to 266.6, looking at the
               range of the upper and lower bounds of the confidence
               interval. This trend follows on the searches including the term
               'Terrorism'. From the data, it cannot be concluded that 
               frequency of searches including the words 'Muslim' and 
               'Terrorism' do not correlate with rises or decreases in 
               anti-muslim or anti-arab hate crimes."),
             br(),
             gt_output(outputId = "cinewshate"))))
            

# Define server to produce my graphs. There are graphs, a gif for the animated
# graph, and the two model tabels.

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
        labs(title = "1996-2018 Count of Hate Crimes Motivated By Anti-Muslim 
             or Anti-Arab Sentiment",
                 subtitle = "Counts victims of hate crimes within the United 
             States",
                 x = "Year",
                 y = "Victim Count",
                 caption = "Source: FBI Hate Crimes") +
            scale_color_manual(name = "Hate Crime Motivation",
                               labels = c("Anti-Arab \nSentiment", 
                                          "Anti-Muslim \nSentiment"),
                               values = c("olivedrab3", "lightskyblue2")) +
            theme(axis.text.x = element_text(size = 10, angle = 45),
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
        
        ggplot(data = totalwords, 
               mapping = aes(x = reorder(word, n), y = n)) +
            geom_col(fill = "slategray2") +
            xlab(NULL) +
            coord_flip() +
            theme_minimal() +
            labs(title = "Top 10 Most Frequent Words Across Tweets Including
            the Words 'Muslim, 'Islam', 
                 'Arab', 'South Asian', and 'Anti-Democratic",
                 subtitle = "Tweets gathered Nov. 3 - Nov. 17, 2020",
                 caption = "Source: Twitter", 
                 x = "Count",
                 y = "Unique Words")
        
    })
    
    output$word_association <- renderPlot({
        if(input$word_association == "muslim") {
        
            ggplot(data = muslimwords, 
                   mapping = aes(x = reorder(word, n), y = n)) +
                geom_col(fill = "firebrick2") +
                xlab(NULL) +
                coord_flip() +
                theme_minimal() +
                labs(title = "Top 10 Most Frequent Words In Tweets Including
                     the Word 'Muslim'",
                     subtitle = "Tweets gathered Nov. 3 - Nov. 17, 2020",
                     caption = "Source: Twitter", 
                     x = "Count",
                     y = "Unique Words")}
        
        else{
            if(input$word_association == "islam") {
                ggplot(data = islamwords, 
                       mapping = aes(x = reorder(word, n), y = n)) +
                    geom_col(fill = "firebrick2") +
                    xlab(NULL) +
                    coord_flip() +
                    theme_minimal() +
                    labs(title = "Top 10 Most Frequent Words In Tweets
                         Including the Word 'Islam'",
                         subtitle = "Tweets gathered Nov. 3 - Nov. 17, 2020",
                         caption = "Source: Twitter", 
                         x = "Count",
                         y = "Unique Words")
            }
            
            else{
                if(input$word_association == "arabsa") {
                    ggplot(data = arabsawords, 
                           mapping = aes(x = reorder(word, n), y = n)) +
                        geom_col(fill = "firebrick2") +
                        xlab(NULL) +
                        coord_flip() +
                        theme_minimal() +
                        labs(title = "Top 10 Most Frequent Words In Tweets
                        Including the Word 'Arab' 
                             and 'South Asian",
                           subtitle = "Tweets gathered Nov. 3 - Nov. 17, 2020",
                             caption = "Source: Twitter", 
                             x = "Count",
                             y = "Unique Words")
                }
                
                else{
                    if(input$word_association == "antidem") {
                        ggplot(data = antidemwords, 
                               mapping = aes(x = reorder(word, n), y = n)) +
                            geom_col(fill = "firebrick2") +
                            xlab(NULL) +
                            coord_flip() +
                            theme_minimal() +
                            labs(title = "Top 10 Most Frequent Words In Tweets
                                 Including the Word 'Anti-Democratic'",
                          subtitle = "Tweets gathered Nov. 3 - Nov. 17, 2020",
                                 caption = "Source: Twitter", 
                                 x = "Count",
                                 y = "Unique Words")
                    }
                    
                    else{ 
                        if(input$word_association == "ethnic") {
                        ggplot(data = ethnicwords, 
                               mapping = aes(x = reorder(word, n), y = n)) +
                            geom_col(fill = "firebrick2") +
                            xlab(NULL) +
                            coord_flip() +
                            theme_minimal() +
                            labs(title = "Top 10 Most Frequent Words Across
                                 Tweets Including the Ethno-Religious Words",
                          subtitle = "Tweets gathered Nov. 3 - Nov. 17, 2020",
                                 caption = "Source: Twitter", 
                                 x = "Count",
                                 y = "Unique Words")
                        }
                    }
                }
            }
                
                
                
            }
                
                }
        
        
        
        )
    
    output$aggregate_word_sentiment <- renderPlot({
        
        ggplot(data = tweet_aggregate_sentiment, 
               mapping = aes(x = reorder(word, n), y = n, fill = sentiment)) +
            geom_col(show.legend = FALSE) +
            facet_wrap(~sentiment, scale = "free_y") +
        labs(title = "Sentiment In Tweets Including the Words 'Muslim, 'Islam', 
       'Arab', 'South Asian', and 'Anti-Democratic'", 
                 subtitle = "Tweets gathered Nov. 3 - Nov. 17, 2020",
                 caption = "Source: Twitter", 
                 x = " ",
                 y = "Contribution to Sentiment") +
            coord_flip() +
            theme_bw() +
            theme(text = element_text(family = "Palatino")) +
            scale_fill_manual(values = c("brown2", "olivedrab3"))
        
    })
    
    output$word_sentiment <- renderPlot({
        if(input$word_sentiment == "muslim2") {
            ggplot(data = tweet_muslim_sentiment, 
                mapping = aes(x = reorder(word, n), y = n, fill = sentiment)) +
            geom_col(show.legend = FALSE) +
                facet_wrap(~sentiment, scale = "free_y") +
                labs(title = "Sentiment In Tweets Including the Word 'Muslim'", 
                     subtitle = "Tweets gathered Nov. 3 - Nov. 17, 2020",
                     caption = "Source: Twitter", 
                     x = " ",
                     y = "Contribution to Sentiment") +
                coord_flip() +
                theme_bw() +
                theme(text = element_text(family = "Palatino")) +
                scale_fill_manual(values = c("brown2", "olivedrab3"))}
        
        else{
            if(input$word_sentiment == "islam2") {
                ggplot(data = tweet_islam_sentiment,
               mapping = aes(x = reorder(word, n), y = n, fill = sentiment)) +
                    geom_col(show.legend = FALSE) +
                    facet_wrap(~sentiment, scale = "free_y") +
                labs(title = "Sentiment In Tweets Including the Word 'Islam'", 
                         subtitle = "Tweets gathered Nov. 3 - Nov. 17, 2020",
                         caption = "Source: Twitter", 
                         x = " ",
                         y = "Contribution to Sentiment") +
                    coord_flip() +
                    theme_bw() +
                    theme(text = element_text(family = "Palatino")) +
                    scale_fill_manual(values = c("brown2", "olivedrab3"))
            }
            
            else{
                if(input$word_sentiment == "arabsa2") {
                    ggplot(data = tweet_arab_sa_sentiment,
              mapping = aes(x = reorder(word, n), y = n, fill = sentiment)) +
                        geom_col(show.legend = FALSE) +
                        facet_wrap(~sentiment, scale = "free_y") +
            labs(title = "Sentiment In Tweets Including the Words 'Arab' and '
       South Asian'", 
                          subtitle = "Tweets gathered Nov. 3 - Nov. 17, 2020",
                             caption = "Source: Twitter", 
                             x = " ",
                             y = "Contribution to Sentiment") +
                        coord_flip() +
                        theme_bw() +
                        theme(text = element_text(family = "Palatino")) +
                        scale_fill_manual(values = c("brown2", "olivedrab3")) 
                }
                
                else{
                    if(input$word_sentiment == "antidem2") {
                        ggplot(data = tweet_antidem_sentiment, 
                               mapping = aes(x = reorder(word, n),
                                             y = n, 
                                             fill = sentiment)) +
                            geom_col(show.legend = FALSE) +
                            facet_wrap(~sentiment, scale = "free_y") +
    labs(title = "Sentiment In Tweets Including the Words 'Anti-Democratic'", 
                           subtitle = "Tweets gathered Nov. 3 - Nov. 17, 2020",
                                 caption = "Source: Twitter", 
                                 x = " ",
                                 y = "Contribution to Sentiment") +
                            coord_flip() +
                            theme_bw() +
                            theme(text = element_text(family = "Palatino")) +
                        scale_fill_manual(values = c("brown2", "olivedrab3")) 
                    }
                    
                    else{ 
                        if(input$word_sentiment == "ethnic2") {
                            ggplot(data = tweet_ethnic_sentiment,
                                    mapping = aes(x = reorder(word, n), 
                                                  y = n, 
                                                  fill = sentiment)) +
                                geom_col(show.legend = FALSE) +
                                facet_wrap(~sentiment, scale = "free_y") +
                                labs(title = "Sentiment In Tweets Including 
                                the Words 'Muslim, 'Islam',
       'Arab', and 'South Asian'", 
                          subtitle = "Tweets gathered Nov. 3 - Nov. 17, 2020",
                                     caption = "Source: Twitter", 
                                     x = " ",
                                     y = "Contribution to Sentiment") +
                                coord_flip() +
                                theme_bw() +
                              theme(text = element_text(family = "Palatino")) +
                         scale_fill_manual(values = c("brown2", "olivedrab3"))
                        }
                    }
                }
            }
            
            
            
        }
        
    })
    
    output$aggregate_sentiment_range <- renderPlot({
        
        ggplot(data = aggregate_numerical, mapping = aes(x = value)) +
            geom_histogram(bins = 30, fill = "lightskyblue") +
            ylim(0, 500) +
            labs(title = "Numerical Scale of Sentiment in Tweets Including
                 the Words 'Muslim', 'Islam', 'Arab, 'South Asian',
                 and 'Anti-Democratic'", 
                 subtitle = "Tweets gathered Nov. 3 - Nov. 17, 2020",
                 caption = "Source: Twitter", 
                 x = "Score",
                 y = "Count") +
            theme_bw() +
            theme(text = element_text(family = "Palatino"))
        
        
    })
    
    output$sentiment_range <- renderPlot({
        if(input$sentiment_range == "muslim3") {
            ggplot(data = muslim_numerical, 
                   mapping = aes(x = value)) +
                geom_histogram(bins = 30, fill = "lightskyblue") +
                ylim(0, 500) +
                labs(title = "Numerical Scale of Sentiment in Tweets
                     Including the Word 'Muslim'", 
                     subtitle = "Tweets gathered Nov. 3 - Nov. 17, 2020",
                     caption = "Source: Twitter", 
                     x = "Score",
                     y = "Count") +
                theme_bw() +
                theme(text = element_text(family = "Palatino"))}
        
        else{
            if(input$sentiment_range == "islam3") {
                ggplot(data = islam_numerical, 
                       mapping = aes(x = value)) +
                    geom_histogram(bins = 30, fill = "lightskyblue") +
                    ylim(0, 500) +
                    labs(title = "Numerical Scale of Sentiment in Tweets
                         Including the Word 'Islam'", 
                         subtitle = "Tweets gathered Nov. 3 - Nov. 17, 2020",
                         caption = "Source: Twitter", 
                         x = "Score",
                         y = "Count") +
                    theme_bw() +
                    theme(text = element_text(family = "Palatino"))
            }
            
            else{
                if(input$sentiment_range == "arabsa3") {
                    ggplot(data = arabsa_numerical, mapping = aes(x = value)) +
                        geom_histogram(bins = 30, fill = "lightskyblue") +
                        ylim(0, 500) +
                        labs(title = "Numerical Scale of Sentiment in Tweets
                             Including the Words 'Arab' and 'South Asian'", 
                          subtitle = "Tweets gathered Nov. 3 - Nov. 17, 2020",
                             caption = "Source: Twitter", 
                             x = "Score",
                             y = "Count") +
                        theme_bw() +
                        theme(text = element_text(family = "Palatino")) 
                }
                
                else{
                    if(input$sentiment_range == "antidem3") {
                        ggplot(data = antidem_numerical, 
                               mapping = aes(x = value)) +
                            geom_histogram(bins = 30, fill = "lightskyblue") + 
                            ylim(0, 500) +
                            labs(title = "Numerical Scale of Sentiment in 
                                 Tweets Including the Word 'Anti-Democratic'", 
                                 subtitle = "Tweets gathered 
                                 Nov. 3 - Nov. 17, 2020",
                                 caption = "Source: Twitter", 
                                 x = "Score",
                                 y = "Count") +
                            theme_bw() +
                            theme(text = element_text(family = "Palatino")) 
                    }
                    
                    else{ 
                        if(input$sentiment_range == "ethnic3") {
                            ggplot(data = ethnic_numerical, mapping = aes(x = value)) +
                                geom_histogram(bins = 30, fill = "lightskyblue") +
                                ylim(0, 500) +
                                labs(title = "Numerical Scale of Sentiment in
                                Tweets Including the Words 'Muslim', 'Islam', 
                                     'Arab', and 'South Asian'", 
                           subtitle = "Tweets gathered Nov. 3 - Nov. 17, 2020",
                                     caption = "Source: Twitter", 
                                     x = "Score",
                                     y = "Count") +
                                theme_bw() +
                                theme(text = element_text(family = "Palatino")) 
                        }
                    }
                }
            }
            
            
            
        }
        
    })
    output$results_tibble <- renderTable (ci)
    
    output$cinewshate <- render_gt(cinewshate) 
}


# Run the application
shinyApp(ui = ui, server = server)

