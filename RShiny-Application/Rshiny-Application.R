## Interactive Maps depending on the number of reviews using leaflet for Las Vegas.
library(leaflet)
library(leaflet.extras)
library(dplyr)
library(htmltools)
library(shiny)
library(lexicon)
library(modelr)
library(tidyverse)
library(tidytext)
library(janeaustenr)
library(stringr)
library(tidyr)
library(readr)
library(wordcloud)
library(ggplot2)
library(RMySQL)
library(data.table)
library(udpipe)
library(topicmodels)
library(slam) #loaded for setting seed topics for LDA model.
library(textclean)
library(cld3)
library(tm)
library(textclean)

#Conneting to Database
con <- dbConnect(RMySQL::MySQL(),host="localhost",user="root",password="data123",dbname="yelp")

#Reading the business file
business<-readRDS(file='/Users/Varun/business.rds')
business %>% view()

#Filtering the business which contains only restaurants and city as Las Vegas
restaurants <- business %>% 
  filter(str_detect(categories,"Restaurant"))

restaurants_LasVegas <- restaurants %>% 
  filter(city=="Las Vegas")

#Calculating top 10 positive and negative reviews
#Ideally we filter the language first which contains only English

#reviews_LasVegas$lang <- detect_language(reviews_LasVegas$text) 
#reviews_LasVegas <- reviews_LasVegas %>% filter(lang=="en")
#reviews_LasVegas <- reviews_LasVegas %>% select(-lang)

#Reading the merged Business and Reviews which is filtered with Las Vegas city

#saveRDS(reviews_LasVegas,file="/Users/Varun/reviewsLasVegas.rds")
reviews_LasVegas<-readRDS(file='/Users/Varun/reviewsLasVegas.rds')

#Replacing the contractions words such as haven't to have not, couldn't to could not
#reviews_LasVegas$text <- replace_contraction(reviews_LasVegas$text, contraction.key = lexicon::key_contractions,ignore.case = TRUE)
    
#saveRDS(reviews_LasVegas,file="/Users/Varun/reviewsLasVegasCont.rds")

#reviews_LasVegas$wordcount <- sapply(reviews_LasVegas$text, function(i){
# str_count(i,'\\w+')
#})

#Removing the stop words from the reviews text
#hash_sentiment_senticnet is a lexicon
hash_sentiment_senticnet_cust <- hash_sentiment_senticnet %>% 
  mutate(y = y*5)

common_words <- hash_sentiment_senticnet_cust %>% 
  inner_join(stop_words, by=c("x"="word"))

stop_words_non_neg <-  stop_words %>%  
  filter(!word %in% c("no","not","never","without")) %>%  
  filter(!word %in% common_words$word)


#rest_20rev <- reviews_LasVegas %>% count(business_id) %>% filter(n<20)

#reviews_LasVegas_nostop<-reviews_LasVegas %>%
  #anti_join(stop_words_non_neg) 

#reviews_LasVegas_nostop<- reviews_LasVegas_nostop %>%
  #anti_join(stop_words)

#reviews_LasVegas_nostop <- reviews_LasVegas  %>%  
 # mutate(text = removeWords(text,stop_words_non_neg$word))

#reviews_LasVegas_nostop <- reviews_LasVegas_nostop %>% anti_join(rest_20rev)

#saveRDS(reviews_LasVegas_nostop,file="/Users/Varun/reviews_LasVegas_nostop.rds")
reviews_LasVegas_nostop <- readRDS(file="/Users/Varun/reviews_LasVegas_nostop.rds")

#Filtering positive and negative based on stars
reviews_pos <- reviews_LasVegas_nostop %>% filter(stars==4|stars==5)
reviews_neg <- reviews_LasVegas_nostop %>% filter(stars==1|stars==2)

#Getting the average score of the reviews. Here AFINN lexicon is used
reviews_pos_avgscore<- reviews_pos %>% inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(review_id) %>%
  summarize(avg_score = mean(score),
            words = n()) %>%
  ungroup() 

reviews_neg_avgscore <- reviews_neg %>% inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(review_id) %>%
  summarize(avg_score = mean(score),
            words = n()) %>%
  ungroup() 

#Joining the reviews score with the reviews data 
reviews_pos_table <- reviews_pos_avgscore %>% left_join(reviews_LasVegas) 
reviews_neg_table <- reviews_neg_avgscore %>% left_join(reviews_LasVegas) 

reviews <- reviews_LasVegas

reviews_pos_table %>% group_by(business_id) %>% arrange(desc(avg_score)) %>% top_n(10, avg_score) 

reviews_neg_table %>% group_by(business_id) %>% arrange(avg_score) %>% top_n(10, avg_score) 



#Interactive maps using Rshiny
las_vegas_business <- restaurants_LasVegas %>%
  rename("Long" = longitude, "Lat" = latitude)

business_data <- business %>%
  filter(str_detect(categories, "Restaurant")) %>%
  collect()

city_data <- business_data %>%
  count(city)

cities <- city_data$city


#User Interface code
ui <- fluidPage(
  
  # Sidebar layout with a input and output definitions 
  navbarPage("Yelp Data Analysis",
             id = "yelp",
             tabPanel('Map',
                      leafletOutput("leaflet", height=750),
                      
                      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                    draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                    width = 300, height = "auto",
                        style="padding-left:15px;padding-right:15px;",
                                    
                                    h2("Filters"),
                                    
                                    sliderInput(inputId = "rating",
                                                label = "Star Rating",
                                                min = 1,
                                                max = 5,
                                                value = range(4:5),
                                                step = 0.5),
                                    style= "opacity:0.80;z-index:10"
                      )
             ),
             tabPanel('Review Summary',
                      value = "summary",
                      htmlOutput("restaurant_name"),
                      htmlOutput("pos_review"),
                      dataTableOutput('summary_positive'),
                      htmlOutput("neg_review"),
                      dataTableOutput('summary_negative')
             ),
             tabPanel('Word Cloud',
                      plotOutput("word_cloud", width = "100%", height = 500)
             )
  )
)


#Server Code
server <- function(input, output, session) {
  
  reviews_positive <- tibble()
  reviews_negative <- tibble(x = c('a', 'b'))
  
  filteredData <- reactive({
    las_vegas_business %>%
      filter(stars %in% seq(input$rating[[1]], input$rating[[2]], by=0.5))
  })
  
  filterCityData <- reactive({
    rest_data <- city_data %>%
      filter(city == input$city)
    rest_data$name
  })
  
  output$rest_data <- renderUI({
    selectInput(inputId="restaurant",
                label = "Restaurant Selection",
                choices=filterCityData(),
                selected="Select Restaurant")
  })
  
  # Create leaflet object the plotOutput function is expecting
  output$leaflet <- renderLeaflet({
    leaflet(data = filteredData()) %>%
      addTiles(group="OSM") %>%
      mapOptions(zoomToLimits = "always") %>%
      addMarkers(
        #data = one_star_business,
        #color = ~pal(stars_b),
        label=~htmlEscape(name),
        clusterOptions = markerClusterOptions()
      ) %>%
      addResetMapButton()
  })
  
  observeEvent(input$leaflet_marker_click , { 
    p <- input$leaflet_marker_click
    print(p)
    print(p$lat)
    selected_business_data <- business_data %>%
      filter(latitude == p$lat, longitude == p$lng) 
    
    business_review_data <- reviews %>%
      filter(business_id %in% selected_business_data$business_id)
    
    print(selected_business_data$name)
    ## Merge this code with the positive negative review data.
    reviews_negative <- data.frame(row = 1:2, data = c('a', 'b'))
    output$restaurant_name <- renderText({
      paste0("<b> Restaurant: ", selected_business_data$name, "<br> <br> </b>")
      })
    output$pos_review <- renderText({
      "<b>Top positive reviews<br><br></b>"
      })
    output$neg_review <- renderText({
      "<b>Top negative reviews<br><br></b>"
      })
    top_pos_reviews <- reviews_pos_table %>% 
      filter(business_id %in% selected_business_data$business_id) %>%
      arrange(desc(avg_score)) %>% 
      top_n(10, avg_score) %>%
      select(text)
    
    top_neg_reviews <- reviews_neg_table %>% 
      filter(business_id %in% selected_business_data$business_id) %>%
      arrange(avg_score) %>% 
      top_n(10, avg_score) %>%
      select(text)
    unigram_reviews <- reviews %>%
      filter(business_id %in% selected_business_data$business_id) %>%
      unnest_tokens(word, text) %>%
      anti_join(stop_words) %>%
      count(word)
    
    wordcloud_rep <- repeatable(wordcloud)
    
    output$word_cloud <- renderPlot({
      
      wordcloud_rep(unigram_reviews$word, unigram_reviews$n,
                    scale=c(4,0.5),
                    min.freq = 0, max.words=200,
                    colors=brewer.pal(8, "Dark2"))
    })
    
    output$summary_positive <- renderDataTable(top_pos_reviews)
    output$summary_negative <- renderDataTable(top_neg_reviews)
    updateNavbarPage(session, "yelp", selected = "summary")
  })
}

#Call Function
shinyApp(ui = ui, server = server)
  



