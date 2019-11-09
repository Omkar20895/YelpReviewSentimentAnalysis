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
library(modelr)
library(textclean)
library(tm)

## Interactive Maps depending on the number of reviews using leaflet for Las Vegas.

library(htmltools)
library(shiny)
library(leaflet)
library(leaflet.extras)



con <- dbConnect(RMySQL::MySQL(),host="localhost",user="root",password="data123",dbname="yelp")

#business <- tbl(con,"business")
#reviews <- tbl(con, "reviews")

#business <- rename(business, stars_b = stars)
#reviews <- rename(reviews, stars_r = stars)

#reviews_LasVegas <- business %>%
#  filter(str_detect(categories,"Restaurant"))%>%
#  filter(city=="Las Vegas")%>%
#  left_join(reviews,by="business_id") %>% collect()


#Find Top Positive and Negative reviews
#Get sentiments from the reviews - positive,negative,joy etc
#Reading the joined table Business and review and filtered with Las Vegas city

reviews_LasVegas<-readRDS(file='/Users/Varun/reviewsLasVegas.rds')

reviews_LasVegas %>% view()
review_DF<-data.frame(reviews_LasVegas) 
review_DF1 <- review_DF[1:10000,]
review_DF1 %>% view()

#Detecting the language and filtering it with only English
library(cld3)
review_DF1$lang <- detect_language(review_DF1$text) 
review_DF1 <- review_DF1 %>% filter(lang=="en")
review_DF1 <- review_DF1 %>% select(-lang)

#Contraction words such as wasn't couldn't are replaced by was not and could not.
library(textclean)
review_DF1$text <- replace_contraction(review_DF1$text, contraction.key = lexicon::key_contractions,ignore.case = TRUE)

#Contains about 22000 words with score
hash_sentiment_senticnet_cust <- hash_sentiment_senticnet %>% 
  mutate(y = y*5)

common_words <- hash_sentiment_senticnet_cust %>% 
  inner_join(stop_words, by=c("x"="word"))

#CUSTOM STOP WORDS KEEPING NOT REMOVING THE NEGATION WORDS
stop_words_non_neg <-  stop_words %>%  
  filter(!word %in% c("no","not","never","without")) %>%  
  filter(!word %in% common_words$word)

library(tm)
#DISTINCT BUSINESS ID AND NUMBER IF REVIEWS THAT BUSINESS CONTAINS
rest_20rev <- review_DF1 %>% count(business_id) %>% filter(n<20)


#REMOVING THE STOP WORDS FROM THE TEXT
reviews_LasVegas_nostop <- review_DF1 %>%   
  mutate(text = removeNumbers(text)) %>%  
  mutate(text = removeWords(text,stop_words_non_neg$word))

reviews_LasVegas_nostop <- reviews_LasVegas_nostop %>% anti_join(rest_20rev)
reviews_LasVegas_nostop %>% view()

reviews_pos <- reviews_LasVegas_nostop %>% filter(stars==4|stars==5)
reviews_neg <- reviews_LasVegas_nostop %>% filter(stars==1|stars==2)

#reviews_pos %>% view()
#reviews_neg %>% view()

#TOKENIZATION
reviews_pos_bigrams <- reviews_pos %>%   
  unnest_tokens(bigram, text, token = "ngrams",n=2)

reviews_neg_bigrams <- reviews_neg %>%   
  unnest_tokens(bigram, text, token = "ngrams",n=2)

#To implement the context of of negations words
reviews_pos_bigrams_separated <- reviews_pos_bigrams %>%  
  separate(bigram, into=c("word1","word2"), sep=" ")

reviews_neg_bigrams_separated <- reviews_neg_bigrams %>%  
  separate(bigram, into=c("word1","word2"), sep=" ")

reviews_pos_avgscore <- reviews_pos_bigrams_separated %>% 
  inner_join(hash_sentiment_senticnet_cust,by=c("word2"="x")) %>%  
  mutate(y1 = ifelse(word1 %in% c("no","not","never","without"),-y,y)) %>%
  group_by(review_id) %>%   
  summarise(avg_score = mean(y1), words=n()) %>%  
  ungroup()

reviews_neg_avgscore <- reviews_neg_bigrams_separated %>% 
  inner_join(hash_sentiment_senticnet_cust,by=c("word2"="x")) %>%  
  mutate(y1 = ifelse(word1 %in% c("no","not","never","without"),-y,y)) %>%
  group_by(review_id) %>%   
  summarise(avg_score = mean(y1), words=n()) %>%  
  ungroup()

reviews_pos_table <- reviews_pos_avgscore %>% left_join(review_DF) 
reviews_neg_table <- reviews_neg_avgscore %>% left_join(review_DF) 

reviews_pos_table %>% 
  group_by(business_id) %>%
  arrange(desc(avg_score)) %>% 
  top_n(10, avg_score) %>% 
  select(review_id,avg_score,business_id,stars,text) %>% 
  view()

reviews_neg_table %>% 
  group_by(business_id) %>% 
  arrange(avg_score) %>% 
  top_n(10, avg_score) %>% 
  select(review_id,avg_score,business_id,stars,text) %>% 
  view()

##AFFINN

tidy_reviews<-readRDS(file='/Users/Varun/tidy_reviews.rds')


contributions <- tidy_reviews %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(word) %>%
  summarize(contribution = sum(score)) %>% 
  top_n(25, abs(contribution)) %>%
  mutate(word = reorder(word, contribution)) %>%
  ggplot(aes(word, contribution, fill = contribution > 0)) +
  ggtitle(' contributions to positive/negative sentiment in reviews')  +
  geom_col(show.legend = FALSE) +
  coord_flip()

sentiment_messages <- tidy_reviews %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(review_id) %>%
  summarize(sentiment_score = mean(score),
            words = n()) %>%
  ungroup() %>%
  filter(words >= 5)

sentiment_messages %>%
  arrange(sentiment_score) 

sentiment_messages <- sentiment_messages %>% left_join(review_DF) 

sentiment_messages_pos <- sentiment_messages %>% filter(stars==4 | stars==5)
sentiment_messages_neg <- sentiment_messages %>% filter(stars==1 | stars==2)

sentiment_messages_pos %>% 
  group_by(business_id) %>%
  arrange(desc(sentiment_score)) %>% 
  top_n(10, sentiment_score) %>% 
  select(review_id,sentiment_score,business_id,stars,text) %>% 
  view()

sentiment_messages_neg %>% 
  group_by(business_id) %>%
  arrange(desc(sentiment_score)) %>% 
  top_n(10, sentiment_score) %>% 
  select(review_id,sentiment_score,business_id,stars,text) %>% 
  view()



