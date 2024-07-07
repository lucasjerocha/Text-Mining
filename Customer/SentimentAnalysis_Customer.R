### Sentiment analysis ###

library(tidyverse)
library(tidytext)
library(textdata)
library(gridExtra)
library(stopwords)
library(textstem) 

review <- read.csv("customer_review_practice.csv", sep = ",") %>%
  tibble() 
glimpse(review)


review$label <- as.factor(review$label)

review_tok1 <- review %>%
  unnest_tokens(output = word, input = review) %>%
  mutate(word = str_remove_all(word, "[^[:alpha:]]")) %>%
  filter(!word %in% stopwords("en")) %>%
  mutate(word = lemmatize_words(word)) %>%
  filter(!word == "")
review_tok1

### Sentiment lexicons ###

# Checking words of positive sentiments in the comments

review_tok1 %>%
  inner_join(lexicon_bing()) %>% # Through the matching process between the lexicon and the opinion, we merge the remaining columns from each dataset.
  filter(sentiment == "positive") %>%
  count(word, sort = TRUE)

# Checking words of negative sentiments in the comments
  
review_tok1 %>%
  inner_join(lexicon_bing()) %>% # The inner join is always applied to the dataframe created in the previous step
  filter(sentiment == "negative") %>%
  count(word, sort = TRUE)

# The most common words with the sentiment "anger" in the positive comments

nrc_anger <- lexicon_nrc() %>%
  filter(sentiment == "anger")
nrc_anger

review_tok1 %>%
  filter(label == "positive") %>%
  inner_join(nrc_anger) %>%
  count(word, sort = TRUE)

# Number of words with the sentiment "anger" in the positive comments

review_tok1 %>%
  filter(label == "positive") %>%
  inner_join(nrc_anger) %>%
  count(sentiment, sort = TRUE)

# The most common words with the sentiment "anger" in negative comments

review_tok1 %>%
  filter(label == "negative") %>%
  inner_join(nrc_anger) %>%
  count(word, sort = TRUE)

# Number of words with the sentiment "anger" in negative comments

review_tok1 %>%
  filter(label == "negative") %>%
  inner_join(nrc_anger) %>%
  count(sentiment, sort = TRUE)

#Checking the number of words with specific emotions is an effective way to differentiate comments

### Analyzing Employee Reviews ###

### --- Bing dictionary

review_tok1_sentiment_bing <- review_tok1 %>%
  inner_join(get_sentiments("bing")) %>%  
  count(ID, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
  mutate(sentiment = positive - negative) %>%
  inner_join(data.frame(ID = review$ID, label = review$label)) 
review_tok1_sentiment_bing

ggplot(review_tok1_sentiment_bing, aes(ID, sentiment, fill = label)) + 
  geom_col(show.legend = TRUE)

# --- Accuracy 

review_tok1_sentiment_accuracy <- review_tok1 %>%
  inner_join(get_sentiments("bing")) %>%
  count(ID, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  mutate(sentiment = positive - negative, predicted = ifelse(sentiment >= 0, "positive", "negative")) %>%
  inner_join(data_frame(ID = review$ID, label = review$label)) 
review_tok1_sentiment_accuracy

mean(review_tok1_sentiment_accuracy$predicted == review_tok1_sentiment_accuracy$label) 


### --- Afinn dictionary

review_tok1_sentiment_afinn <- review_tok1 %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(ID) %>%
  summarise(sentiment = sum(value)) %>% 
  inner_join(data.frame(ID = review$ID, label = review$label))
review_tok1_sentiment_afinn

ggplot(review_tok1_sentiment_afinn, aes(ID, sentiment, fill = label)) +
  geom_col(show.legend = TRUE)

# Accuracy 
review_tok1_sentiment_afinn_accuracy <- review_tok1 %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(ID) %>%
  summarise(sentiment = sum(value), 
            predicted = ifelse(sentiment >= 0, "positive", "negative")) %>%
  inner_join(data.frame(ID = review$ID, label = review$label))
review_tok1_sentiment_afinn_accuracy

mean(review_tok1_sentiment_afinn_accuracy$predicted == review_tok1_sentiment_afinn_accuracy$label)

### --- sentimentr


library(sentimentr)

# accuracy
review_sentimentr <- review %>%
  mutate(score = sentiment_by(review)$ave_sentiment,
         predicted = ifelse(score >= 0, "positive", "negative"))
opinions_sentimentr

mean(review_sentimentr$label == review_sentimentr$predicted)

ggplot(review_sentimentr, aes(ID, score, fill = label)) +
  geom_col(show.legend = TRUE)
