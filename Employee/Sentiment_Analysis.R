### Sentiment analysis ###

library(tidyverse)
library(tidytext)
library(textdata)
library(gridExtra)
library(stopwords)
library(textstem) # lemmatize_words 

opinions <- read.csv("employee_opinions.csv", sep = ";") %>%
  tibble() %>%
  select(-topic) 
glimpse(opinions)


opinions$assessment <- as.factor(opinions$assessment)

opinions_tok1 <- opinions %>%
  unnest_tokens(output = word, input = comment) %>%
  mutate(word = str_remove_all(word, "[^[:alpha:]]")) %>%
  filter(!word %in% stopwords("en")) %>%
  mutate(word = lemmatize_words(word)) %>%
  filter(!word == "")
opinions_tok1

### Sentiment lexicons ###

# Lexicon nrc 
# (negative, positive, anger, anticipation, disgust, fear, joy, sadness, surprise, trust)

lexicon_nrc() 
lexicon_nrc() %>% filter(word == "happiness")
lexicon_nrc() %>% filter(word == "sadness")

# Lexicon afinn
# (form -5 to 5)

lexicon_afinn()
lexicon_afinn() %>% filter(word == "happiness")
lexicon_afinn() %>% filter(word == "sad")

lexicon_afinn() %>% arrange(desc(value))
lexicon_afinn() %>% arrange(value)

# Lexicon bing
# (Positive or negative)

lexicon_bing()
lexicon_bing() %>% filter(word == "happiness")
lexicon_bing() %>% filter(word == "sadness")

# Checking words of positive sentiments in the comments

opinions_tok1 %>%
  inner_join(lexicon_bing()) %>% # Through the matching process between the lexicon and the opinion, we merge the remaining columns from each dataset.
  filter(sentiment == "positive") %>%
  count(word, sort = TRUE)

# Checking words of negative sentiments in the comments
  
opinions_tok1 %>%
  inner_join(lexicon_bing()) %>% # The inner join is always applied to the dataframe created in the previous step
  filter(sentiment == "negative") %>%
  count(word, sort = TRUE)

# The most common words with the sentiment "anger" in the positive comments

nrc_anger <- lexicon_nrc() %>%
  filter(sentiment == "anger")
nrc_anger

opinions_tok1 %>%
  filter(assessment == "positive") %>%
  inner_join(nrc_anger) %>%
  count(word, sort = TRUE)

# Number of words with the sentiment "anger" in the positive comments

opinions_tok1 %>%
  filter(assessment == "positive") %>%
  inner_join(nrc_anger) %>%
  count(sentiment, sort = TRUE)

# The most common words with the sentiment "anger" in negative comments

opinions_tok1 %>%
  filter(assessment == "negative") %>%
  inner_join(nrc_anger) %>%
  count(word, sort = TRUE)

# Number of words with the sentiment "anger" in negative comments

opinions_tok1 %>%
  filter(assessment == "negative") %>%
  inner_join(nrc_anger) %>%
  count(sentiment, sort = TRUE)

#Checking the number of words with specific emotions is an effective way to differentiate comments

### Analyzing Employee Reviews ###

### --- Bing dictionary

opinion_tok1_sentiment_bing <- opinions_tok1 %>%
  inner_join(get_sentiments("bing")) %>%  # It produces a different plot compared to using the lexicon_bing
  count(commentID, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
  mutate(sentiment = positive - negative) %>% # Directly calculate the sentiment score of one comment
  inner_join(data.frame(commentID = opinions$commentID, assessment = opinions$assessment)) # Data frame with two variables, always joining with the initial data before preprocessing (tokenizing)
opinion_tok1_sentiment_bing

ggplot(opinion_tok1_sentiment_bing, aes(commentID, sentiment, fill = assessment)) + # Fill is preferable to color in terms of graph aesthetics
  geom_col(show.legend = TRUE)

# --- Accuracy (We'll compare the sentiment predicted labels with the actual (assessment labels)

opinion_tok1_sentiment_accuracy <- opinions_tok1 %>%
  inner_join(get_sentiments("bing")) %>%
  count(commentID, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  mutate(sentiment = positive - negative, predicted = ifelse(sentiment >= 0, "positive", "negative")) %>%
  inner_join(data_frame(commentID = opinions$commentID, assessment = opinions$assessment)) 
opinion_tok1_sentiment_accuracy

mean(opinion_tok1_sentiment_accuracy$predicted == opinion_tok1_sentiment_accuracy$assessment) # Mean of a logical vector containing 1 (TRUE) or 0 (FALSE)


### --- Afinn dictionary

opinions_tok1_sentiment_afinn <- opinions_tok1 %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(commentID) %>%
  summarise(sentiment = sum(value)) %>% # Summarise provides summarized statistics (only one row per commentID)
  inner_join(data.frame(commentID = opinions$commentID, assessment = opinions$assessment))
opinions_tok1_sentiment_afinn

ggplot(opinions_tok1_sentiment_afinn, aes(commentID, sentiment, fill=assessment)) +
  geom_col(show.legend = TRUE)

# Accuracy 
opinions_tok1_sentiment_afinn_accuracy <- opinions_tok1 %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(commentID) %>%
  summarise(sentiment = sum(value), 
            predicted = ifelse(sentiment >= 0, "positive", "negative")) %>%
  inner_join(data.frame(commentID = opinions$commentID, assessment = opinions$assessment))
opinions_tok1_sentiment_afinn_accuracy

mean(opinions_tok1_sentiment_afinn_accuracy$predicted == opinions_tok1_sentiment_afinn_accuracy$assessment)

### --- sentimentr
# sentimentr package can calculate sentiment scores on different levels (document/sentence)
# It can also consider valence shifters like negators, amplifiers, and de-amplifiers 

library(sentimentr)

# Examples
sentiment("This is a beautiful day") # in multiple comments we use sentiment_by
sentiment("This is a very beautiful day") # amplifier
sentiment("This is not a beautiful day") # negator
sentiment("This is somewhat a beautiful day") # de-amplifier

# accuracy
opinions_sentimentr <- opinions %>%
  mutate(score = sentiment_by(comment)$ave_sentiment,
         predicted = ifelse(score >= 0, "positive", "negative"))
opinions_sentimentr

mean(opinions_sentimentr$assessment == opinions_sentimentr$predicted)

ggplot(opinions_sentimentr, aes(commentID, score, fill = assessment)) +
  geom_col(show.legend = TRUE)
