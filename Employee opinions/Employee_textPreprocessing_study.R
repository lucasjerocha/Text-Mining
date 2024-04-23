### Text Mining 1 ###
### Text Preprocessing and Exploratory Analysis ###

library(tidyverse)
library(tidytext)
library(stopwords)
#Loading the data

opinions <- read.csv("employee_opinions.csv", sep = ";") %>% as_tibble() #So we can see better the dataframe
opinions

### Don't forget to always change the chr to factor (to categorize) (except the ones that we must do the tokenization) ###
opinions$assessment <- as_factor(opinions$assessment)
opinions$topic <- as_factor(opinions$topic)
glimpse(opinions) #to see the internal structure of the tibble
str(opinions) #even more details than the glimpse function
opinions

### Before analyzing the dataset we must do the preprocessing: ### 
### Tokenization OK ###
### Normalization (lowercase) OK ### (default inside the punctuation code)
### Punctuation and numbers OK ###
### Stop word removal OK ### 
### Stemming/Lemmatization OK ### 
### Rare word removal (optional) ###

###Tokenization###
tok_1 <- opinions %>%
  unnest_tokens(output = word, input = comment)
tok_1

tok_2 <- opinions %>% ### it gives more context to the words ###
  unnest_tokens(output = bigram, input = comment, token = "ngrams", n = 2, to_lower = TRUE) #to_lower = true is a default option, so by this way it is redundant
tok_2

tok_3 <- opinions %>%
  unnest_tokens(output = trigram, input = comment, token = "ngrams", n=3)
tok_3

tok_sentence <- opinions %>% ### Divides into sentences ###
  unnest_tokens(sentence, comment, token = "sentences")
tok_sentence

### Punctuation and numbers ### 
### mutate when working with 1 gram and filter when working with more ###
tok_1clean <- tok_1 %>%  
  mutate(clean = str_remove_all(word,"[^[:alpha:]]")) %>% ##it removes any non alphabetic, including spaces ###
  filter(!clean == "") ### to remove NA's
tok_1clean

tok_2clean <- tok_2 %>%
  separate(bigram, into = c("word1","word2")) %>%
  filter(!str_detect(word1, "[^[:alpha:]]"),!str_detect(word2, "[^[:alpha:]]")) %>% ### we must filter them together so we can still have a bigram after we unite the two columns ###
  unite(bigram,"word1","word2", sep = " ")
tok_2clean
  
tok_3clean <- tok_3 %>%
  separate(trigram, into = c("word1","word2","word3")) %>%
  filter(!str_detect(word1,"[^[:alpha:]]"), !str_detect(word2,"[^[:alpha:]]"), !str_detect(word3,"[^[:alpha:]]")) %>%
  unite(trigram, "word1", "word2", "word3", sep = " ")
tok_3clean

### Remove stopwords ###
### unigram ###
remove_stopwords <- opinions %>%
  unnest_tokens(word, comment) %>%
  mutate(word = str_remove_all(word, "[^[:alpha:]]")) %>%
  filter(!word %in% stopwords("en"))
remove_stopwords
glimpse(remove_stopwords)

### bigram ###
remove_stopwords2 <- opinions %>%
  unnest_tokens(bigram, comment, token = "ngrams", n = 2) %>%
  separate(bigram, into = c("word1", "word2")) %>%
  filter(!word1 %in% stopwords("en"), !word2 %in% stopwords("en")) %>% ### only accepts vetorial arguments so we need to separate them ### 
  unite(bigram, "word1", "word2" , sep = " ") 
remove_stopwords2

### trigram ###
remove_stopwords3 <- opinions %>%
  unnest_tokens(trigram, comment, token = "ngrams", n = 3) %>%
  separate(trigram, into = c("word1", "word2", "word3")) %>%
  filter(!word1 %in% stopwords("en"),!word2 %in% stopwords("en"), !word3 %in% stopwords("en")) %>%
  unite(trigram, "word1", "word2", "word3", sep = " ")
remove_stopwords3

### Stemming/ Lemmatization ###

library(textstem)

opinions %>% unnest_tokens(word, comment) %>%
  mutate(str_remove_all(word,"[^[:alpha:]]")) %>%
  filter(!word %in% stopwords("en")) %>%
  mutate(stem=stem_words(word),lemme=lemmatize_words(word)) #lemmatize is more accurate because it gives results based on the ditionary

opinions %>% unnest_tokens(word, comment) %>%
  mutate(word = str_remove_all(word, "[^[:alpha:]]")) %>%
  filter(!word %in% stopwords("en")) %>%
  mutate(word = lemmatize_words(word)) %>%
  filter(!word == "") ### empty words removed

### Rare word removal ### we will remove words that appear less than 0.1% of the total number (nrow*0.1%)
n_rows <- opinions %>%
  unnest_tokens(word, comment) %>%
  mutate(word = str_remove_all(word, "[^[:alpha:]]")) %>%
  filter(!word %in% stopwords("en")) %>%
  filter(!word=="") %>% 
  mutate(word = lemmatize_words(word)) %>%
  nrow()

#All together (note: the rare words removal will happen only in this step)
opinion_tok1 <- opinions %>%
  unnest_tokens(word, comment) %>%
  mutate(word = str_remove_all(word, "[^[:alpha:]]")) %>%
  filter(!word %in% stopwords("en")) %>%
  filter(!word == "") %>%
  mutate(word = lemmatize_words(word)) %>%
  group_by(word) %>%
  filter(n() > (n_rows*(1/1000))) %>% ### 1/1000 = %0.1
  ungroup(word)
opinion_tok1
