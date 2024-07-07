library(topicmodels)
library(tidyverse)
library(tidyverse)
library(stopwords)
library(textstem)
library(tidytext)


review <- read.csv("customer_review_practice.csv", sep = ",") %>%
  tibble() 
review
review$label <- as.factor(review$label)
glimpse(review)

review_tok1 <- review %>%
  unnest_tokens(word, review) %>%
  mutate(word = str_remove_all(word, "[^[:alpha:]]")) %>%
  filter(!word %in% stopwords("en")) %>%
  mutate(word = lemmatize_words(word)) %>%
  filter(!word == "")
review_tok1

# Create Document-Term Matrix

review_dtm <- review_tok1 %>% 
  count(ID, word) %>%
  cast_dtm(ID, word, n) # (document, term, value)
review_dtm

# Create LDA model 

review_lda <- LDA(review_dtm, k = 2, control = list(seed = 123)) # k = 2 specifies that the model identifies 2 topics  
review_lda

# Word-topic probabilities: beta 

review_topics <- tidy(review_lda, matrix = "beta") # Extracts the word-topic probabilities (beta) from LDA model
review_topics %>% print(n=30)

# Check top terms in each topic 

top_terms <- review_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 15) %>% # Top 15 terms with the highest probabilities in each topic, based on the beta values
  ungroup() %>%
  arrange(topic, -beta) # From the highest to the smallest 
top_terms

top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>% # (x, by, within)
  ggplot(aes(beta, term, fill = factor(topic))) + # Fill needs to be specified by a categorical variable.
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

# Checking for terms with greatest difference

beta_wide <- review_topics %>%
  mutate(topic = paste0("topic_", topic)) %>%
  pivot_wider(names_from = "topic", values_from = beta) %>%
  filter(topic_1 > 0.003 | topic_2 > 0.003) %>% # Significant presence 
  mutate(log_ratio = log2(topic_2 / topic_1)) # Compare quantities using logarithmic scales
beta_wide
beta_wide %>% slice_max(log_ratio, n = 20) # Indicates terms uniquely associated with topic 2
beta_wide %>% slice_min(log_ratio, n = 20) # Indicates terms uniquely associated with topic 1

# document-topic probabilities: Gamma

review_gamma <- tidy(review_lda, matrix = "gamma")
review_gamma
# Performance not good (without dominant topics) 

# Combining comments into Positive/Negative documents

review_two <- review %>%
  group_by(label) %>%
  summarise(review = paste(review, collapse = " ")) # Concatenate the values of the 'comment' variable
review_two

review_two_ok <- review_two %>% 
  unnest_tokens(word, review) %>%
  mutate(word = str_remove_all(word, "[^[:alpha:]]")) %>%
  filter(!word %in% stopwords("en")) %>%
  mutate(word = lemmatize_words(word)) %>%
  filter(!word == "")
review_two_ok

review_dtm2 <- review_two_ok %>%
  count(label, word, sort = TRUE) %>%
  cast_dtm(label, word, n) # (document, term, value)
review_dtm2

review_lda2 <- LDA(review_dtm2, k = 3, control = list(seed = 123))
review_lda2

### Beta 

review_topics2 <- tidy(review_lda2,matrix = "beta")
review_topics2

top_terms2 <- review_topics2 %>%
  group_by(topic) %>%
  slice_max(beta, n = 15) %>%
  ungroup() %>%
  arrange(topic, -beta)
top_terms2

top_terms2 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()
# Maybe topic 2 and 3 are more positive while 1 is more negative (it has more positive connotations compared to those in topics 1 and 2)

### Gamma 

tidy(review_lda2, matrix = "gamma")

### Terms in Topic modeling can also be ngrams
# Bigrams

review_bigram_ok <- review_two %>%
  unnest_tokens(bigram, review, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stopwords("en"), !word2 %in% stopwords("en")) %>%
  filter(!str_detect(word1,"[^[:alpha:]]"), !str_detect(word2, "[^[:alpha:]]")) %>%
  unite(bigram, word1, word2, sep = " ")
review_bigram_ok

review_bigram_dtm <- review_bigram_ok %>%
  count(label, bigram, sort = TRUE) %>%
  cast_dtm(label, bigram, n)

review_lda_bigram <- LDA(review_bigram_dtm, k = 3, control = list(seed = 123))
review_lda_bigram

# Beta
review_topics_bigram <- tidy(review_lda_bigram, matrix = "beta")
review_topics_bigram

top_terms_bigrams <- review_topics_bigram %>%
  group_by(topic) %>%
  slice_max(beta, n = 15) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_bigrams %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

# Gamma

tidy(review_lda_bigram, matrix = "gamma")
# The performance is not very good because this is a relatively small dataset
# Using bigrams will yield insufficient repetitions for representation (small dataset)
