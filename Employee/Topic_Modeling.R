### Topic Modeling ###

library(topicmodels)
library(tidyverse)
library(tidyverse)
library(stopwords)
library(textstem)
library(tidytext)


opinions <- read.csv("employee_opinions.csv", sep = ";") %>%
  tibble() %>%
  select(-topic)
opinions
opinions$assessment <- as.factor(opinions$assessment)
glimpse(opinions)

opinion_tok1 <- opinions %>%
  unnest_tokens(word, comment) %>%
  mutate(word = str_remove_all(word, "[^[:alpha:]]")) %>%
  filter(!word %in% stopwords("en")) %>%
  mutate(word = lemmatize_words(word)) %>%
  filter(!word == "")
opinion_tok1

# Create Document-Term Matrix

opinions_dtm <- opinion_tok1 %>% 
  count(commentID, word) %>%
  cast_dtm(commentID, word, n) # (document, term, value)
opinions_dtm

# Create LDA model 

opinion_lda <- LDA(opinions_dtm, k = 2, control = list(seed = 123)) # k = 2 specifies that the model identifies 2 topics  
opinion_lda

# Word-topic probabilities: beta 

opinions_topics <- tidy(opinion_lda, matrix = "beta") # Extracts the word-topic probabilities (beta) from LDA model
opinions_topics %>% print(n=30)

# Check top terms in each topic 

top_terms <- opinions_topics %>%
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

beta_wide <- opinions_topics %>%
  mutate(topic = paste0("topic_", topic)) %>%
  pivot_wider(names_from = "topic", values_from = beta) %>%
  filter(topic_1 > 0.003 | topic_2 > 0.003) %>% # Significant presence 
  mutate(log_ratio = log2(topic_2 / topic_1)) # Compare quantities using logarithmic scales
beta_wide
beta_wide %>% slice_max(log_ratio, n = 20) # Indicates terms uniquely associated with topic 2
beta_wide %>% slice_min(log_ratio, n = 20) # Indicates terms uniquely associated with topic 1

# document-topic probabilities: Gamma

opinions_gamma <- tidy(opinion_lda, matrix = "gamma")
opinions_gamma
# Performance not good (without dominant topics) 

# Combining comments into Positive/Negative documents

opinions_two <- opinions %>%
  group_by(assessment) %>%
  summarise(comment = paste(comment, collapse = " ")) # Concatenate the values of the 'comment' variable
opinions_two

opinions_two_ok <- opinions_two %>% 
  unnest_tokens(word, comment) %>%
  mutate(word = str_remove_all(word, "[^[:alpha:]]")) %>%
  filter(!word %in% stopwords("en")) %>%
  mutate(word = lemmatize_words(word)) %>%
  filter(!word == "")
opinions_two_ok

opinion_dtm2 <- opinions_two_ok %>%
  count(assessment, word, sort = TRUE) %>%
  cast_dtm(assessment, word, n) # (document, term, value)
opinion_dtm2

opinion_lda2 <- LDA(opinion_dtm2, k = 3, control = list(seed = 123))
opinion_lda2

### Beta 

opinion_topics2 <- tidy(opinion_lda2,matrix = "beta")
opinion_topics2

top_terms2 <- opinion_topics2 %>%
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
# Maybe topic 2 is more positive while 1 and 3 are more negative (it has more positive connotations compared to those in topics 1 and 2)

### Gamma 

tidy(opinion_lda2, matrix = "gamma")

### Terms in Topic modeling can also be ngrams
# Bigrams

opinion_bigram_ok <- opinions_two %>%
  unnest_tokens(bigram, comment, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stopwords("en"), !word2 %in% stopwords("en")) %>%
  filter(!str_detect(word1,"[^[:alpha:]]"), !str_detect(word2, "[^[:alpha:]]")) %>%
  unite(bigram, word1, word2, sep = " ")
opinion_bigram_ok

opinion_bigram_dtm <- opinion_bigram_ok %>%
  count(assessment, bigram, sort = TRUE) %>%
  cast_dtm(assessment, bigram, n)

opinion_lda_bigram <- LDA(opinion_bigram_dtm, k = 3, control = list(seed = 123))
opinion_lda_bigram

# Beta
opinion_topics_bigram <- tidy(opinion_lda_bigram, matrix = "beta")
opinion_topics_bigram
  
top_terms_bigrams <- opinion_topics_bigram %>%
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

tidy(opinion_lda_bigram, matrix = "gamma")
# The performance is not very good because this is a relatively small dataset
# Using bigrams will yield insufficient repetitions for representation (small dataset)
