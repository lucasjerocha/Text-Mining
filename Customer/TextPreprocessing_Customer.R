### Text Mining 1 ###
### Text Preprocessing and Exploratory Analysis ###

library(tidyverse)
library(tidytext)
library(stopwords)
#Loading the data

review <- read.csv("customer_review_practice.csv", sep = ",") %>% as_tibble() #tibble format for better the readability 
review

### Remember to convert character variables to factors for categorical analysis, except those required for tokenization ###
review$label <- as_factor(review$label)
glimpse(review) #Display the structure of the tibble
str(review) #Display more detailed structure
review


###Tokenization###

tok_1 <- review %>%
  unnest_tokens(output = word, input = review) #Tokenize into unigrams
tok_1


### Punctuation and numbers ### 
tok_1clean <- tok_1 %>%  
  mutate(word = str_remove_all(word, "[^[:alpha:]]")) %>% # Removes non-alphabetic characters, including spaces
  filter(!word == "") # Remove NA values 
tok_1clean

### Stop word removal ###
# Unigram
remove_stopwords <- review %>%
  unnest_tokens(output = word, input = review) %>%
  mutate(word = str_remove_all(word, "[^[:alpha:]]")) %>%
  filter(!word %in% stopwords("en")) %>%
  filter(!word == "")
remove_stopwords
glimpse(remove_stopwords)

### Stemming/ Lemmatization ###

library(textstem)

review %>% unnest_tokens(output = word, input = review) %>%
  mutate(word = str_remove_all(word, "[^[:alpha:]]")) %>%
  filter(!word %in% stopwords("en")) %>%
  mutate(stem = stem_words(word), lemme = lemmatize_words(word)) %>% # Lemmatization is more accurate as it generates results based on dictionary entries.
  filter(!word == "") 
  
review %>% unnest_tokens(output = word, input = review) %>%
  mutate(word = str_remove_all(word, "[^[:alpha:]]")) %>%
  filter(!word %in% stopwords("en")) %>%
  mutate(word = lemmatize_words(word)) %>%
  filter(!word == "") 

### Rare word removal ###
# We'll eliminate words that appear in less than 0.1% of the total number of rows (nrow * 0.1%).
n_rows <- review %>%
  unnest_tokens(output = word, input = review) %>%
  mutate(word = str_remove_all(word, "[^[:alpha:]]")) %>%
  filter(!word %in% stopwords("en")) %>%
  filter(!word=="") %>% 
  mutate(word = lemmatize_words(word)) %>%
  nrow()

# All together
review_tok1 <- review %>%
  unnest_tokens(word, review) %>%
  mutate(word = str_remove_all(word, "[^[:alpha:]]")) %>%
  filter(!word %in% stopwords("en")) %>%
  filter(!word == "") %>%
  mutate(word = lemmatize_words(word)) %>%
  group_by(word) %>%
  filter(n() > (n_rows*(1/1000))) %>% # (1/1000 = 0.1%)
  ungroup(word)
review_tok1

### Descriptive analysis ###
review_tok1

word_counts <- review_tok1 %>%
  select(word) %>%
  count(word, sort = TRUE) %>%
  filter(n>2) # Frequency of words occurring at least 3 times
word_counts  

word_counts2 <- review_tok1 %>%
  select(word) %>%
  count(word, sort = TRUE) 
word_counts2

# Visualizing word frequency with wordcloud
library(wordcloud)
wordcloud(word_counts$word, word_counts$n, max.words = 30) # Graph

wordcloud(word_counts2$word, word_counts2$n, min.freq = 3, max.words = 30) # Achieves the same effect as before, but without the need to filter for word counts greater than 2. Instead, we perform the filtering within the wordcloud function.

library(wordcloud2)
wordcloud2(word_counts, size = 0.5, color = "random-dark", shape = "triangle") # Graph


# Comparing word occurrence between negative and positive assessments
word_pos <- review_tok1 %>%
  filter(label == "positive") %>%
  count(word, sort = TRUE) 
word_pos

word_cons <- review_tok1 %>%
  filter(label == "negative") %>%
  count(word, sort = TRUE)
word_cons

library(gridExtra)

plot_pos <- word_pos %>%
  mutate(word = reorder(word,n)) %>% # The words were already ordered by sequence, but this operation also converts character vectors into factors.
  head(20) %>% # Selecting the first 20 rows
  ggplot(aes(n, word)) +  # Horizontal plot
  geom_col(fill= "steelblue", alpha = 0.7, show.legend = FALSE) + # Alpha => transparency for bars; Fill determines the color of the bars 
  labs(y = NULL, title = "Positive comments") 
plot_pos

plot_neg <-  word_cons %>%
  mutate(reorder(word,n)) %>%
  head(20) %>%
  ggplot(aes(n,word)) +
  geom_col(fill="tomato", alpha = 0.7, show.legend = FALSE) +
  labs(y=NULL, title = "Negative comments")
plot_neg
  
grid.arrange(plot_pos, plot_neg, ncol=2) ### The two plots together

library(dplyr)
library(tidyr)

# Combining positive and negative assessments into a single table

count_total1 <- bind_rows( 
  mutate(word_pos, list = "positive"),
  mutate(word_cons, list = "negative")) %>%
  pivot_wider(names_from = list, values_from =  n) # Separating the list into two columns: positive and negative
count_total1

count_total1 %>% arrange(desc(positive)) 
count_total1 %>% arrange(desc(negative))

# Another option is to use spread(var, values), which accepts the same inputs as pivot_wider. Unlike pivot_wider, spread preserves the original sequence of the data
count_total2 <- bind_rows(
  mutate(word_pos, list = "positive"),
  mutate(word_cons, list = "negative")) %>%
  spread(list, n)
  # na.omit() if we need to omit NA values
count_total2

count_total2  %>% arrange(desc(positive)) 
count_total2  %>% arrange(desc(negative))


### Term frequency - how frequently a word occurs in a document 

### Inverse document frequency (idf), which decreases the weight for commonly used words and increases the weight for words that are not used very much in a collection of documents. 

### The statistic tf-idf is intended to measure how important a word is to a document in a collection (or corpus) of documents.

# Visualizing in proportion (term frequency, tf)
frequency <- bind_rows(
  mutate(word_pos, list = "positive"),
  mutate(word_cons, list = "negative")) %>%
  group_by(list) %>% # We need to calculate the proportion n/sum(n) with respect to the total count of each factor.
  mutate(tf = n/sum(n)) %>%
  select(-n) %>% # Remove the n column 
  spread(list, tf)
frequency

frequency %>% arrange(desc(positive)) # "positive" and "negative" are column names, not the values. Be careful not to confuse them.
frequency %>% arrange(desc(negative))

plot_pos <- word_pos %>%
  mutate(list = "positive") %>%
  mutate(tf = n/sum(n)) %>%
  select (-n) %>%
  mutate(word = reorder(word, tf)) %>% # When creating a plot, it's essential to use reorder instead of arrange. Reorder directly affects the factor levels in the plot, while arrange does not.
  head(20) %>% 
  ggplot(aes(tf, word)) +
  geom_col(fill = "steelblue", alpha = 0.7, show.legend = FALSE) +
  labs(y=NULL, title = "Term frequency - Positive Opinions") +
  theme_classic()
plot_pos
    
plot_neg <- word_cons %>%
  mutate(list = "negative") %>%
  mutate(tf = n/sum(n)) %>%
  mutate(word = reorder(word, tf)) %>%
  select(-n) %>%
  head(20) %>%
  ggplot(aes(tf, word)) +
  geom_col(fill = "tomato", alpha = 0.7, show.legend = FALSE) +
  labs(y = NULL, title = "Term frequency - Negative opinions") +
  theme_classic()
plot_neg
    
grid.arrange(plot_pos, plot_neg, ncol = 2)

# Visualizing term frequency-inverse document frequency (tf-idf)
count_total <- review_tok1 %>%
  count(label, word, sort = TRUE) %>% ## We will count the number of combinations of these two variables
  bind_tf_idf(word, label, n) ## (term, document, n) # We will create two distinct documents: one for positive opinions and another for negative opinions
count_total # idf=0 suggests that the term is either too common or too rare across the corpus.

plot_pos <- count_total %>%
  filter(label == "positive" & tf_idf > 0) %>%
  mutate(word = reorder(word, tf_idf)) %>%
  select(-n) %>%
  head(20) %>%
  ggplot(aes(tf_idf, word)) +
  geom_col(fill = "steelblue", alpha = 0.7, show.legend = FALSE) +
  labs(y = NULL, title = "tf_idf - Positive") +
  theme_classic()
plot_pos

plot_neg <- count_total %>%
  filter(label=="negative" & tf_idf > 0) %>%
  mutate(word=reorder(word, tf_idf)) %>%
  select(-n) %>%
  head(20) %>%
  ggplot(aes(tf_idf, word)) +
  geom_col(fill = "tomato", alpha=0.7, show.legend = FALSE) +
  labs(y=NULL, title = "tf_idf - Negative") +
  theme_classic()
plot_neg

grid.arrange(plot_pos, plot_neg, ncol = 2)
