### Text Mining 1 ###
### Text Preprocessing and Exploratory Analysis ###

library(tidyverse)
library(tidytext)
library(stopwords)
#Loading the data

opinions <- read.csv("employee_opinions.csv", sep = ";") %>% as_tibble() #tibble format for better the readability 
opinions

### Remember to convert character variables to factors for categorical analysis, except those required for tokenization ###
opinions$assessment <- as_factor(opinions$assessment)
opinions$topic <- as_factor(opinions$topic)
glimpse(opinions) #Display the structure of the tibble
str(opinions) #Display more detailed structure
opinions

### Prior to dataset analysis, preprocessing steps are necessary: ### 
### - Tokenization: OK ###
### - Normalization (lowercase): OK ### (automatically performed within the Tokenization code)
### - Punctuation and numbers removal: OK ###
### - Stop word removal: OK ### 
### - Stemming/Lemmatization: OK ### - Lemmatization is preferred over stemming for accuracy, as it returns words to their dictionary root.
### - Optional: Rare word removal: OK ###

###Tokenization###
tok_1 <- opinions %>%
  unnest_tokens(output = word, input = comment) #Tokenize into unigrams
tok_1

tok_2 <- opinions %>% ### it gives more context to the words ###
  unnest_tokens(output = bigram, input = comment, token = "ngrams", n = 2) #Tokenize into bigrams
tok_2

tok_3 <- opinions %>%
  unnest_tokens(output = trigram, input = comment, token = "ngrams", n=3) #Tokenize into trigrams
tok_3

tok_sentence <- opinions %>% 
  unnest_tokens(sentence, comment, token = "sentences") #Tokenize into sentences
tok_sentence

### Punctuation and numbers ### 
# Use 'mutate' for 1-gram operations, and 'filter' for operations involving more than one gram 
tok_1clean <- tok_1 %>%  
  mutate(word = str_remove_all(word, "[^[:alpha:]]")) %>% # Removes non-alphabetic characters, including spaces
  filter(!word == "") # Remove NA values 
tok_1clean

tok_2clean <- tok_2 %>% # Using 'str_remove_all' may lead to the loss of bigrams in certain ID's, hence it's not suitable for all cases.
  separate(bigram, into = c("word1", "word2")) %>%
  filter(!str_detect(word1, "[^[:alpha:]]"), !str_detect(word2, "[^[:alpha:]]")) %>% # To preserve bigrams, it's essential to filter both words together 
  unite(bigram, "word1", "word2", sep = " ")
tok_2clean
  
tok_3clean <- tok_3 %>%
  separate(trigram, into = c("word1", "word2", "word3")) %>%
  filter(!str_detect(word1, "[^[:alpha:]]"), !str_detect(word2, "[^[:alpha:]]"), !str_detect(word3, "[^[:alpha:]]")) %>%
  unite(trigram, "word1", "word2", "word3", sep = " ")
tok_3clean

### Stop word removal ###
# Unigram
remove_stopwords <- opinions %>%
  unnest_tokens(output = word, input = comment) %>%
  mutate(word = str_remove_all(word, "[^[:alpha:]]")) %>%
  filter(!word %in% stopwords("en")) %>%
  filter(!word == "")
remove_stopwords
glimpse(remove_stopwords)

# Bigram 
remove_stopwords2 <- opinions %>%
  unnest_tokens(bigram, comment, token = "ngrams", n = 2) %>%
  separate(bigram, into = c("word1", "word2")) %>%
  filter(!word1 %in% stopwords("en"), !word2 %in% stopwords("en")) %>% # Only accepts vector arguments, hence they need to be separated before passing them 
  unite(bigram, "word1", "word2" , sep = " ") 
remove_stopwords2

### trigram ###
remove_stopwords3 <- opinions %>%
  unnest_tokens(trigram, comment, token = "ngrams", n = 3) %>%
  separate(trigram, into = c("word1", "word2", "word3")) %>%
  filter(!word1 %in% stopwords("en"), !word2 %in% stopwords("en"), !word3 %in% stopwords("en")) %>%
  unite(trigram, "word1", "word2", "word3", sep = " ")
remove_stopwords3

### Stemming/ Lemmatization ###

library(textstem)

opinions %>% unnest_tokens(output = word, input = comment) %>%
  mutate(word = str_remove_all(word, "[^[:alpha:]]")) %>%
  filter(!word %in% stopwords("en")) %>%
  mutate(stem = stem_words(word), lemme = lemmatize_words(word)) %>% # Lemmatization is more accurate as it generates results based on dictionary entries.
  filter(!word == "") 
  
opinions %>% unnest_tokens(output = word, input = comment) %>%
  mutate(word = str_remove_all(word, "[^[:alpha:]]")) %>%
  filter(!word %in% stopwords("en")) %>%
  mutate(word = lemmatize_words(word)) %>%
  filter(!word == "") 

### Rare word removal ###
# We'll eliminate words that appear in less than 0.1% of the total number of rows (nrow * 0.1%).
n_rows <- opinions %>%
  unnest_tokens(output = word, input = comment) %>%
  mutate(word = str_remove_all(word, "[^[:alpha:]]")) %>%
  filter(!word %in% stopwords("en")) %>%
  filter(!word=="") %>% 
  mutate(word = lemmatize_words(word)) %>%
  nrow()

# All together
opinion_tok1 <- opinions %>%
  unnest_tokens(word, comment) %>%
  mutate(word = str_remove_all(word, "[^[:alpha:]]")) %>%
  filter(!word %in% stopwords("en")) %>%
  filter(!word == "") %>%
  mutate(word = lemmatize_words(word)) %>%
  group_by(word) %>%
  filter(n() > (n_rows*(1/1000))) %>% # (1/1000 = 0.1%)
  ungroup(word)
opinion_tok1

### Descriptive analysis ###
opinion_tok1

word_counts <- opinion_tok1 %>%
  select(word) %>%
  count(word, sort = TRUE) %>%
  filter(n>2) # Frequency of words occurring at least 3 times
word_counts  

word_counts2 <- opinion_tok1 %>%
  select(word) %>%
  count(word, sort = TRUE) 
word_counts2

# Visualizing word frequency with wordcloud
library(wordcloud)
wordcloud(word_counts$word, word_counts$n, max.words = 30) # Graph

wordcloud(word_counts2$word, word_counts2$n, min.freq = 3, max.words = 30) # Achieves the same effect as before, but without the need to filter for word counts greater than 2. Instead, we perform the filtering within the wordcloud function.

library(wordcloud2)
wordcloud2(word_counts, size = 0.5, color = "random-dark", shape = "triangle") # Graph

# Visualizing word frequency with ggplot
ggplot(word_counts, aes(label = word, size = n, color = word)) + # The 'label' aesthetic is mandatory for plotting, where 'size' corresponds to word frequency.
  geom_text_wordcloud_area(rm_outside = TRUE) + # Elements that fall outside the plotting area should be removed
  scale_size_area(max_size = 15) +
  theme_classic()

# Comparing word occurrence between negative and positive assessments
word_pos <- opinion_tok1 %>%
  filter(assessment == "positive") %>%
  count(word, sort = TRUE) 
word_pos

word_cons <- opinion_tok1 %>%
  filter(assessment == "negative") %>%
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
  spread(list, Termfrequency)
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
count_total <- opinion_tok1 %>%
  count(assessment, word, sort = TRUE) %>% ## We will count the number of combinations of these two variables
  bind_tf_idf(word, assessment, n) ## (term, document, n) # We will create two distinct documents: one for positive opinions and another for negative opinions
count_total # idf=0 suggests that the term is either too common or too rare across the corpus.

plot_pos <- count_total %>%
  filter(assessment == "positive" & tf_idf > 0) %>%
  mutate(word = reorder(word, tf_idf)) %>%
  select(-n) %>%
  head(20) %>%
  ggplot(aes(tf_idf, word)) +
  geom_col(fill = "steelblue", alpha = 0.7, show.legend = FALSE) +
  labs(y = NULL, title = "tf_idf - Positive") +
  theme_classic()
plot_pos

plot_neg <- count_total %>%
  filter(assessment=="negative" & tf_idf > 0) %>%
  mutate(word=reorder(word, tf_idf)) %>%
  select(-n) %>%
  head(20) %>%
  ggplot(aes(tf_idf, word)) +
  geom_col(fill = "tomato", alpha=0.7, show.legend = FALSE) +
  labs(y=NULL, title = "tf_idf - Negative") +
  theme_classic()
plot_neg

grid.arrange(plot_pos, plot_neg, ncol = 2)
