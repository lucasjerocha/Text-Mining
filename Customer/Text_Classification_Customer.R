### Text Classification ###

library(tidyverse)
library(tidymodels)
library(textrecipes)
library(parsnip)
library(textdata)
library(ranger)


# load data 
review <- read.csv("customer_review_practice.csv", sep=",") 
review
review$label <- as.factor(review$label)
glimpse(review)

#--- Text Classification using TF/TF-IDF

### Base model ###
# unigram, tf

set.seed(123123) # To ensure reproducibility of results

# Splitting data into training and testing sets

review_split <- initial_split(review, strata = "label", prop = 0.8) # 80% training and 20% testing
train_data <- review_split %>% 
  training()
test_data <- review_split %>%
  testing()

# Data preprocessing with recipe

data_rec <- recipe(formula = label ~ review, data = train_data) %>% # The testing data should remain untouched
  step_filter(review != "") %>%
  step_tokenize(review) %>%
  step_stopwords(review, language = "en", keep = FALSE) %>%
  step_tokenfilter(review, max_times = 200, min_times = 2) %>% # Removing infrequent or overly frequent terms 
  step_tf(review) %>% # Transforming tokens into Term-Frequencies
  prep(training = train_data) # Prepare the transformations for the training data
data_rec # Define preprocessing

train_baked <- data_rec %>% bake(new_data = train_data) # Apply preprocessing
test_baked <- data_rec %>% bake(new_data = test_data)

# use logistic regression model (lrm) with elastic-net regularization 

glm_model <- logistic_reg(mode = "classification", mixture = 0, penalty = 0.1) %>% # Performs classification, mixture = 0 (regression model)
  set_engine("glmnet") # Modeling engine 
glm_model

# Training the lrm on the preprocessed training data 

final_model <- glm_model %>%
  fit(label ~ ., data = train_baked) # Specifies the formula for the model; assessment is the output variable

# Testing

predictions_glm <- final_model %>%
  predict(new_data = test_baked) %>% # The data on which we want to make predictions
  bind_cols(test_baked %>% select(label)) # To compare the model predictions (test_baked) with the actual labels of the test data
predictions_glm

predictions_glm %>%
  conf_mat(label, .pred_class) # Confusion matrix

predictions_glm %>%
  metrics(label, .pred_class) %>% # To calculate important metrics as precision, recall, AUC-ROC, etc
  select(-.estimator) %>%
  filter(.metric == "accuracy")

# ROC curve

test_baked %>%
  select(label) %>%
  mutate(
    my_class = parsnip:::predict_class(final_model, test_baked), # (object, new_data) - to store the predicted class labels
    my_prop = parsnip:::predict_classprob(final_model, test_baked) %>% pull("negative")) %>% # To store the predicted probabilities of the myprop$negative column
  roc_curve(label, my_prop) %>%
  autoplot()
# With a ROC curve, we are trying to find a good model that optimizes the trade off between the False Positive Rate and True Positive Rate 
# The larger the area under the curve better

### Model 2 ###
# Use bigram, tf 

# Data Preprocessing: 

data_rec2 <- recipe(formula = label ~ review, data = train_data) %>%
  step_filter(review != "") %>% 
  step_tokenize(review, token = "ngrams", options = list(n = 2)) %>%
  step_stopwords(review, language = "en", keep = FALSE) %>%
  step_tokenfilter(review, max_times = 200, min_times = 2) %>%
  step_tf(review) %>%
  prep(training = train_data)
data_rec2

train_baked2 <- data_rec2 %>% bake(new_data = train_data)
test_baked2 <- data_rec2 %>% bake(new_data = test_data)

# Model building

glm_model2 <- logistic_reg(mode = "classification", mixture = 0, penalty = 0.1) %>%
  set_engine("glmnet")
glm_model2

# Training

final_model2 <- glm_model2 %>%
  fit(label ~., data = train_baked2)

# Testing

predictions_glm2 <- final_model2 %>%
  predict(new_data = test_baked2) %>%
  bind_cols(test_baked2 %>% select(label))

# Confusion matrix
predictions_glm2 %>%
  conf_mat(label, .pred_class)

predictions_glm2 %>%
  metrics(label, .pred_class) %>%
  select(-.estimator) %>%
  filter(.metric == "accuracy")

test_baked2 %>%
  select(label) %>%
  mutate(
    my_class2 = parsnip:::predict_class(final_model2, test_baked2),
    my_prop2 = parsnip:::predict_classprob(final_model2, test_baked2) %>% pull("negative")
  ) %>%
  roc_curve(label, my_prop2) %>%
  autoplot()
# Worse performance 

### Model 3 ###
# Use unigram, tf-idf

data_rec3 <- recipe(formula = label ~ review, data = train_data) %>%
  step_filter(review != "") %>% 
  step_tokenize(review) %>%
  step_stopwords(review, language = "en", keep = FALSE) %>%
  step_tokenfilter(review, max_times = 200, min_times = 2) %>%
  step_tfidf(review) %>% # The contrast with model 1
  prep(training = train_data)
data_rec3

train_baked3 <- data_rec3 %>% bake(new_data = train_data) 
test_baked3  <- data_rec3 %>% bake(new_data = test_data)

glm_model3 <- logistic_reg(mode = "classification", mixture = 0, penalty = 0.1) %>%
  set_engine("glmnet")
glm_model3

# Training

final_model3 <- glm_model3 %>%
  fit(label ~ ., data = train_baked3)

# Testing

predictions_glm3 <- final_model3 %>%
  predict(new_data = test_baked3) %>%
  bind_cols(test_baked3 %>% select(label))

# Confusion matrix

predictions_glm3 %>% 
  conf_mat(label, .pred_class)

predictions_glm3 %>%
  metrics(label, .pred_class) %>%
  select(-.estimator) %>%
  filter(.metric == "accuracy")

test_baked3 %>% 
  select(label) %>%
  mutate(
    my_class = parsnip:::predict_class(final_model3, test_baked3),
    my_prop = parsnip:::predict_classprob(final_model3, test_baked3) %>% pull("negative")
  ) %>%
  roc_curve(label, my_prop) %>%
  autoplot()
# Enhanced performance due to TF-IDF, which weights words based on their frequency across the entire document corpus

### Model 4 ###
# same as above but using Random Forest

library(ranger)
set.seed(12123572)
tree_model <- rand_forest(trees = 100) %>% # It initializes a random forest model with 100 trees
  set_mode("classification") %>%
  set_engine("ranger")

# Training

final_model4 <- tree_model %>%
  fit(label ~ ., data = train_baked3)

# Testing

predictions_tree <- final_model4 %>%
  predict(new_data = test_baked3) %>%
  bind_cols(test_baked3 %>% select(label))

# Confusion matrix

predictions_tree %>%
  conf_mat(label, .pred_class)

predictions_tree %>%
  metrics(label, .pred_class) %>%
  select(-.estimator) %>%
  filter(.metric == "accuracy")

test_baked3 %>% 
  select(label) %>%
  mutate(
    my_class = parsnip:::predict_class(final_model4, test_baked3),
    my_prop = parsnip:::predict_classprob(final_model4, test_baked3) %>% pull("negative")
  ) %>%
  roc_curve(label, my_prop) %>%
  autoplot()

### Model 5 ###
# same as above but using xgBoost 

library(xgboost)
set.seed(123)
xg_model <- boost_tree(trees = 10) %>% # The model will consist of 10 decision trees
  set_mode("classification") %>% set_engine("xgboost")

# Training

final_model5 <- xg_model %>%
  fit(label ~ ., data = train_baked3)

# Testing

predictions_xg <- final_model5 %>%
  predict(new_data = test_baked3) %>%
  bind_cols(test_baked3 %>% select(label))

# Confusion matrix

predictions_xg %>%
  conf_mat(label, .pred_class)

predictions_xg %>%
  metrics(label, .pred_class) %>%
  select(-.estimator) %>%
  filter(.metric == "accuracy") 

test_baked3 %>% 
  select(label) %>%
  mutate(
    my_class = parsnip:::predict_class(final_model5, test_baked3),
    my_prop = parsnip:::predict_classprob(final_model5, test_baked3) %>% pull("negative")
  ) %>%
  roc_curve(label, my_prop) %>%
  autoplot()
# The top-performing model

### Text Classification using sentiment score ###
# use sentiment scores as the word embedding to predict the assessment of each comment

set.seed(123123123)

review_split <- initial_split(review, strata = "label", prop = 0.8)
train_data <- review_split %>% training()
test_data <- review_split %>% testing()

# Using lexicon bing

lexicon_bing()
lexicon_score <- lexicon_bing() %>%
  mutate(score = if_else(sentiment == "positive", 1, -1)) %>% # More stringent compared to ifelse
  select(-sentiment)
lexicon_score

# Preprocessing recipe

data_rec <- recipe(formula = label ~ review, data = train_data) %>%
  step_filter(review != "") %>% 
  step_tokenize(review) %>%
  step_stopwords(review, language = "en", keep = FALSE) %>%
  step_word_embeddings(review, embeddings = lexicon_score) %>% # The function calculates word embeddings based on the sentiment scores
  prep(training = train_data)
data_rec

train_baked <- data_rec %>% bake(new_data = train_data)
test_baked  <- data_rec %>% bake(new_data = test_data)

glm_model <- logistic_reg(mode = "classification") %>%
  set_engine("glm")
glm_model

# Training 

final_model <- glm_model %>%
  fit(label ~ ., data = train_baked)

# Testing

predictions_glm <- final_model %>%
  predict(new_data = test_baked) %>%
  bind_cols(test_baked %>% select(label))
predictions_glm

# Confuse matrix

predictions_glm %>%
  conf_mat(label, .pred_class)

predictions_glm %>%
  metrics(label, .pred_class) %>%
  select(-.estimator) %>%
  filter(.metric == "accuracy") 

test_baked %>% 
  select(label) %>%
  mutate(
    my_class = parsnip:::predict_class(final_model, test_baked),
    my_prop = parsnip:::predict_classprob(final_model, test_baked) %>% pull("negative")
  ) %>%
  roc_curve(label, my_prop) %>%
  autoplot()

# Using lexicon afinn

# Preprocessing

data_rec <- recipe(formula = label ~ review, data = train_data) %>%
  step_filter(review != "") %>% 
  step_tokenize(review) %>%
  step_stopwords(review, language = "en", keep = FALSE) %>%
  step_word_embeddings(review, embeddings = lexicon_afinn()) %>%
  prep(training = train_data)
data_rec

train_baked <- data_rec %>% bake(new_data = train_data)
test_baked  <- data_rec %>% bake(new_data = test_data)

glm_model <- logistic_reg(mode = "classification") %>%
  set_engine("glm")
glm_model

# Training

final_model <- glm_model %>%
  fit(label ~ ., data = train_baked)

# Testing

predictions_glm <- final_model %>%
  predict(new_data = test_baked) %>%
  bind_cols(test_baked %>% select(label))
predictions_glm

# Confuse matrix

predictions_glm %>%
  conf_mat(label, .pred_class)

predictions_glm %>%
  metrics(label, .pred_class) %>%
  select(-.estimator) %>%
  filter(.metric == "accuracy") 

test_baked %>% 
  select(label) %>%
  mutate(
    my_class = parsnip:::predict_class(final_model, test_baked),
    my_prop = parsnip:::predict_classprob(final_model, test_baked) %>% pull("negative")
  ) %>%
  roc_curve(label, my_prop) %>%
  autoplot()

# Using lexicon nrc

lexicon_nrc_wide <- lexicon_nrc() %>%
  mutate(var = 1) %>%
  pivot_wider(names_from = sentiment, values_from = var, values_fill = list(var=0))

# Preprocessing

data_rec <- recipe(formula = label ~ review, data = train_data) %>%
  step_filter(review != "") %>% 
  step_tokenize(review) %>%
  step_stopwords(review, language = "en", keep = FALSE) %>%
  step_word_embeddings(review, embeddings = lexicon_nrc_wide, prefix = "nrc") %>%
  prep(training = train_data)
data_rec

train_baked <- data_rec %>% bake(new_data = train_data)
test_baked  <- data_rec %>% bake(new_data = test_data)

glmnet_model <- logistic_reg(mode = "classification") %>%
  set_engine("glm")

# Training

final_model <- glmnet_model %>%
  fit(label ~ ., data = train_baked)

# Testing

predictions_glmnet <- final_model %>%
  predict(new_data = test_baked) %>%
  bind_cols(test_baked %>% select(label))

# Confuse  matrix

predictions_glmnet %>%
  conf_mat(label, .pred_class)

predictions_glmnet %>%
  metrics(label, .pred_class) %>%
  select(-.estimator) %>%
  filter(.metric == "accuracy") 

test_baked %>% 
  select(label) %>%
  mutate(
    my_class = parsnip:::predict_class(final_model, test_baked),
    my_prop = parsnip:::predict_classprob(final_model, test_baked) %>% pull("negative")
  ) %>%
  roc_curve(label, my_prop) %>%
  autoplot()

