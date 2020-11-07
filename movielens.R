library(tidyverse)
library(caret)
library(data.table)

#IMPORT DATA FILES - PLEASE REPLPACE WITH YOUR OWN DIRECTORY

edx<- fread("edx.csv")
validation<- fread("validation.csv")


# EXTRACT CRITICAL FEATURES (TO SAVE ON RAM)
edx<- edx[,c("rating","title", "movieId", "userId")]


#SPLIT INTO TEST AND TRAIN SETS
set.seed(7)
test_index <- createDataPartition(edx$rating, times = 1, p = 0.2, list = FALSE)
train_set <- edx %>% slice(-test_index)
test_set <- edx %>% slice(test_index)
mu_train<-mean(train_set$rating)


#GENERATE COEFFICIENTS FOR USER AND MOVIE EFFECTS, SELECT LAMBDA 

# lambda is a tuning parameter to penalise outliers
lambdas <- seq(0, 10, 0.25)

# For each lambda,find b_i & b_u, and then compute RMSE. NOTE: THIS TAKES A WHILE TO RUN
rmses <- sapply(lambdas, function(l){
  
  b_i <- train_set %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu_train)/(n()+l))
  
  b_u <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu_train)/(n()+l))
  
  predicted_ratings <- test_set %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu_train + b_i + b_u) 
  
  predicted_ratings[is.na(predicted_ratings)] <-0

  predicted_ratings <- predicted_ratings$pred
  
  return(RMSE(test_set$rating,predicted_ratings))
})
# Plot rmses vs lambdas to select the optimal lambda
qplot(lambdas, rmses) 


#PERFORM PREDICTIONS, CALCULATE RMSE
l <- lambdas[which.min(rmses)]
b_i <- train_set %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu_train)/(n()+l))

b_u <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu_train)/(n()+l))

predicted_ratings <- validation%>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu_train + b_i + b_u) 

predicted_ratings[is.na(predicted_ratings)] <-0
predicted_ratings <- predicted_ratings$pred
RMSE(predicted_ratings, validation$rating)
