library(readxl)
library(dplyr)
library(tidyverse)
library(glmnet)
library(caret)
#load the data
trump <- read_excel("Final.model.data.xlsx")
trump$press.type <-ifelse(trump$PRESS == "FOX", "1", "0")
trump <- trump[,-c(1:2)]

trump$press.
#preparing the data
set.seed(12345)
training.samples <- trump$press.type%>%
  createDataPartition(p=0.8, list=FALSE)

train.data <- trump[training.samples,]
test.data  <- trump[-training.samples,]


# Dumy code categorical predictor variables
x <- model.matrix(press.type~., train.data, na.omit=TRUE)[,-1]
# Convert the outcome (class) to a numerical variable
y <- ifelse(train.data$press.type =="1", 1, 0)

# Find the best lambda using cross-validation
set.seed(12345) 
cv.lasso <- cv.glmnet(x, y, alpha = 1, family = "binomial")
plot(cv.lasso)
cv.lasso$lambda.min
cv.lasso$lambda.1se
coef(cv.lasso, cv.lasso$lambda.min)
# Fit the final model on the training data
lasso.model <- glmnet(x, y, alpha = 1, family = "binomial",lambda = cv.lasso$lambda.min)
#lasso.model.one <-glmnet(x, y, alpha = 1, family = "binomial",lambda = cv.lasso$lambda.1se)

# Make prediction on test data
x.test <- model.matrix(press.type ~., test.data)[,-1]
probabilities <- lasso.model %>% predict(newx = x.test)
predicted.classes <- ifelse(probabilities > 0.9, "1", "0")
# Model accuracy
observed.classes <- test.data$press.type
mean(predicted.classes == observed.classes)
