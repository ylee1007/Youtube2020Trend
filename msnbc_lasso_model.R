library(dplyr)
library(tidyverse)
library(glmnet)
library(caret)
fox_matrix <- read.csv("fox_matrix_model.csv")
fox_matrix <- fox_matrix[,-2]


#preparing the data
set.seed(123)
training.samples <- fox_matrix$View%>%
  createDataPartition(p=0.8, list=FALSE)
train.data <- fox_matrix[training.samples,]
test.data <- fox_matrix[-training.samples,]


#additional data preparation
x <-model.matrix(View~., train.data)[,-1]
y <-train.data$View


#computing lasso regression
set.seed(123)
cv <- cv.glmnet(x,y,alpha=1)
cv$lambda.min
model <- glmnet(x, y, alpha = 1, lambda = cv$lambda.min)
coef(model)

# Make predictions on the test data
x.test <- model.matrix(View ~., test.data)[,-1]
predictions <- model %>% predict(x.test) %>% as.vector()
# Model performance metrics
data.frame(
  RMSE = RMSE(predictions, test.data$View),
  Rsquare = R2(predictions, test.data$View)
)


#remove global environment before jump over to msnbc model
#########################################################################
#msnbc


msnbc_matrix <- read.csv("msnbc_matrix_1.csv")
#msnbc_matrix <- msnbc_matrix[,-2]

is.na(msnbc_matrix)

#preparing the data
set.seed(123)
training.samples <- msnbc_matrix$View%>%
  createDataPartition(p=0.8, list=FALSE )
train.data <- msnbc_matrix[training.samples,]
test.data <- msnbc_matrix[-training.samples,]


#additional data preparation
x <-model.matrix(View~., train.data)[,-1]
y <-train.data$View


#computing lasso regression
set.seed(123)
cv <- cv.glmnet(x,y,alpha=1)
cv$lambda.min
model <- glmnet(x, y, alpha = 1, lambda = cv$lambda.min)
coef(model)

plot(cv)
# Make predictions on the test data
x.test <- model.matrix(View ~., test.data)[,-1]
predictions <- model %>% predict(x.test) %>% as.vector()
# Model performance metrics
data.frame(
  RMSE = RMSE(predictions, test.data$View),
  Rsquare = R2(predictions, test.data$View)
)

###################Penalized Logstic Regression#########################
#######################################################################33

msnbc_matrix <- read.csv("msnbc_matrix_1.csv")
#summary(msnbc_matrix$View)
msnbc_matrix$view.logic <- ifelse(msnbc_matrix$View >= 100000, "large", "small")
msnbc_matrix <- msnbc_matrix[,-1]
#msnbc_matrix <- msnbc_matrix[,-2]


#preparing the data
set.seed(123)
training.samples <- fox_msnbc_matrix$view.logic%>%
  createDataPartition(p=0.8, list=FALSE )
train.data <- fox_msnbc_matrix[training.samples,]
test.data <- fox_msnbc_matrix[-training.samples,]


#mean(fox_msnbc_matrix$View)
#summary(fox_msnbc_matrix$View)

# Dumy code categorical predictor variables
x <- model.matrix(train.data$view.logic~., train.data)[,-1]
# Convert the outcome (class) to a numerical variable
y <- ifelse(train.data$view.logic =="large", 1, 0)

library(glmnet)
# Find the best lambda using cross-validation
set.seed(123) 
cv.lasso <- cv.glmnet(x, y, alpha = 1, family = "binomial")
plot(cv.lasso)
cv.lasso$lambda.min
cv.lasso$lambda.1se
coef(cv.lasso, cv.lasso$lambda.min)
# Fit the final model on the training data
lasso.model <- glmnet(x, y, alpha = 1, family = "binomial",lambda = cv.lasso$lambda.min)
lasso.model.one <-glmnet(x, y, alpha = 1, family = "binomial",lambda = cv.lasso$lambda.1se)

# Make prediction on test data
x.test <- model.matrix(view.logic ~., test.data)[,-1]
probabilities <- lasso.model %>% predict(newx = x.test)
predicted.classes <- ifelse(probabilities > 0.5, "large", "small")
# Model accuracy
observed.classes <- test.data$view.logic
mean(predicted.classes == observed.classes)


v<-coef(lasso.model)
order(v)
sort(v, decreasing = TRUE)


#########################################################################
####################msnbc+fox model #######################################
############################################################################

fox_msnbc_matrix <- read.csv("fox_msnbc_matrix_revised.csv")

summary(fox_msnbc_matrix$View)
#plot(density(fox_msnbc_matrix$View))
#hist(fox_msnbc_matrix$View, breaks=100, col="red")
fox_msnbc_matrix$view.logic <- ifelse(fox_msnbc_matrix$View >= 250000, "large", "small")
fox_msnbc_matrix <- fox_msnbc_matrix[,-1]
#msnbc_matrix <- msnbc_matrix[,-2]


#preparing the data
set.seed(12345)
training.samples <- fox_msnbc_matrix$view.logic%>%
  createDataPartition(p=0.8, list=FALSE )
train.data <- fox_msnbc_matrix[training.samples,]
test.data <- fox_msnbc_matrix[-training.samples,]


#mean(msnbc_matrix$View)
#summary(msnbc_matrix$View)

# Dumy code categorical predictor variables
x <- model.matrix(view.logic~., train.data, na.omit=TRUE)[,-1]
# Convert the outcome (class) to a numerical variable
y <- ifelse(train.data$view.logic =="large", 1, 0)

library(glmnet)
# Find the best lambda using cross-validation
set.seed(123) 
cv.lasso <- cv.glmnet(x, y, alpha = 1, family = "binomial")
plot(cv.lasso)
cv.lasso$lambda.min
cv.lasso$lambda.1se
coef(cv.lasso, cv.lasso$lambda.min)
# Fit the final model on the training data
lasso.model <- glmnet(x, y, alpha = 1, family = "binomial",lambda = cv.lasso$lambda.min)
#lasso.model.one <-glmnet(x, y, alpha = 1, family = "binomial",lambda = cv.lasso$lambda.1se)

# Make prediction on test data
x.test <- model.matrix(view.logic ~., test.data)[,-1]
probabilities <- lasso.model %>% predict(newx = x.test)
predicted.classes <- ifelse(probabilities > 0.9, "large", "small")
# Model accuracy
observed.classes <- test.data$view.logic
mean(predicted.classes == observed.classes)


v<-coef(lasso.model)
order(v)
sort(v, decreasing = TRUE)
