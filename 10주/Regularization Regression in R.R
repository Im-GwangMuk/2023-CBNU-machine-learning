library(MASS)  # Package needed to generate correlated precictors
library(glmnet)  # Package to fit ridge/lasso/elastic net models

# Example 1
# Generate data - Example 1
set.seed(19875)  # Set seed for reproducibility
n <- 1000  # Number of observations
p <- 5000  # Number of predictors included in model
real_p <- 15  # Number of true predictors
x <- matrix(rnorm(n*p), nrow=n, ncol=p)
y <- apply(x[,1:real_p], 1, sum) + rnorm(n)

# Split data into train (2/3) and test (1/3) sets
train_rows <- sample(1:n, .66*n)
x.train <- x[train_rows, ]
x.test <- x[-train_rows, ]

y.train <- y[train_rows]
y.test <- y[-train_rows]

# Fit models 
# (For plots on left):
fit.lasso <- glmnet(x.train, y.train, family="gaussian", alpha=1)
fit.ridge <- glmnet(x.train, y.train, family="gaussian", alpha=0)
fit.elnet <- glmnet(x.train, y.train, family="gaussian", alpha=.5)

# 10-fold Cross validation for each alpha = 0, 0.1, ... , 0.9, 1.0
# (For plots on Right)
for (i in 0:10) {
  assign(paste("fit", i, sep=""), cv.glmnet(x.train, y.train, type.measure="mse", 
                                            alpha=i/10,family="gaussian"))
}

# Plot solution paths:
par(mfrow=c(3,2))
# For plotting options, type '?plot.glmnet' in R console
plot(fit.lasso, xvar="lambda")
#plot(fit10, main="LASSO")
x11()
plot(fit.ridge, xvar="lambda")
#plot(fit0, main="Ridge")

plot(fit.elnet, xvar="lambda")
#plot(fit5, main="Elastic Net")


fit10
yhat0 <- predict(fit0, s=fit0$lambda.1se, newx=x.test)
yhat1 <- predict(fit1, s=fit1$lambda.1se, newx=x.test)
yhat2 <- predict(fit2, s=fit2$lambda.1se, newx=x.test)
yhat3 <- predict(fit3, s=fit3$lambda.1se, newx=x.test)
yhat4 <- predict(fit4, s=fit4$lambda.1se, newx=x.test)
yhat5 <- predict(fit5, s=fit5$lambda.1se, newx=x.test)
yhat6 <- predict(fit6, s=fit6$lambda.1se, newx=x.test)
yhat7 <- predict(fit7, s=fit7$lambda.1se, newx=x.test)
yhat8 <- predict(fit8, s=fit8$lambda.1se, newx=x.test)
yhat9 <- predict(fit9, s=fit9$lambda.1se, newx=x.test)
yhat10 <- predict(fit10, s=fit10$lambda.1se, newx=x.test)

mse0 <- mean((y.test - yhat0)^2)
mse1 <- mean((y.test - yhat1)^2)
mse2 <- mean((y.test - yhat2)^2)
mse3 <- mean((y.test - yhat3)^2)
mse4 <- mean((y.test - yhat4)^2)
mse5 <- mean((y.test - yhat5)^2)
mse6 <- mean((y.test - yhat6)^2)
mse7 <- mean((y.test - yhat7)^2)
mse8 <- mean((y.test - yhat8)^2)
mse9 <- mean((y.test - yhat9)^2)
mse10 <- mean((y.test - yhat10)^2)

a=c()

for(i in 0:10){
 a = c(a,paste("mse", i, sep=""))
}

mse=c()

for(i in 0:10){
  mse[i] = eval(parse(text=a)[i])
}
mse
# 가장작은 mse값의 index
which.min(mse)-1

x11(); plot(fit7, main="Elastic Net")
fit7

min(c(mse0,mse10,mse5))

which.min(c(mse0,mse10,mse5))

# Example 2
library(MASS)  # Package needed to generate correlated precictors
library(glmnet)  # Package to fit ridge/lasso/elastic net models

# Generate data
set.seed(19874)
n <- 1000    # Number of observations
p <- 5000     # Number of predictors included in model
real_p <- 1500  # Number of true predictors
x <- matrix(rnorm(n*p), nrow=n, ncol=p)
y <- apply(x[,1:real_p], 1, sum) + rnorm(n)

# Split data into train and test sets
train_rows <- sample(1:n, .66*n)
x.train <- x[train_rows, ]
x.test <- x[-train_rows, ]

y.train <- y[train_rows]
y.test <- y[-train_rows]


# Fit models:
fit.lasso <- glmnet(x.train, y.train, family="gaussian", alpha=1)
fit.ridge <- glmnet(x.train, y.train, family="gaussian", alpha=0)
fit.elnet <- glmnet(x.train, y.train, family="gaussian", alpha=.5)


# 10-fold Cross validation for each alpha = 0, 0.1, ... , 0.9, 1.0
fit.lasso.cv <- cv.glmnet(x.train, y.train, type.measure="mse", alpha=1, 
                          family="gaussian")
fit.ridge.cv <- cv.glmnet(x.train, y.train, type.measure="mse", alpha=0,
                          family="gaussian")
fit.elnet.cv <- cv.glmnet(x.train, y.train, type.measure="mse", alpha=.5,
                          family="gaussian")

for (i in 0:10) {
  assign(paste("fit", i, sep=""), cv.glmnet(x.train, y.train, type.measure="mse", 
                                            alpha=i/10,family="gaussian"))
}


# Plot solution paths:
par(mfrow=c(3,2))
# For plotting options, type '?plot.glmnet' in R console
plot(fit.lasso, xvar="lambda")
#plot(fit10, main="LASSO")
x11()
plot(fit.ridge, xvar="lambda")
#plot(fit0, main="Ridge")

plot(fit.elnet, xvar="lambda")
#plot(fit5, main="Elastic Net")


yhat0 <- predict(fit0, s=fit0$lambda.1se, newx=x.test)
yhat1 <- predict(fit1, s=fit1$lambda.1se, newx=x.test)
yhat2 <- predict(fit2, s=fit2$lambda.1se, newx=x.test)
yhat3 <- predict(fit3, s=fit3$lambda.1se, newx=x.test)
yhat4 <- predict(fit4, s=fit4$lambda.1se, newx=x.test)
yhat5 <- predict(fit5, s=fit5$lambda.1se, newx=x.test)
yhat6 <- predict(fit6, s=fit6$lambda.1se, newx=x.test)
yhat7 <- predict(fit7, s=fit7$lambda.1se, newx=x.test)
yhat8 <- predict(fit8, s=fit8$lambda.1se, newx=x.test)
yhat9 <- predict(fit9, s=fit9$lambda.1se, newx=x.test)
yhat10 <- predict(fit10, s=fit10$lambda.1se, newx=x.test)

mse0 <- mean((y.test - yhat0)^2)
mse1 <- mean((y.test - yhat1)^2)
mse2 <- mean((y.test - yhat2)^2)
mse3 <- mean((y.test - yhat3)^2)
mse4 <- mean((y.test - yhat4)^2)
mse5 <- mean((y.test - yhat5)^2)
mse6 <- mean((y.test - yhat6)^2)
mse7 <- mean((y.test - yhat7)^2)
mse8 <- mean((y.test - yhat8)^2)
mse9 <- mean((y.test - yhat9)^2)
mse10 <- mean((y.test - yhat10)^2)


#Ridge is the winner! Ridge in general is good at prediction, but is not very interpretable.


#Example 3

# Generate data
set.seed(19873)
n <- 100    # Number of observations
p <- 50     # Number of predictors included in model
CovMatrix <- outer(1:p, 1:p, function(x,y) {.7^abs(x-y)})
x <- mvrnorm(n, rep(0,p), CovMatrix)
y <- 10 * apply(x[, 1:2], 1, sum) + 
  5 * apply(x[, 3:4], 1, sum) +
  apply(x[, 5:14], 1, sum) +
  rnorm(n)

y
# Split data into train and test sets
train_rows <- sample(1:n, .66*n)
x.train <- x[train_rows, ]
x.test <- x[-train_rows, ]

y.train <- y[train_rows]
y.test <- y[-train_rows]

# Fit models:
fit.lasso <- glmnet(x.train, y.train, family="gaussian", alpha=1)
fit.ridge <- glmnet(x.train, y.train, family="gaussian", alpha=0)
fit.elnet <- glmnet(x.train, y.train, family="gaussian", alpha=.5)


# 10-fold Cross validation for each alpha = 0, 0.1, ... , 0.9, 1.0
fit.lasso.cv <- cv.glmnet(x.train, y.train, type.measure="mse", alpha=1, 
                          family="gaussian")
fit.ridge.cv <- cv.glmnet(x.train, y.train, type.measure="mse", alpha=0,
                          family="gaussian")
fit.elnet.cv <- cv.glmnet(x.train, y.train, type.measure="mse", alpha=.5,
                          family="gaussian")

for (i in 0:10) {
  assign(paste("fit", i, sep=""), cv.glmnet(x.train, y.train, type.measure="mse", 
                                            alpha=i/10,family="gaussian"))
}



# Plot solution paths:
par(mfrow=c(3,2))
# For plotting options, type '?plot.glmnet' in R console
plot(fit.lasso, xvar="lambda")
plot(fit10, main="LASSO")
x11();
plot(fit.ridge, xvar="lambda")
plot(fit0, main="Ridge")

plot(fit.elnet, xvar="lambda")
plot(fit5, main="Elastic Net")

yhat0 <- predict(fit0, s=fit0$lambda.1se, newx=x.test)
yhat1 <- predict(fit1, s=fit1$lambda.1se, newx=x.test)
yhat2 <- predict(fit2, s=fit2$lambda.1se, newx=x.test)
yhat3 <- predict(fit3, s=fit3$lambda.1se, newx=x.test)
yhat4 <- predict(fit4, s=fit4$lambda.1se, newx=x.test)
yhat5 <- predict(fit5, s=fit5$lambda.1se, newx=x.test)
yhat6 <- predict(fit6, s=fit6$lambda.1se, newx=x.test)
yhat7 <- predict(fit7, s=fit7$lambda.1se, newx=x.test)
yhat8 <- predict(fit8, s=fit8$lambda.1se, newx=x.test)
yhat9 <- predict(fit9, s=fit9$lambda.1se, newx=x.test)
yhat10 <- predict(fit10, s=fit10$lambda.1se, newx=x.test)

mse0 <- mean((y.test - yhat0)^2)
mse1 <- mean((y.test - yhat1)^2)
mse2 <- mean((y.test - yhat2)^2)
mse3 <- mean((y.test - yhat3)^2)
mse4 <- mean((y.test - yhat4)^2)
mse5 <- mean((y.test - yhat5)^2)
mse6 <- mean((y.test - yhat6)^2)
mse7 <- mean((y.test - yhat7)^2)
mse8 <- mean((y.test - yhat8)^2)
mse9 <- mean((y.test - yhat9)^2)
mse10 <- mean((y.test - yhat10)^2)

mse10
#Elastic Net is the winner! It’s interesting to note the best solution is “close” to #Ridge, but Ridge (α=0) in fact performs the worst.


mse0

mse10

mse5



## Another example

library(tidyverse)
library(caret)
library(glmnet)
#Glmnet은 패널티 최대 우도(penalized maximum likelihood)를 통해서 일반화 선형 모델#(generalized linear model)을 적합하는 패키지

data("Boston", package = "MASS")
help(Boston)
#set a seed so you can reproduce the results
set.seed(1212)

#split the data into training and test data
sample_size <- floor(0.75 * nrow(Boston))

training_index <- sample(seq_len(nrow(Boston)), size = sample_size)

train <- Boston[training_index, ]

test <- Boston[-training_index, ]

# Predictor
x <- model.matrix(medv~., train)[,-1]
# Response
y <- train$medv

cor(x)
ols<-lm(y~x)
summary(ols)


#Performing Ridge regression
# As we mentioned in class, 
#lambda values have a large effect on coefficients 
#so now we will compute and chose a suitable one.

#Here we perform a cross validation and take a peek at the lambda value 
#corresponding to the lowest prediction error before fitting the data to the model #and viewing the coefficients.

model.ridge0 <- glmnet(x, y, alpha = 0)
coef(model.ridge0)
plot(model.ridge0, xvar="lambda")
plot(model.ridge0, main="Ridge")

cv.r <- cv.glmnet(x, y, alpha = 0)
cv.r$lambda.min
model.ridge <- glmnet(x, y, alpha = 0, lambda = cv.r$lambda.min)
coef(model.ridge)

#We can see here that certain coefficients have been pushed towards zero and #minimized while RM (number of rooms) has a significantly higher weight than the #rest

#We now look at how our model performs by using our test data on it.
x.test.ridge <- model.matrix(medv ~., test)[,-1]
predictions.ridge <- model.ridge %>% predict(x.test.ridge) %>% as.vector()

data.frame(
  RMSE.r = RMSE(predictions.ridge, test$medv),
  Rsquare.r = R2(predictions.ridge, test$medv)
)


#Performing Lasso regression
#The steps will be identical to what we have done for ridge regression. The value #of alpha is the only change here (remember 𝞪 = 1 denotes lasso)

model.lasso0 <- glmnet(x, y, alpha = 1)
plot(model.lasso0, xvar="lambda")
plot(model.lasso0, main="LASSO")

cv.l <- cv.glmnet(x, y, alpha = 1)
cv.l$lambda.min

model.lasso <- glmnet(x, y, alpha = 1, lambda = cv.l$lambda.min)
coef(model.lasso)
x.test.lasso <- model.matrix(medv ~., test)[,-1]
predictions.lasso <- model.lasso %>%  predict(x.test.lasso) %>%   as.vector()

data.frame(
  RMSE.l = RMSE(predictions.lasso, test$medv),
  Rsquare.l = R2(predictions.lasso, test$medv)
)

#Performing Elastic Net regression
#Performing Elastic Net requires us to tune parameters to identify the best alpha #and lambda values and for this we need to use the caret package. We will tune #the model by iterating over a number of alpha and lambda pairs and we can see #which pair has the lowest associated error.

model.net <- train(
  medv ~., data = train, method = "glmnet",
  trControl = trainControl("cv", number = 10),
  tuneLength = 10)

model.net$bestTune

coef(model.net$finalModel, model.net$bestTune$lambda)

x.test.net <- model.matrix(medv ~., test)[,-1]
predictions.net <- model.net %>% predict(x.test.net)

data.frame(
  RMSE.net = RMSE(predictions.net, test$medv),
  Rsquare.net = R2(predictions.net, test$medv))
