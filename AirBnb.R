####################Linear regression################

airbnb.df<- read.csv("Airbnb.csv")
airbnb.df<-airbnb.df[,-c(1,2,3,4,13,15)]

view(airbnb.df)
summary(airbnb.df)

#replacing reviews per month with 0
airbnb.df$reviews_per_month[is.na(airbnb.df$reviews_per_month)] <- 0

#creating dummy variables of neighbourhood_group
xtotal1 <- model.matrix(~ 0 + neighbourhood_group, data = airbnb.df)
xtotal1 <- as.data.frame(xtotal1)
t(t(names(xtotal1)))  # check the names of the dummy variables
xtotal1 <- xtotal1[, -5]  # drop one of the dummy variables, since queens and staten island had same median price, we decided to drop staten island. 
head(xtotal1)

#creating dummy variables of room_type
xtotal2 <- model.matrix(~ 0 + room_type, data = airbnb.df)
xtotal2 <- as.data.frame(xtotal2)
t(t(names(xtotal2)))  # check the names of the dummy variables
xtotal2 <- xtotal2[, -3]  # drop one of the dummy variables, since there were only 1000 listings of shared room we decided to drop that dummy variable. 
head(xtotal2)

# Column binding roomtype and neighbourhood group with data set

airbnb.df <- cbind(airbnb.df[, -c(1)], xtotal1)
airbnb.df <- cbind(airbnb.df[, -c(4)], xtotal2)

#removing neighbourhood
airbnb.df<-airbnb.df[,-2]

set.seed(2)
train.index <- sample(c(1:dim(airbnb.df)[1]), dim(airbnb.df)[1]*0.6)  
train.df <- airbnb.df[train.index, ]
valid.df <- airbnb.df[-train.index, ]


# use lm() to run a linear regression of Price on all predictors in the
# training set.
airbnb.lm <- lm(price ~ ., data = train.df)
options(scipen = 999)
summary(airbnb.lm)



library(forecast)
# use predict() to make predictions on a new set.
airbnb.lm.pred <- predict(airbnb.lm, valid.df)
options(scipen=999, digits = 0)
some.residuals <- valid.df$price[1:20] - airbnb.lm.pred[1:20]
data.frame("Predicted" = airbnb.lm.pred[1:20], "Actual" = valid.df$price[1:20],
           "Residual" = some.residuals)


options(scipen=999, digits = 3)
# use accuracy() to compute common accuracy measures.
accuracy(airbnb.lm.pred, valid.df$price)

library(forecast)
airbnb.lm.pred <- predict(airbnb.lm, valid.df)
all.residuals <- valid.df$price - airbnb.lm.pred
length(all.residuals[which(all.residuals > -63000 & all.residuals < 63000)])/10000
hist(all.residuals, breaks = 5, xlab = "Residuals", main = "")
hist(all.residuals, xlab = "Residuals")


airbnb.lm.step <- step(airbnb.lm, direction = "backward")
summary(car.lm.step) # Which variables did it drop?
airbnb.lm.step.pred <- predict(airbnb.lm.step, valid.df)
accuracy(airbnb.lm.step.pred, valid.df$price)

airbnb.lm.step <- step(airbnb.lm, direction = "forward")
summary(airbnb.lm.step) # Which variables did it drop?
airbnb.lm.step.pred1 <- predict(airbnb.lm.step, valid.df)
accuracy(airbnb.lm.step.pred1, valid.df$price)

#################Logistic Regression###############

airbnb.df<- read.csv("Airbnb.csv")
airbnb.df<-airbnb.df[,-c(1,2,3,4,13,15)]


airbnb.df$pricecat<-cut(airbnb.df$price, breaks=c(0,150,Inf), labels=c("Below Average","Above Average"))
airbnb.df$pricecat<- as.factor(airbnb.df$pricecat)
airbnb.df$pricecat<-factor(airbnb.df$pricecat,levels=c("Below Average","Above Average"),labels = c(0,1),ordered = T)


#replacing reviews per month with 0
airbnb.df$reviews_per_month[is.na(airbnb.df$reviews_per_month)] <- 0


airbnb.df$neighbourhood_group<- as.factor(airbnb.df$neighbourhood_group)
airbnb.df$room_type<- as.factor(airbnb.df$room_type)

#removing price,neighbourhood and longitude,reviews per month
airbnb.df <- airbnb.df[,-c(2,4,6,9)]

#Partitioning the dataset
library(dplyr)
train.df <-airbnb.df %>% dplyr::filter(row_number() %% 2 == 0) #Select even rows
valid.df<-airbnb.df %>% dplyr::filter(row_number() %% 2 == 1) #Select odd rows

logit.reg <- glm(pricecat ~ ., data = train.df, family = "binomial") 
options(scipen=999)
summary(logit.reg)



# use predict() with type = "response" to compute predicted probabilities. 
logit.reg.pred <- predict(logit.reg, valid.df[, -10], type = "response")

data.frame(actual = airbnb.df$pricecat[1:10], predicted = logit.reg.pred[1:10])


logit.reg.pred.train <- predict(logit.reg,train.df[,-10],type = "response")
confusionMatrix(as.factor(1 * (logit.reg.pred.train > 0.5)),as.factor(train.df$pricecat))
logit.reg.pred.valid <- predict(logit.reg, valid.df[, -10], type = "response")
confusionMatrix(as.factor(1 * (logit.reg.pred.valid > 0.5)), as.factor(valid.df$pricecat))



#ROC for Logistic Regression:

r <- roc(valid.df$pricecat,logit.reg.pred.valid)
plot.roc(r)
# compute auc
auc(r)

############ Multinomial Regression################

airbnb.df<- read.csv("Airbnb.csv")
airbnb.df<-airbnb.df[,-c(1,2,3,4,13,15)]


airbnb.df$pricecat<-cut(airbnb.df$price, breaks=c(0,100,200,Inf), labels=c("between 1-100","between 100-200","Above 200"))
airbnb.df$pricecat<- as.factor(airbnb.df$pricecat)
airbnb.df$neighbourhood_group<- as.factor(airbnb.df$neighbourhood_group)
airbnb.df$room_type<- as.factor(airbnb.df$room_type)


airbnb.df$reviews_per_month[is.na(airbnb.df$reviews_per_month)] <- 0



library(dplyr)
train.df <-airbnb.df %>% dplyr::filter(row_number() %% 2 == 0) #Select even rows
valid.df<-airbnb.df %>% dplyr::filter(row_number() %% 2 == 1) #Select odd rows

library(nnet)
test<-multinom(pricecat ~ neighbourhood_group + room_type + number_of_reviews + availability_365,data=train.df)

summary(test)
head(pp<-fitted(test))

#################Decision Tree###################

airbnb.df<- read.csv("Airbnb.csv")
airbnb.df<-airbnb.df[,-c(1,2,3,4,13,15)]


airbnb.df$pricecat<-cut(airbnb.df$price, breaks=c(0,150,Inf), labels=c("Below Average","Above Average"))
airbnb.df$pricecat<- as.factor(airbnb.df$pricecat)
airbnb.df$pricecat<-factor(airbnb.df$pricecat,levels=c("Below Average","Above Average"),labels = c(0,1),ordered = T)


#replacing reviews per month with 0
airbnb.df$reviews_per_month[is.na(airbnb.df$reviews_per_month)] <- 0


airbnb.df$neighbourhood_group<- as.factor(airbnb.df$neighbourhood_group)
airbnb.df$room_type<- as.factor(airbnb.df$room_type)

#removing price,neighbourhood and longitude,reviews per month
airbnb.df <- airbnb.df[,-c(2,6,9)]

#Partitioning the dataset
library(dplyr)
train.df <-airbnb.df %>% dplyr::filter(row_number() %% 2 == 0) #Select even rows
valid.df<-airbnb.df %>% dplyr::filter(row_number() %% 2 == 1) #Select odd rows


#Implementing a decision tree 
default.ct <- rpart(pricecat ~ ., data = train.df ,method = "class")
prp(default.ct, type = 1, extra = 2, under = TRUE, split.font = 1, varlen = -10)
length(default.ct$frame$var[default.ct$frame$var == "<leaf>"])



# classify records in the validation data.
# set argument type = "class" in predict() to generate predicted class membership.
default.ct.point.pred.train <- predict(default.ct,train.df,type = "class")
# generate confusion matrix for training data
confusionMatrix(default.ct.point.pred.train, as.factor(train.df$pricecat))

# generate confusion matrix for Validation data
default.ct.point.pred.valid <- predict(default.ct,valid.df,type = "class")
confusionMatrix(default.ct.point.pred.valid, as.factor(valid.df$pricecat))

##Implementing a decision tree with maxdepth
default.ct.maxdep <- rpart(pricecat ~ ., data = train.df ,control = rpart.control(maxdepth = 2),method = "class")
prp(default.ct.maxdep, type = 1, extra = 2, under = TRUE, split.font = 1, varlen = -10)

library(pROC)
# ROC for Decision Tree:

r <- roc(valid.df$pricecat,as.integer(default.ct.point.pred.valid))
plot.roc(r)
# compute auc
auc(r)


########3#######Boosted Trees################

library(adabag)
library(rpart)
library(caret)
boost <- boosting(pricecat~., data = ifelse(is.na(train.df),0,train.df))
pred <- predict(boost.boston, valid.df)
confusionMatrix(pred$class, valid.df$pricecat)

ifelse(is.na(train.df),0,train.df)

library(rpart)
library(gbm)

boost.boston = gbm(ifelse(is.na(pricecat),0,pricecat)~., data = train.df, distribution = "gaussian", n.trees = 10000, shrinkage = 0.01, interaction.depth = 4)
summary(boost.boston)

n.trees = seq(from = 100, to = 10000, by = 100)
predmat = predict(boost.boston, newdata = valid.df, n.trees = n.trees)
boost.err = with(valid.df, apply( (predmat - pricecat)^2, 2, mean) )
plot(n.trees, boost.err, pch = 23, ylab = "Mean Squared Error", xlab = "# Trees", main = "Boosting Test Error")
abline(h = min(test.err), col = "red")

confusionMatrix(predmat, valid.df$pricecat)

plot(boost.boston,i="lstat")
plot(boost.boston,i="rm")
###########################################


