####################################################################
# Produce48 Ranking Prediction with Machine Learning               #
# Created By Bigdata From Dcinside                                 #
####################################################################

install.packages("readxl")
install.packages("writexl")
install.packages("dplyr")
install.packages("data.table")
install.packages("car")
install.packages("gvlma")
install.packages("lm.beta")
install.packages("lmtest")
install.packages("magrittr")
install.packages("tidyverse")
install.packages("caret")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("randomForest")
install.packages("ggplot2")
install.packages("ggthemes")
install.packages("gplots")
install.packages("knitr")
install.packages("sqldf")
install.packages("e1071")
library(readxl)
library(writexl)
library(dplyr)
library(data.table)
library(car)
library(gvlma)
library(lm.beta)
library(lmtest)
library(magrittr)
library(tidyverse)
library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)
library(ggplot2)
library(ggthemes)
library(gplots)
library(knitr)
library(sqldf)
library(e1071)

setwd(dir = "c:/Users/eljuw/Desktop/")


s1 <- readxl::read_excel(path = "Produce.xlsx", sheet = 1, col_names = TRUE)
s2 <-  readxl::read_excel(path = "Produce.xlsx", sheet = 2, col_names = TRUE)
s3 <-  readxl::read_excel(path = "Produce.xlsx", sheet = 3, col_names = TRUE)

str(s1)

s1 <- data.table(s1, key = "Name")
s2 <- data.table(s2, key = "Name")
s3 <- data.table(s3, key = "Name")

lm.produce.s1 <- lm(Final ~ . -Name, data = s1)
lm.produce.s1.f <- step(lm.produce.s1, direction = "forward")
lm.produce.s1.b <- step(lm.produce.s1, direction = "backward")
lm.produce.s1.s <- step(lm.produce.s1, direction = "both")

AIC(lm.produce.s1)
AIC(lm.produce.s1.f)
AIC(lm.produce.s1.b)
AIC(lm.produce.s1.s)

summary(lm.produce.s1.s)

produce.train <- readxl::read_excel(path = "Produce.xlsx", sheet = 5, col_names = TRUE)
produce.test <- readxl::read_excel(path = "Produce.xlsx", sheet = 6, col_names = TRUE)

produce.train$Final <- as.numeric(produce.train$Final)
produce.test$Final <- as.factor(produce.test$Final)

produce.tree <- rpart(Final ~., method="class", data = produce.train)

rpart.plot(produce.tree)

pred <- predict(produce.tree, produce.test, type = "class")
summary(pred)
table(pred, produce.test$Final)

View(produce.train)
str(produce.train)

produce.lm <- lm(Final ~ ., data = produce.train)
lm.produce <- step(produce.lm, direction = "both")

summary(lm.produce)

predicted <-  predict(lm.produce,produce.test) 
summary(predicted)

produce.svm <- svm(Final ~ ., data = produce.train)
svm.pred <- predict(produce.tree, produce.test)
table(svm.pred)


produce.rf <- randomForest::randomForest(Final ~. ,data = produce.train, ntree = 300, importance = TRUE)
summary(rf.pred)
rf.pred <- predict(produce.rf, produce.test)

table(rf.pred)
