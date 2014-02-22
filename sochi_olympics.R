library("ggplot2", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")
library("dplyr", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")
library("reshape2", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")
setwd("~/Dropbox/Code/sochi/kimono/automated")
df_athletes <- read.csv("~/Dropbox/Code/sochi/kimono/automated/df_athletes.csv")
df_athletes %.% group_by(medal_winner) %.% summarise(ma = mean(age))
df_hist_by_year <- read.csv("~/Dropbox/Code/sochi/kimono/automated/df_hist_by_year.csv")
train <- df_athletes[,c('age','height','weight')]
train$country_id <- as.numeric(df_athletes$country_id)
train$gender <- as.numeric(df_athletes$gender)
library("randomForest", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")
train$medal_winner <- df_athletes$medal_winner
train %.% group_by(gender) %.% summarise(ah = mean(height, na.rm = TRUE))
na_heights <- is.na(train$height)
train[na_heights & (train$gender == 1),'height'] <- 1.67
train[na_heights & (train$gender == 2),'height'] <- 1.81
pred_weight <- predict(weight_height, newdata = data.frame(height = train[is.na(train$weight),'height']))
na_weights <- is.na(train$weight)
train[na_weights,'weight'] <- pred_weight
medal_model <- randomForest(medal_winner ~ ., data = train, importance=TRUE)
ggplot(df_hist_by_year %.% group_by(year) %.% summarise(total_medals = sum(total)), aes(x = year, y = total_medals)) + geom_point() + stat_smooth()
