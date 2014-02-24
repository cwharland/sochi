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
df_total_years <- df_total_years[order(df_total_years[,1]), ]
df_total_years$cum_total <- cumsum(df_total_years$total_medals)
df_total_years$cum_frac <- df_total_years$cum_total / sum(df_total_years$total_medals)
df_total_years$medal_frac <- df_total_years$total_medals / max(df_total_years$total_medals)
df_country_stats <- df_hist_by_year %.% group_by(country) %.% summarise(start_year = min(year), total_medals = sum(total))
df_hist_by_year <- inner_join(df_hist_by_year, df_total_years[,c('year','medal_frac', 'total_medals')])
df_hist_by_year$adj_total <- df_hist_by_year$total / df_hist_by_year$medal_frac
df_hist_by_year$total_frac <- df_hist_by_year$total / df_hist_by_year$total_medals

long_countries <- df_country_stats[(df_country_stats$total_medals >= 120) & (df_country_stats$start_year <=1950), 'country']
country_mask <- df_hist_by_year$country %in% long_countries
ggplot(df_hist_by_year[country_mask, ], aes(x = year, y = total, color = country)) + stat_smooth(se=FALSE, size=1.3)
ggplot(df_hist_by_year[country_mask, ], aes(x = year, y = adj_total, color = country)) + stat_smooth(se=FALSE, size=1.3)
ggplot(df_hist_by_year[country_mask, ], aes(x = year, y = total_frac, color = country)) + stat_smooth(se=FALSE, size=1.3)
