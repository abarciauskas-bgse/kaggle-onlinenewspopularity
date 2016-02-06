# IN THIS FILE #
# The theory is that weekdays and weekends will experience very different effects.
# Therefore, we should include interaction terms for the weekday/weekend indicator.

filepath <- '/home/beeb/Documents/Data_Science/News Competition/OnlineNewsPopularity'
setwd(filepath)
library(reshape2)
library(ggplot2)
library(dplyr)
newspop <- read.csv('news_popularity_training.csv')
newspop <- newspop[,4:ncol(newspop)]
newspop$t <- as.factor(newspop$popularity)
for(class in 1:4) {
  newspop$class <- 0
  newspop$class[newspop$t==class] <- 1
  names(newspop)[length(names(newspop))] <- paste0('class', class)
}

weekday <- rep(7, nrow(newspop))
weekday[newspop$weekday_is_monday==1] <- 1
weekday[newspop$weekday_is_tuesday==1] <- 2
weekday[newspop$weekday_is_wednesday==1] <- 3
weekday[newspop$weekday_is_thursday==1] <- 4
weekday[newspop$weekday_is_friday==1] <- 5
weekday[newspop$weekday_is_saturday==1] <- 6

training.sample <- sample(nrow(newspop), 0.8*nrow(newspop))
newspop.train <- newspop[training.sample,]
newspop.train <- newspop.train[,setdiff(1:ncol(newspop.train), 
                                        grep('weekday', names(newspop)))]

coeff.matrix <- matrix(NA, nrow=51, ncol = 2)
for(i in c(1,0)) {
  current <- newspop.train[newspop.train$is_weekend==i, 1:50]
  current$t <- newspop.train$class2[newspop.train$is_weekend==i]
  cat('.')
  cat(nrow(current))
  model <- glm(t ~ ., data = current, family=binomial)
  coeff.matrix[,i+1] <- coef(model)
  assign(paste0('model', i), model)
}

coeff.names <- append('intercept', names(current)[1:length(names(current))-1])
rownames(coeff.matrix) <- coeff.names

deviations <- apply(coeff.matrix, 2, function(y) {
  return((max(y)-min(y))/min(y))
})
coeffs <- as.data.frame(coeff.matrix)
coeffs$id <- as.factor(coeff.names)
coeffs <- melt(coeffs)
coeffs <- na.omit(coeffs)
ggplot(data = coeffs, aes(y = value, fill = id, x = as.factor(variable))) + geom_bar(stat='identity')

# This is the most epic over-fitting ever.
# Run this line of code below in order to understand why this is happening:
filter(coeffs, abs(value)>100)

# In order to make the effects a bit more visible in the graph, we get rid of the 
# really wildly huge coefficients.
coeffs <- filter(coeffs, abs(value) < 200)
ggplot(data = coeffs, aes(y = value, fill = id, x = as.factor(variable))) + geom_bar(stat='identity')

# This is a complete mess. The problem is with the highly correlated variables - we're
# getting wildly large values on two different correlated variables that then cancel one
# another out. Zsuzsa's idea about pca-in-groups seems to be the appropriate way to handle
# this.