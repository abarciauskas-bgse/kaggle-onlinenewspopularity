# RANDOM FOREST
# This will attempt to de-outlier the random forest. Plan:
# The ongoing problem is that 3s and 1s are being under-classified. We have about 77%
# accuracy on 2s, whereas 3s and 1s are correctly classified 8% and 45% of the time respectively.
# What shall we do about this? 
# For each sample, the code will run 5 times. Each of those times, the predictions will
# be captured. What I'm hoping is that we'll find something like, 'actual 3s are likely
# to be predicted as 3 2/3 of the time, while 2s are predicted as 3s 1/3 of the time'. 
# If this is the case, we an simply run the random forest 9 times and classify as 3s
# anything that is predicted as 3 more than 1/2 the time. It's a kind of ad-hoc boosting,
# if you will. 
# Inshallah.
# It's also possible that pattern won't turn up - let's hope it does.

library(randomForest)
filepath <- '/home/beeb/Documents/Data_Science/News Competition/OnlineNewsPopularity'
setwd(filepath)
newspop <- read.csv('news_popularity_training.csv')

#Create binary from y variable
for(t in sort(unique(newspop[,'popularity']))) {
  newspop[paste("pop",t,sep="")] <- ifelse( newspop[,'popularity'] == t , 1 , 0 )
}

# Now do the same thing but with the engineered data
source('../../kaggle-onlinenewspopularity/Feature engineering/feature engineering v2-3.R')
newdata$popularity <- as.factor(newdata$popularity)
newspop <- newspop[,4:ncol(newspop)]
reps <- 20
success.rate <- rep(NA, reps)
success.rate2 <- rep(NA, reps)



training.sample <- sample(nrow(newspop), 0.8*nrow(newspop))
newspop.train <- newspop[training.sample,]
newspop.test <- newspop[setdiff(1:nrow(newspop), training.sample),]
prediction.frame <- data.frame(newspop.test$popularity)

for(i in 1:reps) {
newspop.train$popularity <- as.factor(newspop.train$popularity)

newtree <- randomForest(popularity ~ ., data = newspop.train[,1:59])

# it is so random and forestyyyyyyyyyyyyyy

# omg takes nearly as long as the knn algo

# foreeeeeeeeeeeeeest
#now what do I do?!
random.predictions <- predict(newtree, newdata = newspop.test)
success.rate[i] <- length(which(random.predictions==newspop.test$popularity))/nrow(newspop.test)
prediction.frame[i+1] <- random.predictions
}

# let's do some things
# These tables show misclassification stats
for(i in 2:ncol(prediction.frame)) {
  t <- as.matrix(table(unlist(prediction.frame[1]), unlist(prediction.frame[i])))
  t <- cbind(t, rowSums(t))
  keeptrack <- rep(NA, 5)
  for(k in 1:5) {
    keeptrack[k] <- t[k,k]/t[k,6]
  }
  t <- cbind(t, keeptrack)
  assign(paste0('t', i), t)
}

# This will give us a list of how many times 1s and 3s were listed as probably 1 or 3
prediction.frame$count.3s <- 0
prediction.frame$count.1s <- 0
for(i in 2:ncol(prediction.frame)) {
  prediction.frame$count.3s[prediction.frame[i] == 3] <- prediction.frame$count.3s[prediction.frame[i] == 3] + 1
  prediction.frame$count.1s[prediction.frame[i] ==1] <- prediction.frame$count.1s[prediction.frame[i] == 1] + 1
  }

# Now let's see if we can use this in a way to give us analytical leverage
table(filter(prediction.frame, count.1s>10)$newspop.test.popularity)
table(filter(prediction.frame, count.1s>10 & count.1s < 20)$newspop.test.popularity)
table(filter(prediction.frame, count.1s>10 & count.1s < 18)$newspop.test.popularity)
table(filter(prediction.frame, count.1s>10 & count.1s < 13)$newspop.test.popularity)

# This is very promising - basically, it says that if we run the random forest many
# times, then we can use that to boost the number of 1s we are classifying.
table(filter(prediction.frame, count.1s>5)$newspop.test.popularity)
table(filter(prediction.frame, count.1s>5 & count.1s < 15)$newspop.test.popularity)

# In fact - perhaps astonishingly - this is true for *any number over 0*
table(prediction.frame$newspop.test.popularity)
table(filter(prediction.frame, count.1s>0)$newspop.test.popularity)
# This could be A Thing.

# Now let's do the same with 3s.
table(filter(prediction.frame, count.3s>0)$newspop.test.popularity)
table(filter(prediction.frame, count.3s>3)$newspop.test.popularity)
table(filter(prediction.frame, count.3s>5)$newspop.test.popularity)
table(filter(prediction.frame, count.3s>10)$newspop.test.popularity)
table(filter(prediction.frame, count.3s>15)$newspop.test.popularity)
table(filter(prediction.frame, count.3s>18)$newspop.test.popularity)

# So it looks like around 5 is where the effect starts to kick in, but it's much
# smaller than the effect with 1s.

# The last thing to worry about is what to do with the numbers where count.1s>5 AND
# count.3s > 5
table(filter(prediction.frame, count.3s>3, count.1s > 0)$newspop.test.popularity)
table(filter(prediction.frame, count.3s>5, count.1s > 0)$newspop.test.popularity)
# There's basically none of them. Phew!

# OK, let's now just do the same thing with the proper submission data.
# PUT THE KETTLE ON. THE CODE MUST RUN.
test <- read.csv('news_popularity_test.csv')
sample <- read.csv('news_popularity_sample.csv')
final.predict <- predict(newtree, newdata = test)
sample$popularity <- final.predict
prediction.frame <- data.frame(rep(NA, nrow(test)))

for(i in 1:reps) {
  newspop$popularity <- as.factor(newspop$popularity)
  
  newtree <- randomForest(popularity ~ ., data = newspop[,1:59])
  
  random.predictions <- predict(newtree, newdata = test)
  prediction.frame[i+1] <- random.predictions
}

final.predict <- rep(2, nrow(test))
final.predict[prediction.frame$count.1s>1] <- 1
length(which(final.predict==1))
final.predict[prediction.frame$count.3s>5] <- 3
length(which(final.predict==1))
length(which(final.predict == 3))
final.predict[prediction.frame$count.3s>5 & prediction.frame$count.1s > 1] <- 2
# Now let's see if this matches with the observed frequencies of the 1s, 2s etc in
# the training data
table.final.predict <- as.matrix(table(final.predict))
table.final.predict <- cbind(table.final.predict, table.final.predict/nrow(test))
table.training <- as.matrix(table(newspop$popularity))
table.training <- cbind(table.training, table.training/nrow(newspop))
# Well, we still have too many 2s..... *but* we have increased the numbers of 1s and 3s
# in our predictions. The question is: have we chosen the correct ones to predict?
# Tune in next week....

sample$popularity <- final.predict
write.csv(sample, 'finalrforestextra.csv', row.names = FALSE)
