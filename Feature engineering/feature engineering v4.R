# Load in data
filepath <- '~/Projects/kaggle-onlinenewspopularity/data/'
setwd(filepath)

if (!require('nnet')) install.packages('nnet')
library(nnet)
if (!require('gbm')) install.packages('gbm')
library(gbm)

newspop <- read.csv('news_popularity_training.csv')
# trying to limit differentiation of 4's and 5's
newspop$popularity.grouped <- ifelse(newspop$popularity %in% c(3,4,5), 3, newspop$popularity)
newspop <- newspop[,setdiff(colnames(newspop),c('url','id'))]

names <- names(newspop)
# from the 328_report
highest.fisher.vars <- c('kw_avg_avg','LDA_02','data_channel_is_world','is_weekend','data_channel_is_socmed','weekday_is_saturday',
                         'LDA_04','data_channel_is_entertainment','data_channel_is_tech','kw_max_avg','weekday_is_sunday','LDA_04',
                         'num_hrefs','global_subjectivity','kw_min_avg','global_sentiment_polarity','rate_negative_words','kw_min_min',
                         'title_subjectivity','LDA_01')

good.interactions <- matrix(ncol = 3)
colnames(good.interactions) <- c('first.var','second.var','rate')
# Check one variable from each group
# (at random, maybe expand later)
# and see if it's interaction with a variable from another group has predictive power
for (first.var.idx in 1:length(highest.fisher.vars)) {
  for (second.var.idx in 1:length(highest.fisher.vars)) {
    if (!(first.var.idx == second.var.idx)) {
      first.var.name <- highest.fisher.vars[first.var.idx]
      second.var.name <- highest.fisher.vars[second.var.idx]
      x <- newspop[,first.var.name]*newspop[,second.var.name]
      x <- data.frame(x, newspop[,first.var.name], newspop[,second.var.name])
      y=newspop$popularity.grouped
      model <- multinom(y~as.matrix(x))
      preds <- predict(model)
      summary(preds)
      rate <- success.rate(preds, y)
      # If success rate is > all 2's, keep the interaction around
      if (rate > (table(newspop$popularity)[[2]]/nrow(newspop)+0.001)) {
        print('success!')
        good.interactions <- rbind(good.interactions, c(first.var.name, second.var.name, rate))
      }
    }
  }
}

# Using the good interactions, add them as variables to the x
# because simple things are hard
(good.interactions <- good.interactions[2:nrow(good.interactions),])
write.csv(unique(good.interactions), 'good-interactions-fisher.csv', row.names = FALSE)

good.interactions <- read.csv('good-interactions-fisher.csv', stringsAsFactors = FALSE)
# Get the unique set (there may be duplicates, which should be fixed in the loop above)
good.interactions.set <- unique(good.interactions[,1:2])
nrow(good.interactions.set)

# add an interaction term to the training data
for (pair.idx in 1:nrow(good.interactions.set)) {
  first.var.data <- good.interactions.set[pair.idx,1]
  second.var.data <- good.interactions.set[pair.idx,2]
  newspop[,paste(good.interactions.set[pair.idx,],collapse = '*')] <- newspop[,first.var.data]*newspop[,second.var.data]
}

y <- newspop$popularity.grouped
x <- newspop[,setdiff(colnames(newspop),c('popularity','popularity.grouped'))]

gbm1 <- gbm.fit(x = x, y = y,
                distribution = 'multinomial',
                n.trees = 200,
                shrinkage = 0.1)

best.iter <- gbm.perf(gbm1,method="OOB")
print(best.iter)
preds <- apply(predict(gbm1, x, best.iter), 1, which.max)
summary(preds)
success.rate(preds, y)
# with 1,2,3,(4,5) --> 0.5202 # only a marginal improvement
# with 1,2,(3,4,5) --> 0.5328
# randomly assign 3,4,5 as predictions on 3's and see if rate improves
total.345 <- sum(table(newspop$popularity)[3:5])
rate.3s <- table(newspop$popularity)[3]/total.345
rate.4s <- table(newspop$popularity)[4]/total.345
rate.5s <- table(newspop$popularity)[5]/total.345

rands <- t(rmultinom(total.345, size = 1, prob=c(rate.3s,rate.4s,rate.5s)))
rands <- max.col(rands) + 2
preds.alt <- c()
rands.idx <- 1
for (i in 1:length(preds)) {
  if (preds[i] == 3) {
    preds.alt[i] <- rands[rands.idx]
    rands.idx <- rands.idx + 1
  } else {
    preds.alt[i] <- preds[i]
  }
}
success.rate(preds.alt, y)
# NO GOOD --> 0.5244

# Now we need to try this stuff on the full set
# See where we are getting things wrong
# for every prediction we get wrong, see how we predict instead
# Or predict 1,2,3,(4,5)?
mistakes <- matrix(ncol=2)
colnames(mistakes) <- c('prediction','actual')
for (i in 1:length(preds)) {
  if (!(preds[i] == y[i])) {
    mistakes <- rbind(mistakes, c(preds[i],y[i]))
  }
}
counts <- table(mistakes[,'prediction'], mistakes[,'actual'])
barplot(counts, main="Prediction vs Actual",
        xlab="Actual", col=c("green","darkblue","orange","pink"),
        legend = rownames(counts), beside=TRUE)


data.test <- read.csv('news_popularity_test.csv')
# remove id and url
x.test <- data.test[,3:ncol(data.test)]
preds <- apply(predict(gbm1, x.test, best.iter), 1, which.max)
summary(preds)
predictions <- cbind(id=data.test[,'id'], popularity=preds)
head(predictions)
write.csv(predictions, '02022016-2-predictions.csv', row.names = FALSE)
