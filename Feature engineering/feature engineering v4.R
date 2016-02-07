# Attempt 2: PCA-in-groups

# The point of this file:
# Clearly, some sort of variable selection must be done. Early attempts at PCA
# were, shall we say, less than promising. In this file, I use Zsuzsa's grouped
# variables to do a smaller PCA-in-groups.

# Load in data
filepath <- '~/Projects/kaggle-onlinenewspopularity/data/'
setwd(filepath)

if (!require('nnet')) install.packages('nnet')
library(nnet)
newspop <- read.csv('news_popularity_training.csv')
newspop <- newspop[,setdiff(colnames(newspop),c('url','id'))]

# Split these two into the groups Zsuzsa made
names <- names(newspop)

groups <- list()
groups[['words']] <- c('n_non_stop_words', 'n_unique_tokens', 'n_non_stop_unique_tokens')
groups[['tokens.title']] <- c('n_tokens_title')
groups[['tokens.content']] <- c('n_tokens_content')
groups[['hrefs']] <- c('num_hrefs','num_self_hrefs')
groups[['shares']] <- c('self_reference_min_shares', 'self_reference_max_shares', 'self_reference_avg_sharess')
groups[['imgs']] <- c('num_imgs')
groups[['videos']] <- c('num_videos')
groups[['weekend']] <- c('is_weekend','weekday_is_saturday','weekday_is_sunday')
groups[['weekday']] <- c('weekday_is_monday','weekday_is_tuesday','weekday_is_wednesday','weekday_is_thursday','weekday_is_friday')
# might come back to this
groups[['key']] <- names[12:27]
# and this
groups[['NLP']] <- names[39:59]

good.interactions <- matrix(ncol = 3)
colnames(good.interactions) <- c('first.var','second.var','rate')
# Check one variable from each group
# (at random, maybe expand later)
# and see if it's interaction with a variable from another group has predictive power
for (first.group.idx in 1:length(groups)) {
  for (second.group.idx in 1:length(groups)) {
    if (!(first.group.idx == second.group.idx)) {
      first.var.name <- sample(groups[[first.group.idx]],1)
      second.var.name <- sample(groups[[second.group.idx]],1)
      x <- newspop[,first.var.name]*newspop[,second.var.name]
      x <- data.frame(x, newspop[,first.var.name], newspop[,second.var.name])
      y=newspop$popularity
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
# Get the unique set (there may be duplicates, which should be fixed in the loop above)
good.interactions.set <- unique(good.interactions[,1:2])
# add an interaction term to the training data
for (pair.idx in 1:nrow(good.interactions.set)) {
  first.var.data <- good.interactions.set[pair.idx,1]
  second.var.data <- good.interactions.set[pair.idx,2]
  newspop[,paste(good.interactions.set[pair.idx,],collapse = '*')] <- newspop[,first.var.data]*newspop[,second.var.data]
}

y <- newspop$popularity
x <- newspop[,setdiff(colnames(newspop),c('popularity'))]

gbm1 <- gbm.fit(x = x, y = y,
                distribution = 'multinomial',
                n.trees = 200,
                shrinkage = 0.1)

best.iter <- gbm.perf(gbm1,method="OOB")
print(best.iter)
preds <- apply(predict(gbm1, x, 100), 1, which.max)
summary(preds)
success.rate(preds, y)
# (with only 1-3) 0.5387511!

# so now we have to do this with cross validation and tuning the shrinkage parameter
# I'm not sure how to get the built in cross-validation to work so doing it by hand
# Need to break up data into 5 sub-parts
# Train data on 4 sub-parts and predict on 1
data <- cbind(x,y)
no.obs <- nrow(data)
no.subsets <- 5
batch.size <- floor(no.obs/no.subsets)
batches <- list()
batch.pointer <- batch.size
starting.position <- 1

# break data into 5 subsets
for (batch.idx in 1:no.subsets) {
  batches[[batch.idx]] <- data[starting.position:batch.pointer,]
  starting.position <- starting.position + batch.size
  batch.pointer <- batch.pointer + batch.size
}

rates <- c()
for (test.batch.idx in 1:no.subsets) {
  # join the training subsets subsets
  train.batch.idcs <- setdiff(1:no.subsets,test.batch.idx)
  training <- batches[[train.batch.idcs[1]]]
  lapply(train.batch.idcs[2:length(train.batch.idcs)], function(idx) {
    training <<- rbind(training, batches[[idx]])
  })
  
  test <- batches[[test.batch.idx]]
  test.x <- test[,1:(ncol(test)-1)]
  test.y <- test[,'y']
  # train a model
  # predict the last subset
  
  gbm2 <- gbm.fit(x=training[,1:(ncol(training)-1)],y=training[,'y'],
                  distribution = 'multinomial',
                  n.trees = 200,
                  shrinkage = 0.1)
  
  best.iter <- gbm.perf(gbm2,method="OOB")
  preds <- apply(predict(gbm2, test.x, best.iter), 1, which.max)
  rates <- append(rates, success.rate(preds, test.y))
}

# Now to optimize over the shrinkage parameter...
# also need to do all of this with all 5 categories... :(
shrinkages <- append(c(0.001,0.005,0.01,0.05,0.1), seq(0.1,0.3,0.05))
shrinkage.rates <- matrix(NA, ncol=2)
for (shrinkage.idx in 1:length(shrinkages)) {
  shrinkage <- shrinkages[shrinkage.idx]
  gbm2 <- gbm.fit(x=training[,1:(ncol(training)-1)],y=training[,'y'],
                  distribution = 'multinomial',
                  n.trees = 200,
                  shrinkage = shrinkage)
  
  best.iter <- gbm.perf(gbm2,method="OOB")
  preds <- apply(predict(gbm2, test.x, best.iter), 1, which.max)
  shrinkage.rates <- rbind(shrinkage.rates, c(shrinkage, success.rate(preds, test.y)))
}

#It appears 0.1 is the best

# Now we need to try this stuff on the full set
# See where we are getting things wrong
# for every prediction we get wrong, see how we predict instead
# Or predict 1,2,3,(4,5)?
mistakes <- matrix(ncol=2)
colnames(mistakes) <- c('prediction','actual')
for (i in 1:length(preds)) {
  if (!(preds[i] == test.y[i])) {
    mistakes <- rbind(mistakes, c(preds[i],test.y[i]))
  }
}
counts <- table(mistakes[,'prediction'], mistakes[,'actual'])
barplot(counts, main="Prediction vs Actual",
        xlab="Actual", col=c("green","darkblue","orange"),
        legend = rownames(counts), beside=TRUE)
