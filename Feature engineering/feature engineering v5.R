# Steps:
# 1. Engineer features on 1,2,3 and use to predict 1,2,(3,4,5)
#
# 2. Engineer features on 3,4,5 train model using them on 3,4,5
#
# 3. When doing predictions, predict using model from (1) gbm.lower
# 
# 4. With predictions from (3), use model from (2) gbm.upper to re-predict 3's as 3-5
#

# SOME BASELINE RESULTS FOR COMPARISON
# y <- newspop$popularity.grouped
# x <- newspop[,setdiff(colnames(newspop),c('popularity','popularity.grouped'))]

# gbm1 <- gbm.fit(x = x, y = y,
#                 distribution = 'multinomial',
#                 n.trees = 200,
#                 shrinkage = 0.1)
# 
# best.iter <- gbm.perf(gbm1,method="OOB")
# print(best.iter)
# preds <- apply(predict(gbm1, x, best.iter), 1, which.max)
# summary(preds)
# success.rate(preds, y)
# Before extra vars, ungrouped 1,2,3,4,5: 0.51788
# Before extra vars, grouped: 1,2,(3,4,5): 0.5296
# 

## SETUP
filepath <- '~/Projects/kaggle-onlinenewspopularity/data/'
setwd(filepath)
if (!require('nnet')) install.packages('nnet')
library(nnet)
if (!require('gbm')) install.packages('gbm')
library(gbm)
source('../explorations/setup.R')

newspop <- read.csv('news_popularity_training.csv')
newspop <- newspop[,setdiff(colnames(newspop),c('url','id'))]

## STEP 1:
# split data into 1,2,(3,4,5)
# (MAYBE SHOULD TRY THIS WITH JUST 1,2,3?)
newspop$popularity.grouped <- ifelse(newspop$popularity %in% c(3,4,5), 3, newspop$popularity)

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

good.interactions <- good.interactions[2:nrow(good.interactions),]
nrow(good.interactions)
good.interactions.set <- unique(t(apply(good.interactions, 1, sort)))
nrow(good.interactions.set)

# save them to (new file) may look for more random interactions later
file.prefix <- 'good-interactions-12(345)-'
write.csv(good.interactions.set, paste0(file.prefix,as.numeric(Sys.time()),'.csv'), row.names = FALSE)

# Read all good interactions files
(filenames <- Sys.glob(paste0(file.prefix,"*")))
good.interactions.lower <- matrix(ncol=2)
colnames(good.interactions.lower) <- c('v1','v2')
for (i in 1:length(filenames)) {
  terms <- read.csv(filenames[i], stringsAsFactors = FALSE)[,1:2]
  colnames(terms) <- c('v1','v2')
  good.interactions.lower <- rbind(good.interactions.lower, terms)
}

good.interactions.lower <- na.omit(good.interactions.lower)
(good.interactions.lower.set <- unique(t(apply(good.interactions.lower, 1, sort))))

# add an interaction term to the training data
for (pair.idx in 1:nrow(good.interactions.set)) {
  first.var.data <- good.interactions.set[pair.idx,1]
  second.var.data <- good.interactions.set[pair.idx,2]
  newspop[,paste(good.interactions.set[pair.idx,],collapse = '*')] <- newspop[,first.var.data]*newspop[,second.var.data]
}

# Add interaction terms from fisher scoring
good.interactions.fisher <- read.csv('good-interactions-fisher.csv', stringsAsFactors = FALSE)[,1:2]
good.interactions.fisher <- unique(t(apply(good.interactions.fisher, 1, sort)))

# add an interaction term to the training data
for (pair.idx in 1:nrow(good.interactions.fisher)) {
  first.var.data <- good.interactions.fisher[pair.idx,1]
  second.var.data <- good.interactions.fisher[pair.idx,2]
  newspop[,paste(good.interactions.fisher[pair.idx,],collapse = '*')] <- newspop[,first.var.data]*newspop[,second.var.data]
}

y <- newspop$popularity.grouped
x <- newspop[,setdiff(colnames(newspop),c('popularity','popularity.grouped'))]

gbm.lower <- gbm.fit(x = x, y = y,
                distribution = 'multinomial',
                n.trees = 200,
                shrinkage = 0.1)

best.iter <- gbm.perf(gbm.lower, method="OOB")
print(best.iter)
preds <- apply(predict(gbm.lower, x, best.iter), 1, which.max)
summary(preds)
success.rate(preds, y)
# Rate predicting popularity with 136 vars: 0.51833
# Rate predicting popularity with 3,4,5 grouped with 136 vars: 0.5301667
# Added 3 more vars and success rate -> 0.531


## STEP 2:
# train just 3,4,5
#
newspop <- read.csv('news_popularity_training.csv')
newspop <- newspop[,setdiff(colnames(newspop),c('url','id'))]
newspop.upper.classes <- subset(newspop, popularity >= 3)

# Find good interactions for upper classes
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
      x <- newspop.upper.classes[,first.var.name]*newspop.upper.classes[,second.var.name]
      x <- data.frame(x, newspop.upper.classes[,first.var.name], newspop.upper.classes[,second.var.name])
      y = newspop.upper.classes$popularity
      model <- multinom(y~as.matrix(x))
      preds <- predict(model)
      summary(preds)
      rate <- success.rate(preds, y)
      print(rate)
      # If success rate is > all 3's, keep the interaction around
      if (rate > 0.8452205) {#(table(newspop.upper.classes$popularity)[[1]]/nrow(newspop.upper.classes)+0.001)) {
        print('success!')
        good.interactions <- rbind(good.interactions, c(first.var.name, second.var.name, rate))
      }
    }
  }
}

good.interactions <- na.omit(good.interactions)
nrow(good.interactions)
good.interactions.set <- unique(t(apply(good.interactions, 1, sort)))
nrow(good.interactions.set)

# save them to (new file) may look for more random interactions later
file.prefix <- 'good-interactions-345-'
write.csv(good.interactions.set, paste0(file.prefix,as.numeric(Sys.time()),'.csv'), row.names = FALSE)

# Read all good interactions files
(filenames <- Sys.glob(paste0(file.prefix,"*")))
good.interactions.upper <- matrix(ncol=2)
colnames(good.interactions.upper) <- c('v1','v2')
for (i in 1:length(filenames)) {
  terms <- read.csv(filenames[i], stringsAsFactors = FALSE)[,1:2]
  colnames(terms) <- c('v1','v2')
  good.interactions.upper <- rbind(good.interactions.upper, terms)
}

good.interactions.upper <- na.omit(good.interactions.upper)
good.interactions.upper.set <- unique(t(apply(good.interactions.upper, 1, sort)))

for (pair.idx in 1:nrow(good.interactions.set)) {
  first.var.data <- good.interactions.set[pair.idx,1]
  second.var.data <- good.interactions.set[pair.idx,2]
  newspop.upper.classes[,paste(good.interactions.set[pair.idx,],collapse = '*')] <- newspop.upper.classes[,first.var.data]*newspop.upper.classes[,second.var.data]
}

# Add in interactions from fisher
for (pair.idx in 1:nrow(good.interactions.fisher)) {
  first.var.data <- good.interactions.fisher[pair.idx,1]
  second.var.data <- good.interactions.fisher[pair.idx,2]
  newspop.upper.classes[,paste(good.interactions.fisher[pair.idx,],collapse = '*')] <- newspop.upper.classes[,first.var.data]*newspop.upper.classes[,second.var.data]
}

y <- newspop.upper.classes$popularity-2
x <- newspop.upper.classes[,setdiff(colnames(newspop.upper.classes),c('popularity','popularity.grouped'))]

gbm.upper <- gbm.fit(x = x, y = y,
                distribution = 'multinomial',
                n.trees = 1000,
                shrinkage = 0.4)

best.iter <- gbm.perf(gbm.upper, method="OOB")
print(best.iter)
preds <- apply(predict(gbm.upper, x, best.iter), 1, which.max)
summary(preds)
success.rate(preds, y)
# so far best result with shrinkage == 0.3, 1000 trees -> 0.8464043


# combine results from both
# Make predictions using gbm.lower
# take all predictions of 3 and make predictions using gbm.upper
# or ignore those greater than 3

# Add in all the interaction terms
good.interactions.all <- rbind(good.interactions.upper.set, good.interactions.lower.set, good.interactions.fisher)
good.interactions.all <- unique(t(apply(good.interactions.all, 1, sort)))

for (pair.idx in 1:nrow(good.interactions.all)) {
  first.var.data <- good.interactions.all[pair.idx,1]
  second.var.data <- good.interactions.all[pair.idx,2]
  newspop[,paste(good.interactions.all[pair.idx,],collapse = '*')] <- newspop[,first.var.data]*newspop[,second.var.data]
}


newspop$popularity.grouped <- ifelse(newspop$popularity %in% c(3,4,5), 3, newspop$popularity)
y <- newspop$popularity.grouped
x <- newspop[,setdiff(colnames(newspop),c('popularity','popularity.grouped'))]

gbm.all <- gbm.fit(x = x, y = y,
                     distribution = 'multinomial',
                     n.trees = 1000,
                     shrinkage = 0.1)

best.iter <- gbm.perf(gbm.all, method="OOB")
print(best.iter)
preds <- apply(predict(gbm.all, x, best.iter), 1, which.max)
summary(preds)
success.rate(preds, y)

# Take those predicted as 3's and predict them as 3,4,5
x.with.lower.preds <- cbind(x, lower.prediction = preds)
head(x.with.lower.preds)[,1:3]

# keep ids around for later use
ids <- read.csv('news_popularity_training.csv')[,1]
x.with.lower.preds <- cbind(id = ids, x.with.lower.preds)

x.upper <- subset(x.with.lower.preds, lower.prediction == 3)

# use gbm.upper to predict x.upper - 1
x.upper.features <- x.upper[,setdiff(colnames(x.upper),c('lower.prediction','id'))]

best.iter <- gbm.perf(gbm.upper, method="OOB")
print(best.iter)
preds <- apply(predict(gbm.upper, x.upper.features, 1000), 1, which.max)
summary(preds)
# doesn't do anything. so let's just use gbm.all?

data.test <- read.csv('news_popularity_test.csv')
# remove id and url
x.test <- data.test[,3:ncol(data.test)]
# add features
for (pair.idx in 1:nrow(good.interactions.all)) {
  first.var.data <- good.interactions.all[pair.idx,1]
  second.var.data <- good.interactions.all[pair.idx,2]
  x.test[,paste(good.interactions.all[pair.idx,],collapse = '*')] <- x.test[,first.var.data]*x.test[,second.var.data]
}

preds <- apply(predict(gbm.all, x.test, best.iter), 1, which.max)
summary(preds)
predictions <- cbind(id=data.test[,'id'], popularity=preds)
head(predictions)
write.csv(predictions, '02082016-1-predictions.csv', row.names = FALSE)
