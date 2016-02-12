setwd('~/Projects/kaggle-onlinenewspopularity/data/')
data.train <- read.csv('news_popularity_training.csv')
nobs <- nrow(data.train)

# Remove id and url
data.train <- data.train[,3:ncol(data.train)]

# Use cross validation on multinom from nnet
no.obs <- nrow(data.train)
no.subsets <- 5
batch.size <- floor(no.obs/no.subsets)
batches <- list()
batch.pointer <- batch.size
starting.position <- 1

# break data into 5 subsets
for (batch.idx in 1:no.subsets) {
  batches[[batch.idx]] <- data.train[starting.position:batch.pointer,]
  starting.position <- starting.position + batch.size
  batch.pointer <- batch.pointer + batch.size
}

# Function returns rates for n-fold cross validation
# TODO: add arguments for number of folds and data
#
cross.val <- function(model.function, model.args) {
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
    test.y <- test[,'popularity']
    
    # train a model on training data
    model <- do.call(model.function, model.args)
    # predict the last subset
    preds <- predict(model, test.x)
    rates <- append(rates, success.rate(preds, test.y))
  }
  return(list(rates = rates, mean.rate = mean(rates)))
}

rates.1 <- cross.val(
  model = multinom,
  model.args = list(
    formula = popularity ~ .,
    data = training))
rates
# [1] 0.5021667 0.4950000 0.4743333 0.4935000 0.4966667

rates.2 <- cross.val(model = multinom, 
          model.args = list(
            formula = popularity ~ .,
            data = training,
            entropy = TRUE))
rates.2
# no diff

rates.3 <- cross.val(model = multinom, 
                     model.args = list(
                       formula = popularity ~ .,
                       data = training,
                       maxit = 200, # defaults to 100
                       entropy = TRUE))
rates.3
# > rates.3
# [1] 0.5230 0.5090 0.4905 0.5025 0.5130


rates.4 <- cross.val(model = multinom, 
                     model.args = list(
                       formula = popularity ~ .,
                       data = training,
                       maxit = 300))
rates.4
# > rates.4
# [1] 0.5215000 0.5083333 0.4928333 0.5055000 0.5136667

rates.5 <- cross.val(model = multinom, 
                     model.args = list(
                       formula = popularity ~ .,
                       data = training,
                       maxit = 200,
                       decay = 1)) # defaults to 0
rates.5
# > rates.5
# [1] 0.5221667 0.5098333 0.4906667 0.4991667 0.5133333

rates.6 <- cross.val(model = multinom, 
                     model.args = list(
                       formula = popularity ~ .,
                       data = training,
                       maxit = 200,
                       decay = 5)) # defaults to 0
rates.6
# > rates.6
# [1] 0.5143333 0.5031667 0.4790000 0.4990000 0.5035000

# Switching to gbm
m1 <- gbm(
  popularity ~ .,
  data = data.train,
  distribution = 'multinomial',
  n.trees = 2000,
  cv.folds = 5)
best.iter <- gbm.perf(m1,method="OOB")
best.iter
preds <- apply(m1$cv.fitted, 1, which.max)
success.rate(data.train[,'popularity'], preds)
# this never terminated

n.trees = 2000
m2 <- gbm(popularity ~ .,
          data = data.train,
          distribution = 'multinomial',
          n.trees = n.trees,
          shrinkage = 0.1)
best.iter <- gbm.perf(m2,method="OOB")
print(best.iter)
preds <- apply(m1$cv.fitted, 1, which.max)
summary(preds)
success.rate(data.train[,'popularity'], preds)
# garbage!

(res <- cross.val(
  model = multinom,
  model.args = list(
    formula = popularity ~ .,
    data = data.train,
    maxit = 1000)))



