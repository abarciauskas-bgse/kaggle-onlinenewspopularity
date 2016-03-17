if (!require('assertthat')) install.packages('assertthat')

# Helper functions

# Function returns rates for n-fold cross validation
#
cross.val <- function(model.function, model.args = list(), data.train, no.subsets = 5) {
  assert_that(nrow(data.train) > 0)
  assert_that(mode(model.function) == 'function')
  assert_that(mode(model.args) == 'list')

  no.obs <- nrow(data.train)
  training <- data.train
  batch.size <- floor(no.obs/no.subsets)
  batches <- list()
  batch.pointer <- batch.size
  starting.position <- 1

  # break data into subsets
  for (batch.idx in 1:no.subsets) {
    batches[[batch.idx]] <- training[starting.position:batch.pointer,]
    starting.position <- starting.position + batch.size
    batch.pointer <- batch.pointer + batch.size
  }
  
  rates <- c()
  # rolling window
  for (test.batch.idx in 1:no.subsets) {
    print(paste0('Processing batch: ', test.batch.idx))
    # join the training subsets
    train.batch.idcs <- setdiff(1:no.subsets,test.batch.idx)
    training <- batches[[train.batch.idcs[1]]]
    lapply(train.batch.idcs[2:length(train.batch.idcs)], function(idx) {
      training <- rbind(training, batches[[idx]])
    })
    
    test <- batches[[test.batch.idx]]
    test.x <- test[,1:(ncol(test)-1)]
    test.y <- test[,'popularity']
    
    # train a model on training data
    if (deparse(substitute(model.function)) == "xgb.train") {
      model.args[['data']] <- xgb.DMatrix(training[,1:59], label = training[,'popularity']-1)
    }
    model <- do.call(model.function, model.args)
    # predict the last subset
    preds <- predict(model, test.x)
    if (deparse(substitute(model.function)) == "xgb.train") {
      preds <- preds + 1
    }
    sr <- success.rate(preds, test.y)
    print(paste0('Success rate: ', as.numeric(sr)))
    rates <- append(rates, as.numeric(sr))
  }
  return(list(rates = rates, mean.rate = mean(rates)))
}

# Example with multinom
# training <- data.frame(data.train)
# rates.1 <- cross.val(
#   model = multinom,
#   model.args = list(
#     formula = popularity ~ .,
#     data = training),
#   data = training)
# rates.1
# $rates
# [1] 0.4941667 0.4810417 0.4658333 0.4770833 0.4935417

# $mean.rate
# [1] 0.4823333

# Function which takes `predictions` as vector of predictions
# And `actual` as vector of actual labels
# And returns a rate of success
#
success.rate <- function(predictions, actual) {
  assert_that(length(predictions) == length(actual))
  errors <- 0
  # count number of mistakes
  for (i in 1:length(actual)) {
    if (predictions[i] != actual[i]) {
      errors <- errors + 1
    }
  }
  print(paste0('Number of errors: ', errors))
  rate <- 1 - errors/length(actual)
  print(paste0('Rate: ', rate))
  return (1 - errors/length(actual))
}

# Only works if model response to devience
#
loglik <- function(model) {
  # deviance = -2 log likelihoods
  -model$deviance/2
}

my.mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
