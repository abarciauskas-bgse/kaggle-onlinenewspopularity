if (!require('randomForest')) install.packages('randomForest')
source('setup.R')
# arguments:
#   - nforests - optional, number of forests in the ensemble
#   - params - optional, arguments to pass to the forest (TODO: may want to add some way to do this randomly)
#   - train.data - training data
#   - test.data - testing data
#   - seed - optional
# return:
#   - all.preds
#   - ensemble.preds
#   - accuracy
#   - seed
# a function which creates an ensemble random forests using a list of parameters
#
#
ensemble.forest <- function(
  nforests = 10,
  params = list(),
  seed = 1402,
  train.data,
  test.data,
  random.classwts = FALSE) {


  set.seed(seed)
  # FIXME: should be argument
  train.portion <- 0.5
  test.data.x <- test.data[,setdiff(colnames(test.data), 'popularity')]
  test.data.y <- test.data[,'popularity']

  # init matrix to store predictions
  all.preds <- matrix(NA, nrow = length(test.data.y), ncol = nforests)
  all.weights <- matrix(NA, nrow = 5, ncol = nforests)
  for (test.idx in 1:nforests) {
    train.idcs <- sample(1:nrow(train.data), round(nrow(train.data)*train.portion))
    train.data.x <- train.data[train.idcs,setdiff(colnames(train.data), 'popularity')]
    train.data.y <- train.data[train.idcs,'popularity']
    print(paste('training data:', head(train.idcs)))
    print(paste0('starting test: ', test.idx))
    # create a forest
    classwts <- params$classwt
    if (random.classwts == TRUE) {
      # although fyi this need not sum up to 1
      rand.wts <- runif(5)
      classwts <- rand.wts/sum(rand.wts)
    }
    print(paste0('classwts:', classwts))

    forest <- NA
    if (length(params) > 0) {
      # FIXME: params is not really an option here
      forest <- randomForest(train.data.x, train.data.y, classwt = classwts)
    } else {
      forest <- randomForest(train.data.x, train.data.y)
    }
    # make and store predictions
    preds <- predict(forest, test.data.x)
    ts <- as.numeric(Sys.time())
    filename <- paste0('preds',ts,'.csv')
    print(paste('writing file:', filename))
    write.csv(preds, filename, row.names = FALSE)
    all.preds[,test.idx] <- preds
    if (!is.null(classwts)) all.weights[,test.idx] <- classwts
  }

  # return accuracy for each odd number of forest sizes
  sub.forest.sizes <- seq(3,nforests,2)
  sub.forest.accuracies <- rep(NA, length(sub.forest.sizes))
  for (i in 1:length(sub.forest.sizes)) {
    forest.size <- sub.forest.sizes[i]
    # get accuracy for first forest.size forests
    sub.ensemble.preds <- apply(all.preds[,1:forest.size], 1, my.mode)
    sub.ensemble.accuracy <- success.rate(sub.ensemble.preds, test.data.y)
    print(paste0('accuracy for ensemble size:', forest.size, ' is ', sub.ensemble.accuracy))
    sub.forest.accuracies[i] <- sub.ensemble.accuracy
  }
  sub.ensembles.mat <- cbind(sub.forest.sizes, sub.forest.accuracies)
  
  # make ensemble predictions
  ensemble.preds <- apply(all.preds, 1, my.mode)
  accuracy <- success.rate(ensemble.preds, test.data.y)
  return(list(
    all.preds = all.preds,
    all.weights = all.weights,
    ensemble.preds = ensemble.preds,
    sub.ensemble.results = sub.ensembles.mat,
    accuracy = accuracy,
    seed = seed))
}
