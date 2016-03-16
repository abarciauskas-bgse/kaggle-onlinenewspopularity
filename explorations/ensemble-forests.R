source('2016-03-16-rf.R')

# now we have access to function ensemble.forest
# load the data and create training and test partitions
library(caret)
partition <- createDataPartition(data$popularity, times = 2, p = 0.20)
train.data <- data[partition$Resample1,]
test.data <- data[partition$Resample2,]

# try initial test with 3 forests (e.g. minimum ensemble since we need an odd number)
ensemble.three <- ensemble.forest(nforests = 3, train.data = train.data, test.data = test.data)

# try this for odd number of ensembles and see if accuracy improves
(ensemble.sizes <- seq(5,17,2))
# store results
size.results <- rep(NA, length(ensemble.sizes))
for (sidx in 1:length(ensemble.sizes)) {
  size <- ensemble.sizes[sidx]
  print(paste0('starting ensemble of size: ', size))
  ensemble <- ensemble.forest(nforests = size, train.data = train.data, test.data = test.data)
  size.results[sidx] <- ensemble$accuracy
}

size.results.mat <- cbind(ensemble.sizes, size.results)
write.csv(size.results.mat, 'datadir/rfsizes.csv')
# this was a failure

source('2016-03-16-rf.R')
# what if we try random class weights?
# also i just realized this was a silly way to do things, should add one forest at a time...
classwts <- rep(0.2, 5)
classwts.ensemble <- ensemble.forest(
  nforests = 17,
  train.data = train.data,
  test.data = test.data,
  params = list(classwt = classwts))
