# create 3 random forests.2 and see if the predictions improve
fiftypct.rand.idcs <- sample(1:nrow(data), nrow(data)/2)
train.data <- data[fiftypct.rand.idcs,]
test.data <- data[-fiftypct.rand.idcs,]
train.x <- train.data[,setdiff(colnames(train.data), 'popularity')]
train.y <- train.data[,'popularity']
test.x <- test.data[,setdiff(colnames(test.data), 'popularity')]
test.y <- test.data[,'popularity']

forests.2 <- list()
nforests.2 <- 6
for (fi in 1:nforests.2) {
  forests.2[[fi]] <- randomForest(train.x, train.y, mtry = 2, importance = TRUE)
}

# apparently you can also do something like rf.all <- combine(rf1, rf2, rf3)

# check individual predictive accuracy
ensemble.preds <- lapply(forests.2, function(mod) {
  return(predict(mod, test.x))
})

ensemble.preds <- do.call(cbind, ensemble.preds)

for (fi in 1:nforests.2) {
  print(success.rate(ensemble.preds[,fi], test.y))
}

ensemble.preds.gathered <- apply(ensemble.preds, 1, my.mode)
success.rate(ensemble.preds.gathered, test.y)
# 0.5191333 w/o importance

# should keep importance?
  

