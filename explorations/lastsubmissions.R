source('2016-03-16-rf.R')

data.test <- read.csv('../data/news_popularity_test.csv')
# remove id and url
test.data <- data.test[,3:ncol(data.test)]
# just adding this here so I don't have to fix the function to deal with real test data
test.data[,'popularity'] <- rep(1, nrow(test.data))

# Random classwts
system.time(
  classwts.ensemble.random.3 <- ensemble.forest(
    nforests = 101,
    train.data = data,
    test.data = test.data,
    seed = 1404, 
    random.classwts = TRUE)
)
classwts.ensemble.preds <- classwts.ensemble.random.3$ensemble.preds
table(classwts.ensemble.preds)
# 1 with default seed (1402), 2 with seed 1403, 3 with seed 1404
write.csv(classwts.ensemble.preds, 'datadir/final-rfs-ensemblepreds-3.csv')

(files <- Sys.glob("datadir/final-rfs-ensemblepreds-*.csv"))
all.preds <- sapply(1:length(files), function(f.idx) {
  read.csv(files[f.idx])[,2]
})

voted.preds <- apply(all.preds, 1, my.mode)
voted.predictions <- cbind(id=data.test[,'id'], popularity=voted.preds)
write.csv(voted.predictions, '16032016-final-predictions.csv', row.names = FALSE)

classwts <- sapply(1:5, function(cl) {
  sum(data[,'popularity'] == cl)/nrow(data)
})
data.x <- data[,setdiff(colnames(data), 'popularity')]
data.y <- data[,'popularity']
set.seed(0214)
final.forest <- randomForest(data.x, data.y, classwt = classwts)

final.preds <- predict(final.forest, test.data)
final.predictions <- cbind(id=data.test[,'id'], popularity=final.preds)
write.csv(final.predictions, '16032016-2-final-predictions.csv', row.names = FALSE)
