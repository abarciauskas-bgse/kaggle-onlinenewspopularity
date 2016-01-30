# Sampling for features selection

# going to limit classification problem to 1,2,3 as it contains 96.6% of the data
sum(round(table(data.train[,'popularity'])/nrow(data.train),3)[1:3])
# [1] 0.966

data.train <- subset(data.train, popularity %in% c(1,2,3))
data.validation <- subset(data.validation, popularity %in% c(1,2,3))

# redoing logistic regression with only our first three categories
X <- data.train[order(data.train[,'popularity']),]
X <- as.matrix(X[,1:(ncol(data.train)-1)])
countPerCategory <- list()
for (i in 1:3) {
  countPerCategory[toString(i)] <- nrow(subset(data.train, popularity == i))
}

Y <- cbind(cat1.target = c(rep(1, as.numeric(countPerCategory['1'])),
                           rep(0, as.numeric(countPerCategory['2'])),
                           rep(0, as.numeric(countPerCategory['3']))),
           cat2.target = c(rep(0, as.numeric(countPerCategory['1'])),
                           rep(1, as.numeric(countPerCategory['2'])),
                           rep(0, as.numeric(countPerCategory['3']))),
           cat3.target = c(rep(0, as.numeric(countPerCategory['1'])),
                           rep(0, as.numeric(countPerCategory['2'])),
                           rep(1, as.numeric(countPerCategory['3']))))
weightsOptim <- ginv(t(X)%*%X) %*% t(X) %*% Y
# there's probably a better way to do this, rounding is wrong
predictions.percents <- as.matrix(data.validation[,1:(ncol(data.train)-1)]) %*% weightsOptim
predictions = apply(predictions.percents, 1, which.max)
success.rate(predictions, data.validation[,ncol(data.validation)])
# up to 48.985% !


# We have 60 features
# We probabilty don't want most of them
# Randomly subset different sizes and see what size of model is most likely?
features <- colnames(data.train[,setdiff(colnames(data.train), 'popularity')])
res <- cor(data.train)
cors <- res[setdiff(features, 'popularity'),'popularity']
which.max(cors)
# kw_avg_avg -> Avg. keyword (avg. shares)
cors <- cors[order(cors, decreasing = TRUE)]
