#Generates submission file for logistic regression model
setwd("/home/zsuzsa/Documents/kaggle")

#Read in data
data.test <- read.csv('data/news_popularity_test.csv')
data.test[ ,c(x.numeric,x.rate )] = scale(data.test[ ,c(x.numeric,x.rate )])

prediction.test = list()
for (yvar in y.bin) {
  fit = fisher.best.model[[yvar]]
  prediction.test[[yvar]] = predict.glm( fit , data.test, type="response" )
}

prediction.test = as.data.frame(prediction.test)
temp = apply(prediction.test,1, function(x) as.numeric(substr(names(which.max(x)),4,4)) )
prediction.test = data.frame(id = as.integer(data.test[,"id"]),
                                popularity = as.integer(temp))
write.csv(prediction.test,file = "submission1.csv",row.names = F)

