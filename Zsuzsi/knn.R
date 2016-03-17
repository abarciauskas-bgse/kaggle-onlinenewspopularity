source("/home/zsuzsa/Documents/kaggle/kaggle-onlinenewspopularity/Zsuzsi/data_read.R")
library("class")

#Define data to use - standardised variables
data.knn = data.sd
#Separate final test set
temp = validation(data.knn,0.3)
data.test = temp$validation
data.knn = temp$train
#Separate validation set to choose between specifications
temp = validation(data.knn,0.3)
data.train = temp$train
data.validation = temp$validation
rm(temp)

#Only numeric variables are used for k-nn fitting
#Categorical variables are used to separate data and run separate k-nn for each
x.knn = c(x.numeric,x.rate)

#K-values to try
k.values= c(1,3,5,7,9,11,13,15,17,21,31,51,101)

#Initialize matrix to save optimal k-value by channel and weekend
k.opt <- matrix(NA, 2 , length(unique(data.train$data_chanel )))
rownames(k.opt) = c("0","1")
colnames(k.opt) = unique(data.train$data_chanel)

#Run separate k-nn by weekend and channel
#choose optimal k value based on validation set accuracy
for (weekend in 0:1) {
  for (channel in unique(data.train$data_chanel ) ) {
    data.temp.train = data.train[data.train$is_weekend == weekend & 
                                   data.train$data_chanel == channel, ]
    data.temp.validation = data.validation[data.validation$is_weekend == weekend & 
                                   data.validation$data_chanel == channel, ]
    accuracy = rep( NA,length(k.values) )
    for (i in 1:length(k.values)) {
      prediction.test = as.integer( knn(data.temp.train[,x.knn],
                                        data.temp.validation[,x.knn],
                                        data.temp.train[,y],k=k.values[i]) )
      accuracy[i] = success.rate(prediction.test, data.temp.validation[,y] )
      }
    k.opt[as.character(weekend),channel] = k.values[which.max(accuracy)]
  }
}

#Initialize matrices to save accuracy of k-nn with optimal k value on test set
accuracy.test <- matrix(NA, 2 , length(unique(data.train$data_chanel )))
rownames(accuracy.test) = c("0","1")
colnames(accuracy.test) = unique(data.train$data_chanel)

num.ok.test <- matrix(NA, 2 , length(unique(data.train$data_chanel )))
rownames(num.ok.test) = c("0","1")
colnames(num.ok.test) = unique(data.train$data_chanel)

#Function to compute number of mistakes
success.num <- function(predictions, actual) {
  errors <- 0
  # count number of mistakes
  for (i in 1:length(actual)) {
    if (predictions[i] != actual[i]) {
      errors <- errors + 1
    }
  }
  return (length(actual) - errors)
}

#Compute error of k-nn with best k value on test set
for (weekend in 0:1) {
  for (channel in unique(data.train$data_chanel ) ) {
    data.temp.train = data.train[data.train$is_weekend == weekend & 
                                   data.train$data_chanel == channel, ]
    data.temp.test = data.test[data.test$is_weekend == weekend & 
                                             data.test$data_chanel == channel, ]
    prediction.test = as.integer( knn(data.temp.train[,x.knn],
                                        data.temp.test[,x.knn],
                                        data.temp.train[,y],k=k.opt[as.character(weekend),channel]) )
    accuracy.test[as.character(weekend),channel] = success.rate(prediction.test, data.temp.test[,y] )
    num.ok.test[as.character(weekend),channel] = success.num(prediction.test, data.temp.test[,y] )
  }
}

#Have high accuracy in some categories but overall it is 0.494 only
accuracy.test
num.ok.test
sum(num.ok.test)/nrow(data.test)

############################################################################
#joint k-nn using categorical variables as well
x.knn = c(x.numeric,x.rate,x.cat)
#Standardise categorical variables
data.train[ ,x.cat] = scale(data.train[ , x.cat])
data.validation[ ,x.cat] = scale(data.validation[ , x.cat])
data.test[ ,x.cat] = scale(data.test[ , x.cat])

accuracy = rep(NA,length(k.values))

for (i in 1:length(k.values)) {
    prediction.test = as.integer( knn(data.train[,x.knn],
                                        data.validation[,x.knn],
                                        data.train[,y],k=k.values[i]) )
      accuracy[i] = success.rate(prediction.test, data.validation[,y] )
      }

#best k-value: 101
k.values[which.max(accuracy)]

png(filename = "/home/zsuzsa/Documents/kaggle/kaggle-onlinenewspopularity/report/k_nn.png",
    width = 480, height = 300)
plot(k.values,accuracy, type="l", main="Prediction accuracy of k-nn specifications", xlab="k", ylab="Test accuracy")
dev.off()

prediction.test =  knn(data.train[,x.knn],
                                      data.test[,x.knn],
                                      data.train[,y],k=k.values[which.max(accuracy)])
success.rate(prediction.test, data.test[,y] )
#Accuracy is .494 which is almost exactly the same as with the separate k-nn's