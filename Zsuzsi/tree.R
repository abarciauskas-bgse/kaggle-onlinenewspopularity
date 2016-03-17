source("/home/zsuzsa/Documents/kaggle/kaggle-onlinenewspopularity/Zsuzsi/data_read.R")
source("/home/zsuzsa/Documents/kaggle/kaggle-onlinenewspopularity/functions.R")

library(dplyr)
library(ggplot2)
library(reshape2)

#Separate validation set
x.cat.tree = x.cat[!x.cat %in% c("weekday_is_tuesday","is_weekend")]
set.seed(1453)
temp = validation(data.sd[ , c(y,y.bin,x.numeric,x.rate,
                               x.cat.tree) ] ,0.3)
data.train = temp$train[,-c(2,3,4,5,6)]
data.train[,y] = factor(data.train[,y])
data.train[ ,x.cat.tree] = scale(data.train[ , x.cat.tree])

data.validation = temp$validation
data.validation[,y] = factor(data.validation[,y])
data.validation[ ,x.cat.tree] = scale(data.validation[ , x.cat.tree])
#rm(temp)

library(tree) 
classTree <- tree( popularity ~ ., data = data.train)
summary(classTree)

# test error
treePred <- predict(classTree, data.validation, type = "class") 
classifications <- table(treePred, data.validation$popularity)
success.rate(treePred, data.validation$popularity )
#46,2%, everything classified as 2, basicaly useless

library(randomForest)
#bagging
bagging <- randomForest(popularity ~ ., data = data.train,
                        mtry = ncol(data.train)-1, nodesize = 1,
                        #maxnodes = 40,
                        ntree = 250, importance = FALSE)
bagging
baggingPred <- predict(bagging, data.validation, type = "class") 
classifications <- table(baggingPred, data.validation$popularity)
success.rate(baggingPred, data.validation$popularity )
#51.1%

#random forest
rf <- randomForest( popularity ~., data = data.train,
                    mtry = floor(sqrt(ncol(data.train)-1)),
                    #maxnodes = 40,
                    ntree = 400, importance = TRUE )

rf
rfPred <- predict(rf, data.validation, type = "class") 
classifications <- table(rfPred, data.validation$popularity)
success.rate(rfPred, data.validation$popularity )
#51,5%

imp <- data.frame(round(importance(rf), 2)) %>% mutate(names = rownames(importance(rf))) %>% arrange(MeanDecreaseGini)
ggplot(data = imp,
       aes(x = reorder(names, order(imp$MeanDecreaseGini)), y = MeanDecreaseGini)) +
  geom_bar(stat = 'identity') + coord_flip() + theme_bw()

#Boosting
library(gbm)

noIterations <- 400 
#Without small shrinkage parameter something goes wrong with the algorithm
boost <-gbm(formula = popularity~. ,
            distribution ="multinomial", 
            data = data.train, 
            n.trees = noIterations, 
            interaction.depth = 3, 
            #regularization
            shrinkage = 0.1,
            bag.fraction = 0.5,
            train.fraction = 0.5,
            cv.folds = 3
            )

# extracting training and test errors
iter.vec = seq(1,noIterations,10)
trainError <- testError <- rep(NA, length(iter.vec))
for (iter in 1:length(iter.vec)) { 
    i = iter.vec[iter]
    predict.temp = predict(boost, data.train, n.trees = i)
    trainError[iter] <- success.rate( apply(predict.temp , 1 ,function(x) colnames(predict.temp)[which.max(x)] ) , data.train$popularity )
    predict.temp = predict(boost, data.validation, n.trees = i)
    testError[iter] <- success.rate( apply(predict.temp,1,function(x) colnames(predict.temp)[which.max(x)] ) , data.validation$popularity )
  }

best.iter = iter.vec[which.max(testError)]
predict.boost.mult = predict(boost, data.validation, n.trees = best.iter)
predict.boost.mult = apply(predict.boost.mult,1,function(x) colnames(predict.boost.mult)[which.max(x)])

errors <- data.frame(Iterations = iter.vec,Train = trainError, Test = testError) %>%
  melt(id.vars = "Iterations")

jpeg(file="/home/zsuzsa/Documents/kaggle/kaggle-onlinenewspopularity/report/boost.jpeg", height = 300, width = 460)
ggplot(data = errors, aes(x = Iterations, y = value, color = variable)) +
  geom_line()
dev.off()
#Best test error 51.4% at 101 iterations 

#Try boosting and random forest on binary variables
data.train = temp$train
data.train[,c(y,y.bin)] = lapply( data.train[,c(y,y.bin)], function(x) factor(x) )
data.train[ ,x.cat.tree] = scale(data.train[ , x.cat.tree])

data.validation = temp$validation
data.validation[,c(y,y.bin)] = lapply( data.validation[,c(y,y.bin)], function(x) factor(x) )
data.validation[ ,x.cat.tree] = scale(data.validation[ , x.cat.tree])

#random forest
rf_bin <- list()
#boost_bin <- list()

model = paste( c(x.numeric,x.rate, x.cat.tree),collapse = " + ")

for (y.var in y.bin) {
  model.temp  = paste(y.var,model, sep=" ~ ") 
  rf_bin[[y.var]] <- randomForest( formula = as.formula(model.temp) , data = data.train,
                      mtry = floor(sqrt(ncol(data.train)-1)),
                      #maxnodes = 40,
                      ntree = 250, importance = F )
  #boosting is not working for some reason
  #boost_bin[[y.var]] <-gbm(formula = as.formula(model.temp),
  #            distribution ="bernoulli", 
  #            data = data.train, 
  #            n.trees = 1000, 
  #            interaction.depth = 1, 
              #regularization
              #shrinkage = 0.1,
  #            bag.fraction = 0.5#,
              #train.fraction = 0.5,
              #cv.folds = 3
  #)
  
} 

rfPred_bin <- data.frame(matrix(NA,nrow(data.validation),5)) 
colnames(rfPred_bin) <- y.bin
for (y.var in y.bin) {
  rfPred_bin[,y.var] <- predict(rf_bin[[y.var]], data.validation, type = "prob")[,"1"]
}

rf_classifications_bin <- apply(rfPred_bin,1,function(x) substr(colnames(rfPred_bin)[which.max(x)],4,4))
success.rate(rf_classifications_bin, data.validation$popularity )
#52.17%

#boostPred_bin <- data.frame(matrix(NA,nrow(data.validation),5)) 
#colnames(rfPred_bin) <- y.bin
#for (y.var in y.bin) {
#  best.iter <- gbm.perf(boost_bin[[y.var]],method="OOB")
#  rfPred_bin[,y.var] <- predict(boost_bin[[y.var]], data.validation, n.trees = best.iter)
#}

#rf_classifications_bin <- apply(rfPred_bin,1,function(x) substr(colnames(rfPred_bin)[which.max(x)],4,4))
#success.rate(rf_classifications_bin, data.validation$popularity )

#TRy gaussian boosting
data.train = temp$train[,-c(2,3,4,5,6)]
data.train[ ,x.cat.tree] = scale(data.train[ , x.cat.tree])

data.validation = temp$validation
data.validation[ ,x.cat.tree] = scale(data.validation[ , x.cat.tree])

noIterations <- 1000 
#Without small shrinkage parameter something goes wrong with the algorithm
boost2 <-gbm(formula = popularity~. ,
            distribution ="gaussian", 
            data = data.train, 
            n.trees = noIterations, 
            interaction.depth = 3, 
            #regularization
            shrinkage = 0.1,
            bag.fraction = 0.5,
            train.fraction = 0.5,
            cv.folds = 3
)

# extracting training and test errors
iter.vec = seq(1,noIterations,10)
trainError <- testError <- rep(NA, length(iter.vec))
for (iter in 1:length(iter.vec)) { 
  i = iter.vec[iter]
  predict.temp = predict(boost2, data.train, n.trees = i)
  predict.temp = cut(predict.temp, breaks = c(0 , 1.7 , 2.6 , 3.5,4.5) , labels = F )
  trainError[iter] <- success.rate( predict.temp , data.train$popularity )
  predict.temp = predict(boost2, data.validation, n.trees = i)
  predict.temp = cut(predict.temp, breaks = c(0 , 1.7 , 2.6 , 3.5,4.5) , labels = F )
  testError[iter] <- success.rate( predict.temp , data.validation$popularity )
}

best.iter.gauss = iter.vec[which.max(testError)]
predict.boost.gauss = predict(boost2, data.validation, n.trees = best.iter.gauss)
predict.boost.gauss = cut(predict.boost.gauss, breaks = c(0 , 1.7 , 2.6 , 3.5,4.5) , labels = F )
success.rate(predict.boost.gauss,data.validation$popularity)

str(rfPred) 
str(factor(predict.boost.mult))
str(factor(rf_classifications_bin)) 
str(factor(predict.boost.gauss))

pred.all = data.frame(rf.multi = rfPred,
                      boost.multi = factor(predict.boost.mult),
                      rf.bin = factor(rf_classifications_bin),
                      boost.gauss = factor(predict.boost.gauss))

pred.all.final = apply(pred.all,1,function(x) names(which.max(table(as.numeric(x)))) )

