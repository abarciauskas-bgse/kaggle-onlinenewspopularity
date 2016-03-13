source("/home/zsuzsa/Documents/kaggle/kaggle-onlinenewspopularity/Zsuzsi/data_read.R")
library(dplyr)
library(ggplot2)

#Separate validation set
x.cat.tree = x.cat[!x.cat %in% c("weekday_is_tuesday","is_weekend")]
temp = validation(data.sd[ , c(y,x.numeric,x.rate,
                               x.cat.tree) ] ,0.3)
data.train = temp$train
data.train[,y] = factor(data.train[,y])
data.train[ ,x.cat.tree] = scale(data.train[ , x.cat.tree])

data.validation = temp$validation
data.validation[,y] = factor(data.validation[,y])
data.validation[ ,x.cat.tree] = scale(data.validation[ , x.cat.tree])
rm(temp)

#simple classification tree
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
rf <- randomForest( popularity ~ ., data = data.train,
                    mtry = floor(sqrt(ncol(data.train)-1)),
                    #maxnodes = 40,
                    ntree = 250, importance = TRUE )

rf
rfPred <- predict(rf, data.validation, type = "class") 
classifications <- table(rfPred, data.validation$popularity)
success.rate(rfPred, data.validation$popularity )
#51,5%

imp <- data.frame(round(importance(rf), 2)) %>% mutate(names = rownames(importance(rf))) %>% arrange(MeanDecreaseGini)
ggplot(data = imp,
       aes(x = reorder(names, order(imp$MeanDecreaseGini)), y = MeanDecreaseGini)) +
  geom_bar(stat = 'identity') + coord_flip() + theme_bw()
