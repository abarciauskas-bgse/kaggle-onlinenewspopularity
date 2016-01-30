filepath <- '/home/beeb/Documents/Data_Science/News Competition/OnlineNewsPopularity'
setwd(filepath)
library(reshape2)
library(dplyr)
newspop <- read.csv('news_popularity_training.csv')
newspop <- newspop[,4:ncol(newspop)]


#Soooo.... PCA?
#Create a testing and a training dataset
nobs <- nrow(newspop)
validation.indices <- sample(nobs, 0.2*nobs)
train.indices <- setdiff(1:nobs, validation.indices)
newspop.test <- newspop[validation.indices,]
newspop.train <- newspop[train.indices,]

# Find the principal components
newspop.pca <- prcomp(newspop.train)

# This plot shows us that the first four principal components are the most important ones
plot(newspop.pca$sdev)
pca4.newspop <- as.data.frame(newspop.pca$x[,1:4])
pca4.newspop$t <- as.factor(newspop.train$popularity)

#I think this creates a set of principal components for the new data that's congruent
# with the last set
pred <- predict(newspop.pca, newdata = newspop.test)
pred <- as.data.frame(pred[,1:4])
pred$t <- newspop.test$popularity


#Create predictions
for(class in 1:4) {
  pca4.newspop$class <- 0
  pca4.newspop$class[pca4.newspop$t==class] <- 1
  pca4.model <- glm(class ~ PC1 + PC2 + PC3 + PC4, data = pca4.newspop,
                    family=binomial(link='logit'))
  pred$class <- 0
  pred$class[pred$t==class] <- 1
  pred$predvals <- predict(pca4.model, newdata=pred, type = 'response')
  assign(paste0('model', class), pca4.model)
  names(pca4.newspop)[length(names(pca4.newspop))] <- paste0('class', class)
  names(pred)[length(names(pred))] <- paste0('pred.class', class)
}

# Something has gone very wrong here
length(which(pred$pred.class2>pred$pred.class4))

# Categorise the classification
mat <- as.matrix(pred[,7:10])
pred$pred.t <- apply(mat, 1, which.max)
pred$correct <- 0
pred$correct[pred$pred.t == pred$t] <- 1

# This is very bad.
summary(pred$correct)

# Basically, it has predicted that everything is a 2, and come up with a 47% success
# rate, because 47% of entries are 2s. Eeeexcellent.

# In fact, we'd actually get a slightly higher rate if we just predicted 2 for everything.
table(pred$correct, pred$t)

# We're now going to do some outlier detection. Linear discriminant functions typically
# are very susceptible to outliers, so in order to train a better function we're simply
# going to re-run the estimation without looking at the outliers.

# The idea here is that we're going to 
pca4.newspop$prob1 <- fitted.values(model1)
pca4.newspop$prob2 <- fitted.values(model2)
pca4.newspop$prob3 <- fitted.values(model3)
pca4.newspop$prob4 <- fitted.values(model4)

for(class in c(1,3,4)) {
  pca4.newspop.outliers <- pca4.newspop
  place <- which(names(pca4.newspop.outliers) == paste0('prob', class))
  pca4.newspop.outliers$prob <- pca4.newspop.outliers[,place] 
  place <- which(names(pca4.newspop.outliers) == paste0('class', class))
  pca4.newspop.outliers$class <- pca4.newspop.outliers[, place]
  for(i in 1:10) {
    cat(length(pca4.newspop.outliers$prob))
    pca4.newspop.outliers <- filter(pca4.newspop.outliers, 
                                    (class == 1 & prob2 < 0.5) | class3 == 0)
    model <- glm(class ~ PC1 + PC2 + PC3 + PC4, data = pca4.newspop.outliers,
                                    family=binomial(link='logit'))
    pca4.newspop.outliers$prob <- fitted.values(model)

  }
  pred$predvals <- predict(model, newdata=pred, type = 'response')
  names(pred)[length(names(pred))] <- paste0('pred.class', class, '.2')
  assign(paste0('model', class, '.2'), model)  
} 
pred$pred.class2.2 <- pred$pred.class2
#Oh ffs
pred[,16] <- pred$pred.class4.2
pred[,14] <- pred$pred.class2

# LO! IT IS DONE
# Categorise the classification
mat <- as.matrix(pred[,13:16])
pred$pred.t.2 <- apply(mat, 1, which.max)
pred$correct.2 <- 0
pred$correct.2[pred$pred.t.2 == pred$t] <- 1

# This is very bad.
pred$pred.t.2 <- as.factor(pred$pred.t.2)
summary(pred$pred.t.2)
summary(pred$correct.2)

# I HAVE MADE ONE EXTRA CORRECT PREDICTION
# FML.
table(pred$correct, pred$t)
table(pred$correct.2, pred$t)