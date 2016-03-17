filepath <- '/home/beeb/Documents/Data_Science/News Competition/OnlineNewsPopularity'
setwd(filepath)
library(reshape2)
library(ggplot2)
library(dplyr)
newspop <- read.csv('news_popularity_training.csv')
newspop <- newspop[,4:ncol(newspop)]
newspop$t <- as.factor(newspop$popularity)
for(class in 1:4) {
  newspop$class <- 0
  newspop$class[newspop$t==class] <- 1
  names(newspop)[length(names(newspop))] <- paste0('class', class)
}

#Create a testing and a training dataset
nobs <- nrow(newspop)
validation.indices <- sample(nobs, 0.2*nobs)
train.indices <- setdiff(1:nobs, validation.indices)
newspop.test <- newspop[validation.indices,]
newspop.train <- newspop[train.indices,]

# Initialise a dataframe for adding variables to and a list to show which groups were added
currentmod <- data.frame(t = newspop.train$class1)
incvars <- list()
newspop.train <- select(newspop.train, -c(class1, class2, t, class4, class3, 
                                          is_weekend, weekday_is_sunday, popularity))
# The first thing we're going to do is see if any of the x's are correlated
cors <- cor(newspop.train)
m <- ncol(cors)

# Pick out those that have correlation above 0.5
together <- apply(abs(cors)>0.47, 2, which)
toshow <- together[sapply(together, length)>1]
# Stepwise!
# Take the first 50 most important variables (we can examine subsets later on)
for(k in 1:50) {
  cat(k)
  j <- 1
  likelihoods <- sapply(together, function(y) {
    cat(names(y))
    mod <- cbind(currentmod, select(newspop.train, one_of(names(y)))) 
    model <- glm(t ~ ., data = mod, family=binomial)
    return(AIC(model))
  })

  # Choose the variable which will give the best log likelihood
  bestvar <- which(likelihoods == min(likelihoods))
  incvars[[k]] <- bestvar
  #cat('HERE')
  # Move the best variables from the list of vars under consideration 
  # to the dataframe of selected variables
  names <- names(together[[bestvar[1]]])
  for(name in names) {
    position <- which(names(together) == name)
  together <- together[setdiff(1:length(together), position)]
  cat(name)
  }

  currentmod <- cbind(currentmod, select(newspop.train, one_of(names)))
}

# This bit exists due to the complication of needing to treat certain variables in groups
# That is, highly correlated variables should go together
# 'Steps' will tell us; first take the first three vars; then the next two vars; then
# one by itself ; etc etc.
steps <- cumsum(sapply(incvars, length))  
incvars2 <- unlist(incvars)
collect.r2 <- rep(0, length(steps))
num.outliers <- rep(0, length(steps))
j <- 1
newspop.train$t <- newspop.train$class2
newspop.test$t <- newspop.test$class2

for(i in steps) {
  currentmod <- select(newspop.train, t, one_of(names(incvars2)[1:i]))
  model <- glm(t ~ ., data = currentmod, family=binomial(link='log'))
  assign(paste0('model', j), model)
  newspop.test$predvals <- predict(model, newdata = newspop.test, type= 'response')
  cat(i)
  # We collect the R^2 of using the training model on the testing data.
  # Not sure if there's an automatic way to do this.
  newspop.test$residuals <- newspop.test$predvals - newspop.test$t
  
  ressumsquare <- sum((newspop.test$residuals)**2)
  totsumsquare <- sum((newspop.test$t - mean(newspop.test$t))**2)
  r.squared <- 1 - (ressumsquare/totsumsquare)
  collect.r2[j] <- r.squared
  
  j <- j + 1
}


collect.r2 <- as.data.frame(collect.r2)
ggplot(data = collect.r2, aes(y = collect.r2, x = c(1:length(collect.r2)))) +
  geom_point() +
  labs(x = 'Model Number', y = expression(R^2)) +
  scale_x_discrete(breaks = seq(0,50,5)) +
  scale_y_continuous(breaks = seq(0, 1, 0.05), minor_breaks = seq(0, 1, 0.01))

#BMA
y <- newspop$class1
x <- newspop[,1:58]
x <- select(x, -weekday_is_sunday, -is_weekend)
glm.out <- bic.glm(x, y, binomial)
# That took a long time to achieve a whole lotta nothing
test <- cbind(x, y)
model <- glm(y ~., data = test, family = binomial)
sds <- apply(x, 2, sd)
variability <- sds*model$coefficients[2:57]
