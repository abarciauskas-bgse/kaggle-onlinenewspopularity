setwd('~/Projects/kaggle-onlinenewspopularity/explorations/')
source('setup.R')

# helpful caret vignette
# https://cran.r-project.org/web/packages/caret/vignettes/caret.pdf
# caret models list
# http://topepo.github.io/caret/modelList.html

# collect training data
training.sample <- createDataPartition(y = data$popularity, ## the outcome data are needed
                                       p = 0.1, ## The percentage of data in the training set
                                       times = 2,
                                       list = FALSE)
# 2 small data sets
data.train <- data[training.sample,]
data.test <- data[-training.sample,]
# are_equal(nrow(data.train) + nrow(data.test), nrow(data))

# start with a baseline model, like...knn?
# by default this will do some tuning and cv from what i understand
# type of resampling: The simple bootstrap is used by default. We will have the
#   function use three repeats of 10–fold cross–validation
# 
# results reported below are only for 0.05% of the training data (e.g. ~1501 rows)
#
train.ctrl <- trainControl(method = "repeatedcv",
                           repeats = 3,
                           number = 10)

knn.fit <- train(popularity ~ .,
                 data = data.train,
                 method = "knn",
                 trControl = train.ctrl,
                 ## Center and scale the predictors for the training
                 ## set and all future samples.
                 preProc = c("center", "scale"))
# 0.455 with k = 9

kknn.fit <- train(popularity ~ .,
                  data = data.train,
                  method = "kknn",
                  trControl = train.ctrl,
                  ## Center and scale the predictors for the training
                  ## set and all future samples.
                  preProc = c("center", "scale"))
kknn.fit
# 0.435 with kmax = 9, distance = 2 and kernel = optimal

ctree.fit <- train(popularity ~ .,
                   data = data.train,
                   method = "ctree",
                   trControl = train.ctrl,
                   ## Center and scale the predictors for the training
                   ## set and all future samples.
                   preProc = c("center", "scale"))
ctree.fit
# 0.461
# mincriterion = 0.99

ctree2.fit <- train(popularity ~ .,
                    data = data.train,
                    method = "ctree2",
                    trControl = train.ctrl,
                    ## Center and scale the predictors for the training
                    ## set and all future samples.
                    preProc = c("center", "scale"))
ctree2.fit
# 0.447
# maxdepth = 1

# FYI cannot use ada package

train.ctrl <- trainControl(method = "repeatedcv",
                           repeats = 1,
                           number = 2)
# WARNING this method is super slow
# adabagm1.fit <- train(popularity ~ .,
#                       data = data.train,
#                       method = "AdaBoost.M1",
#                       trControl = train.ctrl,
#                       tuneLength = 1,
#                       ## Center and scale the predictors for the training
#                       ## set and all future samples.
#                       preProc = c("center", "scale"))
# adabagm1.fit
# 0.459
# mfinal = 50, maxdepth = 1 and coeflearn = Breiman

adabag.fit <- train(popularity ~ .,
                    data = data.train,
                    method = "AdaBag",
                    trControl = train.ctrl,
                    ## Center and scale the predictors for the training
                    ## set and all future samples.
                    preProc = c("center", "scale"))
adabag.fit
# 0.471
# mfinal = 150 and maxdepth = 3

system.time(
avNNet.fit <- train(popularity ~ .,
                    data = data.train,
                    method = "avNNet",
                    trControl = train.ctrl, # using larger train ctrl 3 / 10
                    ## Center and scale the predictors for the training
                    ## set and all future samples.
                    preProc = c("center", "scale")))
# stopped after 100 iterations
# user   system  elapsed 
# 5501.748  159.784 5760.287
avNNet.fit
# 0.492 / 0.479
# size = 1, decay = 0.1 and bag = FALSE
avnnet.preds <- predict(avNNet.fit, newdata = x.test)

rf.fit <- train(popularity ~ .,
                data = data.train,
                method = "rf",
                trControl = train.ctrl,
                ## Center and scale the predictors for the training
                ## set and all future samples.
                preProc = c("center", "scale"))
rf.fit
# 0.501
# mtry = 2

# First attempt at stacking
# take two fits and combine them and see if they are better on predicting with each individual
#
# benchmark each predictive power of each rf and knn
# 
# knn
knn.preds <- predict(knn.fit, newdata = data.test)
# accuracy of basic knn
sr.knn <- success.rate(knn.preds, data.test$popularity)
# 
# rf
rf.preds <- predict(rf.fit, newdata = data.test)
sr.rf <- success.rate(rf.preds, data.test$popularity)

# predictions<-(lm_predictions+rf_predictions)/2
stupid.ensemble.preds <- round((as.numeric(knn.preds)+as.numeric(rf.preds))/2)
success.rate(stupid.ensemble.preds, data.test$popularity)
# worse than rf alone

avnnet.preds <- predict(avNNet.fit, newdata = data.test)
sr.avnnet <- success.rate(avnnet.preds, data.test$popularity)

# three-party ensemble
# take a weighted majority, which basically amounts to taking majority
# for each prediction, take the most popular of the three models
ensemble.preds <- rep(NA, length(data.test$popularity))
for (i in 1:length(data.test$popularity)) {
  ensemble.preds[i] <- my.mode(c(avnnet.preds[i], rf.preds[i], knn.preds[i]))
}
(sr.ensemble <- success.rate(ensemble.preds, data.test$popularity))
