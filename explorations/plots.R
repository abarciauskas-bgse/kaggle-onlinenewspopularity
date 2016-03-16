if (!require('corrplot')) install.packages('corrplot')
setwd('~/Projects/kaggle-onlinenewspopularity/explorations/datadir/')
source('../setup.R')

# test data
train.test.idcs <- read.csv('dataidcs.csv')
train.idcs <- train.test.idcs[,'Resample1']
test.idcs <- train.test.idcs[,'Resample2']
models <- c('adabag','gbm','glmnet','multinom','rfferns','rfrules','rf','knn','pda')

all.models.preds <- matrix(NA, nrow = 3002, ncol = length(models))
colnames(all.models.preds) <- models
for (m.idx in 1:length(models)) {
  # read file into all.models.preds
  model.name <- models[m.idx]
  preds <- read.csv(paste0(model.name,'preds.csv'))[,2]
  all.models.preds[,model.name] <- as.numeric(preds)
}

png('ensemblecorr.png')
corrplot(cor(all.models.preds))
dev.off()
# only rf predictions
rf.success.rate <- success.rate(all.models.preds[,'rf'], data[test.idcs,'popularity'])

# from looking at the corr plot we see that the least correlated predictions are
# knn, rfrules, multinom (by inspection)
# the ensemble would be
init.ensemble <- all.models.preds[,c('knn','multinom','rfrules')]
init.ensemble.preds <- apply(init.ensemble, 1, my.mode)
success.rate(init.ensemble.preds, data[test.idcs,'popularity'])
# much lower prediction rate

# try randomly sampling
ntests <- 1000
ensemble.results <- data.frame(NA, nrow = ntests, ncol = 3)
colnames(ensemble.results) <- c('ensemble', 'ensemble.size', 'accuracy')

for (i in 1:ntests) {
  print(paste('starting test:', i))
  random.ensemble.size <- sample(2:length(models), 1)
  print(random.ensemble.size)
  random.ensemble.set <- sample(models, random.ensemble.size)
  print(random.ensemble.set)
  random.ensemble.preds <- all.models.preds[,random.ensemble.set]
  random.ensemble.preds <- apply(random.ensemble.preds, 1, my.mode)
  accuracy <- success.rate(random.ensemble.preds, data[test.idcs,'popularity'])
  ensemble.results[i,'ensemble'] <- paste(random.ensemble.set, collapse = ',')
  ensemble.results[i,'ensemble.size'] <- random.ensemble.size
  ensemble.results[i,'accuracy'] <- accuracy
}

write.csv(ensemble.results, 'ensemble-results.csv', row.names = FALSE)

add.alpha <- function(col, alpha=1){
  if(missing(col))
    stop("Please provide a vector of colours.")
  apply(sapply(col, col2rgb)/255, 2, 
                     function(x) 
                       rgb(x[1], x[2], x[3], alpha=alpha))  
}

ensemble.results <- read.csv('ensemble-results.csv')
png('randomensemblesims.png')
plot(ensemble.results[,'accuracy'],
  col = add.alpha('blue', 0.2), pch = 19,
  ylim = c(0.3,0.6),
  ylab = 'accuracy',
  main = '1000 random ensembles')
points(rf.success.rate, col = 'red', pch = 19)
dev.off()

ensemble.results[which.max(ensemble.results[,'accuracy']),]

