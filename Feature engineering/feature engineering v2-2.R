# Attempt 2: PCA-in-groups

# The point of this file:
# Clearly, some sort of variable selection must be done. Early attempts at PCA
# were, shall we say, less than promising. In this file, I use Zsuzsa's grouped
# variables to do a smaller PCA-in-groups.

# Load in data
filepath <- '/home/beeb/Documents/Data_Science/News Competition/OnlineNewsPopularity'
setwd(filepath)
library(reshape2)
library(ggplot2)
library(dplyr)
newspop <- read.csv('news_popularity_training.csv')

# Split these two into the groups Zsuzsa made
names <- names(newspop)

x.words <- names[c(4:8,13)]
x.links <- names[c(9:10,30:32)]
x.dig <- names[11:12]
x.time <- names[c(3,33:40)]
x.key <- names[14:29]
x.NLP <- names[41:61]
newspop$t <- as.factor(newspop$popularity)

# WHAT IS THIS WITCHCRAFT???
#Create binary from y variable
for(t in sort(unique(newspop[,'popularity']))) {
  newspop[paste("pop",t,sep="")] <- ifelse( newspop[,'popularity'] == t , 1 , 0 )
}
# Clever, Zsuzsa. Very clever.

groups <- list()
groups[[1]] <- x.words
groups[[2]] <- x.links
groups[[3]] <- x.dig
groups[[4]] <- x.time
groups[[5]] <- x.key
groups[[6]] <- x.NLP

grouped.data <- lapply(groups, function(y) {
  return(select(newspop, one_of(y)))
})

cors <- lapply(grouped.data, cor)
# Looking at these correlations, I'm not 100% convinced we've grouped these correctly
# eg, look at 
cors[[1]]
# Seems quite clear that n_non_stop_words, n_unique_tokens, n_non_stop_unique_tokens,
# and average_token_length should go together; the other two not so much. same is true 
# if you look at 
cors[[2]]
# But we can come back to that, all we'd have to do is re-write a few lines of code.
grouped.data.pca <- lapply(grouped.data, function(x) {
  pr <- prcomp(x)
  plot(pr$sdev, main = length(x))
  return(pr)
})

# Irritatingly, it seems like the breakdown with the least-neat selection of PC's is
# the sixth group, which is arguably the most important (check plots!)

# The plan: take one PC from the first group; two from the second; one from the third;
# get rid of the fourth entirely, it's not useful; three from the fifth; shall we say 8
# from the sixth? That's 88% of the variance.

newdata <- data.frame(grouped.data.pca[[1]]$x[,1], 
                              grouped.data.pca[[2]]$x[,1:2],
                              grouped.data.pca[[3]]$x[,1],
                              grouped.data.pca[[5]]$x[,1:3],
                              grouped.data.pca[[6]]$x[,1:8],
                              newspop$is_weekend,
                              newspop[,grep('pop', names(newspop))])
# Something must be done about these variable names. These are like the most confusing
# thing ever.
# Meh.
# Labels labels labels. Names names names.
names <- paste0('PC', 1:15)
names(newdata)[1:15] <- names
training.sample <- sample(nrow(newdata), nrow(newdata)*0.8)
newdata.train <- newdata[training.sample,]
newdata.test <- newdata[setdiff(1:nrow(newdata), training.sample),]
xvals <- newdata.train[,1:16]

hmm <- matrix(NA, nrow=nrow(newdata.test), ncol = 5)
j <- 1
for(i in grep('pop[0-9]', names(newdata.train))) {
  t <- newdata.train[,i]
  current <- cbind(xvals, t)
  model <- glm(t ~ . + newspop.is_weekend*., data = current, family=binomial)
  predict.model <- predict(model, newdata=newdata.test, type='response')
  assign(paste0('model', i), model)
  assign(paste0('predict', i), predict.model)
  hmm[,j] <- predict.model
  j <- j + 1
}

colnames(hmm) <- names(newdata.test)[grep('pop[0-9]', names(newdata.test))]

# This is not looking bad, actually.
which <- apply(hmm, 1, which.max)

table(newdata.test$popularity, which)
correct <- rep(0, nrow(newdata.test))
correct[newdata.test$popularity == which] <- 1
sum(correct)/length(correct)

# This is marginally better than we would have done using only PREDICT ALL AS 2
