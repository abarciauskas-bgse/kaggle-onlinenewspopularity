# Attempt 2.3: PCA-in-groups

# The point of this file:
# Attempt 2.2 made an accuracy improvement of 2% over previous attempts. However, that's
# really not very much. In this file, we basically repeat the process, but we base our
# groups on observed correlations rather than on apriori reasoning.
# I've tried to be as transparent as possible about how variables were grouped, but a few
# decisions may not be hugely obvious.

# Load in data
filepath <- '/home/beeb/Documents/Data_Science/News Competition/OnlineNewsPopularity'
setwd(filepath)
library(reshape2)
library(ggplot2)
library(dplyr)
newspop <- read.csv('news_popularity_training.csv')

#Create binary from y variable
for(t in sort(unique(newspop[,'popularity']))) {
  newspop[paste("pop",t,sep="")] <- ifelse( newspop[,'popularity'] == t , 1 , 0 )
}

# Let's get us some groups
names <- names(newspop)

cor.mat <- cor(newspop[,4:61])
significant <-  apply(abs(cor.mat)>0.5, 2, which)
examine <- sapply(significant, length)
examine <- examine[examine>1]
examine
# I have typed out the actual names for transparency
group1 <- c('n_unique_tokens', 'n_non_stop_words', 'n_non_stop_unique_tokens')
group2 <- c('kw_max_avg', 'kw_avg_avg', 'kw_max_min', 'kw_avg_min')
group3 <- c('kw_min_min', 'kw_max_max', 'kw_avg_max') # note that kw_min_avg and kw_min_max are 'free'
group4 <- c('self_reference_min_shares', 'self_reference_max_shares', 'self_reference_avg_sharess')
group5 <- c('global_subjectivity', 'avg_positive_polarity', 'max_positive_polarity', 'min_positive_polarity')
group6 <- c('global_sentiment_polarity', 'global_rate_positive_words', 
            'rate_positive_words', 'rate_negative_words', 'global_rate_negative_words')
group7 <- c('avg_negative_polarity', 'min_negative_polarity', 'max_negative_polarity')
group8 <- c('title_subjectivity','abs_title_subjectivity', 'title_sentiment_polarity', 'abs_title_sentiment_polarity')

# Leftover issues: the fact two of the kw variables don't fit in neatly anywhere;
# the fact that average_token_length has a weird correlation with a load of other stuff,
# don't understand that; LDAs and channels.

groups <- list()
groups[[1]] <- group1
groups[[2]] <- group2
groups[[3]] <- group3
groups[[4]] <- group4
groups[[5]] <- group5
groups[[6]] <- group6
groups[[7]] <- group7
groups[[8]] <- group8

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

#Now it's time to figure out how many PCs to take. Less clear this time than last time.

# Generally take 1-2 PCs from each group. Then add in all the vars that we didn't 
# put into groups
# There must be an easier way of doing this?!
included <- append(group1, append(group2, append(group3, 
                                                 append(group4, append(group5, 
                                                append(group6, 
                                                  append(group7, group8)))))))
excluded <- setdiff(names, included)
# get rid of day-related words
excluded <- excluded[-grep('weekday', excluded)]
# get rid of metadata
excluded <- excluded[4:length(excluded)]

# This is going to end up being a regression on about 33 variables, which is 
# less than ideal.
newdata <- data.frame(grouped.data.pca[[1]]$x[,1], 
                      grouped.data.pca[[2]]$x[,1:2],
                      grouped.data.pca[[3]]$x[,1:2],
                      grouped.data.pca[[5]]$x[,1:2],
                      grouped.data.pca[[6]]$x[,1:2],
                      grouped.data.pca[[7]]$x[,1],
                      grouped.data.pca[[8]]$x[,1],
                      select(newspop, one_of(excluded)))

# VARIABLE NAMES OF DOOM
# Labels labels labels. Names names names.
names <- paste0('PC', 1:11)
names(newdata)[1:11] <- names
training.sample <- sample(nrow(newdata), nrow(newdata)*0.8)
newdata.train <- newdata[training.sample,]
newdata.test <- newdata[setdiff(1:nrow(newdata), training.sample),]
xvals <- newdata.train[,1:33]

hmm <- matrix(NA, nrow=nrow(newdata.test), ncol = 5)
j <- 1
for(i in grep('pop[0-9]', names(newdata.train))) {
  t <- newdata.train[,i]
  current <- cbind(xvals, t)
  #model <- glm(t ~ . + is_weekend*., data = current, family=binomial)
  model <- glm(t ~ . , data = current, family=binomial)
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

# Having run the code three times, I got 49.1%, 50.2%, and 49.6% accuracy out-of-sample.


