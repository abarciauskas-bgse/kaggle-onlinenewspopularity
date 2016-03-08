# K MEANS CLUSTERING

# IN THIS FILE
# We try out k-means clustering on the untransformed variables, and perform a logistic
# regression on each of these. In this file, we are only trying to figure out how many
# clusters to use.
library(ggbiplot)
library(party)
library(dplyr)
library(ggplot2)
library(reshape2)
filepath <- '/home/beeb/Documents/Data_Science/News Competition/OnlineNewsPopularity'
setwd(filepath)
newspop <- read.csv('news_popularity_training.csv')

#Create binary from y variable
for(t in sort(unique(newspop[,'popularity']))) {
  newspop[paste("pop",t,sep="")] <- ifelse( newspop[,'popularity'] == t , 1 , 0 )
}

newspop <- newspop[,4:ncol(newspop)]
newspop[,1:11] <- apply(newspop[,1:11], 2, function(x) { return(x/sd(x))})
newspop[,18:29] <- apply(newspop[,18:29], 2, function(x) { return(x/sd(x))})
newspop[,38:58] <- apply(newspop[,38:58], 2, function(x) { return(x/sd(x))})

training.sample <- sample(nrow(newspop), 0.8*nrow(newspop))
newspop.train <- newspop[training.sample,]
newspop.test <- newspop[setdiff(1:nrow(newspop), training.sample),]

#newspop.train$popularity <- as.factor(newspop.train$popularity)

# And once again I am competing for "world's slowest code" award
reps <- 5
clusters <- 20
between <- matrix(NA, nrow= clusters, ncol = reps)
within <- matrix(NA, nrow = clusters, ncol = reps)
for(i in 1:reps) {
  for(k in 1:clusters) {
    newspop.k <- kmeans(newspop.train, centers = k)
    between[k, i] <- newspop.k$betweenss
    within[k, i] <- newspop.k$tot.withinss
  }
}
tot.between <- rowSums(between)/reps
tot.within <- rowSums(within)/reps

between.diff <- diff(tot.between) / tot.between[2:clusters]
plot(between.diff)
within.diff <- diff(tot.within) / tot.within[2:clusters]
plot(within.diff)

# That's weird.
# So I guess we need 3/4 clusters, roughly?

# VISUALISING THE CLUSTERS
newspop.k <- kmeans(newspop.train[,1:58], centers = 3)
newspop.train$cluster <- as.factor(newspop.k$cluster)

table(newspop.train$cluster)
# Relationship between cluster and popularity
table(newspop.train$cluster, newspop.train$popularity) / rowSums(table(newspop.train$cluster, newspop.train$popularity))
ggplot(newspop.train, aes(cluster, fill = as.factor(popularity))) + geom_bar(position = 'fill')

# visualising channel vs cluster
channel <- newspop.train[, grep('channel', names(newspop.train))]
channel$cluster <- newspop.train$cluster
channel <- melt(channel)
channel <- channel[channel$value == 1,]
ggplot(channel, aes(variable, fill = cluster)) + geom_bar(position = 'fill')

# visualising weekday vs cluster
weekday <- newspop.train[, grep('week', names(newspop.train))]
weekday$cluster <- newspop.train$cluster
weekday <- melt(weekday)
weekday <- weekday[weekday$value == 1,]
ggplot(weekday, aes(variable, fill = cluster)) + geom_bar(position = 'fill')

# visualising sentiment and polarity vs cluster
senpol <- newspop.train[,43:58]
senpol.pca <- prcomp(senpol, scale. = TRUE)
senpol.pcamat <- as.data.frame(senpol.pca$scores)
cluster <- newspop.train$cluster

ggbiplot(senpol.pca, obs.scale = 1, var.scale = 1,
         groups = cluster, ellipse = TRUE, circle = TRUE) +
  scale_color_discrete(name = '') +
  theme(legend.direction = 'horizontal', legend.position = 'top')

# visualising LDA vs cluster
lda <- newspop.train[,38:42]
lda.pca <- prcomp(lda, scale. = TRUE)

ggbiplot(lda.pca, obs.scale = 1, var.scale = 1,
         groups = cluster, ellipse = TRUE, circle = TRUE) +
  scale_color_discrete(name = '') +
  theme(legend.direction = 'horizontal', legend.position = 'top')

# visualising kw vs cluster
# THUS FAR THE ONLY ONE WITH A NOTICEABLE DIFFERENCE BETWEEN CLUSTERS?!?!?!?
kw <- newspop.train[,18:26]
kw.pca <- prcomp(kw, scale. = TRUE)
ggbiplot(kw.pca, obs.scale = 1, var.scale = 1,
         groups = cluster, ellipse = TRUE, circle = TRUE) +
  scale_color_discrete(name = '') +
  theme(legend.direction = 'horizontal', legend.position = 'top')


# tokens and words
tokwor <- newspop.train[newspop.train$n_non_stop_words < 100,1:11]

tokwor.pca <- prcomp(tokwor, scale. = TRUE)
cluster.nonstop <- newspop.train$cluster[newspop.train$n_non_stop_words < 100]
ggbiplot(tokwor.pca, obs.scale = 1, var.scale = 1,
         groups = cluster.nonstop, ellipse = TRUE, circle = TRUE) +
  scale_color_discrete(name = '') +
  theme(legend.direction = 'horizontal', legend.position = 'top')


