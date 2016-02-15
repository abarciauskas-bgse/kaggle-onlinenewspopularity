# FEATURE ENGINEERING V2-4
filepath <- '/home/beeb/Documents/Data_Science/kaggle-onlinenewspopularity/Feature engineering'
setwd(filepath)
source('feature engineering v2-3.R')
library(genlasso)
# What's new in this edition?
# Basically, last time we climbed to 49-50% accuracy using a model that blindly included
# all the features we'd engineered. That's some 65 odd features! Even with 30,000 obs,
# it would be sensible to worry about over-fitting.
# In this edition, we think more carefully about which features to include.

# Two methods will be tried: one mindlessly selects the significant features, and uses
# those; the second looks at the size of the coefficients rather than their significance.
#############################################################################
# METHOD ONE
###############################################################################

sig.coeffs.35 <- coef(summary(model35))[abs(coef(summary(model35))[,4])<0.05,1]
sig.coeffs.36 <- coef(summary(model36))[abs(coef(summary(model36))[,4])<0.05,1]
sig.coeffs.37 <- coef(summary(model37))[abs(coef(summary(model37))[,4])<0.05,1]
sig.coeffs.38 <- coef(summary(model38))[abs(coef(summary(model38))[,4])<0.05,1]
sig.coeffs.39 <- coef(summary(model39))[abs(coef(summary(model39))[,4])<0.05,1]

training.sample <- sample(nrow(newdata), nrow(newdata)*0.8)
newdata.train <- newdata[training.sample,]
newdata.test <- newdata[setdiff(1:nrow(newdata), training.sample),]

# Not sure how to do this automatically
model1 <- glm(pop1 ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC7 + PC8 + PC9 +
                num_self_hrefs + num_hrefs + num_keywords + data_channel_is_entertainment +
                data_channel_is_bus + data_channel_is_socmed + data_channel_is_tech +
                data_channel_is_world + kw_min_max + kw_min_avg + LDA_00 + LDA_01 +
                LDA_03 + LDA_04 + PC2:is_weekend + num_hrefs:is_weekend + 
                num_self_hrefs:is_weekend + num_videos:is_weekend + data_channel_is_lifestyle:is_weekend +
                data_channel_is_entertainment:is_weekend + kw_min_max:is_weekend,
              data = newdata.train, family = binomial
              )

model2 <- glm(pop2 ~ PC1 + PC2 + PC4 + PC5 + PC7 + PC8 + PC9 + num_imgs + data_channel_is_bus +
                data_channel_is_socmed + data_channel_is_tech + kw_min_avg + LDA_00 + LDA_01 +
                LDA_03 + LDA_04 + PC5:is_weekend + is_weekend:LDA_01 + is_weekend:LDA_02,
              data = newdata.train, family = binomial)

model3 <- glm(pop3 ~ PC2 + PC3 + PC10 + PC11 + num_hrefs + num_self_hrefs + num_imgs +
                average_token_length + num_keywords + data_channel_is_lifestyle + 
                data_channel_is_entertainment + data_channel_is_bus + data_channel_is_socmed +
                data_channel_is_world + kw_min_avg + PC5:is_weekend + data_channel_is_entertainment:is_weekend +
                data_channel_is_bus:is_weekend + kw_min_avg:is_weekend,
              data = newdata.train, family=binomial)

model4 <- glm(pop4 ~ PC2 + PC3 + PC6 + PC7 + PC10 + num_hrefs + num_imgs + data_channel_is_entertainment +
                data_channel_is_lifestyle:is_weekend + is_weekend:LDA_03,
              data = newdata.train, family = binomial)

model5 <- glm(pop5 ~ n_tokens_content + data_channel_is_tech, data = newdata.train,
              family = binomial)

hmm <- matrix(NA, nrow=nrow(newdata.test), ncol = 5)
j <- 1
for(i in 1:5) {
  model <- get(paste0('model', i))
  predict.model <- predict(model, newdata=newdata.test, type='response')
  assign(paste0('predict', i), predict.model)
  hmm[,j] <- predict.model
  j <- j + 1
}

colnames(hmm) <- names(newdata.test)[grep('pop[0-9]', names(newdata.test))]

which <- apply(hmm, 1, which.max)

table(newdata.test$popularity, which)
correct <- rep(0, nrow(newdata.test))
correct[newdata.test$popularity == which] <- 1
sum(correct)/length(correct)

# In three runs, we had 49.0%, 48.6%, and 48.9% accuracy
# Therefore this represents a worsening :(

###################################################################################
# METHOD 2
################################################################################

lasso <- genlasso(newdata[,'pop2'], as.matrix(newdata[,1:33]), diag(1, 33))
lasso.check <- lasso$beta
lasso.check <- t(lasso.check)
colnames(lasso.check) <- names(newdata)[1:33]
lasso.check <- melt(lasso.check)
ggplot(data=lasso.check, aes(x = Var1, y = value, colour = as.factor(Var2))) + geom_line()
# god knows what's going on here

means <- sapply(newdata, mean)
means <- means[1:33]
# Well, this is a ball-ache, isn't it?
# FFS
interactions <- sapply(newdata, function(x) { return(newdata$is_weekend*x)})
colnames(interactions) <- paste0(colnames(interactions),':is_weekend')

means.interactions <- apply(interactions, 2,mean)
means.interactions <- means.interactions[1:33]
means.interactions <- means.interactions[setdiff(names(means.interactions), 'is_weekend:is_weekend')]

means.all <- append(means, means.interactions)

for (i in c(35,36,37,38,39)) {
  model <- get(paste0('model',i))
  coefs <- coef(model)[2:length(coef(model))]

  # This has worked.
  sizecheck <- data.frame(means = means.all, coefs = coefs, both = means.all*coefs, id = names(coefs))
  rownames(sizecheck) <- names(coefs)
  sizecheck <- arrange(sizecheck, desc(abs(both)))
    important <- sizecheck$id[1:20]
  ggplot(data=sizecheck, aes(x = id, y = abs(both))) + geom_bar(stat='identity') +
    ylim(c(0,0.75)) + coord_flip()
  
  sizecheck2 <- filter(sizecheck, id %in% (setdiff(sizecheck$id, names(sig.coeffs.35)))) %>%
    arrange(desc(abs(both)))
  

  assign(paste0('important', i), important)
}

# This might explain the poor results from last time - the coefficients selected,
# while significant, weren't actually particularly good predictors.
# So now literally all we're going to do is select the coefficients with the
# largest effects, rather than the most significant ones.

# Note that the precise top 20 list is subject to some variance

# I would love to know what's a good way of doing this quickly and easily
model1.2 <- glm(pop1 ~ LDA_04 + LDA_03 + LDA_02 + LDA_00 + LDA_01 +
                  average_token_length + num_keywords + kw_min_avg +
                  data_channel_is_entertainment + is_weekend + num_hrefs +
                  average_token_length:is_weekend+ data_channel_is_socmed +
                  data_channel_is_tech +
                  data_channel_is_bus + num_self_hrefs + data_channel_is_world +
                  n_tokens_title:is_weekend + num_hrefs:is_weekend + num_keywords:is_weekend,
                data = newdata.train, family=binomial)
model2.2 <- glm(pop2 ~ LDA_04 + LDA_03 + LDA_02 + LDA_00 + LDA_01 + average_token_length:is_weekend +
                  is_weekend + average_token_length + num_keywords + data_channel_is_tech +
                  data_channel_is_bus + kw_min_avg + data_channel_is_socmed + num_hrefs + 
                  n_tokens_title + n_tokens_title:is_weekend + num_imgs + is_weekend:LDA_02 + 
                  num_keywords:is_weekend + num_self_hrefs, data = newdata.train, family=binomial)
model3.2 <- glm(pop3 ~ LDA_04 + LDA_03 + LDA_02 + LDA_00 + LDA_01 + average_token_length:is_weekend +
                  is_weekend + average_token_length + num_keywords + kw_min_avg +
                  data_channel_is_bus + n_tokens_title + data_channel_is_entertainment +
                  data_channel_is_world + num_hrefs + num_self_hrefs + num_imgs + 
                  n_tokens_content + num_hrefs:is_weekend + n_tokens_content:is_weekend,
                data = newdata.train, family = binomial)
model4.2 <- glm(pop4 ~ LDA_04 + LDA_03 + LDA_02 + LDA_00 + LDA_01 + average_token_length +
                  n_tokens_title + average_token_length:is_weekend + num_hrefs + 
                  kw_min_avg + num_self_hrefs + data_channel_is_bus + num_imgs +
                  n_tokens_content + data_channel_is_entertainment + is_weekend:LDA_02 +
                  is_weekend:LDA_03 + data_channel_is_world + num_keywords:is_weekend +
                  is_weekend:LDA_01, data = newdata.train, family = binomial)
model5.2 <- glm(pop5 ~ LDA_04 + LDA_03 + LDA_02 + LDA_00 + LDA_01 + is_weekend +
                  average_token_length:is_weekend + kw_min_avg:is_weekend +
                  n_tokens_title + n_tokens_title:is_weekend + data_channel_is_socmed +
                  num_keywords + data_channel_is_tech + is_weekend:LDA_03 + num_self_hrefs:is_weekend +
                  num_hrefs:is_weekend + n_tokens_content + data_channel_is_bus:is_weekend +
                  is_weekend:LDA_00 + data_channel_is_bus, data=newdata.train, family=binomial)
# why did you not just use LASSO? Would have saved a lot of time...


hmm <- matrix(NA, nrow=nrow(newdata.test), ncol = 5)
j <- 1
for(i in 1:5) {
  model <- get(paste0('model', i, '.2'))
  predict.model <- predict(model, newdata=newdata.test, type='response')
  assign(paste0('predict', i), predict.model)
  hmm[,j] <- predict.model
  j <- j + 1
}

colnames(hmm) <- names(newdata.test)[grep('pop[0-9]', names(newdata.test))]

which <- apply(hmm, 1, which.max)

table(newdata.test$popularity, which)
correct <- rep(0, nrow(newdata.test))
correct[newdata.test$popularity == which] <- 1
sum(correct)/length(correct)
#50.0, 48.8, 48.8 success rates
