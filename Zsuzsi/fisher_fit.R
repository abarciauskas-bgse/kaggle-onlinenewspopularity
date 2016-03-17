#Fit glm for all levels of y. Include varibles in increasing rank by Fisher score and save BIC and AIC.

source("/home/zsuzsa/Documents/kaggle/kaggle-onlinenewspopularity/Zsuzsi/data_read.R")
source("/home/zsuzsa/Documents/kaggle/kaggle-onlinenewspopularity/functions.R")

data.fisher = data.sd

#Generate training and validation data
temp = validation(data.fisher,0.2)
data.train = temp$train
data.validation = temp$validation
rm(temp)

#Function to calculate score
fish.score <- function(y,x){
  m.0 = mean(x[y==0]) 
  m.1 = mean(x[y==1])
  s.0 = sum((x[y==0] - m.0)**2)
  s.1 = sum((x[y==1] - m.1)**2)
  (m.0 - m.1)**2 / (s.0 + s.1)
}

#X variables to include
x.fisher = c(x.numeric,x.rate, x.cat)

#Fisher score for every x variable and every level of y
fisher = data.frame(var = x.fisher,
                    pop1 = NA,
                    pop2 = NA,
                    pop3 = NA,
                    pop4 = NA,
                    pop5 = NA)

#Rank of variables by fisher score using only training data
fisher.order = data.frame(rank = 1:length(x.fisher))

for (i in y.bin) {
  y.temp = data.train[i]
  j = 0
  for (var in x.fisher) {
    j=j+1
    x.temp = data.train[var]
    fisher[j,i] = fish.score(y.temp,x.temp)
  }
  fisher.order[i] = fisher[order(fisher[i], decreasing=TRUE ),"var"]
}

#Visualize fisher scores
library(reshape2)
fisher.vis <- function(yvar) {
  fisher.melt <- melt(fisher[,c("var",yvar)])
  p <- ggplot(fisher.melt, aes(variable, var))+
    geom_tile(data=fisher.melt, aes(fill=value), color="white")+
    scale_fill_gradient2()+
    theme(axis.text.x = element_text(angle=45, vjust=1, size=11, hjust=1))+
    coord_equal()
  return(p)
}

fisher.vis("pop1") 

#Fit models and save AIC, BIC

#Fit models by including variables in increasing fisher score order
fisher.fit = function(y.var) {
  formula = buildExp(y.var,"1")
  fit <- glm(formula, family = "binomial" , data.train)
  bic = BIC(fit)
  nvars = length(fisher.order[,y.var])
  for ( i in 1:nvars ) {
    formula = buildExp(y.var, fisher.order[1:i,y.var] )
    fit <- glm(formula, family = "binomial" , data.train)
    bic[i+1] = BIC(fit) 
  }
  list(BIC = bic)
}

#Fit models in increasing fisher score order but drop if it decreased BIC
fisher.fit2 = function(y.var) {
  formula = buildExp(y.var,"1")
  fit <- glm(formula, family = "binomial" , data.train)
  bic = BIC(fit)
  fisher.order.temp = fisher.order[,y.var]
  nvars = length(fisher.order.temp)
  var.in = character()
  for ( i in 1:nvars ) {
    var.in = c(var.in,as.character( fisher.order.temp[i] ))
    formula = buildExp(y.var, var.in )
    fit <- glm(formula, family = "binomial" , data.train)
    bic[i+1] = BIC(fit) 
    if ( bic[i+1] <= bic[i] ) {
      var.in[-length(var.in)]
    }
  }
  list(BIC = bic)
}

#Run models and save winning model, predicion and BIC
criterion = rep(NA,length(y.bin))
fisher.best.model = list()
prediction = list()
bic = list()

for (yvar in y.bin) {
  bic[[yvar]] = fisher.fit(yvar)$BIC
  nvars.temp = which.min( bic[[yvar]] ) - 1
  formula = buildExp(yvar, fisher.order[1:(nvars.temp),yvar] )
  fit <- glm(formula, family = "binomial" , data.train)
  fisher.best.model[[yvar]] = fit
  prediction[[yvar]] = predict.glm( fit , data.validation, type="response" )
}

#Check prediction accuracy
prediction = as.data.frame(prediction)

prediction.fisher = apply(prediction,1, function(x) as.numeric(substr(names(which.max(x)),4,4)) )
table(prediction.fisher, data.validation[,y])

data.validation$correct = ifelse(prediction.fisher == data.validation[,y],1,0)

success.rate(prediction.fisher, data.validation[,y] )

#Trying ordered logit to take into account that popularity is an ordered variable 
library(MASS)
data.train[,y] = factor(data.train[,y])
data.validation[,y] = factor(data.validation[,y])

#Define rank of x variables, aggregate their fisher rank to one rank
rank = rep(0,length(x.fisher))
names(rank) = x.fisher
for (var in x.fisher) {
  for (i in 2:6) {
  rank.temp = fisher.order[ which(fisher.order[ , i] == var ) , "rank" ]
  rank[var] = rank[var] + rank.temp
  }
}

fisher.order = names(rank[order(rank)])

#Function to fit ordered models
ordered.fit = function(y.var) {
  model <- list()
  formula = buildExp(y.var,"1")
  fit <- polr(formula, data = data.train, method = "logistic")
  model[[1]] <- fit
  nvars = length(fisher.order)
  for ( i in 1:nvars ) {
    formula = buildExp(y.var, fisher.order[1:i] )
    fit <- polr(formula, data = data.train, method = "logistic")
    model[[i+1]] <- fit
  }
  model
}

models <- ordered.fit("popularity")
#Extracting test error
accuracy.ordered <- rep(NA, length(models)) 
for (i in 1:length(models)) {
  prediction = predict( models[[i]] , data.validation, type="class" )
  accuracy.ordered[i] = success.rate(prediction, data.validation[,y] )
}
#Prediction is only running up to 18 vars but it can already be seen 
#that the accuracy is pretty low (~48,9%) 
