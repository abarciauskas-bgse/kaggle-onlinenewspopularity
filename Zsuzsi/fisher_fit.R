#Fit glm for all levels of y. Include varibles in increasing rank by Fisher score and save BIC and AIC.

source("/home/zsuzsa/Documents/kaggle/kaggle-onlinenewspopularity/Zsuzsi/data_read.R")

data.fisher = data.sd[data.sd$is_weekend == 0,]

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
x.fisher = c(x.numeric,x.rate, x.cat[x.cat!="is_weekend"] )

#Fisher score for every x variable and every level of y
fisher = data.frame(var = x.fisher,
                    pop1 = NA,
                    pop2 = NA,
                    pop3 = NA,
                    pop4 = NA,
                    pop5 = NA)

#Rank of variables by fisher score
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

fisher.order

#Fit models and save AIC, BIC
fisher.fit = function(y.var) {
  formula = buildExp(y.var,"1")
  fit <- glm(formula, family = "binomial" , data.train)
  #aic = AIC(fit)
  bic = BIC(fit)
  nvars = length(fisher.order[,y.var])
  for ( i in 1:nvars ) {
    formula = buildExp(y.var, fisher.order[1:i,y.var] )
    fit <- glm(formula, family = "binomial" , data.train)
    #aic[i+1] = fit$aic
    bic[i+1] = BIC(fit) 
  }
  list(BIC = bic)
}

criterion = rep(NA,length(y.bin))
fisher.best.model = list()
prediction = list()
bic = list()

for (yvar in y.bin[y.bin!="pop5"]) {
  bic[[yvar]] = fisher.fit(yvar)$BIC
  nvars.temp = which.min( bic[[yvar]] ) - 1
  formula = buildExp(yvar, fisher.order[1:(nvars.temp),yvar] )
  fit <- glm(formula, family = "binomial" , data.train)
  fisher.best.model[[yvar]] = fit
  prediction[[yvar]] = predict.glm( fit , data.validation, type="response" )
}

prediction = as.data.frame(prediction)
prediction.fisher = apply(prediction,1, function(x) as.numeric(substr(names(which.max(x)),4,4)) )
table(prediction.fisher, data.validation[,y])

data.validation$correct = ifelse(prediction.fisher == data.validation[,y],1,0)

success.rate(prediction.fisher, data.validation[,y] )

ggplot(data = data.validation, aes(x=data_chanel) ) + 
  geom_bar(aes( fill=factor(correct) ), binwidth = 25)

ggplot(data = data.validation, aes(x=data_chanel ) ) + 
  geom_bar(aes( fill=factor(correct) ), binwidth = 25, position = "fill")

ggplot(data = data.validation, aes(x=weekday) ) + 
  geom_bar(aes( fill=factor(correct) ), binwidth = 25, position = "fill")



for (yvar in y.bin) {
  if (yvar=="pop1") {
    formula = buildExp(yvar, fisher.order[1:9,yvar] )
  } else if (yvar =="pop2") {
    formula = buildExp(yvar, fisher.order[1:10,yvar] )
  } else if (yvar =="pop3") {
    formula = buildExp(yvar, fisher.order[1:12,yvar] )
  } else {
    nvars.temp = which.min( bic[[yvar]] ) - 1
    formula = buildExp(yvar, fisher.order[1:(nvars.temp),yvar] )
  }
  fit <- glm(formula, family = "binomial" , data.train)
  prediction[[yvar]] = predict.glm( fit , data.validation, type="response" )
}

prediction = as.data.frame(prediction)
prediction.fisher = apply(prediction,1, function(x) as.numeric(substr(names(which.max(x)),4,4)) )
table(prediction.fisher, data.validation[,y])
success.rate(prediction.fisher, data.validation[,y] )
