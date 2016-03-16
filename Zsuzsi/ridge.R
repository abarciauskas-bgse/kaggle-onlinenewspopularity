source("/home/zsuzsa/Documents/computing_lab/HW5.R")
source("/home/zsuzsa/Documents/kaggle/kaggle-onlinenewspopularity/Zsuzsi/data_read.R")

#work with standardised data
temp = validation(data.sd,0.3)
data.train = temp$train
data.validation = temp$validation
rm(temp)

#Create X matrix with most important (??) categorical variables
X.train = cbind(1,data.train[,c(x.numeric,x.rate,
                       x.cat[!(x.cat %in% c("is_weekend","weekday_is_monday","data_channel_is_lifesytle")) ])])
X.train = apply(X.train,2,as.numeric)

X.validation = cbind(1,data.validation[,c(x.numeric,x.rate,
                                x.cat[!(x.cat %in% c("is_weekend","weekday_is_monday","data_channel_is_lifesytle")) ])])
X.validation = apply(X.validation,2,as.numeric)

#Define range for possible lamdba values
lambda.range <- seq(0,5,0.5)
#Best lambda?
lambda = 2.5

#Run ridge (linear regression) for each label
final.model = list()
prediction = list()
for (y.var in y.bin) {
  y = as.numeric(data.train[,y.var])
  final.model[[y.var]] <- ridge.reg(y,X.train,15)
  prediction[[y.var]] = X.validation%*%final.model[[y.var]]
}

prediction = as.data.frame(prediction)
#Compute prediction
prediction = apply(prediction,1, function(x) as.numeric(substr(names(which.max(x)),4,4)) )
table(prediction, data.validation[,"popularity"])

data.validation$correct = ifelse(prediction == data.validation[,"popularity"],1,0)

#0.505 sucess rate
success.rate(prediction, data.validation[,"popularity"] )

####################################Try penalized logistic
library(penalized)

#work with standardised data
x.cat.ridge = x.cat[!x.cat %in% c("weekday_is_tuesday","is_weekend")]
temp = validation(data.sd[ , c(y,y.bin,x.numeric,x.rate,
                               x.cat.ridge) ] ,0.3)
data.train = temp$train
data.train[ ,x.cat.ridge] = scale(data.train[ , x.cat.ridge])

data.validation = temp$validation
data.validation[ ,x.cat.ridge] = scale(data.validation[ , x.cat.ridge])
rm(temp)

final.model = list()
prediction = list()
model = as.formula(paste( "~" , paste( c(x.numeric,x.rate, x.cat.ridge),collapse = " + ")))
for (y.var in y.bin) {
  final.model[[y.var]] <- penalized(response = data.train[,y.var] , penalized = model ,
                                    lambda1 = 0, #lasso
                                    lambda2 = 2.5,
                                    data = data.train, 
                                    model = "logistic")
  prediction[[y.var]] = predict( final.model[[y.var]], model, data = data.validation )
}

prediction = as.data.frame(prediction)
#Compute prediction
prediction = apply(prediction,1, function(x) as.numeric(substr(names(which.max(x)),4,4)) )
table(prediction, data.validation[,"popularity"])

#0.499 sucess rate
success.rate(prediction, data.validation[,"popularity"] )

#Optimizing parameters with cross-validation for lasso
final.model = list()
prediction = list()
model = as.formula(paste( "~" , paste( c(x.numeric,x.rate, x.cat.ridge),collapse = " + ")))
for (y.var in y.bin) {
  opt1 <- optL1(response = data.train[,y.var] , penalized = model ,
                                    data = data.train, 
                                    model = "logistic",
                                    fold = 10,
                                    maxlambda1 = 40,
                                    epsilon = 1)
  final.model[[y.var]] <- penalized(response = data.train[,y.var] , penalized = model ,
                                    lambda1 = opt1$lambda, #lasso
                                    lambda2 = 0,
                                    data = data.train, 
                                    model = "logistic")
  prediction[[y.var]] = predict( final.model[[y.var]], model, data = data.validation )
}

prediction = as.data.frame(prediction)
#Compute prediction
prediction = apply(prediction,1, function(x) as.numeric(substr(names(which.max(x)),4,4)) )
table(prediction, data.validation[,"popularity"])

#0.499 sucess rate
success.rate(prediction, data.validation[,"popularity"] )

#Optimizing parameters with cross-validation for ridge
#Optimization fails for some reason
final.model = list()
prediction = list()
model = as.formula(paste( "~" , paste( c(x.numeric,x.rate, x.cat.ridge),collapse = " + ")))
for (y.var in y.bin) {
  opt2 <- optL2(response = data.train[,y.var] , penalized = model ,
                data = data.train, 
                model = "logistic",
                fold = 15,
                maxlambda2 = 10,
                epsilon = 1)
  final.model[[y.var]] <- penalized(response = data.train[,y.var] , penalized = model ,
                                    lambda1 = 0, #lasso
                                    lambda2 = opt2$lambda,
                                    data = data.train, 
                                    model = "logistic")
  prediction[[y.var]] = predict( final.model[[y.var]], model, data = data.validation )
}

prediction = as.data.frame(prediction)
#Compute prediction
prediction = apply(prediction,1, function(x) as.numeric(substr(names(which.max(x)),4,4)) )
table(prediction, data.validation[,"popularity"])
success.rate(prediction, data.validation[,"popularity"] )
