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

