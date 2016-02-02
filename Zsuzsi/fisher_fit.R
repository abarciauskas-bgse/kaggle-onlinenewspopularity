#Fit glm for all levels of y. Include varibles in increasing rank by Fisher score and save BIC and AIC.

source("/home/zsuzsa/Documents/kaggle/kaggle-onlinenewspopularity/Zsuzsi/data_read.R")

#Function to calculate score
fish.score <- function(y,x){
  m.0 = mean(x[y==0]) 
  m.1 = mean(x[y==1])
  s.0 = sum((x[y==0] - m.0)**2)
  s.1 = sum((x[y==1] - m.1)**2)
  (m.0 - m.1)**2 / (s.0 + s.1)
}

#X variables to include
x.fisher = c(x.numeric,x.rate,x.cat)

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

data = validation(data.train,0.2)
data.train = data$train

#Fit models and save AIC, BIC
fisher.fit = function(y.var) {
  formula = buildExp(y.var,"1")
  fit <- glm(formula, family = "binomial" , data.train)
  aic = AIC(fit)
  bic = BIC(fit)
  nvars = length(fisher.order[,y.var])
  for ( i in 1:nvars ) {
    formula = buildExp(y.var, fisher.order[1:i,y.var] )
    fit <- glm(formula, family = "binomial" , data.train)
    aic[i+1] = fit$aic
    bic[i+1] = BIC(fit) 
  }
  list(AIC = aic, BIC = bic)
}

#criterion.pop1 = fisher.fit("pop1")
#criterion.pop2 = fisher.fit("pop2")
#criterion.pop3 = fisher.fit("pop3")
#criterion.pop4 = fisher.fit("pop4")
#criterion.pop5 = fisher.fit("pop5")

#plot(criterion.pop5[["AIC"]])
#plot(criterion.pop5[["BIC"]])

fisher.best.model = list()
criterion = list()
prediction = list()
for (yvar in y.bin) {
  criterion[yvar] = fisher.fit(yvar)$BIC
  nvars.temp = which(min(criterion[[yvar]])==criterion[[yvar]])
  formula = buildExp(yvar, fisher.order[1:(nvars.temp-1),yvar] )
  fit <- glm(formula, family = "binomial" , data.train)
  #fisher.best.model[yvar] = fit
  prediction[yvar] = predict.glm( fit , data[["validation"]], type="response" )
}
