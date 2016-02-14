setwd("/home/zsuzsa/Documents/kaggle")
source("kaggle-onlinenewspopularity/functions.R")

#Read in data
data <- read.csv('data/news_popularity_training.csv')
N <- nrow(data)

#Define different variable categories
y <- "popularity"
names <- names(data)

x.words <- names[c(4:8,13)]
x.links <- names[c(9:10,30:32)]
x.dig <- names[11:12]
x.time <- names[c(3,33:40)]
x.key <- names[14:29]
x.NLP <- names[41:61]

x.rate <- names[c(6:8,41:61)]
x.cat <- names[c(15:20,33:40)]
x.numeric <- names[!(names %in% c(x.rate,x.cat,names[c(1:2,62)]))]

#Create categorical variable from channel and weekday binary variables
data <- within(data, {
  data_chanel = ifelse(data_channel_is_lifestyle == 1, "Lifestyle",
                       ifelse( data_channel_is_entertainment == 1, "Enertainent",
                               ifelse( data_channel_is_bus == 1, "Business",
                                       ifelse( data_channel_is_socmed == 1, "Social Media",
                                               ifelse( data_channel_is_tech == 1, "Tech",
                                                       ifelse( data_channel_is_world == 1, "World","Viral"))))))
})

data <- within(data, {
  weekday = ifelse(weekday_is_monday == 1, "Monday",
                   ifelse( weekday_is_tuesday == 1, "Tuesday",
                           ifelse( weekday_is_wednesday == 1, "Wednesday",
                                   ifelse( weekday_is_thursday == 1, "Thursday",
                                           ifelse( weekday_is_friday == 1, "Friday",
                                                   ifelse( weekday_is_saturday == 1, "Saturday","Sunday"))))))
})

#Create binary from y variable
for(t in unique(data[,y])) {
  data[paste("pop",t,sep="")] <- ifelse( data[,y] == t , 1 , 0 )
}
y.bin = names(data[,65:69])

#Standardise numeric variables
data.sd = data
data.sd[ ,c(x.numeric,x.rate )] = scale(data[ ,c(x.numeric,x.rate )])

#Log variables
data.log = data
to.log = apply(data.log[,c(x.numeric,x.rate)],2,min)
to.log = (to.log == 0)
to.log = c(x.numeric,x.rate)[to.log]
data.log[, to.log] = log(data.log[, to.log] + 1)
data.log[ ,c(x.numeric,x.rate )] = scale(data.log[ ,c(x.numeric,x.rate )])

