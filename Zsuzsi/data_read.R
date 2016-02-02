library("ggplot2")
setwd("/home/zsuzsa/Documents/kaggle")
source("kaggle-onlinenewspopularity/functions.R")

#Read in data
data.train <- read.csv('data/news_popularity_training.csv')
N <- nrow(data.train)

#Define different variable categories
y <- "popularity"
names <- names(data.train)

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
data.train <- within(data.train, {
  data_chanel = ifelse(data_channel_is_lifestyle == 1, "Lifestyle",
                       ifelse( data_channel_is_entertainment == 1, "Enertainent",
                               ifelse( data_channel_is_bus == 1, "Business",
                                       ifelse( data_channel_is_socmed == 1, "Social Media",
                                               ifelse( data_channel_is_tech == 1, "Tech",
                                                       ifelse( data_channel_is_world == 1, "World","Viral"))))))
})

data.train <- within(data.train, {
  weekday = ifelse(weekday_is_monday == 1, "Monday",
                   ifelse( weekday_is_tuesday == 1, "Tuesday",
                           ifelse( weekday_is_wednesday == 1, "Wednesday",
                                   ifelse( weekday_is_thursday == 1, "Thursday",
                                           ifelse( weekday_is_friday == 1, "Friday",
                                                   ifelse( weekday_is_saturday == 1, "Saturday","Sunday"))))))
})

#Create binary from y variable
for(t in unique(data.train[,y])) {
  data.train[paste("pop",t,sep="")] <- ifelse( data.train[,y] == t , 1 , 0 )
}
y.bin = names(data.train[,65:69])