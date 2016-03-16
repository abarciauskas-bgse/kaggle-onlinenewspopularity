# Author: Aimee Barciauskas
# All the stuff I'm doing for setup
# Date last updated: 07 March 2016
#
#if (!require('caret')) install.packages('caret', dependencies = c("Depends", "Suggests"))
if (!require('assertthat')) install.packages('assertthat')
setwd('~/Projects/kaggle-onlinenewspopularity/data/')

# Read in data
data <- read.csv('news_popularity_training.csv')
setwd('../explorations')
source('../utilities.R')
# Remove id and url
data <- data[,3:ncol(data)]
data$popularity <- as.factor(data$popularity)

