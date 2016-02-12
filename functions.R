buildExp <- function(dependant, independants)
{ 
  indPart = paste(independants, collapse = " + ")
  return( paste(dependant, indPart, sep = " ~ ") )
}

validation <- function( data , percentage ){
  nobs <- nrow(data)
  validation.indices <- sample(nobs, percentage*nobs)
  train.indices <- setdiff(1:nobs, validation.indices)
  data.validation <- data[validation.indices,]
  data.train <- data[train.indices,]
  return(list(
    validation = data.validation,
    train = data.train
  ))
}

success.rate <- function(predictions, actual) {
  errors <- 0
  # count number of mistakes
  for (i in 1:length(actual)) {
    if (predictions[i] != actual[i]) {
      errors <- errors + 1
    }
  }
  return (1 - errors/length(actual))
}
