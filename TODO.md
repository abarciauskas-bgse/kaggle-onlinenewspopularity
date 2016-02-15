# TODO

## Classification

* take [weighted] average of classifiers
* testing using rolling windows of test sets / random / mcmc test sets
* knn with different distance metric
* cross-validation to find optimal gamma in boosting
* [in-progress] n-fold cross validation to find (and ignore) outliers
* [aimee would like to do this for fun] implement adaboost
* library for adaboost

## Variable selection

* [BayesVarSel R Package Docs](https://cran.r-project.org/web/packages/BayesVarSel/BayesVarSel.pdf)
* bayes variable selection using bayes factors (base)
    * [reference](https://projecteuclid.org/download/pdf_1/euclid.ba/1340370391)
* gibbs sampling for variable selection
    * [reference](http://www.cs.berkeley.edu/~russell/classes/cs294/f05/papers/george+mcculloch-1993.pdf)
    * [reference](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.57.4258&rep=rep1&type=pdf)
* lasso

## Other

## Done

* simple k-nn
* simple logistic
* random forest
* (moderate success) gbm (best result: 0.529 on submission to leaderboard)
* svm
* some bad feature selection (aimee: or at least my efforts have been hapless and unsuccessful)
* basic pca
* feature identification - split the variables of highly correlated variables and did PCA on the groups
* multinom (nnet)
* xgboost (r package)
* (moderate success) classifying just 1,2,3

