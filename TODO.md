# TODO

## Classification

* [in-progress] stacking / take [weighted] average of classifiers
* knn with different distance metric
* caret to balance classes
* ensemble cross validation: https://github.com/zachmayer/caretEnsemble
* More with cross-val using caret: grid search of parameters, but need systematic way of comparing results. Can run a long time

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

* k-nn: run on standardised data, optimized for best k-value, k-nn optimized separately for different channels and weekend/weekday
* simple logistic: combined with variable selections based on fisher score and BIC
* ridge and lasso regression with linear probability model and logistic regression model 
* random forest
* (moderate success) gbm (best result: 0.529 on submission to leaderboard)
* svm
* some bad feature selection (aimee: or at least my efforts have been hapless and unsuccessful)
* basic pca
* feature identification - split the variables of highly correlated variables and did PCA on the groups
* multinom (nnet)
* xgboost (r package)
* (moderate success) classifying just 1,2,3
* more complex knn
* n-fold cross validation to find (and ignore) outliers
* caret for cross validation and grid search of hyperparamters
