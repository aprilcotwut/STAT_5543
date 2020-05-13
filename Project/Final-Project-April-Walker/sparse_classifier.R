# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# sparse_classifier.py: Welcome to my R script. This sparse classifer
#   explores a variety of feature selection and dimension reduction
#   techniques on a dataset predicting parkinson's disease.
#
# This ones a bit of a mess, I'm sorry it's all one file :(
# I hope my comments make it clearer!
#
# Author: April Walker
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
library(h2o)

## creates "not in" as negation of in
`%nin%` = Negate(`%in%`)

## read dataset
data <- read.csv("pd_speech_features.csv", header = T)
# data$weights <- abs(data$class - 1) + 1
data$class <- as.factor(data$class)

# split dataset based on id
train_ids <- sample(unique(data$id), length(unique(data$id))*0.8)

train <- subset(data, id %in% train_ids)
test <- subset(data, id %nin% train_ids)

train$id <- test$id <- NULL

###
### Principal Component Analysis
###

## Determine sutible number of components
# pca_results <- prcomp(scale(train[,-754], center = T, scale = T))
# summary(pca_results)

## Do PCA for training set
pca <-  prcomp(scale(train[,-754], center = T, scale = T), rank = 29)
train_pca <- pca$x
rownames(train_pca) <- NULL
train_pca <- as.data.frame(train_pca)

### Predict PCA components for testing set
test_pca <- predict(pca, newdata = scale(test[-754], center = T, scale = T))
rownames(test_pca) <- NULL
test_pca <- as.data.frame(test_pca)

# add class / y column
train_pca$class <- as.factor(train$class)
test_pca$class <- as.factor(test$class)
### END PCA


### # # # # # #
### Begin H2O #
### # # # # # #

h2o.init()

train.hex <- as.h2o(train)
test.hex <- as.h2o(test)

###
### Gradient Boosted Machine
###

### GBM hyper parameters
params <- list(learn_rate = c(0.2, 0.4, 0.5),
               max_depth = c(2, 3, 5, 7, 10),
               min_rows = c(1,2))
## Call grid search
grid_name <- "gbm_parkd"
s <- Sys.time()
grid <- h2o.grid("gbm", y = "class",
                        grid_id = grid_name,
                        training_frame = train.hex,
                        validation_frame = test.hex,
                        # weights_column = "weights",
                        # nfolds = 2,
                        learn_rate_annealing = 0.99,
                        ntrees = 100,
                        seed = 1,
                        hyper_params = params)

sink("gbm_results.txt", append = T)
cat('time taken:', Sys.time() - s, '\n')

grid_perf <- h2o.getGrid(grid_id = grid_name,
                         sort_by = "logloss",
                         decreasing = F)
# Evaluate performance
grid_perf

# Choose top performing by accuracy
m <- h2o.getModel(grid_perf@model_ids[[1]])

# Get variable importance
var_imp <- h2o.varimp(m)
vars <- subset(var_imp, var_imp$relative_importance > 0)$variable
vars <- c(vars, "class")

# Save non-zero variables
gbm_train.hex <- train.hex[,vars]
gbm_test.hex <- test.hex[,vars]

## Write confusion matrix of test results and variable importance  to output file
cat('\n')
h2o.confusionMatrix(m, test.hex)
cat('\n')
cat('logloss: ', h2o.performance(m, test.hex)@metrics$logloss, '\n')
cat('mean per class error: ', h2o.performance(m, test.hex)@metrics$mean_per_class_error, '\n')
cat('RMSE: ', h2o.performance(m, test.hex)@metrics$RMSE, '\n')
cat('\n')
head(var_imp, 20)
sink(NULL)

###
### Generalized Linear Model
###

s <- Sys.time()
m <- h2o.glm(y = "class", training_frame = train.hex,
                          validation_frame = test.hex,
                          # weights_column = "weights"
                          # nfold = 2,
                          lambda_search = T,
                          alpha = 1,                       # indicates LASSO
                          family = "binomial")

sink("LASSO_results.txt", append = T)
cat('time taken:', Sys.time() - s, '\n')

# Evaluate performance
h2o.performance(m)

# Get variable importance
var_imp <- h2o.varimp(m)
vars <- subset(var_imp, var_imp$relative_importance > 0)$variable
vars <- c(vars, "class")

# Save non-zero variables
LASSO_train.hex <- train.hex[,vars]
LASSO_test.hex <- test.hex[,vars]

## Write confusion matrix of test results and variable importance to output file
cat('\n')
h2o.confusionMatrix(m, test.hex)
cat('\n')
cat('logloss: ', h2o.performance(m, test.hex)@metrics$logloss, '\n')
cat('mean per class error: ', h2o.performance(m, test.hex)@metrics$mean_per_class_error, '\n')
cat('RMSE: ', h2o.performance(m, test.hex)@metrics$RMSE, '\n')
cat('\n')
head(var_imp, 20)
sink(NULL)

### # #
### Random Forest on PCA Components, LASSO variables, and GBM variables
### # #

## RF hyper parameters
params <- list(max_depth = c(2:15),
               min_rows = c(1:3))


### Call grid search for GBM variables
grid_name <- "rf_parkd_gbm_vars"
s <- Sys.time()
grid <- h2o.grid("randomForest", y = "class",
                                 grid_id = grid_name,
                                 training_frame = gbm_train.hex,
                                 validation_frame = gbm_test.hex,
                                 # weights_column = "weights",
                                 # nfolds = 2,
                                 ntrees = 200,
                                 seed = 1,
                                 hyper_params = params)

sink("rf_gbm_vars_results.txt", append = T)
cat('time taken:', Sys.time() - s, '\n')

grid_perf <- h2o.getGrid(grid_id = grid_name,
                         sort_by = "logloss",
                         decreasing = F)
# Evaluate performance
grid_perf

# Choose top performing by accuracy
m <- h2o.getModel(grid_perf@model_ids[[1]])

## Write confusion matrix of test results and variable importance  to output file
cat('\n')
h2o.confusionMatrix(m, gbm_test.hex)
cat('\n')
cat('logloss: ', h2o.performance(m, gbm_test.hex)@metrics$logloss, '\n')
cat('mean per class error: ', h2o.performance(m, gbm_test.hex)@metrics$mean_per_class_error, '\n')
cat('RMSE: ', h2o.performance(m, gbm_test.hex)@metrics$RMSE, '\n')
cat('\n')
sink(NULL)

### Call grid search for LASSO variables
grid_name <- "rf_parkd_LASSO_vars"
s <- Sys.time()
grid <- h2o.grid("randomForest", y = "class",
                                 grid_id = grid_name,
                                 training_frame = LASSO_train.hex,
                                 validation_frame = LASSO_test.hex,
                                 # weights_column = "weights",
                                 # nfolds = 2,
                                 ntrees = 200,
                                 seed = 1,
                                 hyper_params = params)

sink("rf_LASSO_vars_results.txt", append = T)
cat('time taken:', Sys.time() - s, '\n')

grid_perf <- h2o.getGrid(grid_id = grid_name,
                         sort_by = "logloss",
                         decreasing = F)
# Evaluate performance
grid_perf

# Choose top performing by accuracy
m <- h2o.getModel(grid_perf@model_ids[[1]])

## Write confusion matrix of test results and variable importance to output file
cat('\n')
h2o.confusionMatrix(m, LASSO_test.hex)
cat('\n')
cat('logloss: ', h2o.performance(m, LASSO_test.hex)@metrics$logloss, '\n')
cat('mean per class error: ', h2o.performance(m, LASSO_test.hex)@metrics$mean_per_class_error, '\n')
cat('RMSE: ', h2o.performance(m, LASSO_test.hex)@metrics$RMSE, '\n')
cat('\n')
sink(NULL)

### Call grid search for PCA variables
grid_name <- "rf_parkd_BASELINE_vars"
s <- Sys.time()
grid <- h2o.grid("randomForest", y = "class",
                                 grid_id = grid_name,
                                 training_frame = train.hex,
                                 validation_frame = test.hex,
                                 # weights_column = "weights",
                                 # nfolds = 2,
                                 ntrees = 200,
                                 seed = 1,
                                 hyper_params = params)

sink("rf_BASELINE_vars_results.txt", append = T)
cat('time taken:', Sys.time() - s, '\n')

grid_perf <- h2o.getGrid(grid_id = grid_name,
                         sort_by = "logloss",
                         decreasing = F)
# Evaluate performance
grid_perf

# Choose top performing by accuracy
m <- h2o.getModel(grid_perf@model_ids[[1]])

## Write confusion matrix of test results and variable importance to output file
cat('\n')
h2o.confusionMatrix(m, test.hex)
cat('\n')
cat('logloss: ', h2o.performance(m, test.hex)@metrics$logloss, '\n')
cat('mean per class error: ', h2o.performance(m, test.hex)@metrics$mean_per_class_error, '\n')
cat('RMSE: ', h2o.performance(m, test.hex)@metrics$RMSE, '\n')
cat('\n')
sink(NULL)

### Call grid search for PCA variables
PCA_train.hex <- as.h2o(train_pca)
PCA_test.hex <- as.h2o(test_pca)

grid_name <- "rf_parkd_PCA_vars"
s <- Sys.time()
grid <- h2o.grid("randomForest", y = "class",
                                 grid_id = grid_name,
                                 training_frame = PCA_train.hex,
                                 validation_frame = PCA_test.hex,
                                 # weights_column = "weights",
                                 # nfolds = 2,
                                 ntrees = 200,
                                 seed = 1,
                                 hyper_params = params)

sink("rf_PCA_vars_results.txt", append = T)
cat('time taken:', Sys.time() - s, '\n')

grid_perf <- h2o.getGrid(grid_id = grid_name,
                         sort_by = "logloss",
                         decreasing = F)
# Evaluate performance
grid_perf

# Choose top performing by accuracy
m <- h2o.getModel(grid_perf@model_ids[[1]])

## Write confusion matrix of test results and variable importance to output file
cat('\n')
h2o.confusionMatrix(m, PCA_test.hex)
cat('\n')
cat('logloss: ', h2o.performance(m, PCA_test.hex)@metrics$logloss, '\n')
cat('mean per class error: ', h2o.performance(m, PCA_test.hex)@metrics$mean_per_class_error, '\n')
cat('RMSE: ', h2o.performance(m, PCA_test.hex)@metrics$RMSE, '\n')
cat('\n')
sink(NULL)

## Shutdown h2o
h2o.shutdown(prompt=T)
