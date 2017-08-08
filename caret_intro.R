library("caret")
library(C50)
data(churn)

str(churnTrain)
predictors <- names(churnTrain[names(churnTrain) != "churn"])
# caret uses the first factor in response, some paxckage use the second

# a simple stratified split
# split equally/proportionally on response factors
set.seed(1)
allData <- rbind(churnTest, churnTrain)
inTrainingSet <- createDataPartition(allData$churn,
                                     p = 0.75, list = FALSE)
churnTrain2 <- allData[inTrainingSet,]
churnTest2 <- allData[-inTrainingSet,]

# other functions for other splits;
# createFolds()
# createMultiFolds()
# createResample()

# pre-processing
# centering, scaling, spatial sign transforms, PCA or ICA "signal extraction",
# imputation, BOX-Cox transforms and more
numerics <- c("account_length", "total_day_calls", "total_night_calls")
# step 1 learn "process model"; determine means and sd's
procValues <- preProcess(churnTrain[,numerics],
           method = c("center", "scale", "YeoJohnson")) # order of transforms see help
# step 2 apply "process model"; use the predict methods to do the adjustments
trainScaled <- predict(procValues, churnTrain[,numerics])
testScaled <- predict(procValues, churnTest[,numerics])
procValues

# boosted trees (adaBoost)
# iterated ensemble training
# fit first tree
# increase weight for misclassifieds and fit next tree
# final prediction is weighted average of each tree's
# prediction


# boosting models have 3 tuning parameters;
#   iteration(m trees),
#   splits(tree nodes/leaves),
#   learning rate/speed (shrinkage; rate of weighting)
# => 3D tuning-grid for tuning the model
# model tuning using train()
gbmTune <- train(x = churnTrain[,predictors],
                 y = churnTrain$churn,
                 method = "gbm")
# or using the formaula interface
gbmTune <- train(churn ~ ., data = churnTrain, method = "gbm",
                 verbose = FALSE)

# train uses bootstrap for resampling by default
# swithc to eg. 5 repeats of 10-fold CV;
ctrl <- trainControl(method = "repeatedcv",
                     repeats = 5,
                     classProbs = TRUE, # class probabilities
                     summaryFunction = twoClassSummary) # calcs incl AUC

gbmTune <- train(churn ~ ., data = churnTrain, method = "gbm",
                 verbose = FALSE,
                 trControl = ctrl)
# at this moment train will fit a sequence of different gbm models,
# estimates performance using resampling,
# pick the best performing model,

# expanding the search grid - WARNING processing time;
# 4*20*2 = 160 models are fit! ~ 30 minutes?
grid0 <- expand.grid(interaction.depth = seq(1, 7, by = 2),
                    n.trees = seq(100, 1000, by = 50),
                    shrinkage = c(0.01, 0.1),
                    n.minobsinnode = 7)

# less models; 3*5*3 = 45 models
grid <- expand.grid(interaction.depth = seq(3, 7, by = 2),
                    n.trees = seq(100, 500, by = 100),
                    shrinkage = c(0.05, 0.1, 0.2),
                    n.minobsinnode = 5)

# adding the tuning performance metric "AUC"/"ROC". Default is Cohen's Kappa
set.seed(1)
gbmTune <- train(churn ~ ., data = churnTrain, method = "gbm",
                 metric = "ROC", #*
                 tuneGrid = grid, # check package update
                 verbose = FALSE,
                 trControl = ctrl)

grid
# boosting packages: gbm, ada, blackboost, mboost

gbmTune
# Tuning parameter 'n.minobsinnode' was held constant at a value of 7
# ROC was used to select the optimal model using  the largest value.
# The final values used for the model were n.trees = 150, interaction.depth =
#  3, shrinkage = 0.1 and n.minobsinnode = 7.
names(gbmTune)
gbmTune$finalModel
gbmTune$modelInfo

#
plot(gbmTune)
g <- ggplot(gbmTune)
g <- g + theme(legend.position = "top")
g


gbmPred <- predict(gbmTune, churnTest)
str(gbmPred)

gbmProb <- predict(gbmTune, churnTest, type = "prob")
str(gbmProb)

confusionMatrix(gbmProb, churnTest$churn)

#prc package
rocCurve <- roc(response = churntest$churn,
                predictor = gbmProb[, "yes"],
                levels = rev(levels(churnTest$churn)))
rocCurve
plot(rocCurve)
plot(rocCurve,
     print.thres = c(.5, .2),
     print.thres.pch = 16,
     print.thres.cex = 1.2
     )

set.seed(1)
svmTune <- train(churn ~ .,
                 data = churnTrain,
                 # fit SVM and tune over cost and RBF parameter
                 method = "svmRadial",
                 # pre-process will be applied to this and future data
                 preProc = c("center", "scale"),
                 # tune over different values of cost
                 # 10 values of tuning params = 10 fits
                 tuneLength = 10,
                 trControl = ctrl,
                 metric = "ROC")

set.seed(1)
fdaTune <- train(churn ~ .,
                 data = churnTrain,
                 # fit flexible discriminant using MARS
                 method = "fda",
                 # tune over different values of cost
                 tuneLength = 10,
                 trControl = ctrl,
                 metric = "ROC")























