#------------------------------------------------------------------------------
# File:        IntroToMachineLearning.R
# Author:      Dave Langer
# Description: This code illustrates the usage of the caret package for the An
#              Introduction to Machine Learning with R and Caret" Meetup dated
#              06/07/2017. More details on the Meetup are available at:
#              https://www.meetup.com/data-science-dojo/events/239730653/
#------------------------------------------------------------------------------

#install.packages(c("e1071", "caret", "doSNOW", "ipred", "xgboost"))
library(tidyverse)
library(caret)
library(doSNOW)

#------------------------------------------------------------------------------
# Load Data
#------------------------------------------------------------------------------
# clean up
rm(list = ls())

getwd()
fileTrain <- paste(getwd(), "train.csv", sep = "/")
fileTest <- paste(getwd(), "test.csv", sep = "/")

# check typeof features
spec_csv(fileTrain, col_names = TRUE, col_types = NULL,
         n_max = 0, guess_max = 1000)

# set/overule specific types in col_types
trainHead <- read_csv(fileTrain,
                      n_max = 1000,
                      col_types = cols(
                              PassengerId = col_skip(),
                              Survived = col_factor(NULL, F, F), # auto-level, unordered, NA not as level
                              Pclass = col_factor(NULL, F, F),
                              Sex = col_factor(NULL, F, F),
                              Embarked = col_factor(NULL, F, F)))

# double check column class
spec(trainHead)
str(trainHead)
# read with preformatted columns
train <- read_csv(fileTrain, col_types = spec(trainHead))

#clean up
rm(trainHead)

#------------------------------------------------------------------------------
# Data Wrangling
#------------------------------------------------------------------------------

# explore NA's
tblNa <- . %>% lapply(is.na) %>% lapply(sum) %>% .[. > 0] %>% unlist
train %>% tblNa

# Replace 2 missing embarked values with level "S"
table(train$Embarked)
train$Embarked[is.na(train$Embarked)] <- "S"
table(train$Embarked, !is.na(train$Embarked))

# add feature hasCabin
train$hasCabin <- TRUE
train$hasCabin[is.na(train$Cabin)] <- FALSE
table(train$hasCabin, !is.na(train$Cabin))
mean(train$hasCabin == !is.na(train$Cabin))
train$Cabin[is.na(train$Cabin)] <- "NO" # set NA to NO

# Add a feature for family size.
train$FamilySize <- 1 + train$SibSp + train$Parch

# Add a feature for tracking missing ages.
summary(train$Age)
train$MissingAge <- as.factor(ifelse(is.na(train$Age), "Y", "N"))

# Subset data to features we wish to keep/use.
features <- c("Survived", "Pclass", "Sex", "Age", "SibSp",
              "Parch", "Fare", "Embarked", "MissingAge",
              "FamilySize", "hasCabin")
train <- train[, features]
str(train)

#------------------------------------------------------------------------------
# Impute Missing Ages
#------------------------------------------------------------------------------

# impute missing values for the Age feature.
# First, transform all feature to dummy variables.
dummy.vars <- dummyVars(~ ., data = train[, -1])
train.dummy <- predict(dummy.vars, train[, -1])

# Now, impute!
pre.process <- preProcess(train.dummy, method = "bagImpute")
imputed.data <- predict(pre.process, train.dummy)
train$Age <- imputed.data[, "Age"]

# clean up
rm()

#------------------------------------------------------------------------------
# Split Data
#------------------------------------------------------------------------------

# create a 70/30% split of the training data,
# keeping the proportions of the Survived class
set.seed(54321)
trainIndex <- createDataPartition(train$Survived,
                               times = 1,
                               p = 0.7,
                               list = FALSE)
# add split feature and split to list, rename list nodes
train$Split <- "test"; train[trainIndex,]$Split <- "train"

data <- train %>%
    select(c("Survived", "Split")) %>%
    split(.$Split)
names(data) <- c("test", "train")

m <- Survived
data %>% sapply(`[[`, m) %>% map(as.numeric) %>% map(mean) %>% sapply(function(x) x = x - 1)

titanic.train <- data$train
titanic.train <- data$test

# alternatively
prop.table(table(train$Survived))
prop.table(table(titanic.train$Survived))
prop.table(table(titanic.test$Survived))

#=================================================================
# Train Model
#=================================================================

# Set up caret to perform 10-fold cross validation repeated 3
# times and to use a grid search for optimal model hyperparamter
# values.
tctrl <- trainControl(method = "repeatedcv",
                              number = 10,
                              repeats = 3,
                              search = "grid")

# Leverage a grid search of hyperparameters for xgboost. See
# the following presentation for more information:
# https://www.slideshare.net/odsc/owen-zhangopen-sourcetoolsanddscompetitions1
tgrid <- expand.grid(eta = c(0.05, 0.075, 0.1),
                         nrounds = c(50, 75, 100),
                         max_depth = 6:8,
                         min_child_weight = c(2.0, 2.25, 2.5),
                         colsample_bytree = c(0.3, 0.4, 0.5),
                         gamma = 0,
                         subsample = 1)

# Use the doSNOW package to enable caret to train in parallel.
# Register cluster so that caret will know to train in parallel.
cl <- makeCluster(4, type = "SOCK") # use 4 cores/threads/workers/processes
registerDoSNOW(cl)

# Train the xgboost model using 10-fold CV repeated 3 times
# and a hyperparameter grid search to train the optimal model.
fit <- train(Survived ~ .,
                  data = titanic.train,
                  method = "xgbTree",
                  tuneGrid = tgrid,
                  trControl = tctrl)
# stop paralel
stopCluster(cl)

# Examine caret's processing result
fit

# predict with best model with optimal/tuned hyperparameter values
# show effectiveness/performance
pred <- predict(fit, titanic.test)
confusionMatrix(pred, titanic.test$Survived)

