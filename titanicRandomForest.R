##########################
### Load the data sets ###
##########################

train <- read.csv('train.csv', stringsAsFactors=T, na.strings = '')

test <- read.csv('test.csv', stringsAsFactors=T, na.strings = '')



###################################
### Cleaning, feature selection ###
###################################

# Look for missing values...
sapply(train, function(x){sum(is.na(x))})


# Missing values in Embarked...
train$Embarked <- as.character(train$Embarked)
train[is.na(train$Embarked),]$Embarked <- 'C'
train$Embarked <- as.factor(train$Embarked)

test$Embarked <- as.character(test$Embarked)
test[is.na(test$Embarked),]$Embarked <- 'C'
test$Embarked <- as.factor(test$Embarked)

feature_selection_tree <- c('Survived', 'Pclass', 'Sex', 'Age', 
                            'Fare', 'SibSp', 'Parch', 'Embarked')

test_tree <- test
test_tree$Survived <- NA
test_tree <- test_tree[feature_selection_tree]
sapply(test_tree, function(x){sum(is.na(x))})



################################
### Processing the data sets ###
################################

# We will train our algorithm based on females and males.
training <- train[feature_selection_tree]



###############
### females ###
###############

# Training and testing sets... 
training_female <- subset(training, Sex == 'female')
test_tree_female <- subset(test_tree, Sex == 'female')


# Splitting 'training_female' into training and testing sets
library(caret)
set.seed(125)
inTrain_female <- createDataPartition(training_female$Survived, p = 0.7, list = F)
training_tree_female <- training_female[inTrain_female,]
testing_tree_female <- training_female[-inTrain_female,]

sapply(training_tree_female, function(x){sum(is.na(x))})
sapply(testing_tree_female, function(x){sum(is.na(x))})


# Missing values in Age and Fare
library(RANN)
preProc_female <- preProcess(training_tree_female[,c(4,5,6,7)], method = 'knnImpute')


process_training_female <- cbind(training_tree_female[,c(1,2,3,8,6,7)], 
                                 predict(preProc_female, training_tree_female[,c(4,5,6,7)])[-(3:4)])
process_training_female <- process_training_female[feature_selection_tree]


process_testing_female <- cbind(testing_tree_female[,c(1,2,3,8,6,7)], 
                                predict(preProc_female, testing_tree_female[,c(4,5,6,7)])[-(3:4)])
process_testing_female <- process_testing_female[feature_selection_tree]


process_test_female <- cbind(test_tree_female[,c(1,2,3,8,6,7)], 
                             predict(preProc_female, test_tree_female[,c(4,5,6,7)])[-(3:4)])
process_test_female <- process_test_female[feature_selection_tree]


sapply(process_training_female, function(x){sum(is.na(x))})
sapply(process_testing_female, function(x){sum(is.na(x))})
sapply(process_test_female, function(x){sum(is.na(x))})



##############################
### Train the algorithm... ###
##############################

# Fit the model for 'female'
fitControl_rf <- trainControl(method = 'repeatedcv', number = 5, repeats = 5)
# fitControl_rf <- trainControl(method = 'repeatedcv', number = 10, repeats = 3)
# fitControl_rf <- trainControl(method = 'oob', number = 5, repeats = 1)

set.seed(34536)
modelFit_process_rf_female <- train(process_training_female$Survived ~ . , 
                                    trControl =  fitControl_rf,
                                    data = process_training_female, method = 'rf')
modelFit_process_rf_female


# Predictions for 'female'!
predictions_process_rf_female <- predict(modelFit_process_rf_female, process_testing_female)
predictions_process_rf_female <- as.integer(predictions_process_rf_female > 0.5)


# Confusion matrix for 'female'
confusionMatrix(process_testing_female$Survived, predictions_process_rf_female)



#############
### Males ###
#############

training_male <- subset(training, Sex == 'male')
test_tree_male <- subset(test_tree, Sex == 'male')


# Splitting 'training_male' into training and testing datasets 'training_tree_male' and 'testing_tree_male'
library(caret)
set.seed(125)
inTrain_male <- createDataPartition(training_male$Survived, p = 0.7, list = F)
training_tree_male <- training_male[inTrain_male,]
testing_tree_male <- training_male[-inTrain_male,]

sapply(training_tree_male, function(x){sum(is.na(x))})
sapply(testing_tree_male, function(x){sum(is.na(x))})


# Missing values in Age and Fare...
library(RANN)
preProc_male <- preProcess(training_tree_male[,c(4,5,6,7)], method = 'knnImpute')


process_training_male <- cbind(training_tree_male[,c(1,2,3,8,6,7)], 
                               predict(preProc_male, training_tree_male[,c(4,5,6,7)])[-(3:4)])
process_training_male <- process_training_male[feature_selection_tree]


process_testing_male <- cbind(testing_tree_male[,c(1,2,3,8,6,7)], 
                              predict(preProc_male, testing_tree_male[,c(4,5,6,7)])[-(3:4)])
process_testing_male <- process_testing_male[feature_selection_tree]


process_test_male <- cbind(test_tree_male[,c(1,2,3,8,6,7)], 
                           predict(preProc_male, test_tree_male[,c(4,5,6,7)])[-(3:4)])
process_test_male <- process_test_male[feature_selection_tree]


sapply(process_training_male, function(x){sum(is.na(x))})
sapply(process_testing_male, function(x){sum(is.na(x))})
sapply(process_test_male, function(x){sum(is.na(x))})



###########################
### Train the algorithm ###
###########################

# Fit the model for 'male'
fitControl_rf <- trainControl(method = 'repeatedcv', number = 5, repeats = 5)
#fitControl_rf <- trainControl(method = 'repeatedcv', number = 10, repeats = 3)
# fitControl_rf <- trainControl(method = 'oob', number = 5, repeats = 1)

set.seed(34536)
modelFit_process_rf_male <- train(process_training_male$Survived ~ . , 
                                  trControl =  fitControl_rf,
                                  data = process_training_male, method = 'rf')
modelFit_process_rf_male


# Predictions for 'male'
predictions_process_rf_male <- predict(modelFit_process_rf_male, process_testing_male)
predictions_process_rf_male <- as.integer(predictions_process_rf_male > 0.5)


# Confusion matrix for 'male'
confusionMatrix(process_testing_male$Survived, predictions_process_rf_male)


# Overall evaluation
testing_prediction <- c(predictions_process_rf_female,predictions_process_rf_male)

re_indexing <- order(as.integer(c(row.names(testing_tree_female),
                                  row.names(testing_tree_male))))

testing_prediction <- testing_prediction[re_indexing]
testing_survived <- c(testing_tree_female$Survived, testing_tree_male$Survived)[re_indexing]

confusionMatrix(testing_prediction, testing_survived)



########################
### Final submission ###
########################

final_prediction_female <- as.integer(predict(modelFit_process_rf_female,
                                              process_test_female[,-1]) > 0.5)

final_prediction_male <- as.integer(predict(modelFit_process_rf_male, 
                                            process_test_male[,-1]) > 0.5)


# Prediction for the passengers in the 'test' data set 
final_prediction <- process_test$Survived
final_prediction[process_test$Sex == 'female'] <- final_prediction_female
final_prediction[process_test$Sex == 'male'] <- final_prediction_male

submission <- data.frame('PassengerId'= reshaped_test$PassengerId,
                         'Survived'= final_prediction)

write.csv(submission,file='Titanic_submission_rf.csv',quote=F, row.names=F)





