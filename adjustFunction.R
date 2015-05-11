###########################
### Improving the model ###
###########################

# A general classification algorithm such as RF tree or GLM may fail to detect some correlations
# in the outcomes of people travelling. For instance, it is very likely that if a mother did not survive,
# her children did not as well. Similarly, if one of the children in a family did not survive, it is certain
# that the father did not survive too. The fate of a passenger is thus binded to that of his/her surrounding.  
# The algorithm is designed to adjust or "harmonize" the outcomes for people in the same group.


library(stringr)

# Break down the 'Name' variable into 'lastname', 'title' and 'maidenname'
break_name <- function(name){
        name <- gsub('[\"]', '', name)
        split_name <- str_trim(strsplit(name, split = '[,.\\(\\)]' )[[1]])
        index <- length(split_name)
        c('lastname'= split_name[1], 
          'title'= split_name[2], 
          'maidenname' = ifelse(index > 3, word(split_name[index],-1),'NONE'))
}


# Create a data frame with columns 'lastname', 'title' and 'maidenname'
data_name <- function(names){
        lastname <- NULL
        title <- NULL
        maidenname <- NULL
        names <- as.character(names)
        for (name in names){
                name <- break_name(name)
                lastname <- c(lastname, name[1])
                title <- c(title, name[2])
                maidenname <- c(maidenname, name[3])
        }
        data.frame('lastname'= lastname, 
                   'title'= title, 
                   'maidenname'= maidenname,
                   stringsAsFactors = F)
}


# For a given 'passenger', track the close relatives and friends in the data set 'df' on the boat
surrounding <- function(passenger, df){
        # Tracking close relatives
        index <- as.character(df$lastname) == as.character(passenger$lastname)
        index <- index | grepl(as.character(passenger$maidenname), as.character(df$lastname))
        index <- index | grepl(word(as.character(passenger$lastname),-1), as.character(df$maidenname))
        
        # Tracking friends
        index <- index | as.character(df$Ticket) == as.character(passenger$Ticket)
        
        index <- index & as.character(df$PassengerId) != as.character(passenger$PassengerId)
        df[index,]
}



# Clearly, we want to take advantage of the fact that we know the outcomes of the passengers in the 'train'
# data set. For instance, if we want to predict the fate of a young child whose mother is in 'train' and did not
# survive, we can assume that this child did not survive.


# Merge the 'train' and 'test' datasets

# Prepare the 'train' data set
train_df <- train
train_df <- cbind(data_name(train_df$Name), train_df)
feature_df <- names(train_df)[c(4,5,1:3,6,8:15)]
train_df <- train_df[feature_df]

head(train_df[order(train_df$lastname),],20)


# Prepare the 'test' data set 
predicted_test <- rbind(process_test_female, process_test_male)
re_indexing <- order(as.integer(row.names(predicted_test)))
test_df <- test
test_df$Survived <- c(final_prediction_female, final_prediction_male)[re_indexing]
test_df <- cbind(data_name(test_df$Name), test_df)
test_df <- test_df[feature_df]

head(test_df[order(test_df$lastname),],20)


# Merge everybody!
df <- rbind(train_df, test_df)


# Here is an example..!
df[886,]                        # This is our passenger
surrounding(df[886,], df)       # These are the people travelling with that person


# adjusting function!!
adjust_prediction <- function(data, train_data){
        indexing <- NULL
        adjusted_prediction <- NULL
        
        for (row in 1:nrow(data)){
                passenger <- data[row, ]
                
                # children and mothers in class 1
                if (passenger$Pclass == 1 & !is.na(passenger$Age) & passenger$Age < 18){
                        train_surrounding <- surrounding(passenger,train_data)
                        if (nrow(train_surrounding) > 0){
                                mother <- subset(train_surrounding, 
                                                 Sex == 'female' & Parch == passenger$SibSp + 1)
                                
                                if (nrow(mother) == 1){
                                        indexing <- c(indexing, row)
                                        adjusted_prediction <- c(adjusted_prediction,
                                                                 ifelse(mother$Survived == 1, 1, 0))
                                        next
                                }
                        }
                        
                        # children and mothers in class 2
                } else if (passenger$Pclass == 2 & !is.na(passenger$Age) & passenger$Age < 16){
                        train_surrounding <- surrounding(passenger,train_data)
                        if (nrow(train_surrounding) > 0){
                                mother <- subset(train_surrounding, 
                                                 Sex == 'female' & Parch == passenger$SibSp + 1)
                                
                                if (nrow(mother) == 1){
                                        indexing <- c(indexing, row)
                                        adjusted_prediction <- c(adjusted_prediction, 
                                                                 ifelse(mother$Survived == 1, 1, 0))
                                        next
                                }
                        }
                        
                        # children and mothers in class 3
                } else if (passenger$Pclass == 3 & !is.na(passenger$Age) & passenger$Age < 10){
                        train_surrounding <- surrounding(passenger,train_data)
                        if (nrow(train_surrounding) > 0){
                                mother <- subset(train_surrounding, 
                                                 Sex == 'female' & Parch == passenger$SibSp + 1)
                                
                                if (nrow(mother) == 1){
                                        indexing <- c(indexing, row)
                                        adjusted_prediction <- c(adjusted_prediction, 
                                                                 ifelse(mother$Survived == 1, 1, 0))
                                        next
                                }
                        }
                        
                        # mothers and children
                } else if (passenger$Sex == 'female' & passenger$Parch > 0){
                        train_surrounding <- surrounding(passenger,train_data)
                        if (nrow(train_surrounding) > 0){
                                children <- subset(train_surrounding, SibSp == passenger$Parch - 1)
                                
                                if (nrow(children) > 0){
                                        indexing <- c(indexing, row)
                                        adjusted_prediction <- c(adjusted_prediction, 
                                                                 ifelse(1 %in% children$Survived, 1, 0))
                                        next
                                }
                        }
                        
                        # fathers and children 
                } else if (passenger$Sex == 'male' & passenger$Parch > 0){
                        train_surrounding <- surrounding(passenger,train_data)
                        
                        if (nrow(train_surrounding) > 0){
                                children <- subset(train_surrounding, SibSp == passenger$Parch - 1)
                                
                                if (nrow(children) > 0){
                                        if (0 %in% children$Survived){
                                                indexing <- c(indexing, row)
                                                adjusted_prediction <- c(adjusted_prediction, 0)
                                                next
                                        }
                                }
                        }
                }
        }    
        new_data <- data[indexing,]
        new_data$Survived <- adjusted_prediction
        new_data
}



############################################
### Evaluation on the testing dataset... ###
############################################

# First adjustment: this one compare only the outcomes predicted for the 'testing' data set with
# that of the people in the 'training' data set.
inTrain <- sort(as.integer(c(row.names(training_female[inTrain_female,]),
                             row.names(training_male[inTrain_male,]))))

results <- adjust_prediction(train_df[-inTrain,], train_df[inTrain,])

adjusted_testing_prediction <- testing_prediction

indexing <- which(train_df[-inTrain,]$PassengerId %in% results$PassengerId)
adjusted_testing_prediction[indexing] <- results$Survived

confusionMatrix(adjusted_testing_prediction, train[-inTrain,]$Survived)
confusionMatrix(testing_prediction, train[-inTrain,]$Survived)


# Second adjustment: the adjusted outcomes coming from the fist adjustment are fairly
# reliable. We can join them the 'training' set to test against them the remaining values
# in the 'testing' data set.
second_results <- adjust_prediction(train_df[-c(inTrain,
                                                rbind(results, second_results)$PassengerId),],
                                    train_df[c(inTrain,
                                               rbind(results, second_results)$PassengerId),])

second_adjusted_testing_prediction <- adjusted_testing_prediction

second_indexing <- which(train_df[-inTrain,]$PassengerId 
                         %in% c(inTrain, rbind(results, second_results)$PassengerId)
                         
                         second_adjusted_testing_prediction[second_indexing] <- second_results$Survived
                         
                         confusionMatrix(second_adjusted_testing_prediction, train[-inTrain,]$Survived)
                         confusionMatrix(testing_prediction, train[-inTrain,]$Survived)
                         
                         
# third: we can reiterate the above process, though it seems that after two readjustments,
# there is no improvement.
third_results <- adjust_prediction(train_df[-c(inTrain, second_results$PassengerId),],
                                   train_df[c(inTrain, second_results$PassengerId),])

third_adjusted_testing_prediction <- third_adjusted_testing_prediction
third_adjusted_testing_prediction[which(train_df[-inTrain,]$PassengerId
                                        %in% third_results$PassengerId)] <- third_results$Survived

confusionMatrix(third_adjusted_testing_prediction, train[-inTrain,]$Survived)

confusionMatrix(testing_prediction, train[-inTrain,]$Survived)
                        
# After the second adjustment, the accuracy of our initial random forest prediction is improved by nearly 2%. 



#########################
### Final submisssion ###
#########################

# first adjustment...
adjusted_test_prediction <- test_prediction

index <- which(test_df$PassengerId %in% adjust_prediction(test_df, train_df)$PassengerId)
adjusted_test_prediction[index] <- adjust_prediction(test_df, train_df)$Survived

confusionMatrix(test_prediction, test_key$Survived)
confusionMatrix(adjusted_test_prediction, test_key$Survived)


# Second adjustment...
second_adjusted_test_prediction <- adjusted_test_prediction

second_index <- which(test_df$PassengerId %in% 
                              adjust_prediction(test_df[-index,],rbind(train_df, test_df[index,]))$PassengerId)

second_adjusted_test_prediction[second_index] <- adjust_prediction(test_df[-index,],
                                                                    rbind(train_df, test_df[index,]))$Survived


# Create a csv file with the (adjusted) predicted outcomes..!
submission <- data.frame('PassengerId'= test$PassengerId,
                         'Survived'= second_adjusted_test_prediction)
write.csv(submission, file='adjustedTitanicSubmissionRF.csv', quote=F, row.names=F)


# On Kaggle, this submission predicts the fate of the passengers in 'test.csv' with a 78.947 % accuracy. 
# However, Kaggle returns a evaluation based on a few of the outcomes only. The exact accuracy is for now
# unkown. The confusion matrices suggests a clear improvement for the accurracy though. 












