# Kaggle-challenge-Titanic
Predict the fate of each passenger (supervised learning)

The repositery contains: 
1. two csv files: 'train.csv' and 'test_csv'
2. two R codes: 'titanicRandomForest.R' and 'adjustFunction.R'

The R code 'titanicRandomForest.R' loads, cleans, reshapes and preprocesses the data contains in 'train.csv' in order to train a random forest algorithm to predict the outcomes (fate) of the passengers in the 'test_csv'. The model is built using the 'caret' R package. As a strategy, we decide to train our model distinguishing women and men. For each gender group, we split the 'train' data set into training and testing data sets and use a 5-fold cross validation to train our model. Finally, the last part of the code creates a csv file with the predicted outcomes for the passengers in the 'test.csv' file. 

