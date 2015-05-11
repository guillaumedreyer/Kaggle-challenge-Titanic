# Kaggle-challenge-Titanic
Predict the fate of each passenger (supervised learning)

The repositery contains: 
1. two csv files: 'train.csv' and 'test_csv'
2. two R codes: 'titanicRandomForest.R' and 'adjustFunction.R'

The R code 'titanicRandomForest.R' loads, cleans, reshapes and preprocesses the data contains in 'train.csv' in order to train a random forest algorithm to predict the outcomes (fate) of the passengers in 'test_csv'. The csv files must be placed in the working directory. As a strategy to improve accuracy, the model is trained distinguishing women and men. For each gender group, the algorithm is trained using a 5-fold cross validation. Moreover, to evaluate our model, the 'train' data set is splitted into training and testing data sets. Finally, the "Final Submission" part of the code creates a csv file with the predicted outcomes for the passengers in the 'test.csv' file, based on the outcomes in the 'train.csv'. All this work is done using the amazing 'caret' R package.

