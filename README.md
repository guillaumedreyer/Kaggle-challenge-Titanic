# Kaggle-challenge-Titanic
Predict the fate of the passengers of the Titanic (supervised learning)

The repositery constains 6 files:

1. The two csv files *train.csv* and *test.csv* displaying the data about each passenger;

2. The two R codes files *titanicRandomForest.R* and *adjustFunction.R*;

3. The two csv files *TitanicSubmissionRF.csv* and *adjustedTitanicSubmissionRF.csv* with the predicted outcomes returned by the R codes *titanicRandomForest.R* and *adjustFunction.R*.

The R code *titanicRandomForest.R* loads, cleans, reshapes and processes the data in 'train.csv' in order to train a random forest algorithm that predicts the fate (survived, not survived) of the passengers in the *test.csv* data set. Both csv files must be placed in the working directory. The training is based on a 5-fold cross-valildation. Moreover, to evaluate the accuracy of our model, we split the train dataset into training and testing. Finally, the *Final submission* part in the R code creates a csv file 'TitanicSubmissionRF.csv' with the predicted outcomes for the *test.csv* data set. The model is built using the **caret** R package.

Then the code *adjustedTitanicSubmissionRF.csv* takes the predicted outcomes returned by the R code *titanicRandomForest.R*, and readjusts the outcomes based on family/friends considerations.


