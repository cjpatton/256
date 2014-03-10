library(mlbench)
library(e1071)
library(class)
library(ROCR)
data(LetterRecognition)

## Using naive Bayes Classifier to predict the last 4000 outputs.

train <- rbind(LetterRecognition[1:16000,, 1], LetterRecognition[1:16000,, 2], LetterRecognition[1:16000,, 3], LetterRecognition[1:16000,, 4], LetterRecognition[1:16000,, 5])
#replicate the train data 5 times.

test <- LetterRecognition[16001:20000, ]
#use the last 4000 rows of data for examination.

model.naiveBayes <- naiveBayes(lettr~., train)
#train the naive Bayes classifier.

predictions.naiveBayes <- predict(model.naiveBayes, test)
#predict the last 4000 times

table(test$lettr, predictions.naiveBayes)
#compare the test and the prediction
#further discusstion needed here, any other suggestions?

#Using SVM classifier to predict

model.svm <- svm(lettr~., train)
#train the SVM classifier.

predictions.svm <- predict(model.svm, test)
#predict the last 4000 times

print(table(test$lettr, predictions.svm))
#compare the test and prediction result
#further discussion needed.

##Using knn to predict

predictions.knn <- knn(train[, 2:17], test[, 2:17], train$lettr, k = 5)
#we wanna a more delicate output.
#predict the last 4000 times

print(table(test$lettr, predictions.knn))
#compare the test and prediction result