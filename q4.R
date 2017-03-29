library(ElemStatLearn)
library(caret)

data("vowel.test")
data("vowel.train")

vowel.test$y <- as.factor(vowel.test$y)
vowel.train$y <- as.factor(vowel.train$y)

################### Question 1 ######################
#set seed
set.seed(33833)
#Build Random Forest Model
rfModel <- train(y ~ ., data = vowel.train, method = "rf")
#Build gbm model
gbmModel <- train(y ~ ., data = vowel.train, method = "gbm")

#Predict values
pred_v1 <- predict(rfModel,vowel.test)
pred_v2 <- predict(gbmModel, vowel.test)

#Accuracy for rf
confusionMatrix(pred_v1,vowel.test$y) # Accuracy 0.6039
#Accuracy for gbm
confusionMatrix(pred_v2, vowel.test$y) # Accuracy  0.5346

#Agreement accuracy
confusionMatrix(pred_v1,pred_v2) # Accuracy 0.7121

################### Question 2 ######################

library(caret)
library(gbm)

set.seed(3433)

library(AppliedPredictiveModeling)

data(AlzheimerDisease)

adData = data.frame(diagnosis,predictors)

inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]

training = adData[ inTrain,]

testing = adData[-inTrain,]

set.seed(62433)

rfModel2 <- train(diagnosis ~ ., data = training, method = "rf")

gbmModel2 <- train(diagnosis ~ ., data = training, method = "gbm")

ldaModel2 <- train(diagnosis ~ ., data = training, method = "lda")

#Predict values
p1 <- predict(rfModel2, training)
p2 <- predict(gbmModel2, training)
p3 <- predict(ldaModel2, training)
#Form dataframe
mydata <- data.frame(p1=p1, p2=p2, p3=p3, diagnosis=training$diagnosis)
#Train using randomforest
stackModel <- train(diagnosis ~ . , data=mydata, method="rf")

testStacked <- data.frame(p1=predict(rfModel2,testing),
                          p2=predict(gbmModel2,testing),
                          p3=predict(ldaModel2,testing))


#Stacked accuracy
confusionMatrix(predict(stackModel,testStacked),testing$diagnosis) #Accuracy 80%

#RF accuracy
confusionMatrix(predict(rfModel2,testing),testing$diagnosis) #Accuracy 76%
#GBM accuracy
confusionMatrix(predict(gbmModel2,testing),testing$diagnosis) #Accuracy 80%
#LDA accuracy
confusionMatrix(predict(ldaModel2,testing),testing$diagnosis) #Accuracy 76%

#Conclusion: Stacked Accuracy: 0.80 is better than random forests and lda and the same as boosting.

################### Question 3 ######################

set.seed(3523)

library(AppliedPredictiveModeling)

data(concrete)

inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]

training = concrete[ inTrain,]

testing = concrete[-inTrain,]


