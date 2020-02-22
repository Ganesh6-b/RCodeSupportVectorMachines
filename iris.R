data(iris)
View(iris)

library(caret)

split <- createDataPartition(iris$Species, p = 0.75, list= FALSE)

Train_data <- iris[split,]
Test_data <- iris[-split,]

Train_data[,-5] <- scale(Train_data[,-5])
Test_data[,-5] <- scale(Test_data[,-5])

View(Train_data)

library(e1071)
attach(Train_data)
model1 <- svm(Train_data$Species~., data = Train_data, kernel = "linear")
summary(model1)

pred <- predict(model1, Test_data)
pred

library(gmodels)
a <- CrossTable(pred, Test_data$Species)

mean(pred==Test_data$Species)
