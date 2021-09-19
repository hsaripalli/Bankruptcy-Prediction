########## KNN Model ##########
trctrl <- trainControl(method = "cv", number = 10)

knn_fit <- train(Competitive. ~., data = train, method = "knn",
                 trControl=trctrl,
                 preProcess = c("center", "scale"), 
                 tuneLength = 10)
print(knn_fit)

test_pred <- predict(knn_fit, newdata = test)
confusionMatrix(test_pred, test$Competitive.)