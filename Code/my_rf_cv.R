```{r}
# Creates a function to calculate MSE errors of a random forest prediction
# Takes a data set and a number of folds
# Calculates Cross Validation Error as mean of MSE
my_rf_cv <- function(train, k) {
  # Creates a fold vector to seperate data
  folds <- sample(rep(1:k, length = nrow(train)))
  # Stores an empty vector for MSE
  MSE <- numeric(length = k)
  for (i in 1:k) {
    # Specifies training data based on unused folds
    data_train <- train[folds != i,]
    # Specifies test data based on used folds
    data_test <- train[folds == i,]
    
    # Generates random forest model
    model <- randomForest(
      formula = body_mass_g ~ bill_length_mm + bill_depth_mm +     flipper_length_mm,
      data = data_train,
      ntree = 100)
    
    # Calculates and stores predictions for Sepal Length
    predictions <- as.data.frame(predict(model, data_test[, -6]))
    
    # Calculates MSE based on predictions and test data  
    MSE[i] <- colMeans(predictions - data_test[, 6])^2
  }
  
  # Stores output list of mean MSE
  output <- list("cv_err" = mean(MSE))
  # returns output
  return(output)
}
```