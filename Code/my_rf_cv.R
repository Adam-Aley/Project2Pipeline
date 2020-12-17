#' Random Forest Cross-Validation
#'
#' This function runs a random forest cross-validation prediction
#' for a given set of data and calculates the the MSE errors
#'
#' @param train Data set to run model on
#' @param k Total number of folds per model
#' @keywords prediction
#'
#' @return Output list with values of the average MSE from total folds
#'
#' @examples
#' data("my_penguins")
#' new_penguins <- na.omit(my_penguins)
#' my_rf_cv(new_penguins, 5)
#' my_rf_cv(new_penguins, 10)
#'
#' @import randomForest
#'
#' @export
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
      formula = body_mass_g ~ bill_length_mm + bill_depth_mm + flipper_length_mm,
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