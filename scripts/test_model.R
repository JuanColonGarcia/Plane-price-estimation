print_errors <- function(name, train_score, errors) {
  cat("\n--- ", name, " --- \n")
  cat("Score Train: ", train_score, "\n")
  cat("Score Test: ", errors$r2, "\n")
  cat("MAE Test: ", errors$mae, "\n")
  cat("MSE Test: ", errors$mse, "\n")
  cat("RMSE Test: ", errors$rmse, "\n")
}

test_model <- function(name, pred, test, train_score) {
  r2 <- round(cor(test, pred)^2, 4)
  mae <- round(mean(abs(test - pred)), 3)
  mse <- round(mean((test - pred)^2), 3)
  rmse <- round(sqrt(mse), 3)

  result <- list(
    r2 = r2,
    mae = mae,
    mse = mse,
    rmse = rmse
  )

  print_errors(name, train_score, result)

  return(result)
}

test_results <- function(model, class) {
  cat(class, "\n")
  
  test_val <- model$test_val
  
  test_model("Linear Reg.", model$lm_pred, test_val, model$lm_train_score)
  test_model("Random Forest", model$rf_pred, test_val, model$rf_train_score)
  test_model("Decission Tree", model$dt_pred, test_val, model$dt_train_score)
}