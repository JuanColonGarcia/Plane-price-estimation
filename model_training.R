SEED <- 250

grid_search_rf <- function(x_train, y_train, x_test, y_test, ntree_vals, mtry_vals) {
  best_rf <- NULL
  best_score <- -Inf
  best_params <- list()

  for (nt in ntree_vals) {
    for (mt in mtry_vals) {
      # Entrenar el modelo con la combinación actual de ntree y mtry
      temp_rf <- randomForest(x = x_train, y = y_train, ntree = nt, mtry = mt, importance = TRUE)
      
      # Hacer predicciones en el set de prueba y calcular el R^2
      temp_pred <- predict(temp_rf, newdata = x_test)
      temp_score <- cor(y_test, temp_pred)^2  # R^2 score en test
      
      # Guardar el modelo si el score es el mejor encontrado
      if (temp_score > best_score) {
        best_rf <- temp_rf
        best_score <- temp_score
        best_params <- list(ntree = nt, mtry = mt)
      }
    }
  }

  # Mostrar los mejores hiperparámetros y su rendimiento
  cat("Mejores parámetros: ntree =", best_params$ntree, ", mtry =", best_params$mtry, "\n")
  cat("Mejor R^2 en test:", best_score, "\n")

  # Retornar el mejor modelo y su score
  return(list(best_model = best_rf, best_score = best_score))
}

train_model <- function(data, class_tag) {

  if (class_tag == "Economy") {
    data <- data %>% filter(class_Economy == 1)
  } else if (class_tag == "Business") {
    data <- data %>% filter(class_Business == 1)
  } else {
    stop("Clase no válida. Debe ser 'Economy' o 'Business'.")
  }

  if (nrow(data) < 2) {
    stop("No hay suficientes datos para la clase proporcionada.")
  }

  X <- data %>% select(-price, -class_Economy, -class_Business)

  # Eliminar columnas con varianza cero
  X <- X[, apply(X, 2, var) != 0]


  y <- data$price

  set.seed(SEED)

  train_index <- createDataPartition(y, p = 0.75, list = FALSE)
  x_train <- X[train_index, ]
  x_test <- X[-train_index, ]
  y_train <- y[train_index]
  y_test <- y[-train_index]

  # Estandarizar
  scaler <- preProcess(x_train, method = c("center", "scale"))
  x_train_scaled <- predict(scaler, x_train)
  x_test_scaled <- predict(scaler, x_test)

  # Linear regression
  lm_model <- lm(y_train ~ .,
                 data = as.data.frame(cbind(y_train, x_train_scaled)))
  test_pred_lm <- predict(lm_model, newdata = as.data.frame(x_test_scaled))

  # Random forest
  rf_model <- randomForest(x = x_train_scaled,
                           y = y_train,
                           ntree = 100,
                           importance = TRUE)
  test_pred_rf <- predict(rf_model, newdata = as.data.frame(x_test_scaled))

  # Decission tree
  dt_model <- rpart(y_train ~ .,
                    data = as.data.frame(cbind(y_train, x_train_scaled)))
  test_pred_dt <- predict(dt_model, newdata = as.data.frame(x_test_scaled))

return(list(
    lm_model = lm_model,
    lm_pred = test_pred_lm,
    lm_train_score = round(summary(lm_model)$r.squared, 3),
    rf_model = rf_model,
    rf_pred = test_pred_rf,
    rf_train_score = round(rf_model$rsq[2], 3),
    dt_model = dt_model,
    dt_pred = test_pred_dt,
    dt_train_score = round(cor(y_train, predict(dt_model, newdata = as.data.frame(x_train_scaled)))^2, 3),
    test_val = y_test,
    x_train_scaled = x_train_scaled,
    y_train = y_train,
    x_test_scaled = x_test_scaled,
    y_test = y_test
))
}

