# Función para eliminar columnas innecesarias
eliminar_columnas <- function(datos) {
  datos <- select(datos, -X, -flight)
  return(datos)
}

# Función para eliminar outliers en la columna "price"
eliminar_outliers <- function(datos, outliers) {
  datos <- datos[!(datos$price %in% outliers), ]
  return(datos)
}

# Función para identificar y eliminar outliers en la columna "price"
eliminar_outliers1 <- function(datos) {
  datos_economy <- subset(datos, class == "Economy")
  datos_business <- subset(datos, class == "Business")
  outliers_economy <- boxplot(price ~ airline, data = datos_economy, plot = FALSE)$out
  outliers_business <- boxplot(price ~ airline, data = datos_business, plot = FALSE)$out
  datos <- datos[!(datos$price %in% c(outliers_economy, outliers_business)), ]
  return(datos)
}
