library(ggplot2)
library(dplyr)

colores_personalizados <- c("#ff0000", "#00b7ff", "#07db3c", "#f1f51a", "#cf1af3", "#ff7be9", "#ff9900")

crear_boxplot <- function(datos, clase = NULL, colores = colores_personalizados) {
  if (!is.null(clase)) {
    datos <- subset(datos, class == clase)
  }
  datos$airline <- as.factor(datos$airline)
  
  # Crear boxplot
  boxplot(price ~ airline, data = datos,
          main = paste("Distribución de Precios por Aerolínea", ifelse(!is.null(clase), paste("(Clase", clase, ")"), "")),
          xlab = "Aerolínea",
          ylab = "Precio",
          col = colores)
  
  # Contar outliers si se especifica la clase
  if (!is.null(clase)) {
    outliers <- boxplot(price ~ airline, data = datos, plot = FALSE)$out
    cat("Número de outliers en clase", clase, ":", length(outliers), "\n")
  }
}

# Función para crear gráfico de barras de precio promedio por aerolínea y clase
crear_grafico_precio_clase <- function(data) {
    precio_clase_aerolinea <- data %>%
    group_by(airline, class) %>%
    summarise(price = mean(price))

    # Crear el gráfico
    ggplot(precio_clase_aerolinea, aes(x = airline, y = price, fill = class)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    labs(
        title = "Precio por Aerolínea según Clase de Vuelo",
        x = "Aerolínea",
        y = "Precio Promedio",
        fill = "Clase de Vuelo"
    ) +
    scale_fill_manual(values = c('#3fc2ff', '#b7d9ff', '#87CEFA')) + # Colores personalizados
    theme_minimal() 

}

# Función para crear gráfico de barras del precio promedio por aerolínea y número de paradas
crear_grafico_precio_paradas <- function(data) {
  data_agrupados <- data %>%
    group_by(airline, stops) %>%
    summarise(price = mean(price, na.rm = TRUE)) %>%
    arrange(desc(price))
  
  ggplot(data_agrupados, aes(x = airline, y = price, fill = factor(stops))) +
    geom_bar(stat = "identity", position = position_dodge()) +
    labs(
      title = "Precio promedio por aerolínea y número de paradas",
      x = "Aerolínea",
      y = "Precio Promedio",
      fill = "Número de Paradas"
    ) +
    theme_minimal()
}


graficar_economy <- function(data) {
  # Filtrar y resumir los datos de Economy
  plane_economy <- data %>%
    filter(class == "Economy") %>%
    group_by(airline, days_left) %>%
    summarise(price = mean(price, na.rm = TRUE)) %>%
    ungroup()

  # Crear la gráfica de líneas para Economy
  ggplot(plane_economy, aes(x = days_left, y = price, color = as.factor(airline))) +
    geom_line() +
    labs(
      title = 'Precios en clase económica según los días faltantes a la fecha de viaje',
      x = 'Días faltantes al vuelo',
      y = 'Precio',
      color = 'Aerolínea'
    )
}

# Función para graficar los precios de la clase Business
graficar_business <- function(data) {

  plane_business <- data %>%
    filter(class == "Business") %>%
    group_by(airline, days_left) %>%
    summarise(price = mean(price, na.rm = TRUE)) %>%
    ungroup()

  # Crear la gráfica de líneas para Business
  ggplot(plane_business, aes(x = days_left, y = price, color = as.factor(airline))) +
    geom_line() +
    labs(
      title = 'Precios en clase business según los días faltantes a la fecha de viaje',
      x = 'Días faltantes al vuelo',
      y = 'Precio',
      color = 'Aerolínea'
    )
}

