# Función para calcular las correlaciones con 'price'
calcular_correlacion_price <- function(data, threshold = 0.7) {

  matriz_correlacion <- cor(data)
  correlaciones_price <- matriz_correlacion["price", , drop = FALSE]
  print(correlaciones_price)

  # Visualización de la matriz de correlación
  corrplot(matriz_correlacion, method = "circle", type = "upper", order = "hclust", 
           addrect = 2, col = colorRampPalette(c("red", "white", "blue"))(200))

  # Filtrar correlaciones con 'price' y mayor que el umbral
  corr_with_price <- as.data.frame(as.table(matriz_correlacion)) %>%
    filter(abs(Freq) > threshold & (Var1 == "price" | Var2 == "price")) %>%
    filter(Var1 != Var2)  # Excluir la diagonal (auto-correlación)

  return(corr_with_price)
}


