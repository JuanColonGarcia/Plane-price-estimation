
# Función para cargar y explorar los data
cargar_data <- function(data) {
  print(colnames(data))
  print(str(data))
  print(head(data))
  return(data)
}

# Función para mostrar estadísticas básicas
explorar_data <- function(data) {
  print(summary(data))
  print(dim(data))
  print(sum(is.na(data)))
}
