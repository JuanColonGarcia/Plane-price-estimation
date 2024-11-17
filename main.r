source("carga_datos.R")
source("limpieza_datos.R")
source("visualizacion.R")
source("analisis.R")
source("load_libraries.R")
source("transformacion.R")
source("preprocessing.R")
source("model_training.R")
source("test_model.R")

# Llamada a la función con las librerías necesarias

FLIGHTS_DATASET_PATH_1 <- file.path("Clean_Dataset.csv")

install_and_load(c("randomForest", "dplyr", "rpart", 
"ggplot2", "tidyr", "arules", "corrplot", "caret"))

# 1. Cargar los datos
data <- readr::read_csv(FLIGHTS_DATASET_PATH_1)

# Explorar los datos
data <- cargar_data(data)   
explorar_data(data)       

# Limpiar data
data <- eliminar_columnas(data)
data <- eliminar_outliers(data)
data <- eliminar_outliers1(data)


# Visualizar data
 crear_boxplot(data)
 crear_grafico_precio_clase(data)
 crear_grafico_precio_paradas(data)
 graficar_economy(data)
 graficar_business(data)

#Transformacion

data <- numerizar_variables(data)
data <- escalar_columnas(data) 

#Analisis
calcular_correlacion_price(data)

write.csv(data, "flights.csv", row.names = FALSE)

# Asegúrate de que 'df' es tu DataFrame




#######################################################################
## PREDICCION
####################################################################

DATASETS_PATH <- "../datasets"
FLIGHTS_DATASET_PATH_2 <- file.path("flights.csv")


# 1. Cargar los datos
data <- readr::read_csv(FLIGHTS_DATASET_PATH_2)

# 2. Preprocessing
data_model <- preprocess(data)

# 3. Entrenar los modelos
business_results <- train_model(data_model,  "Business")
economic_results <- train_model(data_model, "Economy")


# 4. Resultados
test_results(business_results, "Business")
test_results(economic_results, "Economy")


