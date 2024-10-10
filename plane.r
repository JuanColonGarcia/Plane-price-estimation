library(dplyr)
library(tidyr)
library(ggplot2)
library(arules)


plane <- read.csv("C:/Users/UPV/R/plane/Clean_Dataset.csv")
colnames(plane)

str(plane) #INDICA EL TIPO
head(plane) #INDICA LAS PRIMERAS FILAS
str(plane)

#################################################################
#ELIMINAMOS COLUMNA NO NECESARIAS
#################################################################

#COLUMNA X

plane2 <- select(plane, -X)
head(plane2)
#COLUMNA flight

#plane2 <- select(plane, -flight)
#head(plane2)

unique(plane2$stops)    

#################################################################
#VALORES FALTANTES 
#################################################################
valores_faltantes <- sum(is.na(plane2))
valores_faltantes

#################################################################
#NUMERIZAR 
#################################################################
#NUMERIZAR LA VARIABLES STOPS

plane2 <- plane2 %>%
  mutate(stops = case_when(
    stops == "zero" ~ 0,
    stops == "one"  ~ 1,
    stops == "two_or_more"  ~ 2,
  ))
head(plane2)

#NUMERIZAR LA VARIABLES STOPS

plane2 <- plane2 %>%
  mutate(class = case_when(
    class == "Economy" ~ 0,
    class == "Business"  ~ 1,
  ))
head(plane2)

#NUMERIZAR LA VARIABLES AIRLINES

plane2 <- plane2 %>%
  mutate(airline = case_when(
    airline == "SpiceJet" ~ 0,
    airline == "AirAsia"  ~ 1,
    airline == "Vistara" ~ 2,
    airline == "GO_FIRST"  ~ 3,
    airline == "Indigo" ~ 4,
    airline == "Air_India"  ~ 5,
  ))
head(plane2)

#NUMERIZAR LA VARIABLES SOURCE CITY

plane2 <- plane2 %>%
  mutate(source_city = case_when(
    source_city == "Delhi" ~ 0,
    source_city == "Mumbai"  ~ 1,
    source_city == "Bangalore" ~ 2,
    source_city == "Kolkata"  ~ 3,
    source_city == "Hyderabad" ~ 4,
    source_city == "Chennai"  ~ 5,
  ))
head(plane2)

#NUMERIZAR LA VARIABLES DEPARTURE TIME

plane2 <- plane2 %>%
  mutate(departure_time = case_when(
    departure_time == "Early_Morning" ~ 0,
    departure_time == "Morning"  ~ 1,
    departure_time == "Afternoon" ~ 2,
    departure_time == "Evening"  ~ 3,
    departure_time == "Night" ~ 4,
    departure_time == "Late_Night"  ~ 5,
  ))
head(plane2)

#NUMERIZAR LA VARIABLES DESTINATION CITY

# 30 combinaciones 

plane2 <- plane2 %>%
  mutate(destination_city = case_when(
    destination_city == "Delhi" ~ 0,
    destination_city == "Mumbai"  ~ 1,
    destination_city == "Bangalore" ~ 2,
    destination_city == "Kolkata"  ~ 3,
    destination_city == "Hyderabad" ~ 4,
    destination_city == "Chennai"  ~ 5,
  ))
head(plane2)

#NUMERIZAR LA VARIABLES ARRIVAL TIME

plane2 <- plane2 %>%
  mutate(arrival_time = case_when(
    arrival_time == "Early_Morning" ~ 0,
    arrival_time == "Morning"  ~ 1,
    arrival_time == "Afternoon" ~ 2,
    arrival_time == "Evening"  ~ 3,
    arrival_time == "Night" ~ 4,
    arrival_time == "Late_Night"  ~ 5,
  ))
head(plane2)

unique(plane2$destination_city)    

str(plane2)

#################################################################
#CAMBIA LA DURACION DE HORAS POR MINUTOS
#################################################################

plane2 <- plane2 %>%
  mutate(duration = duration * 60)

head(plane2)

#################################################################
#CAMBIAMOS PRECIO DE DIRHAM POR EURO
#################################################################

plane2 <- plane2 %>%
  mutate(price_euro = price / 10.8)

head(plane2)

unique(plane2$class)    

#################################################################
#REPRESENTACIONES
#################################################################


# Histograma para la variable 'price_euro'
ggplot(plane2, aes(x = price_euro)) +
  geom_histogram(bins = 30, fill = "#0c8b01", color = "black") +
  labs(title = "Distribución de Precios en Euros", x = "Precio en Euros", y = "Frecuencia")

library(ggplot2)

# Gráfico de dispersión para 'duration' vs 'price_euro'
ggplot(plane2, aes(x = duration, y = price_euro)) +
  geom_point(alpha = 0.5, color = "blue") +
  labs(title = "Relación entre Duración y Precio en Euros", x = "Duración (horas)", y = "Precio en Euros")


#################################################################
#OUTLIERS
#################################################################


caja_bigotes <- ggplot(plane2, aes(x = "", y = price)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 1) +
  labs(title = "Boxplot del Precio", y = "Precio") +
  theme_minimal()

caja_bigotes

outliers <- boxplot(plane2$price)$out
length(outliers)

nrow(plane2)

#El numero de outliers son 123 de 300153, eso supone el 0.04% al ser 
#menos que el 5% del dataset, se pueden eliminar

plane2 <- plane2[!(plane2$price %in% outliers),]


caja_bigotes2 <- ggplot(plane2, aes(x = "", y = price)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 1) +
  labs(title = "Boxplot del Precio", y = "Precio") +
  theme_minimal()

caja_bigotes2

#################################################################
#CORRELACIONES
#################################################################

# Precio y Duración
cor(plane2$price, plane2$duration)

# Precio y Número de Paradas

cor(plane2$price, plane2$stops)

# Precio y Días Restantes

cor(plane2$price, plane2$days_left)

# Calcular la matriz de correlación
matriz_correlacion <- cor(plane2 %>% select(airline ,source_city, departure_time, stops, arrival_time,
                        destination_city, class, duration, days_left, price, price_euro))

# Mostrar la matriz
print(matriz_correlacion)

# Visualizar la matriz de correlación
library(ggcorrplot)
ggcorrplot(matriz_correlacion, method = "circle")

# Histograma para la variable 'price_euro'
# Gráfico de barras con la media del precio por clase
ggplot(plane2, aes(x = class, y = price)) +
  stat_summary(fun = mean, geom = "bar", fill = "#0c8b01", color = "black") +
  labs(title = "Precio Medio por Clase", x = "Clase", y = "Precio Medio") +
  theme_minimal()

library(ggplot2)

nrow(filter(plane2, class == 1))
nrow(filter(plane2, class == 0))

nrow(filter(plane2, destination_city == 5))
nrow(filter(plane2, destination_city == 5))



# Filtrar el dataframe para obtener solo las filas de clase "Economy"
filter_economy <- plane2 %>%
  filter(class == 0) %>%  # 0 para 'Economy'
  group_by(airline, days_left) %>%
  summarise(price = mean(price)) %>%
  ungroup()

# Crear la gráfica de líneas
ggplot(filter_economy, aes(x = days_left, y = price, color = as.factor(airline))) +
  geom_line() +
  labs(
    title = 'Precios en clase económica según los días faltantes a la fecha de viaje',
    x = 'Días faltantes al vuelo',
    y = 'Precio',
    color = 'Aerolínea'
  ) 


# Crear el gráfico
ggplot(df_business, aes(x = days_left, y = price, color = as.factor(airline))) +
  geom_line(linewidth = 1) +  
  scale_color_manual(values = colors) +
  labs(
    title = 'Precios en clase business según los días faltantes a la fecha de viaje',
    x = 'Días faltantes al vuelo',
    y = 'Precio',
    color = 'Aerolínea'
  )