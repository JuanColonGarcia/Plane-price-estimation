

numerizar_variables <- function(data) {
  # Numerización de variables
  data <- data %>% 
    mutate(
      # Crear columnas binarias para 'stops'
      stops_zero = if_else(stops == "zero", 1, 0),
      stops_one = if_else(stops == "one", 1, 0),
      stops_two_or_more = if_else(stops == "two_or_more", 1, 0),

      # Crear columnas binarias para 'class'
      class_Economy = if_else(class == "Economy", 1, 0),
      class_Business = if_else(class == "Business", 1, 0),

      # Crear columnas binarias para 'airline'
      airline_SpiceJet = if_else(airline == "SpiceJet", 1, 0),
      airline_AirAsia = if_else(airline == "AirAsia", 1, 0),
      airline_Vistara = if_else(airline == "Vistara", 1, 0),
      airline_GO_FIRST = if_else(airline == "GO_FIRST", 1, 0),
      airline_Indigo = if_else(airline == "Indigo", 1, 0),
      airline_Air_India = if_else(airline == "Air_India", 1, 0),

      # Crear columnas binarias para 'source_city'
      source_city_Delhi = if_else(source_city == "Delhi", 1, 0),
      source_city_Mumbai = if_else(source_city == "Mumbai", 1, 0),
      source_city_Bangalore = if_else(source_city == "Bangalore", 1, 0),
      source_city_Kolkata = if_else(source_city == "Kolkata", 1, 0),
      source_city_Hyderabad = if_else(source_city == "Hyderabad", 1, 0),
      source_city_Chennai = if_else(source_city == "Chennai", 1, 0),

      # Crear columnas binarias para 'departure_time'
      departure_time_Early_Morning = if_else(departure_time == "Early_Morning", 1, 0),
      departure_time_Morning = if_else(departure_time == "Morning", 1, 0),
      departure_time_Afternoon = if_else(departure_time == "Afternoon", 1, 0),
      departure_time_Evening = if_else(departure_time == "Evening", 1, 0),
      departure_time_Night = if_else(departure_time == "Night", 1, 0),
      departure_time_Late_Night = if_else(departure_time == "Late_Night", 1, 0),

      # Crear columnas binarias para 'destination_city'
      destination_city_Delhi = if_else(destination_city == "Delhi", 1, 0),
      destination_city_Mumbai = if_else(destination_city == "Mumbai", 1, 0),
      destination_city_Bangalore = if_else(destination_city == "Bangalore", 1, 0),
      destination_city_Kolkata = if_else(destination_city == "Kolkata", 1, 0),
      destination_city_Hyderabad = if_else(destination_city == "Hyderabad", 1, 0),
      destination_city_Chennai = if_else(destination_city == "Chennai", 1, 0),

      # Crear columnas binarias para 'arrival_time'
      arrival_time_Early_Morning = if_else(arrival_time == "Early_Morning", 1, 0),
      arrival_time_Morning = if_else(arrival_time == "Morning", 1, 0),
      arrival_time_Afternoon = if_else(arrival_time == "Afternoon", 1, 0),
      arrival_time_Evening = if_else(arrival_time == "Evening", 1, 0),
      arrival_time_Night = if_else(arrival_time == "Night", 1, 0),
      arrival_time_Late_Night = if_else(arrival_time == "Late_Night", 1, 0)
    ) %>%
    select(-airline, -source_city, -departure_time, -destination_city, -arrival_time, -class, -stops)  # Eliminar las columnas originales
  
  # Mostrar las primeras filas de las columnas seleccionadas
  selected_columns <- c("stops_zero", "stops_one", "stops_two_or_more",
                        "class_Economy", "class_Business", 
                        "airline_SpiceJet", "airline_AirAsia", 
                        "airline_Vistara", "airline_GO_FIRST", "airline_Indigo", "airline_Air_India",
                        "source_city_Delhi", "source_city_Mumbai", "source_city_Bangalore", 
                        "source_city_Kolkata", "source_city_Hyderabad", "source_city_Chennai",
                        "departure_time_Early_Morning", "departure_time_Morning", 
                        "departure_time_Afternoon", "departure_time_Evening", "departure_time_Night", 
                        "departure_time_Late_Night",
                        "destination_city_Delhi", "destination_city_Mumbai", 
                        "destination_city_Bangalore", "destination_city_Kolkata", 
                        "destination_city_Hyderabad", "destination_city_Chennai",
                        "arrival_time_Early_Morning", "arrival_time_Morning", 
                        "arrival_time_Afternoon", "arrival_time_Evening", "arrival_time_Night", 
                        "arrival_time_Late_Night")
  head(data[, selected_columns])
  
  return(data)
}


# Función para escalar las columnas 'duration' y 'days_left' usando Min-Max Scaling
escalar_columnas <- function(data) {
  
  data$duration <- (data$duration - min(data$duration)) / (max(data$duration) - min(data$duration))
  
  data$days_left <- (data$days_left - min(data$days_left)) / (max(data$days_left) - min(data$days_left))
  

  return(data)
}
