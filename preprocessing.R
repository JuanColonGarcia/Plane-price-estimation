preprocess <- function(data) {
  data_filtered <- data %>%
    filter(class_Economy == 1 | class_Business == 1)  
  
  data_filtered$class_Economy <- as.factor(data_filtered$class_Economy)
  data_filtered$class_Business <- as.factor(data_filtered$class_Business)

  data_model <- data_filtered %>% select(everything())

  return(data_model)
}
