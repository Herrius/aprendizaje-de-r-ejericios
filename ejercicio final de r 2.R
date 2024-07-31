library(data.table)  # Usar data.table para manejo eficiente de datos grandes

# Cargar los datos con fread para una lectura más rápida
data <- fread("C:/Users/enrique/Downloads/r ejericios/2008.csv")

# Inspección rápida y manejo de NA
data <- na.omit(data)

# Convertir variables categóricas a factor utilizando data.table para mejor manejo de memoria
data[, c("UniqueCarrier", "Origin", "Dest") := .(as.factor(UniqueCarrier), as.factor(Origin), as.factor(Dest))]

# Normalizar las características numéricas (escalado eficiente)
numerical_features <- c("DepTime", "CRSDepTime", "ArrTime", "CRSArrTime", "FlightNum", "AirTime", "Distance", "TaxiIn", "TaxiOut", "DepDelay")
data[, (numerical_features) := lapply(.SD, scale), .SDcols = numerical_features]

# Selección de características basada en la importancia o correlación
# Suponiendo que has realizado un análisis para identificar las más relevantes
relevant_features <- c("DepTime", "CRSDepTime", "AirTime", "DepDelay", "Distance", "UniqueCarrier", "Origin", "Dest", "ArrDelay")
data <- data[, ..relevant_features]

# Muestreo simple para reducir el tamaño del dataset
set.seed(123)
sampled_data <- data[sample(.N, 2000)]  # Ajustar el número de muestras según la capacidad de tu máquina

library(caret)
# Dividir datos en entrenamiento y prueba
train_indices <- createDataPartition(sampled_data$ArrDelay, p = 0.8, list = FALSE)
train_data <- sampled_data[train_indices, ]
test_data <- sampled_data[-train_indices, ]

# Configuración de trainControl para manejar mejor los recursos
train_control <- trainControl(method = "cv", number = 5, allowParallel = TRUE)

# Entrenar el modelo SVM con un subconjunto de datos
svm_model <- train(ArrDelay ~ ., data = sampled_data, method = "svmRadial",
                   trControl = train_control,
                   preProcess = "scale",
                   tuneLength = 3)  
# Realizar predicciones y evaluar el modelo
predictions <- predict(svm_model, newdata = test_data)
conf_matrix <- confusionMatrix(predictions, test_data$ArrDelay)
print(svm_model)
print(conf_matrix)


library(e1071)
library(boot)
# Bootstrap con un número reducido de repeticiones
bootstrap_results <- boot(sampled_data, statistic = function(data, indices) {
  sample_data <- data[indices, ]
  fit <- svm(ArrDelay ~ ., data = sample_data)
  return(summary(fit)$r.squared)
}, R = 200)  # Reducir el número de repeticiones

# Análisis de resultados Bootstrap
print(summary(bootstrap_results))
hist(bootstrap_results$t, main = "Distribución de R cuadrado Bootstrap", xlab = "R cuadrado")


