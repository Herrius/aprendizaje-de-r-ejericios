#importar datos
library(data.table)
data <- fread("C:/Users/enrique/Downloads/r ejericios/2008.csv")
#exploracion
summary(data)
str(data)

head(data)
#limpieza de datos
clean_data <- na.omit(data, cols = c("DepDelay", "ArrDelay", "Origin", "Dest"))

clean_data[is.na(CarrierDelay), CarrierDelay := median(CarrierDelay, na.rm = TRUE)]
clean_data[is.na(WeatherDelay), WeatherDelay := median(WeatherDelay, na.rm = TRUE)]

#Transformar variables
clean_data[, `:=`(DayOfMonth = as.factor(DayofMonth), 
                  Month = as.factor(Month), 
                  DayOfWeek = as.factor(DayOfWeek))]

# Tomar una muestra representativa
sample_data <- clean_data[sample(.N, 100000)]

# Crear gráfico con geom_bin2d
library(ggplot2)
ggplot(sample_data, aes(x = DepDelay, y = ArrDelay)) + 
  geom_bin2d() + 
  theme_minimal()
#Modelo lineal simple para cada compañía
library(dplyr)
results <- clean_data %>%
  group_by(UniqueCarrier) %>%
  summarise(correlation = cor(DepDelay, ArrDelay, use = "complete.obs")) %>%
  arrange(desc(correlation))

top_company <- results %>% slice(1) %>% pull(UniqueCarrier)

#Filtrar datos de la compañía
company_data <- clean_data[UniqueCarrier == top_company]

#Ajuste del Modelo Complejo
library(biglm)
model <- biglm(ArrDelay ~ DepDelay + Origin + Dest + DayOfMonth + Month + DayOfWeek, data = company_data)
summary(model)

#Generalización y Escalabilidad
model_function <- function(data, company) {
  company_data <- data[UniqueCarrier == company]
  model <- biglm(ArrDelay ~ DepDelay + Origin + Dest + DayOfMonth + Month + DayOfWeek, data = company_data)
  return(summary(model))
}
#Implementación escalable
library(ff)
ff_data <- read.csv.ffdf(file="C:/Users/enrique/Downloads/r ejericios/airlines.csv")

