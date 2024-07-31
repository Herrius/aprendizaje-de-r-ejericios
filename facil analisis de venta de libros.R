# Cargar librerías necesarias
library(readr)
library(dplyr)
library(ggplot2)
library(visdat)
library(corrplot)
#carga datos
sales_data <- read_csv(file.path(r"(C:\Users\enrique\Downloads\archive\Books_Data_Clean.csv)"))
# Ver estructura de datos
str(sales_data)
# resumen de los datos
summary(sales_data)
# Visualizar datos faltantes
vis_miss(sales_data)
# Histograma de precio
ggplot(sales_data, aes(x=`sale price`)) + geom_histogram(bins = 10, fill = "#006ee6", color = "black") +
  labs(title = "Distribución de Precios de Libros", x = "Precio", y = "Frecuencia")

ggplot(sales_data, aes(x=`Publishing Year`)) + geom_histogram(bins = 10, fill = "#006ee6", color = "black") +
  labs(title = "Distribución de Precios de Libros", x = "Precio", y = "Frecuencia")
# Gráfico de barras de una variable categórica (ajusta 'categoria' a tu variable real)
ggplot(sales_data, aes(x=`language_code`)) + geom_bar(fill = "#ff9100", color = "black") +
  labs(title = "Frecuencia de Categorías de Libros", x = "Categoría", y = "Cantidad")

# Boxplot para identificar outliers en una variable numérica
ggplot(sales_data, aes(y=`sale price`, x=`genre`)) + geom_boxplot(fill = "#ff9100", color = "black") +
  labs(title = "Distribución de Precios por Categoría", x = "Generos", y = "Precio")

# Tabla de contingencia para categorías
table(sales_data$genre)
# Buscar duplicados
sales_data[duplicated(sales_data), ]

# Muestra aleatoria de 10 filas
sample_n(sales_data, 10)

# Los valores nulos colocar la variable categoria mas frecuente en language_code
# Identificar la categoría más frecuente en 'language_code'
categoria_mas_frecuente <- sales_data %>%
  group_by(language_code) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  slice(1) %>%
  pull(language_code)
# Sustituir valores nulos en 'language_code' con la categoría más frecuente
sales_data <- sales_data %>%
  mutate(language_code = ifelse(is.na(language_code), categoria_mas_frecuente, language_code))

vis_miss(sales_data)
# Eliminar datos nulos sin titulo de book name
# Eliminar filas donde 'book_name' es nulo
sales_data <- sales_data %>%
  filter(!is.na(`Book Name`))

vis_miss(sales_data)

# Transformar 'language_code' para unificar variantes de inglés
sales_data <- sales_data %>%
  mutate(language_code = ifelse(grepl("en", language_code), "eng", language_code))

ggplot(sales_data, aes(x=`language_code`)) + geom_bar(fill = "#ff9100", color = "black") +
  labs(title = "Frecuencia de Categorías de Libros", x = "Categoría", y = "Cantidad")

# Analisis de los datos
#Tendencias a lo Largo del Tiempo por calificacion
ggplot(sales_data, aes(x = `Publishing Year`, y = Book_average_rating)) +
  geom_line() +
  labs(title = "Evolución de la Calificación Promedio por Año", x = "Año de Publicación", y = "Calificación Promedio")
#Tendencias a lo Largo del Tiempo por numero de calificacion
ggplot(sales_data, aes(x = `Publishing Year`, y = Book_ratings_count)) +
  geom_line() +
  labs(title = "Evolución del Número de Calificaciones por Año", x = "Año de Publicación", y = "Número de Calificaciones")
#---
# Calcular C, la media de calificaciones promedio de todos los libros
C <- mean(sales_data$Book_average_rating, na.rm = TRUE)

# Establecer m, el mínimo de votos requeridos para ser considerado
m <- quantile(sales_data$Book_ratings_count, 0.75)  # Por ejemplo, el percentil 75

# Calcular la puntuación ponderada
sales_data <- sales_data %>%
  mutate(weighted_rating = (Book_ratings_count / (Book_ratings_count + m) * Book_average_rating) +
           (m / (Book_ratings_count + m) * C))
ggplot(sales_data, aes(x = `Publishing Year`, y = weighted_rating)) +
  geom_line() +
  labs(title = "Evolución de la Calificación Ponderada por Año", x = "Año de Publicación", y = "Calificación Ponderada")

#Comparación de Calificaciones por Autor
ggplot(sales_data, aes(x = Author_Rating, y = Book_average_rating, fill = Author_Rating)) +
  geom_boxplot() +
  labs(title = "Distribución de Calificaciones Promedio por Calificación del Autor", x = "Calificación del Autor", y = "Calificación Promedio del Libro")
#Correlaciones
cor.test(sales_data$Book_average_rating, sales_data$Book_ratings_count)
# Ajusta las columnas según las que tengas
matriz_correlacion <- cor(sales_data[, c("Publishing Year", "Book_average_rating", "Book_ratings_count",
                                         "gross sales","publisher revenue","sale price","sales rank",
                                         "units sold","weighted_rating")], use = "complete.obs")

# Visualizar la matriz de correlación
corrplot(matriz_correlacion, method = "circle")

#  2. Correlaciones Moderadas
#Calificación promedio y calificación ponderada: Existe una correlación positiva moderada entre la calificación promedio del libro y la calificación ponderada, lo que sugiere que la metodología de ponderación no altera drásticamente la percepción de la calidad del libro.
#Precio de venta y rango de ventas: La correlación negativa moderada entre el precio de venta y el rango de ventas sugiere que libros más caros tienden a tener un rango de venta peor (un número más alto en el rango indica una posición de venta inferior).
#3. Correlaciones Débiles o Inexistentes
#Año de publicación y otras variables: El año de publicación parece tener poco o ningún impacto directo en las otras variables como calificaciones, ventas y precio, lo que podría indicar que la popularidad o éxito de un libro no está fuertemente ligado a su año de lanzamiento en este dataset.

#análisis multivariable
modelo <- lm(Book_average_rating ~ `Publishing Year` + `sale price` + Book_ratings_count + weighted_rating, data = sales_data)
summary(modelo)




