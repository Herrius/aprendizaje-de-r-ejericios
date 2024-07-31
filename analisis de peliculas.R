#carga datos
library(readr)
movies <- read_csv(file.path(r"(C:\Users\enrique\Downloads\ml-25m\ml-25m\movies.csv)"))
ratings <- read_csv(file.path(r"(C:\Users\enrique\Downloads\ml-25m\ml-25m\ratings.csv)"))
tags <- read_csv(file.path(r"(C:\Users\enrique\Downloads\ml-25m\ml-25m\tags.csv)"))
genome_scores <- read_csv(file.path(r"(C:\Users\enrique\Downloads\ml-25m\ml-25m\genome-scores.csv)"))
genome_tags <- read_csv(file.path(r"(C:\Users\enrique\Downloads\ml-25m\ml-25m\genome-tags.csv)"))

#Inspeccion inicial
str(movies)
str(ratings)
str(tags)
#Sumario estadistico
summary(ratings)
summary(movies)
summary(tags)
#vista de datos
# Primeros datos de 'movies'
head(movies)

# Primeros datos de 'ratings'
head(ratings)

# Primeros datos de 'tags'
head(tags)

# Primeros datos de 'genome_scores'
head(genome_scores)

# Primeros datos de 'genome_tags'
head(genome_tags)
#Limpieza de datos
movies <- na.omit(movies)
ratings <- na.omit(ratings)
tags <- na.omit(tags)
#Transformacion de datos
movies$genres <- strsplit(as.character(movies$genres), "\\|")

ratings$timestamp <- as.POSIXct(ratings$timestamp, origin="1970-01-01")

tags$timestamp <- as.POSIXct(tags$timestamp, origin="1970-01-01")
#Integracion de Genome Scores
genome <- merge(genome_scores, genome_tags, by = "tagId")
#Creación de la Matriz Usuario-Película
ratings$userId <- as.numeric(factor(ratings$userId))
ratings$movieId <- as.numeric(factor(ratings$movieId))
sparse_matrix <- sparseMatrix(i = ratings$userId,
                              j = ratings$movieId,
                              x = ratings$rating,
                              dims = c(max(ratings$userId), max(ratings$movieId)))
summary(sparse_matrix)
# Entrenamiento del modelo
if (!require("recommenderlab")) install.packages("recommenderlab", dependencies = TRUE)
library(recommenderlab)
#Crear un objeto de recomendación
rating_matrix <- as(sparse_matrix, "realRatingMatrix")
#División de datos en conjunto de entrenamiento y prueba
set.seed(123)  # para reproducibilidad
evaluation_scheme <- evaluationScheme(rating_matrix, method = "split", train = 0.8, given = 10, goodRating = 4)
# Entrenar un modelo de recomendación
recommender_model <- Recommender(getData(evaluation_scheme, "train"), method = "UBCF", parameter = list(nn = 30))
# Evaluar el modelo
predicted_ratings <- predict(recommender_model, getData(evaluation_scheme, "known"), type = "ratings")
accuracy <- calcPredictionAccuracy(predicted_ratings, getData(evaluation_scheme, "unknown"))
# Recomendaciones
recommendations <- predict(recommender_model, getData(evaluation_scheme, "known"), type = "topNList", n = 5)
