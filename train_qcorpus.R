# cargamos las librerias necesarias
library(jsonlite)
library(quanteda)

# leemos el archivo json
data <- stream_in(file("../datos/spanish_train.jsonl"))

# creamos el corpus
corpus <- corpus(data$text)
# añadimos las diferentes docvars necesarias
docvars(corpus, "id") <- data$id
docvars(corpus, "url") <- data$url
docvars(corpus, "title") <- data$title
docvars(corpus, "summary") <- data$summary

# guardamos el corpus
saveRDS(corpus, "../datos/spanish_train.qcorpus.rds")
