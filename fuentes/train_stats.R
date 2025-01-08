library(spacyr)
library(quanteda)
library(udpipe)

corps <- readRDS("datos/spanish_train.qcorpus.rds")
destination_file <- "datos/spanish-ancora-ud-2.5-191206.udpipe"

# Descargar el modelo si no existe
if (!file.exists(destination_file)) {
  cat("Descargando el modelo UDPipe...\n")
  dir.create("datos", showWarnings = FALSE) # Crear carpeta si no existe
  udpipe_download_model(language = "spanish-ancora", model_dir = "datos")
  cat("Modelo descargado en:", destination_file, "\n")
} else {
  cat("El modelo ya existe en:", destination_file, "\n")
}

udmodel_es <- udpipe_load_model(file = destination_file)
cat("Modelo UDPipe cargado correctamente.\n")

spacy_initialize(model = "es_core_news_sm")

if (!file.exists("datos/words_title.rds")) {
  words_title <- spacy_parse(corps$title)
  saveRDS(words_title, "datos/words_title.rds")
} else {
  words_title <- readRDS("datos/words_title.rds")
}

if (!file.exists("datos/words_summary.rds")) {
  words_summary <- spacy_parse(corps$summary)
  saveRDS(words_summary, "datos/words_summary.rds")
} else {
  words_summary <- readRDS("datos/words_summary.rds")
}
spacy_finalize()

words_title_clear <- words_title[words_title$pos != "PUNCT" | -is.na(words_title$lemma), ]
words_summary_clear <- words_summary[words_summary$pos != "PUNCT" | -is.na(words_summary$lemma), ]
titulos <- words_title_clear[, c("doc_id", "lemma")]
summary <- words_summary_clear[, c("doc_id", "lemma")]

lista_agrupada <- function(dataframe_text_lemma) {
  # Verificar si el dataframe está vacío
  if (nrow(dataframe_text_lemma) == 0) {
    return(list()) # Devolver una lista vacía
  }
  titulos_unicos <- unique(dataframe_text_lemma$doc_id)
  lista_titulos <- list()
  pos_titulo_unico <- 1
  palabras <- 0
  for (i in 1:nrow(dataframe_text_lemma)) {
    if (dataframe_text_lemma[i, 1] == titulos_unicos[pos_titulo_unico]) {
      palabras <- palabras + 1
    } else if (dataframe_text_lemma[i, 1] != titulos_unicos[pos_titulo_unico]) {
      lista_titulos[titulos_unicos[pos_titulo_unico]] <- palabras
      pos_titulo_unico <- pos_titulo_unico + 1
      palabras <- 1
    }
  }
  # Agregar el último grupo
  lista_titulos[[titulos_unicos[pos_titulo_unico]]] <- palabras
  return(lista_titulos)
}

lista_titulos <- lista_agrupada(titulos)
lista_summary <- lista_agrupada(summary)
n_tokens_titulos <- sapply(lista_titulos, function(x) {
  x
})
n_tokens_summary <- sapply(lista_summary, function(x) {
  x
})

par(oma = c(0, 0, 1, 0)) # Increase the top outer margin (third value)
layout(matrix(1:2, nrow = 1, ncol = 2))
hist(n_tokens_titulos,
  breaks = seq(0, 50, 1), ylim = c(0, 3500), main = "Histograma titulos", col = "blue",
  border = "white", xlab = "numero_tokens", ylab = "Frecuencia"
)
hist(n_tokens_summary,
  breaks = seq(0, 180, 2), ylim = c(0, 3500), main = "Histograma summary", col = "red",
  border = "white", xlab = "numero_tokens", ylab = "Frecuencia"
)
mtext("Resultados con SpacyR", side = 3, outer = TRUE, line = -1, cex = 1.5)

if (file.exists("datos/data_frame_titulos_udpipe.rds")) {
  df_titulos <- readRDS("datos/data_frame_titulos_udpipe.rds")
} else {
  titulos_con_udpipe <- udpipe_annotate(udmodel_es, corps$title)
  df_titulos <- as.data.frame(titulos_con_udpipe)
  saveRDS(df_titulos, "datos/data_frame_titulos_udpipe.rds")
}

if (file.exists("datos/data_frame_summary_udpipe.rds")) {
  df_summary <- readRDS("datos/data_frame_summary_udpipe.rds")
} else {
  summary_con_udpipe <- udpipe_annotate(udmodel_es, corps$summary)
  df_summary <- as.data.frame(summary_con_udpipe)
  saveRDS(df_summary, "datos/data_frame_summary_udpipe.rds")
}

df_titulos_clear <- df_titulos[df_titulos$upos != "PUNCT" | -is.na(df_titulos$lemma), ]
df_summary_clear <- df_summary[df_summary$upos != "PUNCT" | -is.na(df_summary$lemma), ]
titulos_udpipe <- df_titulos_clear[, c("doc_id", "lemma")]
summary_udpipe <- df_summary_clear[, c("doc_id", "lemma")]
#
lista_titulos_udpipe <- lista_agrupada(titulos_udpipe)
lista_summary_udpipe <- lista_agrupada(summary_udpipe)

n_tokens_titulos_udpipe <- sapply(lista_titulos_udpipe, function(x) {
  x
})
n_tokens_summary_udpipe <- sapply(lista_summary_udpipe, function(x) {
  x
})

par(oma = c(0, 0, 1, 0)) # Increase the top outer margin (third value)
layout(matrix(1:2, nrow = 1, ncol = 2))
hist(n_tokens_titulos_udpipe,
  breaks = seq(0, 50, 1), ylim = c(0, 3500), main = "Histograma titulo", col = "blue",
  border = "white", xlab = "numero_tokens", ylab = "Frecuencia"
)
hist(n_tokens_summary_udpipe,
  breaks = seq(0, 180, 2), ylim = c(0, 3500), main = "Histograma summary", col = "red",
  border = "white", xlab = "numero_tokens", ylab = "Frecuencia"
)
mtext("Resultados con udpipe", side = 3, outer = TRUE, line = -1, cex = 1.5)

max_summary <- max(n_tokens_summary)
max_summary_udpipe <- max(n_tokens_summary_udpipe)

max_titulos <- max(n_tokens_titulos)
max_titulos_udpipe <- max(n_tokens_titulos_udpipe)
