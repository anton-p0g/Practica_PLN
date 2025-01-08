# Load testthat
library(testthat)


# Colocamos la función que testearemos
lista_agrupada <- function(dataframe_text_lemma) {
  # Verificar si el dataframe está vacío
  if (nrow(dataframe_text_lemma) == 0) {
    return(list())  # Devolver una lista vacía
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

library(spacyr)
library(quanteda)
library(udpipe)

# Test 1: Importación del corpus
test_that("Corpus and model load without errors", {
    expect_error(corps <- readRDS("datos/spanish_train.qcorpus.rds"), NA)
    expect_error(udmodel_es <- udpipe_load_model(file = "datos/spanish-ancora-ud-2.5-191206.udpipe"), NA)
})


# Test 2: Prueba de nuestra función - lista_agrupada
test_that("lista_agrupada function works correctly", {
    sample_data <- data.frame(
        doc_id = c("doc1", "doc1", "doc2", "doc2", "doc2"),
        lemma = c("palabra1", "palabra2", "palabra1", "palabra2", "palabra3")
    )
    result <- lista_agrupada(sample_data)
    print(result)
    expect_equal(length(result), 2)
    expect_equal(result[["doc1"]], 2)
    expect_equal(result[["doc2"]], 3)
    print(result[["doc1"]])
    print(result[["doc2"]])
})


# Test 3: Prueba de nuestra función - lista_agrupada 2
test_that("lista_agrupada maneja un único documento", {
  sample_data <- data.frame(
    doc_id = c("doc1", "doc1", "doc1"),
    lemma = c("palabra1", "palabra2", "palabra3")
  )
  resultado <- lista_agrupada(sample_data)
  print(resultado)
  expect_equal(resultado$doc1, 3)
  expect_null(resultado$doc2)
  print(resultado$doc1)
  print(resultado$doc2)
})

# Test 4: Prueba de nuestra función - lista_agrupada 3
test_that("lista_agrupada maneja un dataframe vacío", {
  sample_data <- data.frame(
    doc_id = character(0),
    lemma = character(0)
  )
  resultado <- lista_agrupada(sample_data)
  expect_length(resultado, 0)
  print(resultado)
})


# Test 5: Probamos si se guarda correctamente
test_that("RDS files save and load correctly", {
    test_data <- data.frame(a = 1:5, b = letters[1:5])
    saveRDS(test_data, "test_data.rds")
    loaded_data <- readRDS("test_data.rds")
    expect_equal(test_data, loaded_data)
    file.remove("test_data.rds")
})

# Test 6: Limpieza de los signos de puntuación
test_that("Data cleaning removes punctuation", {
    sample_data <- data.frame(
        doc_id = c("doc1", "doc1", "doc2"),
        lemma = c("palabra1", "palabra2", NA),
        pos = c("NOUN", "PUNCT", "PUNCT")
    )
    cleaned_data <- sample_data[sample_data$pos != "PUNCT" & !is.na(sample_data$lemma), ]
    expect_equal(nrow(cleaned_data), 1)
    expect_equal(cleaned_data$lemma, "palabra1")
    print(nrow(cleaned_data))
    print(cleaned_data$lemma)
})

# Test 7: Creación del histograma
test_that("Histogram bins and counts are correct", {
    sample_data <- c(1, 2, 2, 3, 3, 3)
    h <- hist(sample_data, breaks = seq(0, 5, 1), plot = FALSE)
    expect_equal(h$breaks, seq(0, 5, 1))
    expect_equal(h$counts, c(1, 2, 3, 0, 0))
    print(h$breaks)
    print(h$counts)
})

