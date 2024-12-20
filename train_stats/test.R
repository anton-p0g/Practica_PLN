# Load testthat
library(testthat)

# Test 1: Libraries load successfully
test_that("Required libraries load", {
    expect_error(library(spacyr), NA)
    expect_error(library(quanteda), NA)
    expect_error(library(udpipe), NA)
})

# Test 2: Corpus and model load correctly
test_that("Corpus and model load without errors", {
    expect_error(corps <- readRDS("spanish_train.qcorpus.rds"), NA)
    expect_error(udmodel_es <- udpipe_load_model(file = "spanish-ancora-ud-2.5-191206.udpipe"), NA)
})

# Test 3: Spacy initialization and parsing
test_that("Spacy initializes and parses data", {
    expect_error(spacy_initialize(model = "es_core_news_sm"), NA)
    words_title <- spacy_parse("Este es un título de prueba.")
    words_summary <- spacy_parse("Este es un resumen de prueba.")
    expect_true(all(c("doc_id", "token", "lemma", "pos") %in% colnames(words_title)))
    spacy_finalize()
})

# Test 4: Custom function - lista_agrupada
test_that("lista_agrupada function works correctly", {
    sample_data <- data.frame(
        doc_id = c("doc1", "doc1", "doc2", "doc2", "doc2"),
        lemma = c("palabra1", "palabra2", "palabra1", "palabra2", "palabra3")
    )
    result <- lista_agrupada(sample_data)
    expect_equal(length(result), 2)
    expect_equal(result[["doc1"]], 2)
    expect_equal(result[["doc2"]], 3)
})

# Test 5: File handling (RDS saving and loading)
test_that("RDS files save and load correctly", {
    test_data <- data.frame(a = 1:5, b = letters[1:5])
    saveRDS(test_data, "test_data.rds")
    loaded_data <- readRDS("test_data.rds")
    expect_equal(test_data, loaded_data)
    file.remove("test_data.rds")
})

# Test 6: Data cleaning removes punctuation
test_that("Data cleaning removes punctuation", {
    sample_data <- data.frame(
        doc_id = c("doc1", "doc1", "doc2"),
        lemma = c("palabra1", "palabra2", NA),
        pos = c("NOUN", "PUNCT", "PUNCT")
    )
    cleaned_data <- sample_data[sample_data$pos != "PUNCT" & !is.na(sample_data$lemma), ]
    expect_equal(nrow(cleaned_data), 1)
    expect_equal(cleaned_data$lemma, "palabra1")
})

# Test 7: Histogram output
test_that("Histogram bins and counts are correct", {
    sample_data <- c(1, 2, 2, 3, 3, 3)
    h <- hist(sample_data, breaks = seq(0, 5, 1), plot = FALSE)
    expect_equal(h$breaks, seq(0, 5, 1))
    expect_equal(h$counts, c(0, 1, 2, 3, 0))
})
