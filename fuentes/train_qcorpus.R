library(jsonlite)
library(quanteda)
library(R.utils)

url <- "https://huggingface.co/datasets/csebuetnlp/xlsum/resolve/main/data/spanish_XLSum_v2.0.tar.bz2"
destination_file <- "datos/spanish_XLSum_v2.0.tar.bz2"
decompressed_file <- "datos/spanish_XLSum_v2.0.tar"
extracted_file_path <- "datos/spanish_train.jsonl"


# Download the file if it doesn't exist
if (!file.exists(destination_file)) {
  cat("Downloading the file...\n")
  download.file(url, destfile = destination_file, mode = "wb")
  cat("File downloaded to:", destination_file, "\n")
} else {
  cat("The compressed file already exists. Skipping download.\n")
}

# Decompress the .bz2 file using R.utils
if (!file.exists(decompressed_file)) {
  cat("Decompressing the .bz2 file using R.utils...  \n")
  bunzip2(destination_file, destname = decompressed_file, overwrite = TRUE, remove = FALSE)
  if (file.exists(decompressed_file)) {
    cat("Decompressed file saved to:", decompressed_file, "  \n")
  } else {
    stop("Decompression failed. The .tar file was not created.")
  }
}

# Extract the .tar file using R's untar
if (!file.exists(decompressed_file)) {
  stop("Decompressed file not found. Extraction failed.")
}

cat("Extracting the .tar file using R's untar function...  \n")
tryCatch(
  {
    untar(decompressed_file, exdir = "./datos", verbose = TRUE)
    cat("Extraction completed successfully.\n")
  },
  error = function(e) {
    stop("Extraction failed. Error:", conditionMessage(e))
  }
)

files_to_delete <- c("datos/spanish_XLSum_v2.0.tar.bz2", "datos/spanish_XLSum_v2.0.tar", "datos/spanish_val.jsonl", "datos/spanish_test.jsonl")

# Loop through and delete files if they exist
for (file in files_to_delete) {
  if (file.exists(file)) {
    cat("Deleting file:", file, "\n")
    unlink(file)
  } else {
    cat("File not found:", file, "\n")
  }
}

# Create the corpus
if (!file.exists(extracted_file_path)) {
  stop("Error: The extracted file was not found.  \n")
}

data <- stream_in(file(extracted_file_path)) # Load the JSONL data

corpus <- corpus(data$text)

# Add docvars
docvars(corpus, "id") <- data$id
docvars(corpus, "url") <- data$url
docvars(corpus, "title") <- data$title
docvars(corpus, "summary") <- data$summary

saveRDS(corpus, file = "datos/spanish_train.qcorpus.rds")
cat("Corpus created and saved to datos/spanish_train.qcorpus.rds  \n")
