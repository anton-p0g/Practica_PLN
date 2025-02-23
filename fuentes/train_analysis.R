library(quanteda)
library(spacyr)

corps = readRDS("datos/spanish_train.qcorpus.rds")
spacy_initialize("es_core_news_sm")


file_path <- "datos/words_title.rds"

if (file.exists(file_path)) {
  words_title <- readRDS(file_path)
  cat("File exists. Loaded words_title from", file_path, "  \n")
} else {
  words_title <- spacy_parse(corps$title)
  saveRDS(words_title, file_path)
  cat("File not found. Parsed words_title using spacy_parse and saved to", file_path, "  \n")
}
verbs_title <- words_title[words_title$pos == "VERB", c("doc_id", "lemma")]
head(verbs_title)


file_path <- "datos/words_summary.rds"
if (file.exists(file_path)) {
  words_summary <- readRDS(file_path)
  cat("File exists. Loaded words_summary from", file_path, "  \n")
} else {
  words_summary <- spacy_parse(corps$summary)
  saveRDS(words_summary, file_path)
  cat("File not found. Parsed words_summary using spacy_parse and saved to", file_path, "  \n")
}
verbs_summary <- words_summary[words_summary$pos == "VERB", c("doc_id", "lemma")]
head(verbs_summary)


file_path_verbs <- "datos/verbs_text.rds"
file_path_words <- "datos/words_text.rds"

if (file.exists(file_path_verbs)) {
  verbs_text <- readRDS(file_path_verbs)
  cat("File exists. Loaded verbs_text from", file_path_verbs, "  \n")
} else {
  # If verbs_text doesn't exist, check if words_text exists
  
  if (file.exists(file_path_words)) {
    words_text <- readRDS(file_path_words)
    verbs_text <- words_text[words_text$pos == "VERB", c("doc_id", "lemma")]
    saveRDS(verbs_text, file_path_verbs) # Save verbs_text for future use
    cat("File not found for verbs_text. Extracted verbs_text from words_text and saved to", file_path_verbs, "  \n")
    
  } else {
    words_text <- spacy_parse(corps)
    saveRDS(words_text, file_path_words) # Save words_text for future use
    verbs_text <- words_text[words_text$pos == "VERB", c("doc_id", "lemma")]
    saveRDS(verbs_text, file_path_verbs) # Save verbs_text for future use
    cat("File not found for words_text. Parsed words_text with spacy_parse, extracted verbs_text, and saved to", file_path_words, "and", file_path_verbs, "  \n")
  }
}
head(verbs_text)


corpus_ids <- names(corps)

verbs_create_list <- function(df, corpus_ids){
  grouped_verbs <- split(df$lemma, df$doc_id) # Group verbs by doc_id
  
  list_verbs <- vector("list", length(corpus_ids)) # Initializes an empty list with the same size as the corpus
  names(list_verbs) <- corpus_ids
  
  # Populate the list with verbs for each doc_id
  for (doc_id in corpus_ids) {
    if (doc_id %in% names(grouped_verbs)) {
      list_verbs[[doc_id]] <- grouped_verbs[[doc_id]]
    } else {
      list_verbs[[doc_id]] <- character(0) # Empty vector if no verbs
    }
  }
  return(list_verbs)
}


file_path_list_verbs_titles <- "datos/list_verbs_titles.rds"
file_path_list_verbs_summary <- "datos/list_verbs_summary.rds"

if (file.exists(file_path_list_verbs_titles)) {
  list_verbs_titles <- readRDS(file_path_list_verbs_titles)
  cat("File exists. Loaded list_verbs_titles from", file_path_list_verbs_titles, "  \n")
  
} else {
  list_verbs_titles <- verbs_create_list(verbs_title, corpus_ids)
  saveRDS(list_verbs_titles, file_path_list_verbs_titles)
  cat("File not found. Created list_verbs_titles and saved to", file_path_list_verbs_titles, "  \n")
}

if (file.exists(file_path_list_verbs_summary)) {
  list_verbs_summary <- readRDS(file_path_list_verbs_summary)
  cat("File exists. Loaded list_verbs_summary from", file_path_list_verbs_summary, "  \n")
  
} else {
  list_verbs_summary <- verbs_create_list(verbs_summary, corpus_ids)
  saveRDS(list_verbs_summary, file_path_list_verbs_summary)
  cat("File not found. Created list_verbs_summary and saved to", file_path_list_verbs_summary, "  \n")
}
head(list_verbs_titles)
head(list_verbs_summary)


file_path_list_verbs_text <- "datos/list_verbs_text.rds"
if (file.exists(file_path_list_verbs_text)) {
  list_verbs_text <- readRDS(file_path_list_verbs_text)
  cat("File exists. Loaded list_verbs_text from", file_path_list_verbs_text, "  \n")
  
} else {
  list_verbs_text <- verbs_create_list(verbs_text, corpus_ids)
  saveRDS(list_verbs_text, file_path_list_verbs_text)
  cat("File not found. Created list_verbs_text and saved to", file_path_list_verbs_text, "  \n")
}
head(list_verbs_text)


make_unique <- function(list) {
  for (i in seq_along(list)) {
    if (!is.null(list[[i]]) && length(list[[i]]) > 0) {
      list[[i]] <- unique(list[[i]]) 
    }
  }
  return(list)
}

list_verbs_title_unique <- make_unique(list_verbs_titles)
list_verbs_summary_unique <- make_unique(list_verbs_summary)


freq_verbs <- function(text_list, list_comp, corpus_ids) {
  freq <- numeric(length(list_comp)) # Creates an empty vector with a specific length
  names(freq) <- corpus_ids
  
  for (i in seq_along(list_comp)) {
    text_to_compare <- text_list[[i]] # Verbs from the text
    verbs <- list_comp[[i]]  # Verbs from the comparison list
    
    if (length(verbs) > 0) {
      freq[i] <- sum(sapply(verbs, function(verb) {
        sum(text_to_compare == verb) # Count occurrences of the verb
      }))
    }
  }
  return(freq)
}

freq_verbs_titles <- freq_verbs(list_verbs_text, list_verbs_title_unique, corpus_ids)
freq_verbs_summary <- freq_verbs(list_verbs_text, list_verbs_summary_unique, corpus_ids)
freq_verbs_titles[50:100]
freq_verbs_summary[50:100]


freq_table1 <- table(freq_verbs_summary)

bar_midpoints <- barplot(freq_table1, 
                         main = "Frequency of Matched Verbs Between Texts and Summaries", 
                         xlab = "Frequency", 
                         ylab = "Number of Documents", 
                         col = "lightblue", 
                         border = "black",
                         ylim = c(0, max(freq_table1) + 1500), 
                         yaxt = "n", 
                         xaxt = "n") # Suppress both axes

y_max <- max(freq_table1) + 2000
y_ticks <- seq(0, y_max, by = 2000) # y-axis subdivisions
axis(2, at = y_ticks, labels = y_ticks, las = 1, cex.axis = 0.8)

x_labels <- as.numeric(names(freq_table1))
axis(1, at = bar_midpoints, labels = x_labels, las = 2, cex.axis = 0.5) # Rotate labels vertically

if (length(bar_midpoints) == length(x_labels)) {
  axis(1, at = bar_midpoints[length(bar_midpoints)], 
       labels = x_labels[length(x_labels)], las = 2, cex.axis = 0.5)
}

# Add labels above all bars
text(bar_midpoints, freq_table1, 
     labels = freq_table1, 
     pos = 3, srt = 45, cex = 0.4, col = "black")


freq_table1

freq_table2 <- table(freq_verbs_titles)

bar_midpoints <- barplot(freq_table2, 
                         main = "Frequency of Matched Verbs Between Texts and Titles", 
                         xlab = "Frequency", 
                         ylab = "Number of Documents", 
                         col = "lightblue", 
                         border = "black", 
                         ylim = c(0, max(freq_table2) + 1500),
                         yaxt = "n",
                         xaxt = "n")

y_max <- max(freq_table2) + 2000 # Defines the maximum y-axis value
y_ticks <- seq(0, y_max, by = 1500) # subdivisions
axis(2, at = y_ticks, labels = y_ticks, las = 1, cex.axis = 0.8)

x_labels <- as.numeric(names(freq_table2))
axis(1, at = bar_midpoints, labels = x_labels, las = 2, cex.axis = 0.5) # Rotate labels vertically

if (length(bar_midpoints) == length(x_labels)) {
  axis(1, at = bar_midpoints[length(bar_midpoints)], 
       labels = x_labels[length(x_labels)], las = 2, cex.axis = 0.5)
}

# Add labels above all bars
text(bar_midpoints, freq_table2, 
     labels = freq_table2, 
     pos = 3, srt = 45, cex = 0.4, col = "black")

freq_table2

