source("../fuentes/train_analysis.R")

test_freq_verbs <- function(list_verbs_text, list_verbs_title_unique, list_verbs_summary_unique, corpus_ids) {
  
  test_corpus_ids <- corpus_ids[56:60] # Get a range of documents for testing
  
  # Subset the data given the range
  test_text_list <- list_verbs_text[test_corpus_ids]
  test_title_list <- list_verbs_title_unique[test_corpus_ids]
  test_summary_list <- list_verbs_summary_unique[test_corpus_ids]
  
  # Call the freq_verbs function on the new lists
  test_freq_titles <- freq_verbs(test_text_list, test_title_list, test_corpus_ids)
  test_freq_summary <- freq_verbs(test_text_list, test_summary_list, test_corpus_ids)
  
  cat("\n------ Testing freq_verbs Function ------\n\n")
  
  for (i in seq_along(test_corpus_ids)) {
    
    cat("Document ID:", test_corpus_ids[i], "\n")
    cat("Text Verbs:", test_text_list[[i]], "\n\n")
    
    matched_title_verbs <- intersect(test_text_list[[i]], test_title_list[[i]])
    
    cat("Title Verbs:", test_title_list[[i]], "\n")
    cat("Matched Title Verbs:\n", paste(matched_title_verbs, collapse = ", "), "\n")
    
    matched_summary_verbs <- intersect(test_text_list[[i]], test_summary_list[[i]])
    
    cat("Matched Count (Titles):", test_freq_titles[i], "\n\n")
    cat("Matched Verbs:\n", paste(matched_summary_verbs, collapse = ", "), "\n")
    cat("Matched Count (Summaries):", test_freq_summary[i], "\n\n")
  }
}

test_freq_verbs(list_verbs_text, list_verbs_title_unique, list_verbs_summary_unique, corpus_ids)

# Testing function for a specific frequency of summary
test_specific_frequency_summary <- function(freq, freq_verbs_summary, list_verbs_text, list_verbs_summary_unique, corpus_ids) {
  # Find the documents with the specified frequency
  matching_docs <- names(freq_verbs_summary[freq_verbs_summary == freq])
  
  # In case no documents match
  if (length(matching_docs) == 0) {
    cat("No documents found with frequency:", freq, "\n")
    return(NULL)
  }
  
  # Iterate through matching documents and print details
  cat("\n=== Testing for Frequency:", freq, "===\n")
  for (doc_id in matching_docs) {
    text_verbs <- list_verbs_text[[doc_id]]
    summary_verbs <- list_verbs_summary_unique[[doc_id]]
    
    # Calculate matched verbs
    matched_summary_verbs <- intersect(text_verbs, summary_verbs)
    matched_counts <- sapply(matched_summary_verbs, function(verb) sum(text_verbs == verb))
    
    # Print details for the document
    cat("\nDocument ID:", doc_id, "\n")
    cat("Text Verbs:\n", paste(text_verbs, collapse = ", "), "\n\n")
    cat("Summary Verbs:\n", paste(summary_verbs, collapse = ", "), "\n")
    cat("Matched Verbs (Summaries) with Counts:\n")
    cat(paste("\t", matched_summary_verbs, "(", matched_counts, ")", collapse = ", "), "\n")
    cat("Matched Count (Summaries):", length(matched_summary_verbs), "\n")
  }
}
test_specific_frequency_summary(108, freq_verbs_summary, list_verbs_text, list_verbs_summary_unique, corpus_ids)

# Testing function for a specific frequency of titles
test_specific_frequency_titles <- function(freq, freq_verbs_titles, list_verbs_text, list_verbs_title_unique, corpus_ids) {
  # Find the documents with the specified frequency
  matching_docs <- names(freq_verbs_titles[freq_verbs_titles == freq])
  
  # In case no documents match
  if (length(matching_docs) == 0) {
    cat("No documents found with frequency:", freq, "\n")
    return(NULL)
  }
  
  # Iterate through matching documents and print details
  cat("\n=== Testing for Frequency:", freq, "===\n")
  for (doc_id in matching_docs) {
    text_verbs <- list_verbs_text[[doc_id]]
    title_verbs <- list_verbs_title_unique[[doc_id]]
    
    # Calculate matched verbs
    matched_title_verbs <- intersect(text_verbs, title_verbs)
    matched_counts <- sapply(matched_title_verbs, function(verb) sum(text_verbs == verb))
    
    # Print details for the document
    cat("\nDocument ID:", doc_id, "\n")
    cat("Text Verbs:\n", paste(text_verbs, collapse = ", "), "\n\n")
    cat("Title Verbs:\n", paste(title_verbs, collapse = ", "), "\n")
    cat("Matched Verbs (Titles) with Counts:\n")
    cat(paste("\t", matched_title_verbs, "(", matched_counts, ")", collapse = ", "), "\n")
    cat("Matched Count (Titles):", length(matched_title_verbs), "\n")
  }
}
test_specific_frequency_titles(44, freq_verbs_titles, list_verbs_text, list_verbs_title_unique, corpus_ids)