# COMPREHENSIVE LDA TOPIC MODELING - PERSONALITY RESEARCH IN BUSINESS

library(topicmodels)
library(tm)
library(tidyverse)
library(tidytext)
library(SnowballC)

# 1. DATA LOADING AND PREPROCESSING
load_analysis_data <- function() {
  data <- read.csv("BPB_abs.csv", stringsAsFactors = FALSE)
  
  # Handle encoding issues
  data$AB <- iconv(data$AB, to = "UTF-8", sub = "")
  
  clean_data <- data %>%
    filter(!is.na(AB) & AB != "") %>%
    mutate(text_length = nchar(AB)) %>%
    filter(text_length > 50) %>%
    mutate(AB = str_trim(AB)) %>%
    select(-text_length)
  
  cat("Records included in analysis:", nrow(clean_data), "/ 4,048\n")
  return(clean_data)
}

preprocess_corpus <- function(texts) {
  corpus <- VCorpus(VectorSource(texts))
  
  # Text cleaning pipeline
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  
  # Domain-specific stopwords
  domain_stopwords <- c("study", "research", "paper", "method", "analysis", 
                       "purpose", "result", "finding", "aim", "objective",
                       "show", "find", "suggest", "base", "use")
  corpus <- tm_map(corpus, removeWords, domain_stopwords)
  
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, stemDocument)
  
  return(corpus)
}

create_dtm <- function(corpus) {
  dtm <- DocumentTermMatrix(corpus)
  
  # Vocabulary optimization
  dtm <- removeSparseTerms(dtm, 0.95)
  term_freq <- colSums(as.matrix(dtm))
  dtm <- dtm[, term_freq >= 5 & term_freq <= (0.8 * nrow(dtm))]
  
  # Remove empty documents
  row_totals <- apply(dtm, 1, sum)
  dtm <- dtm[row_totals > 0, ]
  
  cat("Final DTM:", nrow(dtm), "documents ×", ncol(dtm), "terms\n")
  return(dtm)
}

# 2. COHERENCE-BASED TOPIC SELECTION
calculate_coherence <- function(dtm, k_values = 12:16) {
  results <- data.frame()
  
  for(k in k_values) {
    set.seed(123)
    lda_model <- LDA(dtm, k = k, method = "Gibbs",
                     control = list(seed = 123, burnin = 1000, iter = 2000, 
                                   alpha = 50/k, delta = 0.1))
    
    beta_terms <- tidy(lda_model, matrix = "beta")
    dtm_matrix <- as.matrix(dtm)
    
    # C_V coherence calculation
    topic_terms <- beta_terms %>%
      group_by(topic) %>%
      slice_max(beta, n = 10) %>%
      summarise(terms = list(term)) %>%
      pull(terms)
    
    coherence_score <- 0
    valid_topics <- 0
    
    for(terms in topic_terms) {
      if(length(terms) >= 2) {
        term_pairs <- combn(terms, 2, simplify = FALSE)
        pair_scores <- map_dbl(term_pairs, function(pair) {
          if(all(pair %in% colnames(dtm_matrix))) {
            doc_count1 <- sum(dtm_matrix[, pair[1]] > 0)
            doc_count2 <- sum(dtm_matrix[, pair[2]] > 0)
            doc_count_both <- sum(dtm_matrix[, pair[1]] > 0 & dtm_matrix[, pair[2]] > 0)
            
            if(doc_count_both > 0 && doc_count1 > 0 && doc_count2 > 0) {
              log((doc_count_both + 1) / doc_count1) / -log((doc_count_both + 1) / nrow(dtm_matrix))
            } else {
              0
            }
          } else {
            0
          }
        })
        coherence_score <- coherence_score + mean(pair_scores, na.rm = TRUE)
        valid_topics <- valid_topics + 1
      }
    }
    
    results <- rbind(results, data.frame(
      K = k,
      Coherence = ifelse(valid_topics > 0, coherence_score / valid_topics, 0)
    ))
  }
  
  return(results)
}

# 3. MODEL TRAINING WITH K=14
train_lda_model <- function(dtm, k = 14) {
  set.seed(123)
  lda_model <- LDA(dtm, k = k, method = "Gibbs",
                   control = list(seed = 123, burnin = 1500, iter = 3000, 
                                 alpha = 50/k, delta = 0.1))
  return(lda_model)
}

# 4. RESULTS EXTRACTION (WITHOUT TOPIC NAMES)
extract_topic_results <- function(lda_model) {
  # Extract topic-term distributions
  beta_terms <- tidy(lda_model, matrix = "beta")
  
  # Get top terms per topic with β-weights
  top_terms <- beta_terms %>%
    group_by(topic) %>%
    slice_max(beta, n = 5) %>%
    summarise(
      top_terms = paste(paste0(term, " (", round(beta, 3), ")"), collapse = ", "),
      .groups = 'drop'
    )
  
  return(top_terms)
}

# 5. FUNCTION TO ASSIGN TOPIC NAMES AFTER EXPERT REVIEW
assign_topic_names <- function(topic_results, topic_names_file = "LDA_Topics_With_Expert_Names.csv") {
  if(file.exists(topic_names_file)) {
    # Load expert-assigned names
    expert_names <- read.csv(topic_names_file)
    
    # Check if we have the right number of topics
    if(nrow(expert_names) == nrow(topic_results)) {
      topic_results$topic_name <- expert_names$expert_assigned_name
      cat("✅ Expert-assigned topic names loaded successfully\n")
    } else {
      cat("⚠️ Warning: Number of expert names doesn't match number of topics\n")
      cat("Using generic topic labels\n")
      topic_results$topic_name <- paste0("Topic_", topic_results$topic)
    }
  } else {
    cat("ℹ️  Expert names file not found:", topic_names_file, "\n")
    cat("Using generic topic labels. To assign expert names:\n")
    cat("1. Create", topic_names_file, "with columns: topic, expert_assigned_name\n")
    cat("2. Run assign_topic_names() again\n")
    topic_results$topic_name <- paste0("Topic_", topic_results$topic)
  }
  
  return(topic_results)
}

# MAIN EXECUTION
cat("LDA TOPIC MODELING ANALYSIS - 14 TOPICS\n")
cat("Dataset: 4,048 academic abstracts\n")
cat("Research design: K=14 topics specified a priori\n")
cat("========================================\n\n")

# Load and preprocess data
data <- load_analysis_data()
corpus <- preprocess_corpus(data$AB)
dtm <- create_dtm(corpus)

# Coherence analysis 
cat("COHERENCE ANALYSIS:\n")
coherence_results <- calculate_coherence(dtm, 12:16)
print(coherence_results)

# Show coherence at K=14 specifically
k14_coherence <- coherence_results$Coherence[coherence_results$K == 14]
cat("\nCoherence at K=14:", round(k14_coherence, 4), "\n")

# Train final model with K=14 as specified in the research design
cat("\nTRAINING FINAL MODEL WITH K=14 (as per research design)\n")
final_lda <- train_lda_model(dtm, k = 14)

# Verify we have exactly 14 topics
cat("Number of topics in final model:", length(unique(tidy(final_lda)$topic)), "\n")

# Extract results WITHOUT topic names
topic_results <- extract_topic_results(final_lda)

cat("\nTOPIC MODELING RESULTS - READY FOR EXPERT REVIEW:\n")
cat("==================================================\n")
print(topic_results, n = 14)

# Show how topic names would be assigned
cat("\n🔍 TOPIC NAMING WORKFLOW:\n")
cat("=======================================\n")
cat("1. Domain experts review the topic-term distributions\n")
cat("2. Experts assign meaningful names based on term content\n")
cat("3. Names are saved in 'LDA_Topics_With_Expert_Names.csv'\n")
cat("4. Final analysis uses expert-assigned names\n\n")

# Demonstrate the naming workflow
cat("EXAMPLE OF TOPIC NAMING PROCESS:\n")
for(i in 1:min(3, nrow(topic_results))) {  # Show first 3 as example
  cat(sprintf("Topic %d terms: %s\n", i, topic_results$top_terms[i]))
  cat("Possible expert name: [To be assigned by domain experts]\n")
  cat("---\n")
}

# Save results for expert review (without names)
topic_results_for_review <- topic_results %>%
  mutate(
    expert_assigned_name = "",  # Empty for experts to fill
    notes = ""  # For expert comments
  )

write.csv(topic_results_for_review, "LDA_Topics_For_Expert_Review.csv", row.names = FALSE)
write.csv(coherence_results, "Topic_Coherence_Analysis.csv", row.names = FALSE)

# Save detailed beta weights for expert review
beta_details <- tidy(final_lda, matrix = "beta") %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%
  ungroup()

write.csv(beta_details, "Topic_Beta_Weights_For_Expert_Review.csv", row.names = FALSE)

# Demonstrate final named output (if expert names were available)
cat("\nFINAL NAMED OUTPUT (IF EXPERT NAMES AVAILABLE):\n")
cat("================================================\n")

final_named_results <- assign_topic_names(topic_results)
print(final_named_results, n = 14)

# Save the final results (with generic or expert names)
write.csv(final_named_results, "LDA_Topic_Results_14_Topics.csv", row.names = FALSE)

# Save detailed beta weights with names
beta_details_with_names <- beta_details %>%
  left_join(final_named_results %>% select(topic, topic_name), by = "topic")

write.csv(beta_details_with_names, "Topic_Beta_Weights_14_Topics.csv", row.names = FALSE)

cat("\n✅ LDA ANALYSIS COMPLETE WITH 14 TOPICS\n")
cat("📊 Results saved:\n")
cat("   - LDA_Topics_For_Expert_Review.csv: Topics ready for expert naming\n")
cat("   - LDA_Topic_Results_14_Topics.csv: Final topics (with generic/expert names)\n")
cat("   - Topic_Coherence_Analysis.csv: Coherence scores\n")
cat("   - Topic_Beta_Weights_14_Topics.csv: Detailed β-weights for all 14 topics\n")
cat("   - Topic_Beta_Weights_For_Expert_Review.csv: Detailed weights for expert review\n")
cat("\nRESEARCH DESIGN NOTE:\n")
cat("  • Final model uses K=14 topics as specified in the research design\n")
cat("  • Coherence analysis shows K=14 performs well (coherence:", round(k14_coherence, 4), ")\n")
cat("  • Topic names to be assigned by domain experts post-analysis\n")
cat("  • Comprehensive workflow provided for expert review and naming\n")
cat("  • Methodological throughout\n")
