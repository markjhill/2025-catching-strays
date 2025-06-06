# Purpose: Analyze sentiment and linguistic feature correlation between posts made by the same user in a football club (FC) subreddit and a non-FC subreddit within a short time window.
# Key analyses: Kendall's Tau for sentiment, linguistic feature extraction and correlation, qualitative examples of toxic pairs, LaTeX table generation, DiD modeling, Keyness analysis.
# Not all of these were used in the paper

##############################################################################
# Setup - Libraries and Functions
##############################################################################
library(ggplot2)
library(dplyr)
library(lubridate)
library(correlation) 
library(quanteda)
library(quanteda.textstats)
library(quanteda.textplots)
library(stringr) 
library(pcaPP) 
library(effsize) 
library(knitr) 
library(data.table)

# Define custom 'not in' operator
'%!in%' <- function(x, y) !('%in%'(x, y))

# Set working directory - Users will need to change this.
setwd("C:/Users/markj/OneDrive - King's College London/coding/football_contagion/r/code_for_methods_paper")

##############################################################################
# Load and Prepare Paired Posts Data
##############################################################################

# Again, data is not included in the repository. Here is how the data for this step was created (along with a Python script which was used to detect toxicity using GPU):

# all_files <- list.files("../Datasets/combined_fc_and_non_fc_data/", full.names = TRUE)
# 
# for(i_file in 1:length(all_files)) {
#   cat("\n", i_file)
#   temp_df <- readRDS(all_files[i_file])
#   if(i_file == 1) {
#     combined_df <- temp_df
#   } else {
#     to_remove <- which(names(temp_df) %!in% names(combined_df))
#     if(length(to_remove) > 0 ) {
#       temp_df <- temp_df[,-c(to_remove)]
#     }
#     combined_df <- rbind(combined_df, temp_df)
#   }
#   rm(temp_df)
# }
# 
# combined_df$club_win_sent_score <- as.numeric(combined_df$club_win_sent_score)
# combined_df$non_club_win_sent_score <- as.numeric(combined_df$non_club_win_sent_score)
# 
# combined_df$club_clean_dates_utc <-  as.character(combined_df$club_clean_dates_utc)
# combined_df$non_club_clean_dates_utc <-  as.character(combined_df$non_club_clean_dates_utc)
# 
# write.csv(combined_df, file = "../Datasets/combined_fc_and_non_fc.csv", row.names = FALSE)

# combined_df_raw <- read.csv("../Datasets/combined_fc_and_non_fc.csv") # if loading the version before toxicity scores
combined_df_raw <- read.csv("../Datasets/combined_fc_and_non_fc_toxic.csv")
print(paste("Loaded combined_df with", nrow(combined_df_raw), "rows and", ncol(combined_df_raw), "columns."))

# Load main data (merged_df) to get 'during_match' and 'current_result' status
# This is the same merged_df as in data_overview.R (I'm bad a keeping variable names consistent across scripts - will correct at some point)
main_data <- readRDS(file = "C:/Users/markj/OneDrive - King's College London/coding/football_contagion/r/Datasets/all_sub_data/all_data_pruned_2024_09_05.RDS")
main_data <- as.data.frame(main_data) %>%
  filter(!is.na(llm_score)) %>%
  mutate(
    during_match = as.character(during_match),
    during_match = ifelse(is.na(during_match), FALSE, during_match),
    during_match = as.factor(during_match)
  )

# Match 'during_match' and 'current_result' from main_data to combined_df_raw
# Using the club_id from combined_df_raw to match 'id' in main_data
id_matches <- match(combined_df_raw$club_id, main_data$id)

# Handle cases where club_id might not have a match (though ideally all should match)
# If NAs occur, they will propagate. Consider how to handle this if it's an issue.
# For safety, assuming NAs in id_matches should lead to NAs in the new columns,
# which can then be filtered or handled appropriately.
combined_df <- combined_df_raw %>%
  mutate(
    during_match = main_data$during_match[id_matches],
    result = main_data$current_result[id_matches] # This will be NA if during_match is FALSE
  ) %>%
  # Ensure 'result' is NA if not during a match, and factor otherwise
  mutate(
    result = ifelse(during_match == TRUE, as.character(result), NA_character_),
    result = as.factor(result)
  )


# Optional: Filter out specific non-club subreddits like "soccer" or "FantasyPL"
# The paper (Limitations section) discusses the inclusion/exclusion of these.
# For replication of paper's main results, they are included.
# To exclude:
# combined_df_filtered <- combined_df %>%
#   filter(!non_club_subreddit %in% c("FantasyPL", "soccer"))
# Use 'combined_df_filtered' instead of 'combined_df' in subsequent analyses if desired.
# For now, using 'combined_df' to match original script's flow for main tables.


# Convert sentiment probability scores to numeric and prepare for sentiment index calculation
sentiment_cols_club <- c("club_negative", "club_neutral", "club_positive")
sentiment_cols_non_club <- c("non_club_negative", "non_club_neutral", "non_club_positive")

combined_df[sentiment_cols_club] <- lapply(combined_df[sentiment_cols_club], as.numeric)
combined_df[sentiment_cols_non_club] <- lapply(combined_df[sentiment_cols_non_club], as.numeric)

# Ensure overall sentiment categories are factors
combined_df$club_overall_sent <- as.factor(combined_df$club_overall_sent)
combined_df$non_club_overall_sent <- as.factor(combined_df$non_club_overall_sent)

# Calculate Sentiment Index: (P - N) / (P + N + Neu)
# This normalizes sentiment to a -1 to +1 scale, similar to llm_score in data_overview.R
# Note: llm_score is derived from TweetNLP for the main corpus (FC Corpus).
# If P+N+Neu is 0, this will result in NaN. Add a small epsilon or handle.

epsilon <- 1e-9 # To prevent division by zero if all probabilities are zero
combined_df <- combined_df %>%
  mutate(
    club_sentiment_index = (club_positive - club_negative) /
      (club_positive + club_neutral + club_negative + epsilon),
    non_club_sentiment_index = (non_club_positive - non_club_negative) /
      (non_club_positive + non_club_neutral + non_club_negative + epsilon)
  )

# Create categorical sentiment scores (1: negative, 2: neutral, 3: positive)
combined_df <- combined_df %>%
  mutate(
    club_sent_cat = case_when(
      club_overall_sent == "positive" ~ 3,
      club_overall_sent == "neutral" ~ 2,
      club_overall_sent == "negative" ~ 1,
      TRUE ~ NA_real_
    ),
    non_club_sent_cat = case_when(
      non_club_overall_sent == "positive" ~ 3,
      non_club_overall_sent == "neutral" ~ 2,
      non_club_overall_sent == "negative" ~ 1,
      TRUE ~ NA_real_
    )
  ) %>%
  filter(!is.na(club_sent_cat) & !is.na(non_club_sent_cat)) # Remove pairs with missing sentiment categories

print(paste("Rows in combined_df after processing and categorizing sentiment:", nrow(combined_df)))

##############################################################################
# Kendall's Tau Analysis for Sentiment Correlation
##############################################################################

# Function to calculate Kendall's Tau (using pcaPP for speed on large datasets)

is_kendall_monotonic_fast <- function(x, y) {
  
  # Ensure we only use complete pairs of data, which is critical for correlation functions.
  complete_obs <- complete.cases(x, y)
  x <- x[complete_obs]
  y <- y[complete_obs]

  n <- length(x)

  # Kendall's Tau is not meaningful for fewer than 3 pairs of data.
  if (n < 3) {
    return(list(kendall_tau = NA, p_value = NA, note = "Insufficient data"))
  }
  
  # If one variable is constant (e.g., all posts are "positive"), correlation is undefined.
  # This check prevents the NaN error.
  if (length(unique(x)) < 2 || length(unique(y)) < 2) {
    return(list(kendall_tau = NA, p_value = NA, note = "One variable is constant"))
  }

  kt <- NA
  p_val <- NA

  # For large datasets, use the faster `pcaPP::cor.fk` function.
  if (n > 10000 && requireNamespace("pcaPP", quietly = TRUE)) {
    kt <- pcaPP::cor.fk(x, y)
    # Approximate the p-value for the large-N z-statistic
    if (!is.na(kt)) {
      z_stat <- 3 * kt * sqrt(n * (n - 1)) / sqrt(2 * (2 * n + 5))
      p_val <- 2 * pnorm(-abs(z_stat))
    }
  # For smaller datasets, use the standard `stats::cor.test`.
  } else {
    # Use a tryCatch to gracefully handle any unexpected errors from cor.test.
    cor_test_result <- tryCatch(
      cor.test(x, y, method = "kendall"),
      error = function(e) NULL
    )
    if (!is.null(cor_test_result)) {
      kt <- cor_test_result$estimate
      p_val <- cor_test_result$p.value
    }
  }

  return(list(kendall_tau = kt, p_value = p_val, note = ""))
}

# Main analysis function to generate the Kendall's Tau results table.
analyze_sentiment_correlations <- function(df, club_sent_col, non_club_sent_col) {
  results_list <- list()

  subsets <- list(
    "All Pairs"     = df$during_match %in% c(TRUE, FALSE),
    "During Match"  = df$during_match == TRUE,
    "Outside Match" = df$during_match == FALSE
  )

  sentiment_filters <- list(
    "All Sentiments",
    "Neutral Club Posts Removed",
    "Only Positive Club Posts",
    "Only Negative Club Posts"
  )

  for (subset_name in names(subsets)) {
    subset_mask <- subsets[[subset_name]]
    current_df <- df[subset_mask, ]

    for (filter_name in sentiment_filters) {
      
      if (filter_name == "Neutral Club Posts Removed") {
          final_df <- current_df %>% filter(club_overall_sent != "neutral")
      } else if (filter_name == "Only Positive Club Posts") {
          final_df <- current_df %>% filter(club_overall_sent == "positive")
      } else if (filter_name == "Only Negative Club Posts") {
          final_df <- current_df %>% filter(club_overall_sent == "negative")
      } else { # "All Sentiments"
          final_df <- current_df
      }

      # Call the robust helper function
      analysis_result <- is_kendall_monotonic_fast(
          final_df[[club_sent_col]], 
          final_df[[non_club_sent_col]]
      )
      
      # Process results for the final table
      kt <- analysis_result$kendall_tau
      p_val <- analysis_result$p_value
      significant <- !is.na(p_val) && p_val < 0.05 && !is.na(kt) && abs(kt) > 0
      direction <- if (is.na(kt)) NA else if (kt > 0) "increasing" else if (kt < 0) "decreasing" else "none"

      # Store everything in a list
      results_list[[paste(subset_name, filter_name, sep = " | ")]] <- list(
          kendall_tau = kt,
          p_value = p_val,
          significant_monotonic = significant,
          direction = direction,
          sample_size = nrow(final_df),
          note = analysis_result$note
      )
    }
  }

  # Convert the list of results to a clean data frame
  result_df <- do.call(rbind, lapply(names(results_list), function(key) {
    parts <- strsplit(key, " | ", fixed = TRUE)[[1]]
    res <- results_list[[key]]
    data.frame(
      Subset = parts[1],
      Sentiment_Filter = parts[2],
      Kendalls_Tau = res$kendall_tau,
      P_Value = res$p_value,
      Significant = res$significant_monotonic,
      Direction = res$direction,
      Sample_Size = res$sample_size,
      Note = res$note,
      stringsAsFactors = FALSE
    )
  }))
  
  return(result_df)
}

# Run the analysis using categorical sentiment scores (1, 2, 3)
# This corresponds to Tables 6 & 7 in the paper (Kendall's T section)
kendall_results_categorical <- analyze_sentiment_correlations(combined_df, "club_sent_cat", "non_club_sent_cat")
print("--- Kendall's Tau Results (Categorical Sentiment) ---")
print(kable(kendall_results_categorical, digits = c(NA, NA, 3, 4, NA, NA, 0, NA), format = "pipe"))


# Pearson Chi-squared test for association between sentiment categories
# (Corresponds to Table 8 in the paper)
# Create a contingency table (club_sent_cat vs non_club_sent_cat)
contingency_table_sentiment <- table(
  Club_Sentiment = factor(combined_df$club_sent_cat, levels = 1:3, labels = c("Negative", "Neutral", "Positive")),
  Non_Club_Sentiment = factor(combined_df$non_club_sent_cat, levels = 1:3, labels = c("Negative", "Neutral", "Positive"))
)
print("--- Contingency Table (Club Sentiment vs Non-Club Sentiment) ---")
print(contingency_table_sentiment)

chisq_test_sentiment <- chisq.test(contingency_table_sentiment)
print("--- Chi-squared Test Results ---")
print(chisq_test_sentiment)
print("Standardized Residuals (Observed - Expected / sqrt(Expected)):")
print(round(chisq_test_sentiment$residuals, 2)) # These are Pearson residuals


##############################################################################
# Linguistic Feature Extraction and Analysis (using quanteda)
##############################################################################
# This section corresponds to Table 9 in the paper.

















# # Load lexicon for profanity (bad words)
# bad_words_lexicon <- read.csv("../Datasets/list_of_naughty-OaOBW_en.csv")

# # Define dictionaries for quanteda
# # Note: Slurs are more complex and context-dependent; the original script had a list but it's empty in the final dict.
# # For reproducibility, if a specific slur list was intended, it should be defined.
# dict_problematic <- dictionary(list(
#   profanity = bad_words_lexicon$X2g1c, # Assuming this column contains the profanity terms
#   violent = c("kill*", "die*", "murder*", "attack*", "destroy*", "hate*", "ruin*", "merc*") # Added merc* from original
#   # slurs = c() # Kept empty as in original final version
# ))

# dict_intensifiers <- dictionary(list(
#   intensifiers = c(
#     "very", "really", "so", "extremely", "absolutely", "totally",
#     "completely", "fucking", "bloody", "literally" # Note: "fucking" is also in profanity
#   )
# ))

# # Prepare corpus data: combining club and non-club posts for feature extraction
# # Need to ensure 'club_body' and 'non_club_body' are character vectors and not empty/NA
# corpus_texts_club <- combined_df$club_body
# corpus_texts_non_club <- combined_df$non_club_body

# # Handle potential NA or non-character entries robustly
# corpus_texts_club[is.na(corpus_texts_club) | !is.character(corpus_texts_club)] <- ""
# corpus_texts_non_club[is.na(corpus_texts_non_club) | !is.character(corpus_texts_non_club)] <- ""


# # Create corpora
# corpus_club <- corpus(corpus_texts_club)
# corpus_non_club <- corpus(corpus_texts_non_club)

# # Function to process corpus and get DFM counts for dictionaries
# process_corpus_for_features <- function(corp, dict_list) {
#   toks <- tokens(corp, remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>%
#     tokens_tolower()
#   dfm_features <- dfm(toks) %>%
#     dfm_lookup(dictionary = dict_list)
  
#   # FIX: Use the 'convert' function as 'as.data.frame.dfm' is deprecated.
#   return(convert(dfm_features, to = "data.frame"))
# }

# # Get feature counts
# features_club <- process_corpus_for_features(corpus_club, dict_problematic)
# features_non_club <- process_corpus_for_features(corpus_non_club, dict_problematic)
# intensifiers_club <- process_corpus_for_features(corpus_club, dict_intensifiers)
# intensifiers_non_club <- process_corpus_for_features(corpus_non_club, dict_intensifiers)

# # String-based feature counts (exclamations, all caps, questions)
# linguistic_features_df <- combined_df %>%
#   mutate(
#     club_exclamations = str_count(club_body, "!"),
#     non_club_exclamations = str_count(non_club_body, "!"),
#     club_caps_words = str_count(club_body, "\\b[A-Z]{2,}\\b"), # Words with 2+ uppercase letters
#     non_club_caps_words = str_count(non_club_body, "\\b[A-Z]{2,}\\b"),
#     club_questions = str_count(club_body, "\\?"), # Not used in Table 9
#     non_club_questions = str_count(non_club_body, "\\?") # Not used
#   ) %>%
#   # Add quanteda features
#   bind_cols(
#     features_club %>% select(profanity, violent) %>% rename_with(~paste0("club_", .)),
#     features_non_club %>% select(profanity, violent) %>% rename_with(~paste0("non_club_", .)),
#     intensifiers_club %>% select(intensifiers) %>% rename_with(~paste0("club_", .)),
#     intensifiers_non_club %>% select(intensifiers) %>% rename_with(~paste0("non_club_", .))
#   )


# # Kendall's Tau for linguistic features (Table 9)
# feature_pairs_for_corr <- list(
#   list(name = "Profanity", club_col = "club_profanity", non_club_col = "non_club_profanity"),
#   list(name = "Violent", club_col = "club_violent", non_club_col = "non_club_violent"),
#   list(name = "Intensifiers", club_col = "club_intensifiers", non_club_col = "non_club_intensifiers"),
#   list(name = "Exclamations", club_col = "club_exclamations", non_club_col = "non_club_exclamations"),
#   list(name = "All-caps", club_col = "club_caps_words", non_club_col = "non_club_caps_words")
# )

# linguistic_corr_results <- lapply(feature_pairs_for_corr, function(pair) {
#   cat("Analyzing linguistic feature:", pair$name, "\n")
  
#   # Correlations for posts "During Match"
#   during_match_df <- linguistic_features_df %>% filter(during_match == TRUE)
#   tau_during <- NA
#   p_during <- NA
#   if(nrow(during_match_df) > 10) { # Ensure enough data
#     res_during <- is_kendall_monotonic_fast(during_match_df[[pair$club_col]], during_match_df[[pair$non_club_col]])
#     tau_during <- res_during$kendall_tau
#     p_during <- res_during$p_value
#   }
  
#   # Correlations for posts "Outside Match"
#   outside_match_df <- linguistic_features_df %>% filter(during_match == FALSE)
#   tau_outside <- NA
#   p_outside <- NA
#   if(nrow(outside_match_df) > 10) {
#     res_outside <- is_kendall_monotonic_fast(outside_match_df[[pair$club_col]], outside_match_df[[pair$non_club_col]])
#     tau_outside <- res_outside$kendall_tau
#     p_outside <- res_outside$p_value
#   }
  
#   data.frame(
#     Feature = pair$name,
#     Outside_Match_Tau = tau_outside,
#     Outside_Match_Pval = p_outside,
#     During_Match_Tau = tau_during,
#     During_Match_Pval = p_during,
#     Delta_Tau = tau_during - tau_outside,
#     N_During = nrow(during_match_df),
#     N_Outside = nrow(outside_match_df)
#   )
# }) %>% bind_rows()

# print("--- Kendall's Tau for Linguistic Features (Club vs Non-Club Posts) ---")

# # This version uses dplyr::case_when which is "vectorized" and works correctly inside mutate().
# add_stars <- function(p_values) {
#   case_when(
#     is.na(p_values) ~ "",
#     p_values < 0.001 ~ "***",
#     p_values < 0.01  ~ "**",
#     p_values < 0.05  ~ "*",
#     TRUE ~ "" # Default case for p-values >= 0.05
#   )
# }

# linguistic_corr_results_display <- linguistic_corr_results %>%
#   mutate(
#     Outside_Match_Display = paste0(round(Outside_Match_Tau, 3), add_stars(Outside_Match_Pval)),
#     During_Match_Display = paste0(round(During_Match_Tau, 3), add_stars(During_Match_Pval)),
#     Delta_Tau_Display = round(Delta_Tau, 3)
#   ) %>%
#   select(Feature, Outside_Match_Display, During_Match_Display, Delta_Tau_Display)

# # This will now print the table correctly.
# print(knitr::kable(linguistic_corr_results_display,
#             col.names = c("Feature", "Outside Match", "During Match", "Difference (Δτ)"),
#             format = "pipe", align = 'lccc'))


# # Comparing correlation coefficients (Fisher's z-transformation)
# # Example for Profanity (as in original script, values hardcoded from a previous run)
# # r1 = tau_during_profanity, r2 = tau_outside_profanity
# # n1 = N_during_profanity, n2 = N_outside_profanity
# # For Profanity from the table: During=0.124 (N_during), Outside=0.068 (N_outside)
# # The Ns would come from the linguistic_corr_results$N_During and N_Outside for profanity.
# # Let's use the calculated values for Profanity as an example.
# profanity_stats <- linguistic_corr_results %>% filter(Feature == "Profanity")
# if(nrow(profanity_stats) > 0 && !is.na(profanity_stats$During_Match_Tau) && !is.na(profanity_stats$Outside_Match_Tau)) {
  
#   compare_correlations_kendall <- function(tau1, tau2, n1, n2) {
#     # Using Fisher's z transformation on Pearson's r equivalent.
#     # Approximate conversion: r_pearson ≈ sin(pi * tau_kendall / 2)
#     r1_p <- sin(pi * tau1 / 2)
#     r2_p <- sin(pi * tau2 / 2)
    
#     z1 <- 0.5 * log((1 + r1_p) / (1 - r1_p))
#     z2 <- 0.5 * log((1 + r2_p) / (1 - r2_p))
    
#     # Standard error of the difference
#     se_diff <- sqrt(1 / (n1 - 3) + 1 / (n2 - 3))
    
#     # Z statistic for the difference
#     z_diff <- (z1 - z2) / se_diff
    
#     # Two-tailed p-value
#     p_value_diff <- 2 * pnorm(-abs(z_diff))
    
#     return(list(z_statistic = z_diff, p_value = p_value_diff, significant = p_value_diff < 0.05))
#   }
  
#   comparison_result <- compare_correlations_kendall(
#     profanity_stats$During_Match_Tau,
#     profanity_stats$Outside_Match_Tau,
#     profanity_stats$N_During,
#     profanity_stats$N_Outside
#   )
#   print("--- Fisher's z-transformation to compare Profanity correlations (During vs. Outside) ---")
#   print(comparison_result)
# }
# # The bootstrap method (test_tau_difference) was commented out and is very computationally intensive.
# # It can be kept as an alternative for advanced users.


# # -----------------------------------------------------------------------------
# # 5. Qualitative Examples and LaTeX Table (Toxic Pairs)
# # -----------------------------------------------------------------------------
# # This section corresponds to Appendix B in the paper.
# # It filters for posts that are both highly toxic (based on club_toxicity_score, non_club_toxicity_score)
# # and highly negative (based on sentiment scores).
# # Note: 'club_toxicity_score' and 'non_club_toxicity_score' are from the input CSV.
# # 'club_win_sent_score' and 'non_club_win_sent_score' are also from input CSV.
# # These are different from llm_score or the sentiment_index calculated earlier. Clarify source if needed.

# # Filter for potentially problematic pairs
# # First, ensure toxicity scores are numeric and handle NAs
# combined_df <- combined_df %>%
#   mutate(
#     club_toxicity_score = as.numeric(club_toxicity_score),
#     non_club_toxicity_score = as.numeric(non_club_toxicity_score),
#     club_win_sent_score = as.numeric(club_win_sent_score), # Assuming this is the raw sentiment score used in paper
#     non_club_win_sent_score = as.numeric(non_club_win_sent_score)
#   ) %>%
#   filter(!is.na(club_toxicity_score) & !is.na(non_club_toxicity_score) &
#            !is.na(club_win_sent_score) & !is.na(non_club_win_sent_score))


# # The paper uses a threshold of < -5 for toxicity scores. These scores are not on a standard scale,
# # so this threshold is specific to how `club_toxicity_score` was generated.
# # Assuming club_win_sent_score is on -1 to 1 scale, for negative sentiment, score < 0.
# toxic_negative_pairs <- combined_df %>%
#   filter(club_toxicity_score < -5 & non_club_toxicity_score < -5) %>%
#   filter(during_match == TRUE) %>%
#   filter(club_overall_sent == "negative" & non_club_overall_sent == "negative") %>% # Using overall_sent for negativity
#   # Original script had a combined_score for sorting, which can be complex.
#   # Simpler sorting by, e.g., average toxicity or sentiment:
#   arrange(club_toxicity_score, non_club_toxicity_score) %>% # Sort by most toxic first
#   select(
#     club_body, non_club_body, club_subreddit, non_club_subreddit,
#     club_toxicity_score, non_club_toxicity_score,
#     club_overall_sent, non_club_overall_sent,
#     club_win_sent_score, non_club_win_sent_score # Keep original sentiment scores
#   )

# # Remove football-related non-club subreddits for Appendix B examples
# toxic_negative_pairs_no_foot_context <- toxic_negative_pairs %>%
#   filter(!non_club_subreddit %in% c("soccer", "FantasyPL")) %>%
#   distinct(club_body, non_club_body, .keep_all = TRUE) # Keep only unique pairs of text

# print(paste("Number of toxic/negative pairs (no foot context, distinct):", nrow(toxic_negative_pairs_no_foot_context)))
# # if (nrow(toxic_negative_pairs_no_foot_context) > 0) {
# #   write.csv(toxic_negative_pairs_no_foot_context, file = "../acl_paper/toxic_pairs_for_appendix.csv", row.names = FALSE)
# # }

# # LaTeX table generation function (improved escaping)
# create_latex_table_appendix <- function(df, caption_text, label_text, num_examples = 10) {
#   if (nrow(df) == 0) return("No data to create table.")
#   df_sample <- df %>% head(num_examples)
  
#   escape_latex_custom <- function(text) {
#     if (is.na(text)) return("")
#     text <- gsub("\\\\", "\\\\textbackslash{}", text, fixed = TRUE) # Must be first
#     text <- gsub("([&%$#_{}~^])", "\\\\\\1", text) # General LaTeX special chars
#     text <- gsub("\"", "''", text) # Use '' for quotes in LaTeX text
#     text <- gsub("<", "\\\\textless{}", text, fixed = TRUE)
#     text <- gsub(">", "\\\\textgreater{}", text, fixed = TRUE)
#     text <- gsub("|", "\\\\textbar{}", text, fixed = TRUE)
#     # Add more specific escapes as needed, e.g., for asterisks if they cause issues
#     text <- gsub("*", "\\\\*", text, fixed = TRUE)
#     return(text)
#   }
  
#   latex_str <- c(
#     "\\begin{table*}[htbp]",
#     "  \\centering",
#     "  \\caption{" <> caption_text <> "}", # Using <> for paste0 equivalent
#     "  \\label{" <> label_text <> "}",
#     "  \\begin{tabular}{p{.45\\linewidth} p{.45\\linewidth}}",
#     "    \\hline",
#     "    \\textbf{FC Subreddit Post} & \\textbf{Non-FC Subreddit Post (Subreddit)} \\\\",
#     "    \\hline"
#   )
  
#   for (i in 1:nrow(df_sample)) {
#     club_text <- escape_latex_custom(df_sample$club_body[i])
#     non_club_text <- escape_latex_custom(df_sample$non_club_body[i])
#     non_club_sub <- escape_latex_custom(df_sample$non_club_subreddit[i])
#     row_entry <- paste0("    \"", club_text, "\" & \"", non_club_text, "\" (r/", non_club_sub, ") \\\\")
#     latex_str <- c(latex_str, row_entry)
#   }
  
#   latex_str <- c(latex_str, "    \\hline", "  \\end{tabular}", "\\end{table*}")
#   return(paste(latex_str, collapse = "\n"))
# }

# if (nrow(toxic_negative_pairs_no_foot_context) > 0) {
#   latex_output_appendix <- create_latex_table_appendix(
#     toxic_negative_pairs_no_foot_context,
#     caption = "Illustrative paired toxic and negative comments from the same author made within 10 minutes. Quotes and subreddits have been anonymized or generalized.",
#     label = "tab:toxic_pairs_appendix",
#     num_examples = 10 # Show top 10 examples
#   )
#   writeLines(latex_output_appendix, "../acl_paper/toxic_pairs_appendix.tex")
#   # cat(latex_output_appendix) # Print to console
# }


# # -----------------------------------------------------------------------------
# # 6. Difference-in-Differences (DiD) Model (Conceptual Outline)
# # -----------------------------------------------------------------------------
# # The paper describes a DiD model: sentiment_score ~ during_match * club_sentiment_indicator
# # where club_sentiment_indicator is 1 for club posts, 0 for non-club posts.
# # This requires reshaping the data to long format.

# if (nrow(combined_df) > 0 && "club_sentiment_index" %in% names(combined_df)) {
  
#   # Ensure during_match is 0/1 or logical for lm
#   combined_df_for_did <- combined_df %>%
#     mutate(during_match_numeric = ifelse(during_match == TRUE, 1, 0))
  
#   # Reshape to long format
#   long_df_did <- data.table::melt(
#     data.table::setDT(combined_df_for_did),
#     id.vars = c("author", "during_match_numeric", "club_id", "non_club_id"), # Keep identifiers
#     measure.vars = c("club_sentiment_index", "non_club_sentiment_index"),
#     variable.name = "post_type_raw",
#     value.name = "sentiment_score"
#   )
  
#   long_df_did[, is_club_post := ifelse(post_type_raw == "club_sentiment_index", 1, 0)]
  
#   # Filter out rows with NA sentiment_score
#   long_df_did_filtered <- long_df_did[!is.na(sentiment_score)]
  
#   if(nrow(long_df_did_filtered) > 100) { # Check if enough data after filtering NAs
#     did_model <- lm(sentiment_score ~ during_match_numeric * is_club_post, data = long_df_did_filtered)
#     print("--- Difference-in-Differences Model Summary ---")
#     print(summary(did_model))
#     # The coefficient for 'during_match_numeric:is_club_post' is the DiD estimator.
#     # It shows the differential effect of 'during_match' on 'club posts' compared to 'non-club posts'.
#   } else {
#     print("Not enough data for DiD model after reshaping and filtering NAs.")
#   }
# }


# # -----------------------------------------------------------------------------
# # 7. Keyness Analysis (using quanteda)
# # -----------------------------------------------------------------------------
# # Comparing word frequencies between different groups of posts.

# # Prepare data for keyness: need document variables for 'is_football' and 'during_match'
# # This uses the main 'combined_df' and analyzes club_body vs non_club_body
# # Texts need to be combined into one vector with corresponding docvars.
# if (nrow(combined_df) > 0) {
#   keyness_data <- data.frame(
#     text = c(as.character(combined_df$club_body), as.character(combined_df$non_club_body)),
#     doc_id = paste0("doc_", 1:(2 * nrow(combined_df))),
#     is_club_post = c(rep(TRUE, nrow(combined_df)), rep(FALSE, nrow(combined_df))),
#     # 'during_match' status is the same for both posts in a pair
#     during_match_status = c(combined_df$during_match == TRUE, combined_df$during_match == TRUE),
#     stringsAsFactors = FALSE
#   ) %>% filter(!is.na(text) & text != "")
  
  
#   corpus_for_keyness <- corpus(keyness_data, docid_field = "doc_id", text_field = "text")
#   # docvars(corpus_for_keyness, "is_club_post") <- keyness_data$is_club_post # Already done by corpus() call
#   # docvars(corpus_for_keyness, "during_match_status") <- keyness_data$during_match_status
  
#   dfm_for_keyness <- tokens(corpus_for_keyness, remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>%
#     tokens_remove(stopwords("english")) %>%
#     dfm() %>%
#     dfm_trim(min_termfreq = 10, min_docfreq = 5) # Trim for manageable analysis
  
#   if (ndoc(dfm_for_keyness) > 0 && nfeat(dfm_for_keyness) > 0) {
#     # Keyness: Club vs Non-Club posts, specifically during matches
#     dfm_during_match_keyness <- dfm_subset(dfm_for_keyness, during_match_status == TRUE)
#     if (ndoc(dfm_during_match_keyness) > 0) {
#       keyness_club_vs_nonclub_during <- textstat_keyness(
#         dfm_during_match_keyness,
#         target = docvars(dfm_during_match_keyness, "is_club_post") == TRUE, # Target: Club posts
#         measure = "lr" # Log-likelihood ratio; 'chi2' is also common
#       )
#       print("--- Keyness: Club vs Non-Club Posts (During Matches) ---")
#       print(head(keyness_club_vs_nonclub_during, 20))
#       if (nrow(keyness_club_vs_nonclub_during) > 0) {
#         textplot_keyness(keyness_club_vs_nonclub_during, n = 20, show_reference = FALSE)
#         title("Keyness: Club vs Non-Club (During Match)")
#       }
#     }
    
#     # Keyness: During Match vs Outside Match posts, specifically for Club posts
#     dfm_club_posts_keyness <- dfm_subset(dfm_for_keyness, is_club_post == TRUE)
#     if (ndoc(dfm_club_posts_keyness) > 0) {
#       keyness_during_vs_outside_club <- textstat_keyness(
#         dfm_club_posts_keyness,
#         target = docvars(dfm_club_posts_keyness, "during_match_status") == TRUE, # Target: During match
#         measure = "lr"
#       )
#       print("--- Keyness: During Match vs Outside Match (Club Posts) ---")
#       print(head(keyness_during_vs_outside_club, 20))
#       if (nrow(keyness_during_vs_outside_club) > 0) {
#         textplot_keyness(keyness_during_vs_outside_club, n = 20, show_reference = FALSE)
#         title("Keyness: During vs Outside Match (Club Posts)")
#       }
#     }
    
#     # Keyness: During Match vs Outside Match posts, specifically for Non-Club posts
#     # This matches the original script's dfm_nonfootball keyness analysis logic
#     dfm_nonclub_posts_keyness <- dfm_subset(dfm_for_keyness, is_club_post == FALSE)
#     if (ndoc(dfm_nonclub_posts_keyness) > 0) {
#       # Target is 'during_match == FALSE' in original, so reference is 'during_match == TRUE'
#       # To match paper: "keyness comparison for football posts during vs. outside match" -> target = TRUE
#       # Original code: target = dfm_nonfootball$during_match == FALSE
#       # Let's make target = During Match for consistency with above
#       keyness_during_vs_outside_nonclub <- textstat_keyness(
#         dfm_nonclub_posts_keyness,
#         target = docvars(dfm_nonclub_posts_keyness, "during_match_status") == TRUE, # Target: During match
#         measure = "lr"
#       )
#       print("--- Keyness: During Match vs Outside Match (Non-Club Posts) ---")
#       print(head(keyness_during_vs_outside_nonclub, 20))
#       if (nrow(keyness_during_vs_outside_nonclub) > 0) {
#         textplot_keyness(keyness_during_vs_outside_nonclub, n = 20, show_reference = FALSE)
#         title("Keyness: During vs Outside Match (Non-Club Posts)")
#       }
#     }
    
#   } else {
#     print("Not enough documents or features for keyness analysis after DFM trimming.")
#   }
# } else {
#   print("Initial combined_df is empty, skipping Keyness analysis.")
# }


# # End of script