library(ggplot2)
library(dplyr)
library(lubridate)
library(MASS)
library(ordinal)
library(effects)
library(GGally)
library(gmodels)
library(reshape2)
library(correlation)
library(quanteda)
library(quanteda.textstats)
library(quanteda.textplots)
library(dplyr)

setwd("C:/Users/markj/OneDrive - King's College London/coding/football_contagion/r/code_for_methods_paper")

'%!in%' <- function(x,y)!('%in%'(x,y))

combined_df <- read.csv("../Datasets/combined_fc_and_non_fc_toxic.csv")

# get existing data to pull match data

merged_df <- readRDS(file = "C:/Users/markj/OneDrive - King's College London/coding/football_contagion/r/Datasets/all_sub_data/all_data_pruned_2024_09_05.RDS")

# Get rid of posts that aren't classified by tweetnlp (0.1%)

merged_df <- as.data.frame(merged_df)

merged_df <- merged_df %>%
  filter(!is.na(llm_score))

merged_df$during_match <- as.character(merged_df$during_match)
merged_df$during_match[which(is.na(merged_df$during_match))] <- FALSE
merged_df$during_match <- as.factor(merged_df$during_match)

####

# Match club_id in combined_df with id in merged_df
matches <- match(combined_df$club_id, merged_df$id)

# Check if there are any NA values in matches
if(any(is.na(matches))) {
  # Handle cases where club_id doesn't have a match
  matches[is.na(matches)] <- 1  # Use a default position or appropriate handling
}

# Get the during_match values
combined_df$during_match <- merged_df$during_match[matches]

# For rows where during_match is TRUE, set the result
combined_df$result <- merged_df$current_result[matches]

####################

table(combined_df$club_overall_sent)
table(combined_df$non_club_overall_sent)

####################

merged_df <- combined_df

# USE BELOW IF YOU WANT TO CHECK WITHOUT FOOTBALL SUBS!
#merged_df <- combined_df[-c(which(combined_df$non_club_subreddit %in% c("FantasyPL", "soccer"))),]
#merged_df <- combined_df[-c(which(combined_df$non_club_subreddit %in% c("soccer"))),]

# create an llm_score that combines all three. 

merged_df$club_negative <- as.numeric(merged_df$club_negative)
merged_df$club_neutral <- as.numeric(merged_df$club_neutral)
merged_df$club_positive <- as.numeric(merged_df$club_positive)

merged_df$non_club_negative <- as.numeric(merged_df$non_club_negative)
merged_df$non_club_neutral <- as.numeric(merged_df$non_club_neutral)
merged_df$non_club_positive <- as.numeric(merged_df$non_club_positive)

merged_df$result <- as.factor(merged_df$result)

merged_df$club_overall_sent <- as.factor(merged_df$club_overall_sent)
merged_df$non_club_overall_sent <- as.factor(merged_df$non_club_overall_sent)

# Weighted Average Sentiment Score

merged_df$club_weighted_average_sent <- 1 * as.numeric(merged_df$club_positive) + 
  0 * as.numeric(merged_df$club_neutral) + 
  (-1) * as.numeric(merged_df$club_negative)

merged_df$non_club_weighted_average_sent <- 1 * as.numeric(merged_df$non_club_positive) + 
  0 * as.numeric(merged_df$non_club_neutral) + 
  (-1) * as.numeric(merged_df$non_club_negative)

# Sentiment Index

# To provide a combined measure that gives an index value ranging from -1 (completely negative) to 1 (completely positive), you can use a combination of the positive and negative probabilities while accounting for the neutral probability.

merged_df$club_sentiment_index <- 
  (merged_df$club_positive - merged_df$club_negative) / 
  (merged_df$club_positive + merged_df$club_neutral + merged_df$club_negative)

merged_df$non_club_sentiment_index <- 
  (merged_df$non_club_positive - merged_df$non_club_negative) / 
  (merged_df$non_club_positive + merged_df$non_club_neutral + merged_df$non_club_negative)

sort(table(merged_df$non_club_subreddit), decreasing = T)[1:25]

###


# cor(merged_df$club_weighted_average_sent, merged_df$non_club_weighted_average_sent)

# ####
# # check if monotonic for kendall


# # Using the provided is_kendall_monotonic_fast function - base is too slow
# is_kendall_monotonic_fast <- function(x, y, sample_size = NULL, show_progress = TRUE) {
#   n <- length(x)
  
#   # For very large datasets, consider sampling
#   if(!is.null(sample_size) && n > sample_size) {
#     message("Using random sampling of ", sample_size, " pairs from ", n, " total")
#     idx <- sample(n, sample_size)
#     x <- x[idx]
#     y <- y[idx]
#     n <- sample_size
#   }
  
#   # Initialize progress bar if requested
#   if(show_progress) {
#     pb <- txtProgressBar(min = 0, max = 10, style = 3, width = 50)
#   }
  
#   # Process in chunks for progress reporting
#   chunk_size <- ceiling(n/10)
#   result <- NULL
  
#   # Use faster implementation for large datasets
#   if(n > 10000) {
#     # Load the pcaPP package which has a faster implementation
#     if(!requireNamespace("pcaPP", quietly = TRUE)) {
#       install.packages("pcaPP")
#       library(pcaPP)
#     } else {
#       library(pcaPP)
#     }
    
#     # Update progress for pre-processing
#     if(show_progress) setTxtProgressBar(pb, 1)
    
#     # Use fast Kendall from pcaPP package
#     kt <- pcaPP::cor.fk(x, y)
    
#     # Update progress to completion
#     if(show_progress) setTxtProgressBar(pb, 10)
    
#     # Calculate p-value (approximate for large samples)
#     z <- 3 * kt * sqrt(n * (n-1)) / sqrt(2 * (2*n + 5))
#     p_val <- 2 * pnorm(-abs(z))
    
#   } else {
#     # For smaller datasets, use standard implementation with progress updates
#     for(i in 1:10) {
#       if(show_progress) setTxtProgressBar(pb, i)
#       if(i == 10) {
#         # On last chunk, calculate the full correlation
#         result <- cor.test(x, y, method = "kendall")
#       }
#     }
    
#     kt <- result$estimate
#     p_val <- result$p.value
#   }
  
#   if(show_progress) close(pb)
  
#   list(
#     kendall_tau = kt,
#     p_value = p_val,
#     significant_monotonic = p_val < 0.05 & abs(kt) > 0,
#     direction = ifelse(kt > 0, "increasing", 
#                        ifelse(kt < 0, "decreasing", "none"))
#   )
# }

# # Create a table of results using is_kendall_monotonic_fast
# analyze_sentiment_data <- function(merged_df, show_progress = FALSE) {
#   # Initialize empty list to store results
#   results <- list()
  
#   # Define data subsets (ALL, DURING MATCH, NOT DURING MATCH)
#   subsets <- list(
#     "ALL DATA" = rep(TRUE, nrow(merged_df)),
#     "DURING MATCH" = merged_df$during_match == TRUE,
#     "NOT DURING MATCH" = merged_df$during_match == FALSE
#   )
  
#   # Define sentiment filters
#   sentiment_filters <- list(
#     "ALL SENTIMENTS" = rep(TRUE, nrow(merged_df)),
#     "DROP NEUTRAL" = merged_df$club_overall_sent != "neutral",
#     "ONLY POSITIVE" = merged_df$club_overall_sent == "positive",
#     "ONLY NEGATIVE" = merged_df$club_overall_sent == "negative"
#   )
  
#   # Create a results table
#   for (subset_name in names(subsets)) {
#     subset_mask <- subsets[[subset_name]]
    
#     for (filter_name in names(sentiment_filters)) {
#       filter_mask <- sentiment_filters[[filter_name]]
      
#       # Combine masks to get final selection
#       final_mask <- subset_mask & filter_mask
      
#       # Skip if insufficient data points
#       if (sum(final_mask) < 3) {
#         results[[paste(subset_name, filter_name, sep = "_")]] <- list(
#           kendall_tau = NA,
#           p_value = NA,
#           significant_monotonic = NA,
#           direction = NA,
#           sample_size = sum(final_mask),
#           note = "Insufficient data"
#         )
#         next
#       }
      
#       # Get data for the current subset and filter
#       club_sent <- merged_df$club_sent_cat[final_mask]
#       non_club_sent <- merged_df$non_club_sent_cat[final_mask]
      
#       # Calculate Kendall's tau using the fast method
#       analysis_result <- is_kendall_monotonic_fast(club_sent, non_club_sent, show_progress = show_progress)
#       analysis_result$sample_size <- sum(final_mask)
#       results[[paste(subset_name, filter_name, sep = "_")]] <- analysis_result
#     }
#   }
  
#   # Convert results to a data frame for easier viewing
#   result_df <- data.frame(
#     Subset = character(),
#     Sentiment_Filter = character(),
#     Kendalls_Tau = numeric(),
#     P_Value = numeric(),
#     Significant = logical(),
#     Direction = character(),
#     Sample_Size = integer(),
#     Note = character(),
#     stringsAsFactors = FALSE
#   )
  
#   for (key in names(results)) {
#     parts <- strsplit(key, "_", fixed = TRUE)[[1]]
#     subset_name <- parts[1]
#     if (length(parts) > 2) {
#       subset_name <- paste(parts[1], parts[2], sep = "_")
#       filter_name <- paste(parts[3:length(parts)], collapse = "_")
#     } else {
#       filter_name <- parts[2]
#     }
    
#     result_df <- rbind(result_df, data.frame(
#       Subset = subset_name,
#       Sentiment_Filter = filter_name,
#       Kendalls_Tau = results[[key]]$kendall_tau,
#       P_Value = results[[key]]$p_value,
#       Significant = results[[key]]$significant_monotonic,
#       Direction = results[[key]]$direction,
#       Sample_Size = results[[key]]$sample_size,
#       Note = ifelse(is.null(results[[key]]$note), "", results[[key]]$note),
#       stringsAsFactors = FALSE
#     ))
#   }
  
#   return(result_df)
# }

# # Example usage:
# # results_table <- analyze_sentiment_data(merged_df)
# # print(results_table)


# cor(merged_df$club_weighted_average_sent, merged_df$non_club_weighted_average_sent)

# # Example
# #is_kendall_monotonic(merged_df$club_weighted_average_sent, merged_df$non_club_weighted_average_sent)
# is_kendall_monotonic_fast(merged_df$club_weighted_average_sent, merged_df$non_club_weighted_average_sent)

# # there is 0.07904096 for all times, p < 0

# is_kendall_monotonic_fast(merged_df$club_weighted_average_sent[which(merged_df$during_match == FALSE)], 
#                           merged_df$non_club_weighted_average_sent[which(merged_df$during_match == FALSE)])

# # weaker when there is no match (.053489)

# is_kendall_monotonic_fast(merged_df$club_weighted_average_sent[which(merged_df$during_match == TRUE)], 
#                           merged_df$non_club_weighted_average_sent[which(merged_df$during_match == TRUE)])


# # stronger during! 0.1096, p <0 

# ### create a ranked numeric for sentiment, rather than the prob index

# merged_df$club_sent_cat <- NA  
# merged_df$club_sent_cat[which(merged_df$club_overall_sent == "positive")] <- 3
# merged_df$club_sent_cat[which(merged_df$club_overall_sent == "neutral")] <- 2
# merged_df$club_sent_cat[which(merged_df$club_overall_sent == "negative")] <- 1

# merged_df$non_club_sent_cat <- NA  
# merged_df$non_club_sent_cat[which(merged_df$non_club_overall_sent == "positive")] <- 3
# merged_df$non_club_sent_cat[which(merged_df$non_club_overall_sent == "neutral")] <- 2
# merged_df$non_club_sent_cat[which(merged_df$non_club_overall_sent == "negative")] <- 1

# cor(merged_df$club_sent_cat, merged_df$non_club_sent_cat,
#     method = "spearman")

# # Example
# #is_kendall_monotonic(merged_df$club_weighted_average_sent, merged_df$non_club_weighted_average_sent)
# is_kendall_monotonic_fast(merged_df$club_sent_cat, merged_df$non_club_sent_cat)

# # Individual analyses (using is_kendall_monotonic_fast as requested):

# # ALL DATA
# all_data_result <- is_kendall_monotonic_fast(merged_df$club_sent_cat, merged_df$non_club_sent_cat)
# cat("ALL DATA: Kendall's tau =", all_data_result$kendall_tau, 
#     ", p-value =", all_data_result$p_value, 
#     ", Significant =", all_data_result$significant_monotonic,
#     ", Direction =", all_data_result$direction, "\n")

# # NOT DURING MATCH
# not_during_mask <- merged_df$during_match == FALSE
# not_during_result <- is_kendall_monotonic_fast(
#   merged_df$club_sent_cat[not_during_mask], 
#   merged_df$non_club_sent_cat[not_during_mask]
# )
# cat("NOT DURING MATCH: Kendall's tau =", not_during_result$kendall_tau, 
#     ", p-value =", not_during_result$p_value, 
#     ", Significant =", not_during_result$significant_monotonic,
#     ", Direction =", not_during_result$direction, "\n")

# # DURING MATCH
# during_mask <- merged_df$during_match == TRUE
# during_result <- is_kendall_monotonic_fast(
#   merged_df$club_sent_cat[during_mask], 
#   merged_df$non_club_sent_cat[during_mask]
# )
# cat("DURING MATCH: Kendall's tau =", during_result$kendall_tau, 
#     ", p-value =", during_result$p_value, 
#     ", Significant =", during_result$significant_monotonic,
#     ", Direction =", during_result$direction, "\n")

# # Generate comprehensive results table
# results_table <- analyze_sentiment_data(merged_df)

# # Display the results
# print(results_table)

# # Additional analyses for the specific subsets you requested

# # DROP ALL NEUTRAL (ALL DATA)
# non_neutral_mask <- merged_df$club_overall_sent != "neutral"
# non_neutral_result <- is_kendall_monotonic_fast(
#   merged_df$club_sent_cat[non_neutral_mask], 
#   merged_df$non_club_sent_cat[non_neutral_mask]
# )
# cat("DROP ALL NEUTRAL (ALL DATA): Kendall's tau =", non_neutral_result$kendall_tau, 
#     ", p-value =", non_neutral_result$p_value, 
#     ", Significant =", non_neutral_result$significant_monotonic,
#     ", Direction =", non_neutral_result$direction, "\n")

# # ONLY POSITIVE (ALL DATA)
# positive_mask <- merged_df$club_overall_sent == "positive"
# positive_result <- is_kendall_monotonic_fast(
#   merged_df$club_sent_cat[positive_mask], 
#   merged_df$non_club_sent_cat[positive_mask]
# )
# cat("ONLY POSITIVE (ALL DATA): Kendall's tau =", positive_result$kendall_tau, 
#     ", p-value =", positive_result$p_value, 
#     ", Significant =", positive_result$significant_monotonic,
#     ", Direction =", positive_result$direction, "\n")

# # ONLY NEGATIVE (ALL DATA)
# negative_mask <- merged_df$club_overall_sent == "negative"
# negative_result <- is_kendall_monotonic_fast(
#   merged_df$club_sent_cat[negative_mask], 
#   merged_df$non_club_sent_cat[negative_mask]
# )
# cat("ONLY NEGATIVE (ALL DATA): Kendall's tau =", negative_result$kendall_tau, 
#     ", p-value =", negative_result$p_value, 
#     ", Significant =", negative_result$significant_monotonic,
#     ", Direction =", negative_result$direction, "\n")


# ####################################
# # OTHER ANALYSES TO MEASURE POS OR NEG STRONGER
# #################################

# new_df <- merged_df
# new_df$club_sent_cat[which(new_df$club_sent_cat == 1)] <- "negative"
# new_df$club_sent_cat[which(new_df$club_sent_cat == 2)] <- "neutral"
# new_df$club_sent_cat[which(new_df$club_sent_cat == 3)] <- "positive"

# # Create a contingency table
# cont_table <- table(new_df$club_sent_cat, new_df$non_club_overall_sent)
# print("Contingency table:")
# print(cont_table)

# # Calculate Pearson residuals
# chisq_test <- chisq.test(cont_table)
# pearson_residuals <- chisq_test$residuals
# print("Pearson residuals (strength of association for each cell):")
# print(pearson_residuals)

# # Calculate and print the strongest associations
# cat("\nStrength of association by emotion type:\n")
# cat("Negative emotion correlation strength:", pearson_residuals["negative", "negative"], "\n")
# cat("Positive emotion correlation strength:", pearson_residuals["positive", "positive"], "\n")
# cat("Neutral emotion correlation strength:", pearson_residuals["neutral", "neutral"], "\n")


# ############


# # Assuming merged_df is your dataframe with the paired data

# new_df <- merged_df
# new_df$club_sent_cat[which(new_df$club_sent_cat == 1)] <- "negative"
# new_df$club_sent_cat[which(new_df$club_sent_cat == 2)] <- "neutral"
# new_df$club_sent_cat[which(new_df$club_sent_cat == 3)] <- "positive"

# # First, let's examine the structure of our categorical variables
# print("Unique values in club_sent_cat:")
# print(unique(new_df$club_sent_cat))
# print("Unique values in non_club_overall_sent:")
# print(unique(new_df$non_club_overall_sent))

# # Extract the pairs where club_sent_cat is either "positive" or "negative" 
# # (assuming these correspond to your original 1 and 3)
# subset_df <- new_df[new_df$club_sent_cat %in% c("positive", "negative"), ]

# # Part 1: Descriptive analysis - Contingency table
# # Creating a contingency table
# contingency_table <- table(subset_df$club_sent_cat, subset_df$non_club_overall_sent)
# print("Contingency Table:")
# print(contingency_table)

# # Calculating proportions within each club_sent_cat group
# prop_table <- prop.table(contingency_table, margin = 1)
# print("\nProportions Table (by row):")
# print(prop_table)

# # Part 2: Statistical Testing

# # Chi-square test of independence 
# chisq_result <- chisq.test(contingency_table)
# print("\nChi-square Test of Independence:")
# print(chisq_result)

# # For paired data, we should consider the Stuart-Maxwell test
# # which is an extension of McNemar's test for more than 2 categories
# # We need the 'coin' package for this
# if (!requireNamespace("coin", quietly = TRUE)) {
#   install.packages("coin")
# }
# library(coin)

# # Convert data to a format suitable for the test
# # For the Stuart-Maxwell test, we need to reshape the data
# # to have one row per pair and columns for each variable
# paired_data <- data.frame(
#   club_cat = subset_df$club_sent_cat,
#   non_club_cat = subset_df$non_club_overall_sent
# )

# # Stuart-Maxwell test (marginal homogeneity test)
# # First make sure both variables have the same levels
# # Check what values are in club_cat
# print("Unique values in club_cat:")
# print(unique(paired_data$club_cat))

# # Assuming club_cat has same text values (positive, negative, neutral)
# paired_data$club_cat <- factor(paired_data$club_cat, levels = c("negative", "neutral", "positive"))
# paired_data$non_club_cat <- factor(paired_data$non_club_cat, levels = c("negative", "neutral", "positive"))

# # Creating a contingency table for the paired data
# paired_table <- table(paired_data$club_cat, paired_data$non_club_cat)
# print("\nContingency Table for Paired Data:")
# print(paired_table)



##################################
########################################################################
# NLP NLP NLP################################################
########################################################################





# First, let's create all the basic linguistic features using standard text processing
toxic_features <- merged_df %>%
  mutate(
    # Count exclamation marks
    football_exclamations = stringr::str_count(club_body, "!"),
    nonfootball_exclamations = stringr::str_count(non_club_body, "!"),
    
    # ALL CAPS words (shouting)
    football_caps_words = stringr::str_count(club_body, "\\b[A-Z]{2,}\\b"),
    nonfootball_caps_words = stringr::str_count(non_club_body, "\\b[A-Z]{2,}\\b"),
    
    # Question marks
    football_questions = stringr::str_count(club_body, "\\?"),
    nonfootball_questions = stringr::str_count(non_club_body, "\\?")
  )


bad_words <- read.csv("../Datasets/list_of_naughty-OaOBW_en.csv")


# # Create the dictionaries first
# dict_toxic <- dictionary(list(
#   profanity = c("fuck*", "shit*", "cunt*", "damn*", "ass*", "dick*"),
#   slurs = c("idiot*", "stupid*", "moron*", "useless", "pathetic", "disgrace", "fag*", 
#             "gay", "nigger", "nigga", "pussy", "homo*"),
#   violent = c("kill*", "die*", "murder*", "attack*", "destroy*", "hate*", "ruin*", "merc")
# ))

dict_toxic <- dictionary(list(
  profanity = c(bad_words$X2g1c),
  violent = c("kill*", "die*", "murder*", "attack*", "destroy*", "hate*", "ruin*", "merc"),
  slurs = c()
))

dict_intensifiers <- dictionary(list(
  intensifiers = c("very", "really", "so", "extremely", "absolutely", "totally", 
                   "completely", "fucking", "bloody", "literally")
))

# Create corpus for football and non-football posts
corpus_football <- corpus(merged_df$club_body)
corpus_nonfootball <- corpus(merged_df$non_club_body)

# Process football corpus - complete sequence
tokens_football <- tokens(corpus_football, 
                          remove_punct = TRUE, 
                          remove_numbers = TRUE) %>%
  tokens_tolower()
dfm_football <- dfm(tokens_football)
dfm_toxic_football <- dfm_lookup(dfm_football, dict_toxic)
dfm_intensifiers_football <- dfm_lookup(dfm_football, dict_intensifiers)

# Process non-football corpus - complete sequence
tokens_nonfootball <- tokens(corpus_nonfootball, 
                             remove_punct = TRUE, 
                             remove_numbers = TRUE) %>%
  tokens_tolower()
dfm_nonfootball <- dfm(tokens_nonfootball)
dfm_toxic_nonfootball <- dfm_lookup(dfm_nonfootball, dict_toxic)
dfm_intensifiers_nonfootball <- dfm_lookup(dfm_nonfootball, dict_intensifiers)

# Extract counts and add to toxic_features
toxic_features$football_profanity <- as.numeric(dfm_toxic_football[, "profanity"])
toxic_features$football_slurs <- as.numeric(dfm_toxic_football[, "slurs"])
toxic_features$football_violent <- as.numeric(dfm_toxic_football[, "violent"])
toxic_features$nonfootball_profanity <- as.numeric(dfm_toxic_nonfootball[, "profanity"])
toxic_features$nonfootball_slurs <- as.numeric(dfm_toxic_nonfootball[, "slurs"])
toxic_features$nonfootball_violent <- as.numeric(dfm_toxic_nonfootball[, "violent"])

toxic_features$football_intensifiers <- as.numeric(dfm_intensifiers_football[, "intensifiers"])
toxic_features$nonfootball_intensifiers <- as.numeric(dfm_intensifiers_nonfootball[, "intensifiers"])

######

# Create composite scores
toxic_features <- toxic_features %>%
  mutate(
    football_toxicity_score = football_profanity + football_slurs + football_violent,
    nonfootball_toxicity_score = nonfootball_profanity + nonfootball_slurs + nonfootball_violent,
    football_emotionality = football_exclamations + football_intensifiers + football_caps_words,
    nonfootball_emotionality = nonfootball_exclamations + nonfootball_intensifiers + nonfootball_caps_words
  )

###


# Summary statistics
toxic_summary <- toxic_features %>%
  group_by(during_match) %>%
  summarize(
    n = n(),
    avg_football_profanity = mean(football_profanity, na.rm = TRUE),
    avg_nonfootball_profanity = mean(nonfootball_profanity, na.rm = TRUE),
    #avg_football_slurs = mean(football_slurs, na.rm = TRUE),
    #avg_nonfootball_slurs = mean(nonfootball_slurs, na.rm = TRUE),
    # Add other features
  )

toxic_summary


# # Visualize comparisons
# library(ggplot2)
# 
# # Paired boxplots
# ggplot(toxic_features, aes(x = during_match)) +
#   geom_boxplot(aes(y = football_profanity, fill = "Football")) +
#   geom_boxplot(aes(y = nonfootball_profanity, fill = "Non-football")) +
#   labs(title = "Profanity Usage: Football vs. Non-football Contexts",
#        y = "Frequency", x = "During Match") +
#   scale_fill_manual(values = c("Football" = "red", "Non-football" = "blue"),
#                     name = "Context")





###2


# Use the fast Kendall function you provided
is_kendall_monotonic_fast <- function(x, y, sample_size = NULL, show_progress = TRUE) {
  n <- length(x)
  
  # For very large datasets, consider sampling
  if(!is.null(sample_size) && n > sample_size) {
    message("Using random sampling of ", sample_size, " pairs from ", n, " total")
    idx <- sample(n, sample_size)
    x <- x[idx]
    y <- y[idx]
    n <- sample_size
  }
  
  # Initialize progress bar if requested
  if(show_progress) {
    pb <- txtProgressBar(min = 0, max = 10, style = 3, width = 50)
  }
  
  # Process in chunks for progress reporting
  chunk_size <- ceiling(n/10)
  result <- NULL
  
  # Use faster implementation for large datasets
  if(n > 10000) {
    # Load the pcaPP package which has a faster implementation
    if(!requireNamespace("pcaPP", quietly = TRUE)) {
      install.packages("pcaPP")
      library(pcaPP)
    } else {
      library(pcaPP)
    }
    
    # Update progress for pre-processing
    if(show_progress) setTxtProgressBar(pb, 1)
    
    # Use fast Kendall from pcaPP package
    kt <- pcaPP::cor.fk(x, y)
    
    # Update progress to completion
    if(show_progress) setTxtProgressBar(pb, 10)
    
    # Calculate p-value (approximate for large samples)
    z <- 3 * kt * sqrt(n * (n-1)) / sqrt(2 * (2*n + 5))
    p_val <- 2 * pnorm(-abs(z))
    
  } else {
    # For smaller datasets, use standard implementation with progress updates
    for(i in 1:10) {
      if(show_progress) setTxtProgressBar(pb, i)
      if(i == 10) {
        # On last chunk, calculate the full correlation
        result <- cor.test(x, y, method = "kendall")
      }
    }
    
    kt <- result$estimate
    p_val <- result$p.value
  }
  
  if(show_progress) close(pb)
  
  list(
    kendall_tau = kt,
    p_value = p_val,
    significant_monotonic = p_val < 0.05 & abs(kt) > 0,
    direction = ifelse(kt > 0, "increasing", 
                       ifelse(kt < 0, "decreasing", "none"))
  )
}

# Define the feature pairs to analyze
feature_pairs <- list(
  list(name = "Profanity", football = "football_profanity", nonfootball = "nonfootball_profanity"),
  list(name = "Slurs", football = "football_slurs", nonfootball = "nonfootball_slurs"),
  list(name = "Violent", football = "football_violent", nonfootball = "nonfootball_violent"),
  list(name = "Intensifiers", football = "football_intensifiers", nonfootball = "nonfootball_intensifiers"),
  list(name = "Exclamations", football = "football_exclamations", nonfootball = "nonfootball_exclamations"),
  list(name = "Caps", football = "football_caps_words", nonfootball = "nonfootball_caps_words")
)

# Process each feature for match and non-match data
process_features <- function(data_split, match_label) {
  cat("\nProcessing features for", match_label, "\n")
  
  # Initialize results data frame
  results <- data.frame(
    match_status = character(),
    feature = character(),
    kendall_tau = numeric(),
    p_value = numeric(),
    significant = logical(),
    direction = character(),
    stringsAsFactors = FALSE
  )
  
  # Process each feature pair
  for(pair in feature_pairs) {
    cat("  - Analyzing", pair$name, "...\n")
    
    # Extract vectors (handle potential NAs)
    x <- data_split[[pair$football]]
    y <- data_split[[pair$nonfootball]]
    valid_idx <- !is.na(x) & !is.na(y)
    
    # Skip if not enough valid data
    if(sum(valid_idx) < 10) {
      cat("    Not enough valid data points for", pair$name, "\n")
      next
    }
    
    # Apply the fast Kendall function
    correlation <- is_kendall_monotonic_fast(
      x[valid_idx], 
      y[valid_idx],
      sample_size = 5000,  # Optional sampling for very large datasets
      show_progress = TRUE
    )
    
    # Add results to data frame
    results <- rbind(results, data.frame(
      match_status = match_label,
      feature = pair$name,
      kendall_tau = correlation$kendall_tau,
      p_value = correlation$p_value,
      significant = correlation$significant_monotonic,
      direction = correlation$direction,
      stringsAsFactors = FALSE
    ))
    
    # Print current result
    cat("    Result:", 
        "tau =", round(correlation$kendall_tau, 3),
        "p =", round(correlation$p_value, 4),
        "direction =", correlation$direction, "\n")
  }
  
  return(results)
}

# Split the data
match_data <- toxic_features[toxic_features$during_match == TRUE, ]
nonmatch_data <- toxic_features[toxic_features$during_match == FALSE, ]

# Process each dataset
match_results <- process_features(match_data, "During Match")
nonmatch_results <- process_features(nonmatch_data, "Outside Match")

# Combine all results
all_results <- rbind(match_results, nonmatch_results)

# Display formatted results table
library(knitr)
kable(all_results, digits = c(NA, NA, 3, 4, NA, NA))

######
# cluband non club


#################
# check if the differences are statistically significant between during and not during.

# SIMPLE LESS COMPUTATIONALLY INTENSIVE VERSION:

# Fisher's z-transformation to compare correlation coefficients
compare_correlations <- function(r1, r2, n1, n2) {
  # Convert Kendall's tau to Pearson's r (approximate conversion)
  # Using the formula: r ≈ sin(π*τ/2)
  r1_pearson <- sin(pi * r1 / 2)
  r2_pearson <- sin(pi * r2 / 2)
  
  # Fisher's z transformation
  z1 <- 0.5 * log((1 + r1_pearson) / (1 - r1_pearson))
  z2 <- 0.5 * log((1 + r2_pearson) / (1 - r2_pearson))
  
  # Standard error
  se <- sqrt(1/(n1 - 3) + 1/(n2 - 3))
  
  # Z statistic
  z <- (z1 - z2) / se
  
  # Two-tailed p-value
  p_value <- 2 * pnorm(-abs(z))
  
  return(list(
    z_statistic = z,
    p_value = p_value,
    significant = p_value < 0.05
  ))
}

# During match: tau = 0.125, n = 234024
# Outside match: tau = 0.065, n = 341839
result <- compare_correlations(0.125, 0.065, 234024, 341839)

# Print results
cat("Fisher's z-transformation test:\n")
cat("Z statistic:", round(result$z_statistic, 3), "\n")
cat("P-value:", format(result$p_value, scientific = TRUE), "\n")
cat("Significant difference:", result$significant, "\n")

## MORE INTENSIVE USING BOOTSTRAP

# This code creates a function that:
#   
#   Takes bootstrap samples from both the during-match and outside-match data
# Uses your fast Kendall's tau function on each sample
# Computes the difference between the coefficients in each bootstrap iteration
# Calculates a p-value and confidence interval for the difference

# 
# # Test the difference between two Kendall's tau values
# # First, let's define a function to calculate the confidence interval for the difference
# 
# test_tau_difference <- function(data, feature_football, feature_nonfootball, n_bootstraps = 1000) {
#   # Prepare data
#   during_match_data <- data[data$during_match == TRUE, ]
#   outside_match_data <- data[data$during_match == FALSE, ]
#   
#   # Initialize arrays to store bootstrap values
#   during_tau_values <- numeric(n_bootstraps)
#   outside_tau_values <- numeric(n_bootstraps)
#   
#   # Bootstrap procedure
#   for (i in 1:n_bootstraps) {
#     # Sample with replacement from during match data
#     during_indices <- sample(nrow(during_match_data), nrow(during_match_data), replace = TRUE)
#     during_sample <- during_match_data[during_indices, ]
#     
#     # Sample with replacement from outside match data
#     outside_indices <- sample(nrow(outside_match_data), nrow(outside_match_data), replace = TRUE)
#     outside_sample <- outside_match_data[outside_indices, ]
#     
#     # Calculate tau using your fast function for during match
#     during_result <- is_kendall_monotonic_fast(
#       during_sample[[feature_football]], 
#       during_sample[[feature_nonfootball]],
#       sample_size = 5000,
#       show_progress = FALSE
#     )
#     during_tau_values[i] <- during_result$kendall_tau
#     
#     # Calculate tau using your fast function for outside match
#     outside_result <- is_kendall_monotonic_fast(
#       outside_sample[[feature_football]], 
#       outside_sample[[feature_nonfootball]],
#       sample_size = 5000,
#       show_progress = FALSE
#     )
#     outside_tau_values[i] <- outside_result$kendall_tau
#   }
#   
#   # Calculate difference in bootstrap samples
#   difference_values <- during_tau_values - outside_tau_values
#   
#   # Calculate observed difference (from your table)
#   observed_difference <- 0.125 - 0.065
#   
#   # Calculate p-value (two-tailed test)
#   p_value <- mean(abs(difference_values) >= abs(observed_difference))
#   
#   # Create confidence interval
#   confidence_interval <- quantile(difference_values, c(0.025, 0.975))
#   
#   # Return results
#   return(list(
#     observed_difference = observed_difference,
#     bootstrap_mean_difference = mean(difference_values),
#     p_value = p_value,
#     confidence_interval = confidence_interval,
#     difference_values = difference_values
#   ))
# }

# THE BELOW TAKES LOADS OF TIME AND ISN'T STAT SIG

###############
# LETS GET POSTS
##############

if(any(is.na(combined_df$club_toxicity_score))) {
  combined_df <- combined_df[-c(which(is.na(combined_df$club_toxicity_score))),]
}
if(any(is.na(combined_df$non_club_toxicity_score))) {
  combined_df <- combined_df[-c(which(is.na(combined_df$non_club_toxicity_score))),]
}


range(combined_df$non_club_toxicity_score)


# Filter for pairs where both toxicity scores are very low (close to -10)
high_toxicity_pairs_during <- combined_df %>%
  filter(club_toxicity_score < -5 & non_club_toxicity_score < -5) %>%
  filter(during_match == TRUE) %>%
  dplyr::select(club_id, non_club_id, club_toxicity_score, non_club_toxicity_score,
                club_body, non_club_body, club_subreddit, non_club_subreddit)

# Sort by the sum of both scores (lowest combined toxicity first)
high_toxicity_pairs_during <- high_toxicity_pairs_during %>%
  mutate(combined_toxicity = club_toxicity_score + non_club_toxicity_score) %>%
  arrange(combined_toxicity)

# View the results
#head(low_toxicity_pairs, 20)

sort(table(high_toxicity_pairs_during$non_club_subreddit))

high_toxicity_pairs_during_no_foot <- high_toxicity_pairs_during[-c(which(high_toxicity_pairs_during$non_club_subreddit %in% 
                                                                    c("soccer", "FantasyPL"))),]

################ combine toix and snet

# Create a single pipeline that builds upon the high toxicity filtering
toxic_negative_pairs <- combined_df %>%
  # Filter for high toxicity pairs during matches
  filter(club_toxicity_score < -5 & non_club_toxicity_score < -5) %>%
  filter(during_match == TRUE) %>%
  # Filter for negative sentiment in both posts
  filter(club_overall_sent == "negative" & non_club_overall_sent == "negative") %>%
  # Calculate composite score
  mutate(
    # Toxicity component (convert to positive for easier sorting)
    toxicity_component = abs(club_toxicity_score) + abs(non_club_toxicity_score),
    # Sentiment component (convert to positive for easier sorting)
    sentiment_component = abs(club_win_sent_score) + abs(non_club_win_sent_score),
    # Combined score (you can adjust weights as needed)
    combined_score = toxicity_component * 0.7 + sentiment_component * 0.3
  ) %>%
  # Sort by combined score in descending order (higher = more toxic/negative)
  arrange(desc(combined_score)) %>%
  # Select relevant columns
  dplyr::select(club_toxicity_score, non_club_toxicity_score,
         club_win_sent_score, non_club_win_sent_score, combined_score,
         club_body, non_club_body, club_subreddit, non_club_subreddit)


toxic_negative_pairs_no_foot <- toxic_negative_pairs[-c(which(toxic_negative_pairs$non_club_subreddit %in% 
                                                                            c("soccer", "FantasyPL"))),]

which(duplicated(toxic_negative_pairs_no_foot$club_body))

toxic_negative_pairs_no_foot$club_body[58]

pairs_to_modify <- write.csv(toxic_negative_pairs_no_foot, file = "../acl_paper/toxic_pairs.csv", row.names = FALSE)

# CREATE LATEX TABLE

# Create formatted LaTeX table output
create_latex_table <- function(df, caption = "Paired toxic comments from club and non-club subreddits.", label = "tab:toxic_pairs") {
  # Start LaTeX table
  latex_output <- "\\begin{table*}\n  \\centering\n  \\begin{tabular}{p{.45\\linewidth} p{.45\\linewidth}}\n    \\hline\n"
  
  # Add header row
  latex_output <- paste0(latex_output, "    \\textbf{FC Subreddit} & \\textbf{Non-FC Subreddit} \\\\\n    \\hline\n")
  
  # Add each row of data
  for(i in 1:nrow(df)) {
    # Escape special LaTeX characters in text
    club_text <- gsub("([#$%&_{}~^\\\\])", "\\\\\\1", df$club_body[i])
    non_club_text <- gsub("([#$%&_{}~^\\\\])", "\\\\\\1", df$non_club_body[i])
    subreddit <- df$non_club_subreddit[i]
    
    # Format quotation marks correctly
    club_text <- gsub('"', "``", club_text)
    non_club_text <- gsub('"', "``", non_club_text)
    
    # Create the row entry
    row_text <- paste0("    \"", club_text, "\" & \"", non_club_text, "\" (r/", subreddit, ") \\\\\n")
    
    # Add to output
    latex_output <- paste0(latex_output, row_text)
  }
  
  # Close the table
  latex_output <- paste0(latex_output, 
                         "    \\hline\n  \\end{tabular}\n",
                         "  \\caption{", caption, "}\n",
                         "  \\label{", label, "}\n\\end{table*}")
  
  return(latex_output)
}

# Use the function on your dataframe
latex_table <- create_latex_table(toxic_negative_pairs_no_foot)

# Write to a file
writeLines(latex_table, "toxic_pairs_latex_table.tex")

# Also print to console for preview
cat(latex_table)

################
# fix text errors

# Create formatted LaTeX table output with better escaping
create_latex_table <- function(df, caption = "Paired toxic comments from club and non-club subreddits.", label = "tab:toxic_pairs") {
  # Start LaTeX table
  latex_output <- "\\begin{table*}\n  \\centering\n  \\begin{tabular}{p{.45\\linewidth} p{.45\\linewidth}}\n    \\hline\n"
  
  # Add header row
  latex_output <- paste0(latex_output, "    \\textbf{FC Subreddit} & \\textbf{Non-FC Subreddit} \\\\\n    \\hline\n")
  
  # Function to escape LaTeX special characters
  escape_latex <- function(text) {
    # Escape common LaTeX special characters
    text <- gsub("\\\\", "\\\\textbackslash{}", text)
    text <- gsub("([#$%&_{}~^])", "\\\\\\1", text)
    
    # Handle quotation marks
    text <- gsub('"', "``", text)
    
    # Handle math mode characters
    text <- gsub("<", "\\\\textless{}", text)
    text <- gsub(">", "\\\\textgreater{}", text)
    text <- gsub("\\|", "\\\\textbar{}", text)
    
    return(text)
  }
  
  # Add each row of data
  for(i in 1:nrow(df)) {
    # Escape special LaTeX characters in text
    club_text <- escape_latex(df$club_body[i])
    non_club_text <- escape_latex(df$non_club_body[i])
    subreddit <- escape_latex(df$non_club_subreddit[i])
    
    # Create the row entry
    row_text <- paste0("    \"", club_text, "\" & \"", non_club_text, "\" (r/", subreddit, ") \\\\\n")
    
    # Add to output
    latex_output <- paste0(latex_output, row_text)
  }
  
  # Close the table
  latex_output <- paste0(latex_output, 
                         "    \\hline\n  \\end{tabular}\n",
                         "  \\caption{", caption, "}\n",
                         "  \\label{", label, "}\n\\end{table*}")
  
  return(latex_output)
}

# Use the function on your dataframe
latex_table <- create_latex_table(toxic_negative_pairs_no_foot)

# Write to a file
writeLines(latex_table, "toxic_pairs_latex_table.tex")

############################
# CREATE DFS FOR FEATURES - no, this is dumb - why would I carae if two posts say "fuck"? use sentiment.

negative_pairs <- intersect(which(combined_df$club_overall_sent == "negative"), which(combined_df$non_club_overall_sent == "negative"))
negative_pairs_df <- combined_df[c(negative_pairs),]

# Assuming negative_pairs_df is your dataframe
# Create a new column that scores the pair based on both positive values
negative_pairs_df$combined_positive_score <- negative_pairs_df$club_positive * negative_pairs_df$non_club_positive

# Order the dataframe by this combined score in descending order
ordered_pairs <- negative_pairs_df[order(negative_pairs_df$combined_positive_score), ]

ordered_pairs <- ordered_pairs[c(which(ordered_pairs$during_match == TRUE)),]

ordered_pairs <- ordered_pairs[-c(which(ordered_pairs$non_club_subreddit %in% c("soccer", "FantasyPL"))),]

col_to_drop <- c("club_id", "club_negative", "club_neutral",
                 "club_positive", "club_created_utc", "club_created_utc", "club_season",
                 "during_match", "result", "non_club_id", "club_clean_dates_utc", 
                 "non_club_negative", "non_club_neutral", "non_club_positive", "non_club_created_utc",
                 "non_club_clean_dates_utc", "non_club_season", "club_overall_sent", "non_club_overall_sent",
                 "club_win_sent_score", "non_club_win_sent_score")


ordered_pairs <- ordered_pairs[,-c(which(names(ordered_pairs) %in% c(col_to_drop)))]


###


positive_pairs <- intersect(which(combined_df$club_overall_sent == "positive"), which(combined_df$non_club_overall_sent == "positive"))
positive_pairs_df <- combined_df[c(positive_pairs),]

# Assuming positive_pairs_df is your dataframe
# Create a new column that scores the pair based on both positive values
positive_pairs_df$combined_positive_score <- positive_pairs_df$club_positive * positive_pairs_df$non_club_positive

# Order the dataframe by this combined score in descending order
ordered_pairs <- positive_pairs_df[order(-positive_pairs_df$combined_positive_score), ]

ordered_pairs <- ordered_pairs[c(which(ordered_pairs$during_match == TRUE)),]

ordered_pairs <- ordered_pairs[-c(which(ordered_pairs$non_club_subreddit %in% c("soccer", "FantasyPL"))),]


####################

# # Run the test for profanity
# profanity_test <- test_tau_difference(
#   toxic_features, 
#   "football_profanity", 
#   "nonfootball_profanity", 
#   n_bootstraps = 2000
# )
# 
# # Print results
# cat("Comparison of Kendall's tau for profanity between match conditions:\n")
# cat("Observed difference (during - outside):", round(profanity_test$observed_difference, 3), "\n")
# cat("Bootstrap mean difference:", round(profanity_test$bootstrap_mean_difference, 3), "\n")
# cat("P-value:", profanity_test$p_value, "\n")
# cat("95% CI:", paste(round(profanity_test$confidence_interval, 3), collapse = " to "), "\n")
# cat("Interpretation:", ifelse(profanity_test$p_value < 0.05, 
#                               "The difference is statistically significant",
#                               "The difference is not statistically significant"), "\n")

# > cat("Comparison of Kendall's tau for profanity between match conditions:\n")
# Comparison of Kendall's tau for profanity between match conditions:
# > cat("Observed difference (during - outside):", round(profanity_test$observed_difference, 3), "\n")
# Observed difference (during - outside): 0.06 
# > cat("Bootstrap mean difference:", round(profanity_test$bootstrap_mean_difference, 3), "\n")
# Bootstrap mean difference: 0.047 
# > cat("P-value:", profanity_test$p_value, "\n")
# P-value: 0.292 
# > cat("95% CI:", paste(round(profanity_test$confidence_interval, 3), collapse = " to "), "\n")
# 95% CI: 0.002 to 0.094 
# > cat("Interpretation:", ifelse(profanity_test$p_value < 0.05, 
# +                               "The difference is statistically significant",
# +                               "The difference is not statistically significant"), "\n")
# Interpretation: The difference is not statistically significant 

# # Visualize the bootstrap distribution
# library(ggplot2)
# ggplot(data.frame(difference = profanity_test$difference_values), aes(x = difference)) +
#   geom_histogram(bins = 50, fill = "steelblue", color = "black") +
#   geom_vline(xintercept = profanity_test$observed_difference, color = "red", linetype = "dashed", size = 1) +
#   geom_vline(xintercept = profanity_test$confidence_interval, color = "darkgreen", linetype = "dotted", size = 1) +
#   labs(title = "Bootstrap Distribution of Difference in Kendall's Tau for Profanity",
#        subtitle = paste("Observed difference =", round(profanity_test$observed_difference, 3)),
#        x = "Difference in Kendall's Tau (During - Outside)",
#        y = "Frequency") +
#   theme_minimal()


###############
# plots
################

library(dplyr)
library(tidyr)
library(ggplot2)

# Calculate proportions for each feature, split by match status
proportion_data <- toxic_features %>%
  # Convert during_match to logical if it's a factor
  mutate(during_match_bool = as.logical(during_match)) %>%
  group_by(during_match_bool) %>%
  summarize(
    # Profanity 
    football_profanity_rate = mean(football_profanity > 0, na.rm = TRUE),
    nonfootball_profanity_rate = mean(nonfootball_profanity > 0, na.rm = TRUE),
    
    # Violent
    football_violent_rate = mean(football_violent > 0, na.rm = TRUE),
    nonfootball_violent_rate = mean(nonfootball_violent > 0, na.rm = TRUE),
    
    # Intensifiers
    football_intensifiers_rate = mean(football_intensifiers > 0, na.rm = TRUE),
    nonfootball_intensifiers_rate = mean(nonfootball_intensifiers > 0, na.rm = TRUE),
    
    # Exclamations
    football_exclamations_rate = mean(football_exclamations > 0, na.rm = TRUE),
    nonfootball_exclamations_rate = mean(nonfootball_exclamations > 0, na.rm = TRUE),
    
    # Caps
    football_caps_rate = mean(football_caps_words > 0, na.rm = TRUE),
    nonfootball_caps_rate = mean(nonfootball_caps_words > 0, na.rm = TRUE)
  ) %>%
  mutate(match_status = ifelse(during_match_bool, "During Match", "Outside Match"))

# Now continue with the rest of the code
# Reshape data for plotting proportions
proportion_long <- proportion_data %>%
  pivot_longer(
    cols = -c(match_status, during_match_bool),
    names_to = "measure",
    values_to = "proportion"
  ) %>%
  mutate(
    feature = case_when(
      grepl("profanity", measure) ~ "Profanity",
      grepl("violent", measure) ~ "Violent",
      grepl("intensifiers", measure) ~ "Intensifiers",
      grepl("exclamations", measure) ~ "Exclamations",
      grepl("caps", measure) ~ "Caps"
    ),
    context = ifelse(grepl("football_", measure), "Football", "Non-football")
  ) %>%
  dplyr::select(match_status, feature, context, proportion)

# Prepare tau data
tau_data <- all_results %>%
  dplyr::select(match_status, feature, kendall_tau) %>%
  filter(!is.na(kendall_tau))

# Create first plot: Proportions by feature, context and match status
proportion_plot <- ggplot(proportion_long, aes(x = feature, y = proportion, fill = interaction(context, match_status))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  scale_fill_brewer(palette = "Set2", name = "Context") +
  labs(title = "Proportion of Posts Containing Emotional Features",
       subtitle = "Split by football/non-football and match status",
       x = "Feature", 
       y = "Proportion of Posts") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Create second plot: Kendall's tau by feature and match status
tau_plot <- ggplot(tau_data, aes(x = feature, y = kendall_tau, fill = match_status)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  scale_fill_brewer(palette = "Set1", name = "Match Status") +
  labs(title = "Emotional Spillover: Kendall's Tau by Feature",
       subtitle = "Correlation between football and non-football emotional features",
       x = "Feature", 
       y = "Kendall's Tau") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Print both plots
print(proportion_plot)
print(tau_plot)

###
# plot of props and tau
###

library(ggplot2)
library(dplyr)
library(gridExtra)

# Filter out exclamations from tau data
tau_plot_data <- all_results %>%
  filter(!is.na(kendall_tau) & feature != "Exclamations")

# Create tau plot with properly spaced bars
tau_plot <- ggplot(tau_plot_data, aes(x = feature, y = kendall_tau, fill = match_status)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  scale_fill_grey(start = 0.3, end = 0.7, name = "Match Status") +
  labs(title = "Emotional Spillover (Kendall's Tau)",
       x = NULL, 
       y = "Kendall's Tau") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

# Calculate proportions manually - but simplify to just have football vs non-football for clarity
proportion_data <- data.frame(
  feature = rep(c("Profanity", "Violent", "Intensifiers", "Caps"), each = 4),
  match_status = rep(rep(c("During Match", "Outside Match"), each = 2), 4),
  context = rep(c("Football", "Non-football"), 8),
  proportion = c(
    # Profanity
    mean(during_match_data$football_profanity > 0, na.rm = TRUE),
    mean(during_match_data$nonfootball_profanity > 0, na.rm = TRUE),
    mean(outside_match_data$football_profanity > 0, na.rm = TRUE),
    mean(outside_match_data$nonfootball_profanity > 0, na.rm = TRUE),
    # Violent
    mean(during_match_data$football_violent > 0, na.rm = TRUE),
    mean(during_match_data$nonfootball_violent > 0, na.rm = TRUE),
    mean(outside_match_data$football_violent > 0, na.rm = TRUE),
    mean(outside_match_data$nonfootball_violent > 0, na.rm = TRUE),
    # Intensifiers
    mean(during_match_data$football_intensifiers > 0, na.rm = TRUE),
    mean(during_match_data$nonfootball_intensifiers > 0, na.rm = TRUE),
    mean(outside_match_data$football_intensifiers > 0, na.rm = TRUE),
    mean(outside_match_data$nonfootball_intensifiers > 0, na.rm = TRUE),
    # Caps
    mean(during_match_data$football_caps_words > 0, na.rm = TRUE),
    mean(during_match_data$nonfootball_caps_words > 0, na.rm = TRUE),
    mean(outside_match_data$football_caps_words > 0, na.rm = TRUE),
    mean(outside_match_data$nonfootball_caps_words > 0, na.rm = TRUE)
  )
)

# Create proportion plot with properly spaced bars
proportion_plot <- ggplot(proportion_data, aes(x = feature, y = proportion, fill = interaction(match_status, context))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  scale_fill_grey(start = 0.1, end = 0.9, name = "Context") +
  labs(title = "Proportion of Posts with Emotional Features",
       x = "Feature", 
       y = "Proportion") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

# Arrange plots together
grid.arrange(proportion_plot, tau_plot, ncol = 1)


###################################################
# user level analysis

# User-level analysis with correct factor handling
user_toxicity <- toxic_features %>%
  group_by(author) %>%
  summarize(
    posts = n(),
    avg_football_toxicity = mean(football_toxicity_score, na.rm = TRUE),
    avg_nonfootball_toxicity = mean(nonfootball_toxicity_score, na.rm = TRUE),
    avg_football_emotionality = mean(football_emotionality, na.rm = TRUE),
    avg_nonfootball_emotionality = mean(nonfootball_emotionality, na.rm = TRUE),
    # Convert factor to numeric or logical for counting
    match_posts = sum(during_match == TRUE, na.rm = TRUE),  # Explicitly compare to TRUE
    nonmatch_posts = sum(during_match == FALSE, na.rm = TRUE)  # Explicitly compare to FALSE
  )

# Identify high-contagion users
user_toxicity$contagion_score <- with(user_toxicity, 
                                      avg_nonfootball_toxicity / avg_football_toxicity)

###############
###################################################
# diff-in-diff
# Compare the difference in toxic language during matches vs. not during matches

# For both football and non-football contexts
did_analysis <- toxic_features %>%
  group_by(during_match) %>%
  summarize(
    football_toxicity = mean(football_toxicity_score, na.rm = TRUE),
    nonfootball_toxicity = mean(nonfootball_toxicity_score, na.rm = TRUE),
    difference = nonfootball_toxicity - football_toxicity
  )

library(data.table)

# Reshape data to long format
long_df <- melt(setDT(merged_df),  # Ensure it's a data.table
                id.vars = "during_match", 
                measure.vars = c("club_sentiment_index", "non_club_sentiment_index"),
                variable.name = "sentiment_type",
                value.name = "sentiment_score")

# Create the treatment variable
long_df[, club_sentiment := fifelse(sentiment_type == "club_sentiment_index", 1, 0)]



did_model <- lm(sentiment_score ~ during_match * club_sentiment, data = long_df)
summary(did_model)

#This tests:
# 
#  β3β3​ (during_match:club_sentiment) → Whether club-related sentiment changes during matches differently from non-club sentiment.

# 
# Interpretation of the Model
# 
# Your regression model:
#   \text{sentiment_score} = \beta_0 + \beta_1 \text{during_match} + \beta_2 \text{club_sentiment} + \beta_3 (\text{during_match} \times \text{club_sentiment}) + \epsilon
# 
# (Intercept) = -0.118
# 
# This is the baseline sentiment score when during_match = FALSE and club_sentiment = 0 (i.e., non-club sentiment during non-match times).
# 
# The average sentiment in this group is slightly negative.
# 
# during_matchTRUE = -0.019 (significant)
# 
# When a post is made during a match, sentiment scores decrease by 0.019 on average.
# 
# This suggests that overall sentiment is slightly more negative during matches.
# 
# club_sentiment = 0.015 (significant)
# 
# Club-related sentiment scores are, on average, 0.015 higher than non-club sentiment scores, controlling for match timing.
# 
# This suggests that club-related discussions tend to be slightly more positive.
# 
# during_matchTRUE:club_sentiment = -0.015 (significant)
# 
# This is the DiD effect: The change in club sentiment scores during matches compared to non-club sentiment.
# 
# It is negative, meaning that club sentiment decreases more during matches than non-club sentiment.
# 
# The magnitude is -0.015, which suggests that the sentiment gap between club and non-club content narrows slightly during matches.
# 
# Summary of Findings
# 
# Overall sentiment declines during matches (-0.019).
# 
# Club-related sentiment is generally more positive than non-club sentiment (+0.015).
# 
# During matches, club sentiment declines relative to non-club sentiment (-0.015), but not as much as general sentiment.


# Create data frame of tau values
spillover_df <- data.frame(
  feature = c("Profanity", "Violent", "Intensifiers", "Exclamations", "Caps"),
  during_tau = c(0.125, 0.031, 0.096, 0.174, 0.095),
  outside_tau = c(0.065, 0.007, 0.053, 0.129, 0.058)
)

# Create visualization
ggplot(spillover_df, aes(x = feature)) +
  geom_bar(aes(y = during_tau, fill = "During Match"), stat = "identity", position = "dodge") +
  geom_bar(aes(y = outside_tau, fill = "Outside Match"), stat = "identity", position = "dodge") +
  labs(title = "Emotional Spillover by Linguistic Feature",
       subtitle = "Kendall's tau values for football → non-football correlation",
       x = "Linguistic Feature", 
       y = "Kendall's tau") +
  theme_minimal() +
  coord_flip()


###########################
# keyness


library(quanteda)
library(quanteda.textplots)

library(quanteda)
library(dplyr)

# Create a data frame with paired document IDs
dfm_data <- data.frame(
  text = c(merged_df$club_body, merged_df$non_club_body),
  docname = c(paste0("fc_", 1:nrow(merged_df)), paste0("nonfc_", 1:nrow(merged_df))),
  is_football = c(rep(TRUE, nrow(merged_df)), rep(FALSE, nrow(merged_df))),
  during_match = c(merged_df$during_match, merged_df$during_match),  # Duplicating the during_match variable
  row_id = c(1:nrow(merged_df), 1:nrow(merged_df))  # To keep track of pairs
)

# Create corpus with metadata
corpus_paired <- corpus(dfm_data$text, 
                        docnames = dfm_data$docname)

# Add document variables
docvars(corpus_paired, "is_football") <- dfm_data$is_football
docvars(corpus_paired, "during_match") <- dfm_data$during_match
docvars(corpus_paired, "row_id") <- dfm_data$row_id

# Create tokens
tokens_paired <- tokens(corpus_paired, 
                        remove_punct = TRUE,
                        remove_numbers = TRUE,
                        remove_symbols = TRUE) %>%
  tokens_remove(stopwords("english"))

# Create DFM
dfm_paired <- dfm(tokens_paired)

# Trim sparse terms for more manageable analysis
dfm_paired_trimmed <- dfm_trim(dfm_paired, min_docfreq = 5, min_termfreq = 10)

# Check the dimensions of the resulting DFM
dim(dfm_paired_trimmed)

# View a sample of the docnames to confirm they're correct
head(docnames(dfm_paired_trimmed), 10)


# Assuming you have a DFM with a docvar indicating match status and post type
# Create keyness comparison for football posts during vs. outside match
keyness_match_status <- textstat_keyness(dfm_paired_trimmed, 
                                         target = dfm_paired_trimmed$during_match == TRUE,
                                         measure = "lr") # Log-likelihood ratio


# Plot the top distinctive features
textplot_keyness(keyness_match_status, n = 20)


dfm_nonfootball <- dfm_subset(dfm_paired_trimmed, is_football == FALSE)

# Assuming you have a DFM with a docvar indicating match status and post type
# Create keyness comparison for football posts during vs. outside match
keyness_match_status <- textstat_keyness(dfm_nonfootball, 
                                         target = dfm_nonfootball$during_match == FALSE,
                                         measure = "lr") # Log-likelihood ratio


# Plot the top distinctive features
textplot_keyness(keyness_match_status, n = 20)

###

# Similarly for football vs. non-football during matches
keyness_post_type <- textstat_keyness(dfm_paired_trimmed[dfm_paired_trimmed$during_match == TRUE,], 
                                      target = dfm_paired_trimmed$is_football == TRUE)
textplot_keyness(keyness_post_type, n = 20)



