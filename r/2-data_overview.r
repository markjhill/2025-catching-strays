# Load data, provide summary statistics, and compare sentiment distributions of general posts versus potentially problematic posts (hate speech, obscene, toxic).

library(ggplot2)
library(dplyr)
library(lubridate)
library(ggpubr)
library(data.table) 
library(stringr)
library(gridExtra) 
library(cowplot) 
library(scales) 
library(effectsize) 
library(effsize) 
library(patchwork)


# 'not in' operator
'%!in%' <- function(x, y) !('%in%'(x, y))

# Set working directory - change this.
setwd("C:/Users/markj/OneDrive - King's College London/coding/football_contagion/r/code_for_methods_paper")
# setwd("OneDrive - King's College London/coding/football_contagion/r/code_for_methods_paper")

##############################################################################
# Load and Prepare Main Dataset (merged_df)
##############################################################################

# merged_df is created in "1-data_prep.R" but data itself not included

merged_df <- readRDS(file = "../Datasets/all_sub_data/all_data_pruned_2024_09_05.RDS")

str(merged_df)

# Classes ‘data.table’ and 'data.frame':	62430372 obs. of  17 variables:
#  $ id                : chr  "1d7r8t" "cf21e90" "ci08xtk" "ci0atam" ...
#  $ subreddit         : chr  "AFCBournemouth" "AFCBournemouth" "AFCBournemouth" "AFCBournemouth" ...
#  $ season            : chr  "2012-2013" "2013-2014" "2013-2014" "2013-2014" ...
#  $ created_utc       : num  1.37e+09 1.39e+09 1.40e+09 1.40e+09 1.40e+09 ...
#  $ llm_sent          : chr  "neutral" "neutral" "neutral" "neutral" ...
#  $ llm_score         : num  0.0723 0.2239 0.0154 0.4052 -0.321 ...
#  $ competition       : chr  NA NA NA NA ...
#  $ time_since_game   : num  597098 79883 2887375 2892057 2931520 ...
#  $ time_until_game   : num  502 241417 5582525 5577843 5538380 ...
#  $ last_game_result  : chr  "3" "3" "0" "0" ...
#  $ next_game_result  : chr  "1" "0" "3" "3" ...
#  $ current_ht_result : chr  NA NA NA NA ...
#  $ current_result    : chr  NA NA NA NA ...
#  $ current_start_time: num  NA NA NA NA NA ...
#  $ during_match      : logi  NA NA NA NA NA NA ...
#  $ unique_game       : chr  "CARLISLE_1366466400" "HUDDERSFIELD_1390938300" "MILLWALL_1399115700" "MILLWALL_1399115700" ...
#  $ league            : chr  "league-one" "championship" "championship" "championship" ...
#  - attr(*, ".internal.selfref")=<externalptr> 

# Make data.frame
merged_df <- as.data.frame(merged_df) 

# Filter out posts with NA llm_score (0.1%)
merged_df <- merged_df %>%
  filter(!is.na(llm_score))

# Convert 'during_match' to factor and handle NAs
merged_df$during_match <- as.character(merged_df$during_match)
merged_df$during_match[is.na(merged_df$during_match)] <- FALSE # NAs become FALSE
merged_df$during_match <- as.factor(merged_df$during_match)

# Convert created_utc to POSIXct datetime
merged_df$created_datetime <- as.POSIXct(merged_df$created_utc, origin = "1970-01-01")

##############################################################################
# Summary Statistics for merged_df
##############################################################################
print("--- Summary Statistics for merged_df ---")
print(paste("Number of posts:", nrow(merged_df)))
print(paste("Number of unique subreddits:", length(unique(merged_df$subreddit))))
print(paste("Start date:", min(merged_df$created_datetime)))
print(paste("End date:", max(merged_df$created_datetime)))
print(paste("Number of seasons:", length(unique(merged_df$season))))
print(paste("Unique competitions:", paste(unique(merged_df$competition), collapse = ", ")))
print(paste("Total unique games:", length(unique(merged_df$unique_game))))
print(paste("Number of posts during a match:", sum(merged_df$during_match == TRUE)))

# Sentiment score (llm_score) summary
print("Summary of LLM sentiment scores:")
summary(merged_df$llm_score)
print(paste("Standard deviation of LLM sentiment scores:", sd(merged_df$llm_score)))
hist(merged_df$llm_score, main = "Histogram of LLM Sentiment Scores", xlab = "LLM Score")
boxplot(merged_df$llm_score, main = "Boxplot of LLM Sentiment Scores", ylab = "LLM Score")

print("Table of LLM sentiment categories (llm_sent):")
table(merged_df$llm_sent)

print("Summary of posts per subreddit:")
summary(as.numeric(table(merged_df$subreddit)))
print("Top subreddits by post count:")
print(head(sort(table(merged_df$subreddit), decreasing = TRUE)))

# Current result distribution for posts during a match
match_locs <- which(merged_df$during_match == TRUE)
print("Distribution of current_result for posts during a match:")
table(merged_df$current_result[match_locs])

##############################################################################
# Load and Prepare Datasets for Problematic Content Comparison
##############################################################################

# Load lexicons and toxicity scores (sources in paper)
bad_words <- read.csv("../Datasets/list_of_naughty-OaOBW_en.csv") 
hate_speech_lexicon <- read.csv("../Datasets/refined_ngram_dict.csv") 
toxic_df_raw <- readr::read_csv("../Datasets/toxic_results_updated.csv") 

# Prepare toxic_df (posts scored for toxicity)
toxic_df <- toxic_df_raw %>%
  filter(!is.na(toxicity_score), !is.na(llm_score))

# Min-max scaling function for toxicity to range [-1, 1]
normalize_toxicity <- function(x) {
  min_x <- min(x, na.rm = TRUE)
  max_x <- max(x, na.rm = TRUE)
  if (min_x == max_x) return(rep(0, length(x))) # Avoid division by zero if all values are the same
  return(((x - min_x) / (max_x - min_x)) * 2 - 1)
}
toxic_df$toxicity_scaled <- normalize_toxicity(toxic_df$toxicity_score)

# Filter for posts with negative toxicity scores (as per original script)
# The paper applies this to only 10% of our corpus (it's computationally heavy) 
# This filtered_data is what's used as the "Toxic" category later.
filtered_toxic_data <- toxic_df %>%
  filter(toxicity_score < 0)
print(paste("Number of posts in filtered_toxic_data (toxicity_score < 0):", nrow(filtered_toxic_data)))

# Spearman correlation between scaled toxicity and llm_score for this subset
correlation_toxicity_sentiment <- cor(filtered_toxic_data$toxicity_scaled, filtered_toxic_data$llm_score, method = "spearman")
print(paste("Spearman correlation (toxicity_scaled vs llm_score) for toxic_score < 0 subset:", round(correlation_toxicity_sentiment, 3)))

# Load posts with text for hate speech matching
posts_w_text <- readRDS("../Datasets/sentiment_score/all_fc_merged_24_09_02.RDS") # this data is not available, but is the FC subreddits with sentiment scores
print(paste("Number of posts loaded for hate speech matching (posts_w_text):", nrow(posts_w_text)))


# Filter hate speech lexicon: remove unigrams if they are part of multi-word ngrams
# This aims to keep multi-word expressions while removing standalone unigrams that might be too general.
unigrams_in_lexicon <- hate_speech_lexicon %>%
  filter(!str_detect(ngram, " ")) %>%
  pull(ngram)

filtered_hate_speech_lexicon <- hate_speech_lexicon %>%
  filter(!str_detect(ngram, " ") | !str_detect(ngram, str_c("\\b", unigrams_in_lexicon, "\\b", collapse = "|")))
print(paste("Size of original hate speech lexicon:", nrow(hate_speech_lexicon)))
print(paste("Size of filtered hate speech lexicon:", nrow(filtered_hate_speech_lexicon)))


# Matching problematic speech terms in posts

# Time-consuming matching process commented out, and instead pre-matched RDS files are loaded.
# If running for first time ensure you uncomment and run the matching process below

# Matching hate speech
# pattern_hate_speech <- paste0(filtered_hate_speech_lexicon$ngram, collapse = "|")
# DT_large <- as.data.table(posts_w_text)
# Sys.time()
# matches_hate_speech <- DT_large[grepl(pattern_hate_speech, body, ignore.case = TRUE)] # Added ignore.case
# Sys.time() # Original script notes this took ~1 hour
# saveRDS(matches_hate_speech, file = "../Datasets/matched_hate_speech.RDS")
matches_hate_speech <- readRDS(file = "../Datasets/matched_hate_speech.RDS")
print(paste("Number of posts matched for potential hate speech:", nrow(matches_hate_speech)))

# Matching obscene words
# pattern_obscene_speech <- paste0(bad_words$X2g1c, collapse = "|")
# DT_large <- as.data.table(posts_w_text)
# Sys.time()
# matches_hate_speech <- DT_large[grepl(pattern_obscene_speech, body, ignore.case = TRUE)] # Added ignore.case
# Sys.time() # Original script notes this took ~1 hour
# saveRDS(matches_hate_speech, file = "../Datasets/matched_hate_speech2.RDS")

matches_obscene_words <- readRDS(file = "../Datasets/matched_hate_speech2.RDS") # Renamed for clarity
print(paste("Number of posts matched for obscene words:", nrow(matches_obscene_words)))

# Toxic data from sample
sample_data_for_plots <- toxic_df # Renamed for clarity. Used as "Sample" in plots.
print(paste("Number of posts in sample_data_for_plots (used as 'Sample'):", nrow(sample_data_for_plots)))

##############################################################################
# Prepare Data for Comparative Density Plots
##############################################################################

hate_data <- matches_hate_speech %>%
  select(llm_score) %>%
  mutate(dataset = "Potential Hate Speech") %>%
  filter(!is.na(llm_score))

badword_data <- matches_obscene_words %>%
  select(llm_score) %>%
  mutate(dataset = "Obscene Words") %>%
  filter(!is.na(llm_score))

# "Toxic" data uses the `filtered_toxic_data` (toxicity_score < 0)
toxic_data_plot <- filtered_toxic_data %>% # Renamed for clarity
  select(llm_score) %>%
  mutate(dataset = "Toxic") %>%
  filter(!is.na(llm_score))

# "Sample" data for comparison is `sample_data_for_plots` (which is `toxic_df`)
sample_plot_data <- sample_data_for_plots %>%
  select(llm_score) %>%
  mutate(dataset = "Sample") %>%
  filter(!is.na(llm_score))

# Combine datasets for plotting
combined_plot_data <- rbind(hate_data, sample_plot_data, badword_data, toxic_data_plot)
names(combined_plot_data) <- c("llm_score", "Dataset") # Ensure consistent naming

# Calculate means and medians for annotations
means <- combined_plot_data %>%
  group_by(Dataset) %>%
  summarise(mean_llm_score = mean(llm_score, na.rm = TRUE), .groups = 'drop')

medians <- combined_plot_data %>%
  group_by(Dataset) %>%
  summarise(median_llm_score = median(llm_score, na.rm = TRUE), .groups = 'drop')

sample_mean <- means$mean_llm_score[means$Dataset == "Sample"]
hate_mean <- means$mean_llm_score[means$Dataset == "Potential Hate Speech"]
badword_mean <- means$mean_llm_score[means$Dataset == "Obscene Words"]
toxic_mean <- means$mean_llm_score[means$Dataset == "Toxic"]

##############################################################################
# Statistical Tests (KS-test and Cliff's Delta)
##############################################################################

# Kolmogorov-Smirnov tests
ks_test_hate <- ks.test(hate_data$llm_score, sample_plot_data$llm_score)
ks_test_badword <- ks.test(badword_data$llm_score, sample_plot_data$llm_score)
ks_test_toxic <- ks.test(toxic_data_plot$llm_score, sample_plot_data$llm_score)

# Cliff's Delta for effect size
# This takes a long time (tens of minutes)
# Note: integer overflows occured (for me) in Positron but no rstudio
cliffs_d_hate <- cliff.delta(hate_data$llm_score, sample_plot_data$llm_score)
cliffs_d_badword <- cliff.delta(badword_data$llm_score, sample_plot_data$llm_score)
cliffs_d_toxic <- cliff.delta(toxic_data_plot$llm_score, sample_plot_data$llm_score)

##############################################################################
# Generate and Save Density Plots
##############################################################################

# Define a common theme for plots for consistency
plot_theme_density <- theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, size = 12),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    text = element_text(family = "Times"), # Ensure Times font is available
    legend.title = element_blank() # No legend title
  )

# Helper function to create density plots to reduce repetition
create_density_plot <- function(data_subset, category_name, category_mean, sample_mean, ks_p_value, cliffs_delta_est, fill_colors) {
  p_value_text <- if (ks_p_value < 0.001) "< 0.001" else round(ks_p_value, 3)
  
  plot <- ggplot(data_subset, aes(x = llm_score, fill = Dataset)) +
    geom_density(alpha = 0.6, adjust = 1.5) +
    scale_fill_manual(values = fill_colors) +
    labs(
      title = paste("Sentiment Score Distribution Comparison\n", category_name),
      x = "Sentiment Score",
      y = "Density",
      subtitle = paste("KS-test p-value", p_value_text, ", Cliff's δ =", round(cliffs_delta_est, 2))
    ) +
    plot_theme_density +
    geom_vline(xintercept = category_mean, color = "black", linetype = "dashed", linewidth = 0.8) +
    geom_vline(xintercept = sample_mean, color = "gray70", linetype = "dotted", linewidth = 0.8) +
    annotate("text", x = category_mean, y = 0, label = paste("Mean =", round(category_mean, 2)), hjust = -0.1, vjust = -1, color = "black", size = 3) +
    annotate("text", x = sample_mean, y = 0, label = paste("Mean =", round(sample_mean, 2)), hjust = -0.1, vjust = -2.5, color = "black", size = 3) +
    guides(fill = guide_legend(nrow = 2, byrow = TRUE))
  
  return(plot)
}

# Plot for Obscene Words
density_badword <- create_density_plot(
  data_subset = combined_plot_data %>% filter(Dataset %in% c("Obscene Words", "Sample")),
  category_name = "Obscene Words",
  category_mean = badword_mean,
  sample_mean = sample_mean,
  ks_p_value = ks_test_badword$p.value,
  cliffs_delta_est = cliffs_d_badword$estimate,
  fill_colors = c("Obscene Words" = "gray30", "Sample" = "gray70")
)
print(density_badword)

#
#ggsave("../acl_paper/sentiment_bw_density_badwords.svg", density_badword, width = 7, height = 5)


# Plot for Potential Hate Speech
density_hate <- create_density_plot(
  data_subset = combined_plot_data %>% filter(Dataset %in% c("Potential Hate Speech", "Sample")),
  category_name = "Potential Hate Speech",
  category_mean = hate_mean,
  sample_mean = sample_mean,
  ks_p_value = ks_test_hate$p.value,
  cliffs_delta_est = cliffs_d_hate$estimate,
  fill_colors = c("Potential Hate Speech" = "gray30", "Sample" = "gray70")
)
print(density_hate)
#ggsave("../acl_paper/sentiment_bw_density_hate.svg", density_hate, width = 7, height = 5)

# Plot for Toxic Posts
# Ensure correct factor levels for plotting if needed (original script did this)
toxic_plot_data_subset <- combined_plot_data %>%
  filter(Dataset %in% c("Toxic", "Sample")) %>%
  mutate(Dataset = factor(Dataset, levels = c("Toxic", "Sample")))

density_toxic <- create_density_plot(
  data_subset = toxic_plot_data_subset,
  category_name = "Toxic Posts",
  category_mean = toxic_mean,
  sample_mean = sample_mean,
  ks_p_value = ks_test_toxic$p.value, # Original script used ks_test_badword$p.value here, seems like a typo. Corrected to ks_test_toxic.
  cliffs_delta_est = cliffs_d_toxic$estimate,
  fill_colors = c("Toxic" = "gray30", "Sample" = "gray70")
)
print(density_toxic)
#ggsave("../acl_paper/sentiment_bw_density_toxic.svg", density_toxic, width = 7, height = 5)


##############################################################################
# Combine Plots for Publication (as per Figure 1 in the paper)
##############################################################################

# Version 1: Combined plot without individual legends 
density_hate_no_legend <- density_hate + theme(legend.position = "none")
density_badword_no_legend <- density_badword + theme(legend.position = "none")
# Original script had a custom way to remove one legend item for toxic plot,
# if the goal is simply no legend, theme(legend.position = "none") is simpler.
density_toxic_no_legend <- density_toxic + theme(legend.position = "none")

combined_plot_no_legends <- 
  (density_hate_no_legend | density_badword_no_legend | density_toxic_no_legend) +
  plot_layout(nrow = 3) +
  plot_annotation(theme = theme(plot.margin = margin(5, 5, 5, 5)))

print(combined_plot_no_legends)
#ggsave("../acl_paper/combined_density_plots.svg", combined_plot_no_legends, width = 4, height = 8)


# Version 2: Combined plot with legends in corner 
# Re-create plots with modified titles (shorter category names for legend) and legend placement
# Data preparation for shorter legend names
combined_data_short_legend <- combined_plot_data %>%
  mutate(Dataset_Short = case_when(
    Dataset == "Potential Hate Speech" ~ "Hate",
    Dataset == "Obscene Words" ~ "Obscene",
    Dataset == "Toxic" ~ "Toxic",
    Dataset == "Sample" ~ "Sample"
  )) %>%
  mutate(Dataset_Short = factor(Dataset_Short, levels = c("Hate", "Obscene", "Toxic", "Sample")))

plot_theme_density_corner_legend <- theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    text = element_text(family = "Times"),
    legend.position = c(0.8, 0.8), # Legend in top-right corner
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.5),
    legend.title = element_blank()
  )

# Helper for corner legend plots
create_density_plot_corner <- function(data_subset, title_text, category_mean, sample_mean, ks_p_val, cliffs_d_val, fill_vals) {
  p_value_text <- if (ks_p_val < 0.001) "< 0.001" else round(ks_p_val, 3)
  
  ggplot(data_subset, aes(x = llm_score, fill = Dataset_Short)) +
    geom_density(alpha = 0.6, adjust = 1.5) +
    scale_fill_manual(values = fill_vals) +
    labs(
      title = title_text,
      x = "Sentiment Score",
      y = "Density",
      subtitle = paste("KS-test p-value", p_value_text, ", Cliff's δ =", round(cliffs_d_val, 2))
    ) +
    plot_theme_density_corner_legend +
    geom_vline(xintercept = category_mean, color = "black", linetype = "dashed", linewidth = 0.8) +
    geom_vline(xintercept = sample_mean, color = "gray70", linetype = "dotted", linewidth = 0.8) +
    annotate("text", x = category_mean, y = 0, label = paste("Mean =", round(category_mean, 2)), hjust = -0.1, vjust = -1, color = "black", size = 3) +
    annotate("text", x = sample_mean, y = 0, label = paste("Mean =", round(sample_mean, 2)), hjust = -0.1, vjust = -2.5, color = "black", size = 3) +
    guides(fill = guide_legend(nrow = 2, byrow = TRUE)) # Keep legend structure from original
}

density_hate_corner <- create_density_plot_corner(
  combined_data_short_legend %>% filter(Dataset_Short %in% c("Hate", "Sample")),
  "Sentiment Score Distribution Comparison", # Main title only for the first plot
  hate_mean, sample_mean, ks_test_hate$p.value, cliffs_d_hate$estimate,
  c("Hate" = "gray30", "Sample" = "gray70")
)

density_badword_corner <- create_density_plot_corner(
  combined_data_short_legend %>% filter(Dataset_Short %in% c("Obscene", "Sample")),
  NULL, # No title for subsequent plots
  badword_mean, sample_mean, ks_test_badword$p.value, cliffs_d_badword$estimate,
  c("Obscene" = "gray30", "Sample" = "gray70")
)

toxic_plot_data_short_subset <- combined_data_short_legend %>%
  filter(Dataset_Short %in% c("Toxic", "Sample")) %>%
  mutate(Dataset_Short = factor(Dataset_Short, levels = c("Toxic", "Sample")))

density_toxic_corner <- create_density_plot_corner(
  toxic_plot_data_short_subset,
  NULL, # No title
  toxic_mean, sample_mean, ks_test_toxic$p.value, cliffs_d_toxic$estimate,
  c("Toxic" = "gray30", "Sample" = "gray70")
)

combined_plot_with_legends <- 
  (density_hate_corner + 
   density_badword_corner + 
   density_toxic_corner) + 
  plot_layout(ncol = 1, nrow = 3) +
  plot_annotation(theme = theme(plot.margin = margin(5, 5, 5, 5)))

print(combined_plot_with_legends)
#ggsave("../acl_paper/combined_density_plots_with_legend.svg", combined_plot_with_legends, width = 4, height = 8)


##############################################################################
# Create and Print Summary Statistics Table (matches Table 3 in the paper)
##############################################################################
# Using the already calculated means and the *_data dataframes for n, median, sd
# FC Corpus data from initial merged_df
fc_corpus_mean <- mean(merged_df$llm_score, na.rm = TRUE)
fc_corpus_median <- median(merged_df$llm_score, na.rm = TRUE)
fc_corpus_sd <- sd(merged_df$llm_score, na.rm = TRUE)
fc_corpus_n <- nrow(merged_df)


summary_stats_table <- data.frame(
  Dataset = c("FC Corpus", "Potential Hate Speech", "Obscene Language", "Toxic"),
  n = c(
    fc_corpus_n, # N for FC Corpus
    nrow(hate_data), # N for Hate
    nrow(badword_data), # N for Badword
    nrow(toxic_data_plot) # N for Toxic
  ),
  Mean = c(
    fc_corpus_mean, # Mean for FC Corpus
    hate_mean,
    badword_mean,
    toxic_mean
  ),
  Median = c(
    fc_corpus_median, # Median for FC Corpus
    median(hate_data$llm_score, na.rm = TRUE),
    median(badword_data$llm_score, na.rm = TRUE),
    median(toxic_data_plot$llm_score, na.rm = TRUE)
  ),
  SD = c(
    fc_corpus_sd, # SD for FC Corpus
    sd(hate_data$llm_score, na.rm = TRUE),
    sd(badword_data$llm_score, na.rm = TRUE),
    sd(toxic_data_plot$llm_score, na.rm = TRUE)
  )
)

print("--- Summary Statistics Table (Sentiment Scores) ---")
print(summary_stats_table)

