# AI REFACTOR OF MY CODE - NEED TO CEHCK!

# Purpose: Generate plots analyzing sentiment in relation to football match events,
# outcomes, and timelines.
# Key Outputs:
# - Figure 2: Box plots of sentiment 24h before/after matches by result.
# - Figure 3: Smoothed sentiment lines during matches by result.
# - Figure 4: Detailed sentiment timeline for a specific match (Arsenal vs Bournemouth).
# - Plot: NUFC season sentiment timeline.
# - Figure 5: Post count and sentiment relative to game time (weeks).

# -----------------------------------------------------------------------------
# 1. Setup - Libraries
# -----------------------------------------------------------------------------
library(ggplot2)
library(dplyr)
library(lubridate)
library(ggpubr) # For ggarrange, text_grob, annotate_figure
library(mgcv)   # For GAM smoothing (geom_smooth(method = "gam"))
library(tidyr)  # For pivot_longer if needed (not explicitly used in final plots here but good for data reshaping)
# library(viridis) # For color scales, though grayscale is used for final plots
# library(hexbin) # Not used in active code
# library(ordinal) # Not used
# library(effects) # Not used
# library(GGally) # Not used
# library(gmodels) # Not used
# library(reshape2) # Not used
# library(correlation) # Not used
# library(tidyquant) # Not used


# Define custom 'not in' operator if used (not explicitly in this script's final version)
'%!in%' <- function(x, y) !('%in%'(x, y))

# Set working directory - Users will need to change this.
setwd("C:/Users/markj/OneDrive - King's College London/coding/football_contagion/r/code_for_methods_paper")

# -----------------------------------------------------------------------------
# 2. Load and Prepare Main Dataset
# -----------------------------------------------------------------------------
# This is the same main dataset used in data_overview.R
merged_df <- readRDS(file = "../Datasets/all_sub_data/all_data_pruned_2024_09_05.RDS")
merged_df <- as.data.frame(merged_df) %>%
  filter(!is.na(llm_score)) %>%
  mutate(
    during_match = as.character(during_match),
    during_match = ifelse(is.na(during_match), FALSE, during_match),
    during_match = as.factor(during_match),
    created_datetime = as.POSIXct(created_utc, origin = "1970-01-01")
  )

# Common theme for plots (consistent with paper's style)
theme_custom_minimal <- theme_minimal(base_size = 12) +
  theme(
    text = element_text(family = "Times"), # Ensure Times font is available
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 11, hjust = 0.5),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "gray95", color = NA), # Light gray background
    plot.background = element_rect(fill = "white", color = NA),   # White plot background
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 10),
    legend.position = "bottom"
  )

# -----------------------------------------------------------------------------
# 3. Figure 2: Sentiment Before and After Matches by Result
# -----------------------------------------------------------------------------
# Define time window (24 hours)
hours_24_sec <- 24 * 60 * 60

# Prepare data for posts around games
posts_around_games <- merged_df %>%
  mutate(
    status = case_when(
      time_since_game > 0 & time_since_game <= hours_24_sec ~ "After",
      time_until_game > 0 & time_until_game <= hours_24_sec ~ "Before",
      TRUE ~ NA_character_
    ),
    # Assign result based on whether it's before or after the game
    # last_game_result for 'After', next_game_result for 'Before'
    # Ensure these columns exist and are correctly populated in merged_df
    relevant_result = case_when(
      status == "After" & !is.na(last_game_result) ~ as.character(last_game_result),
      status == "Before" & !is.na(next_game_result) ~ as.character(next_game_result),
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(status) & !is.na(relevant_result)) %>%
  mutate(
    status = factor(status, levels = c('Before', 'After'), ordered = TRUE),
    result_label = factor(relevant_result,
                          levels = c("0", "1", "3"), # Loss, Draw, Win
                          labels = c("Loss", "Draw", "Win"))
  ) %>%
  filter(!is.na(result_label)) # Remove if result couldn't be labeled


# Function to get p-value from t-test for plot annotation
get_ttest_pvalue_text <- function(data_subset) {
  if (nrow(data_subset) < 10 || length(unique(data_subset$status)) < 2) return("p-value: N/A")
  # Check if there are enough observations in each group
  if (sum(data_subset$status == "Before") < 2 || sum(data_subset$status == "After") < 2) return("p-value: N/A (low N)")
  
  ttest_result <- t.test(llm_score ~ status, data = data_subset)
  p_val <- ttest_result$p.value
  return(paste("p-value", if (p_val < 0.001) "< 0.001" else paste("=", round(p_val, 3))))
}

# Create individual box plots
plot_before_after <- function(data_for_result, title_suffix) {
  p_value_text <- get_ttest_pvalue_text(data_for_result)
  
  ggplot(data_for_result, aes(y = llm_score, x = status)) +
    geom_boxplot(fill = "gray80", outlier.shape = NA, width = 0.6) + # No outliers plotted directly for cleaner look
    geom_jitter(alpha = 0.05, width = 0.1, size = 0.5, color = "gray50") + # Subtle jitter
    geom_smooth(method = "lm", se = FALSE, aes(group = 1), color = "black", linetype = "solid", linewidth = 0.8) +
    labs(
      title = paste("Sentiment 24 Hours\nBefore and After a", title_suffix),
      subtitle = p_value_text,
      y = "Sentiment Score",
      x = ""
    ) +
    theme_custom_minimal +
    theme(plot.title = element_text(size=12)) # Smaller title for combined plot
}

loss_24_plot <- plot_before_after(posts_around_games %>% filter(result_label == "Loss"), "Loss")
draw_24_plot <- plot_before_after(posts_around_games %>% filter(result_label == "Draw"), "Draw")
win_24_plot  <- plot_before_after(posts_around_games %>% filter(result_label == "Win"), "Win")

# Combine plots for Figure 2
figure2_combined <- ggarrange(loss_24_plot, draw_24_plot, win_24_plot,
                              ncol = 1, nrow = 3, common.legend = TRUE) %>%
  annotate_figure(top = text_grob("Match Result and Post Sentiment",
                                  size = 16, family = "Times", face = "bold"))

print(figure2_combined)
ggsave("../acl_paper/figure2_sentiment_before_after_matches.svg", figure2_combined, width = 4, height = 8)

# Calculate median changes (as per original script, for textual summary)
posts_around_games %>%
  group_by(result_label, status) %>%
  summarise(median_sentiment = median(llm_score, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = status, values_from = median_sentiment) %>%
  mutate(median_change = After - Before) %>%
  print()


# -----------------------------------------------------------------------------
# 4. Figure 3: Average Sentiment During a Game by Result
# -----------------------------------------------------------------------------
game_state_df <- merged_df %>%
  filter(during_match == TRUE & !is.na(current_start_time) & !is.na(created_utc) & !is.na(current_result)) %>%
  mutate(
    game_state_minute = round((as.numeric(created_utc) - as.numeric(current_start_time)) / 60),
    result_label = factor(current_result,
                          levels = c(0, 1, 3),
                          labels = c("Loss", "Draw", "Win"))
  ) %>%
  filter(game_state_minute >= 0 & game_state_minute <= 125, !is.na(result_label)) # Cap minutes for plot


figure3_sentiment_during_game <- ggplot(game_state_df,
                                        aes(y = llm_score, x = game_state_minute, linetype = result_label)) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs", k = 15), # GAM smooth with cubic splines
              color = "black", se = TRUE, linewidth = 0.8, fill = "gray80") +
  coord_cartesian(ylim = c(-0.5, 0.2)) + # Zoom in on y-axis
  theme_custom_minimal +
  geom_vline(xintercept = 45, linetype = "dotted", linewidth = 0.7, color = "gray30") +
  annotate("text", x = 47, y = -0.48, label = "Half-time", hjust = 0, size = 3, family = "Times") +
  labs(
    title = "Average Sentiment During Game",
    y = "Sentiment Score",
    x = "Match Minute",
    linetype = "Final Result" # Legend title
  ) +
  scale_linetype_manual(values = c("Loss" = "solid", "Draw" = "dashed", "Win" = "longdash"))

print(figure3_sentiment_during_game)
ggsave("../acl_paper/figure3_sentiment_during_match.svg", figure3_sentiment_during_game, width = 5, height = 4) # Adjusted size for single panel

# -----------------------------------------------------------------------------
# 5. Figure 4: Sentiment During Specific Game (Arsenal vs Bournemouth)
# -----------------------------------------------------------------------------
# Filter for Arsenal subreddits and the specific match day
arsenal_bournemouth_game <- merged_df %>%
  filter(subreddit %in% c("Gunners", "ArsenalFC")) %>%
  filter(as.Date(created_datetime) == as.Date("2023-03-04"))

# Identify actual game start and end times from the 'during_match' data for that day
# This is more robust than hardcoding epochs if data is precise
game_times <- arsenal_bournemouth_game %>%
  filter(during_match == TRUE) %>%
  summarise(
    min_utc = min(created_utc, na.rm = TRUE),
    max_utc = max(created_utc, na.rm = TRUE),
    # Infer game_start_time if possible, or use the predominant one
    game_start = first(current_start_time[!is.na(current_start_time)])
  )

# Add a buffer around the game for smoothing continuity (e.g., 10 mins before/after)
buffer_seconds <- 10 * 60
start_epoch_buffered <- game_times$min_utc - buffer_seconds
end_epoch_buffered <- game_times$max_utc + buffer_seconds # Original had end_epoch as max(created_utc)

specific_game_df <- arsenal_bournemouth_game %>%
  # Include posts slightly before actual kick-off for smoother start of the line
  filter(created_utc >= start_epoch_buffered & created_utc <= end_epoch_buffered) %>%
  # Assign consistent during_match & current_start_time for the game period for minute calculation
  mutate(
    is_during_match_period = (created_utc >= game_times$min_utc & created_utc <= game_times$max_utc),
    # Use a single, consistent start time for minute calculation across all selected posts
    match_start_time_calc = game_times$game_start,
    game_state_minute = round((as.numeric(created_utc) - as.numeric(match_start_time_calc)) / 60)
  ) %>%
  # Exclude neutral sentiment posts for clearer visualization of swings (as in paper notes)
  filter(llm_sent != "neutral")


# Define actual kick-off time for x-axis 0 minute mark
# Match events (times are approximate game minutes from report for annotation)
# Goal times (Bournemouth 1', 57'; Arsenal 62', 70', 97')
# Injury (Trossard 22')
# Half time starts after 45 + stoppage. Assume approx 4 mins for 1st half stoppage.
first_half_stoppage <- 4 # Approximate
half_time_start_minute <- 45 + first_half_stoppage
half_time_duration <- 15

# Adjust game event minutes for plotting if half-time is treated as a continuous block
# For plotting, we often "remove" half-time from the x-axis flow or show it as a gap.
# Here, game_state_minute is continuous. Event times from second half need adjustment if first half had stoppage.
# Example: a goal at 57' actual game time, if 45+4 was HT start, is 57 - 4 = 53 mins of play + HT.
# The original script plots raw game_state_minute, so event lines should match that.
# Goal Bournemouth: 1', 57'
# Goal Arsenal: 62', 70', 97'
# Injury Arsenal: 22' (Trossard)

# Define event times based on continuous game_state_minute
events <- data.frame(
  minute = c(1, 22, 57, 62, 70, 97),
  label = c("Goal Against", "Arsenal Injury", "Goal Against", "Goal For", "Goal For", "Goal For"),
  linetype = c("dotted", "solid", "dotted", "dashed", "dashed", "dashed")
)

figure4_specific_game <- ggplot(specific_game_df, aes(y = llm_score, x = game_state_minute)) +
  geom_jitter(data = ~.x %>% filter(is_during_match_period & game_state_minute >= 0 & game_state_minute <= 110), # Show points only during match
              alpha = 0.1, size = 3, color = "darkgray", width = 0.2, height = 0.01) +
  geom_smooth(method = "loess", span = 0.25, color = "black", se = TRUE, linewidth = 1, fill="gray80") + # LOESS smooth
  geom_vline(data = events, aes(xintercept = minute, linetype = label), linewidth = 0.7) +
  geom_rect(aes(xmin = half_time_start_minute, xmax = half_time_start_minute + half_time_duration,
                ymin = -Inf, ymax = Inf), fill = "grey", alpha = 0.2) + # Half-time shading
  annotate("text", x = half_time_start_minute + half_time_duration / 2, y = max(specific_game_df$llm_score, na.rm=T) * 0.9,
           label = "Half-time", size = 3, family = "Times", color="gray40") +
  scale_linetype_manual(
    name = "Match Event",
    values = c("Goal For" = "dashed", "Goal Against" = "dotted", "Arsenal Injury" = "solid"),
    breaks = c("Goal For", "Goal Against", "Arsenal Injury")
  ) +
  coord_cartesian(ylim = c(-1, 1), xlim = c(-5, 115)) + # Show a bit before kick-off and after typical full time
  scale_x_continuous(breaks = seq(0, 105, by = 15)) +
  theme_custom_minimal +
  labs(
    title = "Arsenal (3) vs Bournemouth (2) - 4 March 2023",
    subtitle = "r/Gunners & r/ArsenalFC Sentiment (Neutral Posts Excluded)",
    y = "Sentiment Score",
    x = "Match Minute (approx.)"
  )

print(figure4_specific_game)
ggsave("../acl_paper/figure4_sentiment_specific_game.svg", figure4_specific_game, width = 7, height = 5)


# -----------------------------------------------------------------------------
# 6. NUFC Season Sentiment (2021-2022)
# -----------------------------------------------------------------------------
nufc_df_season <- merged_df %>%
  filter(subreddit == "NUFC" & season == "2021-2022") %>%
  mutate(date = as.Date(created_datetime)) %>%
  # Exclude neutral posts for this visualization too, if desired for consistency with Fig 4
  filter(llm_sent != "neutral") %>% 
  filter(date <= as.Date("2022-06-01")) # Filter up to end of season

# Get match dates and results for NUFC in that season
nufc_matches_season <- nufc_df_season %>%
  filter(!is.na(unique_game) & during_match == TRUE) %>%
  distinct(unique_game, date, current_result) %>%
  mutate(
    result_label = factor(current_result, levels = c(0, 1, 3), labels = c("Loss", "Draw", "Win")),
    linetype = case_when( # For B&W plot
      result_label == "Win" ~ "solid",
      result_label == "Draw" ~ "dashed",
      result_label == "Loss" ~ "dotted",
      TRUE ~ "blank"
    )
  ) %>% arrange(date) %>% filter(!is.na(result_label))


# Key events for NUFC 2021-2022 season
saudi_takeover_date <- as.Date("2021-10-07")
# Note: "CL impossible_date" was in original script but seems more relevant to Arsenal. Removing for NUFC.

plot_nufc_season <- ggplot(nufc_df_season, aes(x = date, y = llm_score)) +
  geom_smooth(method = "gam", formula = y ~ s(as.numeric(x), k = 30), # GAM with date converted to numeric
              color = "black", se = TRUE, fill = "gray80", linewidth = 0.8) +
  geom_vline(data = nufc_matches_season, aes(xintercept = date, linetype = result_label),
             color = "gray30", linewidth = 0.7) + # Vertical lines for matches
  scale_linetype_manual(name = "Match Result",
                        values = c("Win" = "solid", "Draw" = "dashed", "Loss" = "dotted")) +
  geom_vline(xintercept = saudi_takeover_date, color = "black", linetype = "longdash", linewidth = 0.9) +
  annotate("text", x = saudi_takeover_date, y = min(nufc_df_season$llm_score, na.rm=T) + 0.05, # Adjust y for visibility
           label = "PIF Takeover", angle = 90, vjust = -0.5, hjust = 0, size = 3, family = "Times") +
  theme_custom_minimal +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "r/NUFC Sentiment During 2021/22 Season",
    subtitle = "Smoothed Sentiment Trend (Neutral Posts Excluded)",
    x = "Date",
    y = "Sentiment Score"
  )

print(plot_nufc_season)
ggsave("../acl_paper/nufc_season_sentiment.svg", plot_nufc_season, width = 8, height = 4)


# -----------------------------------------------------------------------------
# 7. Figure 5: Post Count and Sentiment Relative to Matches (Weeks)
# -----------------------------------------------------------------------------
# Prepare data with 'relative_time'
# time_until_game is positive, time_since_game is positive.
# We want negative for time *since* (past game) and positive for time *until* (future game)
# This was defined differently in the original script, let's match paper's x-axis:
# "-1 week" means 1 week *after* a game (past). "+1 week" means 1 week *before* a game (future).
# So, 'time_since_game' should be negative, 'time_until_game' should be positive.
# If a post is between two games, it has a time_since_last and time_until_next.
# 'relative_time' should be the time to the NEAREST game, signed.
# The paper's Figure 5 x-axis: "Time Relative to Game (weeks)". 0 is match time.
# Negative x-values are WEEKS BEFORE match. Positive x-values are WEEKS AFTER match.
# This means: if post is before next_game: relative_time = -time_until_game
#             if post is after last_game: relative_time = time_since_game
#             If during_match, relative_time = 0.

df_relative_time <- merged_df %>%
  mutate(
    relative_time_seconds = case_when(
      during_match == TRUE ~ 0,
      # If time_until_game exists and is smaller than time_since_game (or time_since_game is NA), it's before the nearest game
      (!is.na(time_until_game) & (time_until_game < time_since_game | is.na(time_since_game))) ~ -time_until_game,
      # Otherwise, it's after the nearest game (or time_until_game is NA)
      !is.na(time_since_game) ~ time_since_game,
      TRUE ~ NA_real_ # Should not happen if logic is complete
    )
  ) %>%
  filter(!is.na(relative_time_seconds))


# Define bin size (1 day in seconds, as per original for aggregation then converted to weeks for plot)
bin_size_seconds <- 24 * 60 * 60 # 1 day
# Max time for x-axis: 8 weeks before/after
max_weeks <- 8
max_time_seconds <- max_weeks * 7 * bin_size_seconds

# Aggregate data by daily bins
aggregated_relative_time_daily <- df_relative_time %>%
  mutate(time_bin_daily = floor(relative_time_seconds / bin_size_seconds) * bin_size_seconds + bin_size_seconds / 2) %>%
  filter(time_bin_daily >= -max_time_seconds & time_bin_daily <= max_time_seconds) %>%
  group_by(time_bin_daily) %>%
  summarise(
    mean_sentiment = mean(llm_score, na.rm = TRUE),
    post_count = n(),
    .groups = 'drop'
  ) %>%
  mutate(log_post_count = log(post_count)) %>%
  filter(post_count > 10) # Filter out bins with very few posts for stable smoothing


# Fit LOESS model for sentiment (span found by experimentation for U-shape)
loess_sentiment_relative <- loess(mean_sentiment ~ time_bin_daily,
                                  data = aggregated_relative_time_daily,
                                  weights = post_count, span = 0.2) # Span might need adjustment

# Create smooth data for plotting the LOESS line
smooth_data_relative <- data.frame(
  time_bin_daily = seq(min(aggregated_relative_time_daily$time_bin_daily),
                       max(aggregated_relative_time_daily$time_bin_daily),
                       length.out = 500)
)
smooth_data_relative$predicted_sentiment <- predict(loess_sentiment_relative, newdata = smooth_data_relative)


# For dual axis plot, scale sentiment to fit the log_post_count range
min_pred_sent <- min(smooth_data_relative$predicted_sentiment, na.rm = TRUE)
max_pred_sent <- max(smooth_data_relative$predicted_sentiment, na.rm = TRUE)
min_log_count <- min(aggregated_relative_time_daily$log_post_count, na.rm = TRUE)
max_log_count <- max(aggregated_relative_time_daily$log_post_count, na.rm = TRUE)

# Scale predicted sentiment to the range of log_post_count for plotting
smooth_data_relative$scaled_predicted_sentiment <- scales::rescale(
  smooth_data_relative$predicted_sentiment,
  to = c(min_log_count, max_log_count), # Target range
  from = c(min_pred_sent, max_pred_sent)  # Source range
)
# Second, thicker smoothed line (as in paper's Fig 5)
loess_sentiment_thicker <- loess(mean_sentiment ~ time_bin_daily,
                                 data = aggregated_relative_time_daily,
                                 weights = post_count, span = 0.65) # Larger span for thicker general trend
smooth_data_relative$predicted_sentiment_thicker <- predict(loess_sentiment_thicker, newdata = smooth_data_relative)
smooth_data_relative$scaled_predicted_sentiment_thicker <- scales::rescale(
  smooth_data_relative$predicted_sentiment_thicker,
  to = c(min_log_count, max_log_count),
  from = c(min_pred_sent, max_pred_sent) # Use same original sentiment range for consistency
)


# X-axis labels in weeks
week_in_seconds <- 7 * 24 * 60 * 60
x_axis_breaks <- seq(-max_weeks * week_in_seconds, max_weeks * week_in_seconds, by = week_in_seconds)
x_axis_labels <- round(x_axis_breaks / week_in_seconds)


figure5_count_sentiment_relative <- ggplot(aggregated_relative_time_daily, aes(x = time_bin_daily)) +
  geom_rect(aes(xmin = time_bin_daily - bin_size_seconds / 2,
                xmax = time_bin_daily + bin_size_seconds / 2,
                ymin = 0, ymax = log_post_count, fill = log_post_count)) +
  scale_fill_gradient(low = "gray80", high = "black", name = "Post Count (log)") + # Grayscale fill
  geom_line(data = smooth_data_relative, aes(y = scaled_predicted_sentiment), color = "black", linewidth = 0.7, alpha = 0.6) +
  geom_line(data = smooth_data_relative, aes(y = scaled_predicted_sentiment_thicker), color = "black", linewidth = 1.5) + # Thicker line
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", linewidth = 0.8) +
  scale_x_continuous(
    name = "Time Relative to Game (weeks)",
    breaks = x_axis_breaks,
    labels = x_axis_labels
  ) +
  scale_y_continuous(
    name = "Post Count (log)",
    sec.axis = sec_axis(
      ~ scales::rescale(., to = c(min_pred_sent, max_pred_sent), from = c(min_log_count, max_log_count)),
      name = "Mean Sentiment",
      labels = function(x) sprintf("%.2f", x) # Format to 2 decimal places
    )
  ) +
  coord_cartesian(ylim = c(0, max_log_count * 1.05)) + # Ensure primary y-axis starts at 0
  theme_custom_minimal +
  theme(legend.position = "none") + # No legend as per Figure 5
  labs(title = "Post Count and Sentiment Relative to Matches")

print(figure5_count_sentiment_relative)
ggsave("../acl_paper/figure5_post_count_sentiment_relative_weeks.svg", figure5_count_sentiment_relative, width = 5, height = 4) # Adjusted size


# End of script