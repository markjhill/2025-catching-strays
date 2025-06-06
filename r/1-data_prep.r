# data prep 

library(ggplot2)
library(dplyr)
library(lubridate)
library(ordinal)
library(effects)
library(GGally)
library(gmodels)
library(reshape2)
library(correlation)
library(pbapply)
library(data.table)

setwd("../OneDrive - King's College London/coding/football_contagion/r/code_for_methods_paper/")
setwd("OneDrive - King's College London/coding/football_contagion/r/code_for_methods_paper/")

########
# Load and prep results file

# Note raw data is not included in this repository for legal reasons. The structure of the data is described below.

results <- read.csv("../Datasets/total_results_v2_with_cups_uefa.csv") 

# > str(results)
# 'data.frame':	47426 obs. of  17 variables:
#  $ Date              : chr  "09/08/2008" "09/08/2008" "09/08/2008" "09/08/2008" ...
#  $ Time              : chr  "11:45" "14:00" "14:00" "14:00" ...
#  $ Home.Team         : chr  "BIRMINGHAM" "BLACKPOOL" "CARDIFF" "CHARLTON" ...
#  $ Away.Team         : chr  "SHEFFIELD UTD" "BRISTOL CITY" "SOUTHAMPTON" "SWANSEA" ...
#  $ HT.Home.Score     : int  0 0 1 1 0 0 0 1 3 1 ...
#  $ HT.Away.Score     : int  0 0 1 0 0 0 0 2 1 1 ...
#  $ FT.Home.Score     : int  1 0 2 2 2 0 0 1 4 2 ...
#  $ FT.Away.Score     : int  0 1 1 0 0 0 1 2 1 2 ...
#  $ PT.Home.Score     : int  NA NA NA NA NA NA NA NA NA NA ...
#  $ PT.Away.Score     : int  NA NA NA NA NA NA NA NA NA NA ...
#  $ season            : chr  "2008-2009" "2008-2009" "2008-2009" "2008-2009" ...
#  $ competition       : chr  "championship" "championship" "championship" "championship" ...
#  $ competition_detail: chr  "england_championship" "england_championship" "england_championship" "england_championship" ...
#  $ clean_dates_utc   : chr  "2008-08-09 11:45:00" "2008-08-09 14:00:00" "2008-08-09 14:00:00" "2008-08-09 14:00:00" ...
#  $ epoch_utc         : num  1.22e+09 1.22e+09 1.22e+09 1.22e+09 1.22e+09 ...
#  $ result            : chr  "HOME" "AWAY" "HOME" "HOME" ...
#  $ ht_result         : chr  "DRAW" "DRAW" "DRAW" "HOME" ...

##################################

names(results)[which(names(results) == "Home.Team")] <- "HomeTeam"
names(results)[which(names(results) == "Away.Team")] <- "AwayTeam"

# Subset data by relevant teams:

team_list_results <- c('ARSENAL', 'ASTON VILLA', 'CHELSEA', 'EVERTON', 'LEEDS', 'LEICESTER',
                       'LIVERPOOL', 'MANCHESTER CITY', 'MANCHESTER UTD', 'NEWCASTLE', 'SOUTHAMPTON', 
                       'TOTTENHAM', 'WEST HAM',
                       'WOLVERHAMPTON', 'WEST BROM', 'NOTTINGHAM FOREST', 'NORWICH', 
                       'CARDIFF', 'CRYSTAL PALACE', 'WATFORD','BOURNEMOUTH','SHEFFIELD UTD',
                       'BLACKBURN','SWANSEA','SUNDERLAND','LUTON','QPR','FULHAM',
                       'HULL CITY','BRIGHTON','BOLTON','BIRMINGHAM','BRENTFORD','STOKE CITY',
                       'BURNLEY','READING','MIDDLESBROUGH','HUDDERSFIELD','WIGAN'
)

# Add team column
results$Team <- NA
team_specific_results <- results
team_specific_results <- team_specific_results[-c(1:nrow(team_specific_results)),]

for(i_team in 1:length(team_list_results)) {
  temp_locs <- c(which(results$HomeTeam == team_list_results[i_team]),
                 which(results$AwayTeam == team_list_results[i_team])
  )
  results$Team[temp_locs] <- team_list_results[i_team]
  team_specific_results <- rbind(team_specific_results, results[temp_locs,])
}

results <- team_specific_results

# Add the opposition team for each game (rather than home and away)
# This is useful because data is split by team, and their home/away status isn't that important for this analysis

# for matching subreddits to teams later
results <- results %>%
  mutate(Subreddit = recode(Team, 'ARSENAL' = 'Gunners',
                            'ASTON VILLA' = 'avfc',
                            'CHELSEA' = 'chelseafc',
                            'EVERTON' = 'Everton',
                            'LEEDS' = 'LeedsUnited',
                            'LEICESTER' = 'lcfc',
                            'LIVERPOOL' = 'LiverpoolFC',
                            'MANCHESTER CITY' = 'MCFC',
                            'MANCHESTER UTD' = 'reddevils',
                            'NEWCASTLE' = 'NUFC',
                            'SOUTHAMPTON' = 'SaintsFC',
                            'TOTTENHAM' = 'coys',
                            'WEST HAM' = 'Hammers',
                            'MANCHESTER UTD' = "ManchesterUnited",
                            'WOLVERHAMPTON' = "WWFC",
                            'ARSENAL' = "ArsenalFC",
                            'WEST BROM' = "WBAfootball",
                            'NOTTINGHAM FOREST' = "nffc",
                            'NORWICH' = "NorwichCity", 
                            'CARDIFF' = "bluebirds", 
                            'CRYSTAL PALACE' = "crystalpalace", 
                            'WATFORD' = "Watford_FC",
                            'BOURNEMOUTH' = "AFCBournemouth",
                            'SHEFFIELD UTD' = "SheffieldUnited",
                            'BLACKBURN' = "brfc",
                            'SWANSEA' = "swanseacity",
                            'SUNDERLAND' = "safc",
                            'LUTON' = "COYH",
                            'QPR' = "superhoops",
                            'FULHAM' = "fulhamfc",          
                            'HULL CITY' = "HullCity",
                            'BRIGHTON' = "BrightonHoveAlbion",
                            'BOLTON' = "bwfc",
                            'BIRMINGHAM' = "bcfc",
                            'BRENTFORD' = "Brentford",
                            'STOKE CITY' = "StokeCityFC",
                            'BURNLEY' = "Burnley",
                            'READING' = "Urz",
                            'MIDDLESBROUGH' = "Boro",
                            'HUDDERSFIELD' = "HuddersfieldTownFC",
                            'WIGAN' = "latics"
  )
)

# Add opposition column
results$opposition <- NA

for(i_result in 1:nrow(results)) {
  cat("\r", i_result)
  temp_loc <- which(c(results$HomeTeam[i_result], results$AwayTeam[i_result]) != toupper(results$Team[i_result]))
  results$opposition[i_result] <- c(results$HomeTeam[i_result], results$AwayTeam[i_result])[temp_loc]
}

# Create start and end times for the windows around the match times
results <- results %>%
  mutate(
    #start_time = clean_dates - minutes(60),
    start_epoch = epoch_utc,
    end_epoch = epoch_utc + (120 * 60) # 90 + 15 + 20 for half time break and added time (10 minutes per half))
  )


#######
# Convert result from home/away/draw to a point for the corresponding team 

results$match_result <- results$result

results <- results[,-c(which(names(results) == "result"))]

results$Result <- NA

results$Result[which(results$match_result == "DRAW")] <- 1

home_win_locs <- intersect(
  which(results$HomeTeam == results$Team),
  which(results$match_result == "HOME")
)

results$Result[home_win_locs] <- 3

home_loss_locs <- intersect(
  which(results$HomeTeam == results$Team),
  which(results$match_result == "AWAY")
)

results$Result[home_loss_locs] <- 0

away_win_locs <- intersect(
  which(results$AwayTeam == results$Team),
  which(results$match_result == "AWAY")
)

results$Result[away_win_locs] <- 3


away_loss_locs <- intersect(
  which(results$AwayTeam == results$Team),
  which(results$match_result == "HOME")
)

results$Result[away_loss_locs] <- 0

#######
# HALF TIME SCORE
# Convert result from home/away/draw to a point for the corresponding team 

results$ht_result <- NA

temp_draws <- which(results$HT.Home.Score == results$HT.Away.Score)

results$ht_result[temp_draws] <- "DRAW"

temp_home <- which(results$HT.Home.Score > results$HT.Away.Score)

results$ht_result[temp_home] <- "HOME"

temp_away <- which(results$HT.Home.Score < results$HT.Away.Score)

results$ht_result[temp_away] <- "AWAY"

results$ht_points <- NA

results$ht_points[which(results$ht_result == "DRAW")] <- 1

home_win_locs <- intersect(
  which(results$HomeTeam == results$Team),
  which(results$ht_result == "HOME")
)

results$ht_points[home_win_locs] <- 3

home_loss_locs <- intersect(
  which(results$HomeTeam == results$Team),
  which(results$ht_result == "AWAY")
)

results$ht_points[home_loss_locs] <- 0

away_win_locs <- intersect(
  which(results$AwayTeam == results$Team),
  which(results$ht_result == "AWAY")
)

results$ht_points[away_win_locs] <- 3


away_loss_locs <- intersect(
  which(results$AwayTeam == results$Team),
  which(results$ht_result == "HOME")
)

results$ht_points[away_loss_locs] <- 0


#############
# Prepare subreddit data

club_subs <- list.files("../Datasets/all_sub_data/by_subreddit", full = TRUE)
club_subs <- club_subs[-c(grep("with_results", club_subs))]

# > club_subs
#  [1] "../Datasets/all_sub_data/by_subreddit/sub_AFCBournemouth_coms_and_subs.RDS"    
#  [2] "../Datasets/all_sub_data/by_subreddit/sub_ArsenalFC_coms_and_subs.RDS"         
#  [3] "../Datasets/all_sub_data/by_subreddit/sub_avfc_coms_and_subs.RDS"              
#  [4] "../Datasets/all_sub_data/by_subreddit/sub_bcfc_coms_and_subs.RDS"              
#  [5] "../Datasets/all_sub_data/by_subreddit/sub_bluebirds_coms_and_subs.RDS"         
#  [6] "../Datasets/all_sub_data/by_subreddit/sub_Boro_coms_and_subs.RDS"              
#  [7] "../Datasets/all_sub_data/by_subreddit/sub_Brentford_coms_and_subs.RDS"         
#  [8] "../Datasets/all_sub_data/by_subreddit/sub_brfc_coms_and_subs.RDS"              
#  [9] "../Datasets/all_sub_data/by_subreddit/sub_BrightonHoveAlbion_coms_and_subs.RDS"
# [10] "../Datasets/all_sub_data/by_subreddit/sub_Burnley_coms_and_subs.RDS"           
# [11] "../Datasets/all_sub_data/by_subreddit/sub_bwfc_coms_and_subs.RDS"              
# [12] "../Datasets/all_sub_data/by_subreddit/sub_chelseafc_coms_and_subs.RDS"         
# [13] "../Datasets/all_sub_data/by_subreddit/sub_COYH_coms_and_subs.RDS"              
# [14] "../Datasets/all_sub_data/by_subreddit/sub_coys_coms_and_subs.RDS"              
# [15] "../Datasets/all_sub_data/by_subreddit/sub_crystalpalace_coms_and_subs.RDS"     
# [16] "../Datasets/all_sub_data/by_subreddit/sub_Everton_coms_and_subs.RDS"           
# [17] "../Datasets/all_sub_data/by_subreddit/sub_fulhamfc_coms_and_subs.RDS"          
# [18] "../Datasets/all_sub_data/by_subreddit/sub_Gunners_coms_and_subs.RDS"           
# [19] "../Datasets/all_sub_data/by_subreddit/sub_Hammers_coms_and_subs.RDS"           
# [20] "../Datasets/all_sub_data/by_subreddit/sub_HuddersfieldTownFC_coms_and_subs.RDS"
# [21] "../Datasets/all_sub_data/by_subreddit/sub_HullCity_coms_and_subs.RDS"          
# [22] "../Datasets/all_sub_data/by_subreddit/sub_latics_coms_and_subs.RDS"            
# [23] "../Datasets/all_sub_data/by_subreddit/sub_lcfc_coms_and_subs.RDS"              
# [24] "../Datasets/all_sub_data/by_subreddit/sub_LeedsUnited_coms_and_subs.RDS"       
# [25] "../Datasets/all_sub_data/by_subreddit/sub_LiverpoolFC_coms_and_subs.RDS"       
# [26] "../Datasets/all_sub_data/by_subreddit/sub_ManchesterUnited_coms_and_subs.RDS"  
# [27] "../Datasets/all_sub_data/by_subreddit/sub_MCFC_coms_and_subs.RDS"              
# [28] "../Datasets/all_sub_data/by_subreddit/sub_nffc_coms_and_subs.RDS"              
# [29] "../Datasets/all_sub_data/by_subreddit/sub_NorwichCity_coms_and_subs.RDS"       
# [30] "../Datasets/all_sub_data/by_subreddit/sub_NUFC_coms_and_subs.RDS"              
# [31] "../Datasets/all_sub_data/by_subreddit/sub_reddevils_coms_and_subs.RDS"         
# [32] "../Datasets/all_sub_data/by_subreddit/sub_safc_coms_and_subs.RDS"              
# [33] "../Datasets/all_sub_data/by_subreddit/sub_SaintsFC_coms_and_subs.RDS"          
# [34] "../Datasets/all_sub_data/by_subreddit/sub_SheffieldUnited_coms_and_subs.RDS"   
# [35] "../Datasets/all_sub_data/by_subreddit/sub_StokeCityFC_coms_and_subs.RDS"       
# [36] "../Datasets/all_sub_data/by_subreddit/sub_superhoops_coms_and_subs.RDS"        
# [37] "../Datasets/all_sub_data/by_subreddit/sub_swanseacity_coms_and_subs.RDS"       
# [38] "../Datasets/all_sub_data/by_subreddit/sub_Urz_coms_and_subs.RDS"               
# [39] "../Datasets/all_sub_data/by_subreddit/sub_Watford_FC_coms_and_subs.RDS"        
# [40] "../Datasets/all_sub_data/by_subreddit/sub_WBAfootball_coms_and_subs.RDS"       
# [41] "../Datasets/all_sub_data/by_subreddit/sub_WWFC_coms_and_subs.RDS" 

# Data per sub looks like:

# > str(temp_posts)
# 'data.frame':	12665 obs. of  14 variables:
#  $ id         : chr  "cf21e90" "ci08xtk" "ci0atam" "ci0lth1" ...
#  $ author     : chr  "ClownBaby86" "JamBug" "NineFeetUnderground" "Made_Of_Outer_Space" ...
#  $ body       : chr  "Kermorgant bid accepted too..." "Apparently for around £3m." "£1m less than David Villa &amp; Rickie Lambert. I think we'll take that!" "Sad to see him go, but hopefully we'll invest the money wisely." ...
#  $ subreddit  : chr  "AFCBournemouth" "AFCBournemouth" "AFCBournemouth" "AFCBournemouth" ...
#  $ link_id    : chr  "t3_1wevfc" "t3_27fbkn" "t3_27fbkn" "t3_27fbkn" ...
#  $ parent_id  : chr  "t3_1wevfc" "t3_27fbkn" "t1_ci08xtk" "t3_27fbkn" ...
#  $ permalink  : chr  NA NA NA NA ...
#  $ season     : chr  NA NA NA NA ...
#  $ created_utc: num  1.39e+09 1.40e+09 1.40e+09 1.40e+09 1.41e+09 ...
#  $ negative   : num  0.00993 0.05291 0.03519 0.45742 0.00341 ...
#  $ neutral    : num  0.7562 0.8788 0.5245 0.4062 0.0164 ...
#  $ positive   : num  0.2339 0.0683 0.4404 0.1364 0.9802 ...
#  $ llm_sent   : chr  "neutral" "neutral" "neutral" "negative" ...
#  $ llm_score  : num  0.2239 0.0154 0.4052 -0.321 0.9768 ...

options(warn=2)

#########
# iterative process working through subs, prepping data, and saving the results
#  
for (i_sub_file in 1:length(club_subs)) {
  temp_posts <- readRDS(club_subs[i_sub_file])
  
  new_file_name <- sub(".RDS", "_with_results.RDS", club_subs[i_sub_file])
  
  #if (file.exists(new_file_name)) { next }
  
  temp_sub <- unique(temp_posts$subreddit)
  cat("\n", i_sub_file, "-", temp_sub, "- nrow:", nrow(temp_posts), "\n")
  
  temp_posts <- temp_posts %>%
    mutate(
      competition = NA_character_,
      competition_detail = NA_character_,
      time_since_game = NA_real_,
      time_until_game = NA_real_,
      last_game_result = NA_character_,
      next_game_result = NA_character_,
      last_team_played = NA_character_,
      next_team_played = NA_character_,
      current_opposition = NA_character_,
      current_ht_result = NA_character_,
      current_result = NA_character_,
      current_start_time = NA_real_,
      during_match = NA,
      next_game_comp = NA_character_,
      unique_game = NA_character_,
      league = NA_character_
    )
  
  # Handle the two clubs in this data with two subreddits
  sub_results <- if (temp_sub == "ArsenalFC") {
    results %>% filter(Subreddit == "Gunners")
  } else if (temp_sub == "ManchesterUnited") {
    results %>% filter(Subreddit == "reddevils")
  } else {
    results %>% filter(Subreddit == temp_sub)
  }
  
  # Convert to data.table for faster processing
  setDT(temp_posts)
  setDT(sub_results)
  
  # Sort by created_utc and start_epoch
  setorder(temp_posts, created_utc)
  setorder(sub_results, start_epoch)
  
  # Use findInterval for binary search
  temp_posts[, `:=`(
    last_game_idx = findInterval(created_utc, sub_results$end_epoch)
    #next_game_idx = findInterval(created_utc, sub_results$start_epoch, left.open = TRUE)
  )]
  
  temp_posts[, `:=`(
    last_game_idx = pmax(last_game_idx, 1L)
    #next_game_idx = pmin(next_game_idx, .N)
  )]
  
  temp_posts[last_game_idx > 0, `:=`(
    last_game_result = sub_results$Result[last_game_idx],
    last_team_played = sub_results$opposition[last_game_idx],
    unique_game = paste0(sub_results$opposition[last_game_idx], "_", sub_results$start_epoch[last_game_idx]),
    time_since_game = created_utc - sub_results$end_epoch[last_game_idx]
  )]
  
  # Next game: Only games starting strictly after the post
  temp_posts[, next_game_idx := sapply(created_utc, function(post_time) {
    match_idx <- which(sub_results$start_epoch > post_time)  # strictly greater than post_time
    if (length(match_idx) > 0) {
      return(match_idx[1])  # Take the first future game
    } else {
      return(NA_integer_)
    }
  })]
  
  # During match: Games where the post is happening during the game
  temp_posts[, during_match_idx := sapply(created_utc, function(post_time) {
    match_idx <- which(sub_results$start_epoch <= post_time & sub_results$end_epoch >= post_time)  # Game happening at post time
    if (length(match_idx) > 0) {
      return(match_idx[1])  # Take the first current match (if any)
    } else {
      return(NA_integer_)
    }
  })]
  
  # Handle the next-game cases
  temp_posts[!is.na(next_game_idx), `:=`(
    next_game_result = sub_results$Result[next_game_idx],
    next_team_played = sub_results$opposition[next_game_idx],
    time_until_game = sub_results$start_epoch[next_game_idx] - created_utc,
    next_game_comp = sub_results$competition[next_game_idx]
  )]
  
  # Handle the during-match cases
  temp_posts[!is.na(during_match_idx), `:=`(
    during_match = TRUE,
    current_opposition = sub_results$opposition[during_match_idx],
    current_ht_result = sub_results$ht_points[during_match_idx],
    current_result = sub_results$Result[during_match_idx],
    current_start_time = sub_results$start_epoch[during_match_idx],
    competition = sub_results$competition[during_match_idx],
    competition_detail = sub_results$competition_detail[during_match_idx],
    unique_game = paste0(sub_results$opposition[during_match_idx], "_", sub_results$start_epoch[during_match_idx]),
    time_since_game = 0,
    time_until_game = 0
  )]
  
  # Add season
  temp_posts[, season := {
    post_date <- as.POSIXct(created_utc, origin = "1970-01-01", tz = "UTC")
    year <- as.integer(format(post_date, "%Y"))
    month <- as.integer(format(post_date, "%m"))
    
    ifelse(month <= 7, 
           paste0(year - 1, "-", year), 
           paste0(year, "-", year + 1))
  }]
  
  seasons <- unique(temp_posts$season)
  for(i_season in 1:length(seasons)) {
    temp_locs <- which(temp_posts$season == seasons[i_season])
    temp_posts$league[temp_locs] <- names(sort(table(temp_posts$next_game_comp[temp_locs]), decreasing = TRUE)[1])
  }

  # remove index
  to_remove <- c("last_game_idx", "next_game_idx", "during_match_idx", "next_game_comp")
  temp_posts <- temp_posts %>%
    select(-one_of(to_remove))

  
  saveRDS(temp_posts, new_file_name)
}
options("warn"=0)

################
## Create one large DF of everything, but less data

club_subs <- list.files("../Datasets/all_sub_data/by_subreddit", full = TRUE)
club_subs <- club_subs[c(grep("with_results", club_subs))]

to_remove <- c("author", "body", "link_id", "parent_id", 
               "permalink", "negative", "neutral", "positive",
               "competition_detail",  "last_team_played", "next_team_played", 
               "current_opposition")

for(i_sub_file in 1:length(club_subs)) {
  cat("\r", i_sub_file)
  temp_posts <- readRDS(club_subs[i_sub_file])
  temp_posts <- as.data.table(temp_posts)
  
  temp_posts <- temp_posts %>%
    select(-one_of(to_remove))
  
  if(i_sub_file == 1) {
    merged_df <- temp_posts
  } else {
    merged_df <- rbindlist(list(merged_df, temp_posts))
  }
  rm(temp_posts)
  gc()
  if(i_sub_file == length(club_subs)) {
    saveRDS(merged_df, file = "../Datasets/all_sub_data/all_data_pruned_2024_09_05.RDS")
  }
}
