"
2021.08.19
Chess Digits
Timeframe

Pipelines for article:
[URL]

"

#### long think bad think ####

rm(list=ls())
source("timeframe_fn.R")
df <- load_data(k_games = 200, use_local_file = TRUE, dir="")
df <- slice_sample(df, n = 10000)
bu <- df # backup
df <- bu
df <- add_time_taken(df)
df <- remove_negative_time_taken(df, replace_value = NA)
df <- add_eval_change_at_each_ply(df)
# pivot
df <- get_one_row_per_ply_with_time_taken_and_eval_change(df, vars_to_keep=c("TimeControl"))

# plots
get_plot_eval_change_by_time_taken(df)
get_plot_blunder_by_time_taken(df, min_eval_change_for_blunder=3)

# statistical tests
ana <- get_mlm_blunder_by_time_taken(df, min_eval_change_for_blunder=3, scale_time_within_each_game=TRUE)
summary(ana)

# icc might be interesting here




#### time series ####

rm(list=ls())
source("timeframe_fn.R")
df <- load_data(k_games = 200, use_local_file = TRUE, dir="")
df <- slice_sample(df, n = 10000)
bu <- df # backup
df <- bu
df <- add_time_taken(df)
df <- remove_negative_time_taken(df, replace_value = NA)

# ts
tt <- get_list_time_series_for_time_taken_for_each_game(df, n_games = 100) # time taken
lapply(tt, cor)


