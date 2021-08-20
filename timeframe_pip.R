"
2021.08.19
Chess Digits
Timeframe

Pipelines for article:
[URL]

"

#### long think bad think ####

df <- load_data(k_games = 200, use_local_file = TRUE)
df <- slice_sample(df, n = 10000)
bu <- df # backup
df <- bu
df <- add_time_taken(df)
df <- add_eval_change_at_each_ply(df)
df <- get_one_row_per_ply_with_time_taken_and_eval_change(df)
