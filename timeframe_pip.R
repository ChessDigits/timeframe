"
2021.08.19
Chess Digits
Timeframe

Pipelines for article:
[URL]

"

#### long think bad think ####

df <- load_data(k_games = 200, use_local_file = TRUE)
bu <- df # backup
df <- slice_sample(df, n = 10000)
df <- add_time_taken(df)


df <- add_eval_change_at_each_ply(df)

# check
view(df[,c(
  #"TimeControl", 
  #"increment", 
  paste0("Eval_ply_", 1:10), 
  paste0("Eval_change_ply_", 2:10)#rep(c("white", "black"), 4), rep(2:5, each=2))
)])
