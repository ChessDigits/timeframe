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
#df <- filter(df, abs(WhiteElo - BlackElo) <= 200)
df <- slice_sample(df, n = 10000)
bu <- df # backup
df <- bu
df <- add_time_taken(df)
df <- remove_negative_time_taken(df, replace_value = NA)

# ts
tt <- get_list_time_series_for_time_taken_for_each_game(df, n_games = nrow(df)) # time taken


# histogram
r <- sapply(tt, \(m) cor(m[,1], m[,2]))
hist(r, breaks=50)
(t <- table(r>0))/sum(t) # about 80% > 0
summary(r)

# could look at min, max, sd, etc. (instead of just r)
# compare the two ts on ~
# could restrict to a certain time control and correl across games (e.g. each game gets white and black max/sd/etc., and correl)

# scatterplot time taken
tt_all_games <- as.data.frame(do.call(rbind, tt)) # one df 2 columns
ggplot(tt_all_games %>% slice_sample(n=10000), aes(x=dw, y=db)) + 
  geom_jitter() + 
  xlim(c(0,50)) + ylim(c(0,50)) + 
  geom_smooth()


# tt and eval (e.g. changes in eval)

# moving average


# acf: lag 1 (one player after the other), lag 2 (among players themselves)
a <- get_df_acf_lags(tt)
ggplot(a, aes(x=Lag1)) + geom_histogram() + geom_vline(xintercept = 0, linetype="dashed", size=1.5) + xlim(c(-1,1))
ggplot(a, aes(x=Lag2)) + geom_histogram() + geom_vline(xintercept = 0, linetype="dashed", size=1.5) + xlim(c(-1,1))
(t <- table(a$Lag1>0))/sum(t) # 76%
(t <- table(a$Lag2>0))/sum(t) # 77%




