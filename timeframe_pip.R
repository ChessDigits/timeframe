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
df <- remove_negative_time_taken(df, replace_value = NA)
df <- add_eval_change_at_each_ply(df)
df <- get_one_row_per_ply_with_time_taken_and_eval_change(df, vars_to_keep=c("TimeControl"))


ggplot(df %>% filter(TimeControl == "180+0") %>% slice_sample(n=10000), aes(x=Time_taken, y=Eval_change)) +
  geom_jitter()


df %>%
  filter(TimeControl == "600+0") %>% 
  slice_sample(n=10000) %>% 
  mutate(blunder = abs(Eval_change) >= 3) %>%
  ggplot(aes(x=blunder, y=Time_taken)) + geom_boxplot()

#lmer() to get sig. test

ana <- lme4::glmer(blunder ~ Time_taken + (1|Site), family="binomial", 
                   data=df %>% 
                     filter(TimeControl == "600+0") %>% 
                     mutate(blunder = abs(Eval_change) >= 3) %>% 
                     mutate(Time_taken = as.numeric(scale(Time_taken))) # could scale within games! (i.e. Site); group_by & ungroup
)
summary(ana)


ana2 <- lme4::glmer(blunder ~ Time_taken + (1|Site), family="binomial", 
                   data=df %>% 
                     filter(TimeControl == "600+0") %>% 
                     mutate(blunder = abs(Eval_change) >= 3) %>% 
                     group_by(Site) %>% 
                     mutate(Time_taken = as.numeric(scale(Time_taken))) %>% 
                     ungroup()
)
summary(ana2)

# icc might be interesting here


