"
2021.08.19
Chess Digits
Timeframe

Functions for article:
[URL]

"

#### imports ####
library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)


#### helper fn ####
view <- utils::View


#### variables ####
WHITE_MATE_EVAL <- 200
BLACK_MATE_EVAL <- WHITE_MATE_EVAL*-1



#### load data ####
load_data <- function(k_games=c(200,500), use_local_file=TRUE)
{
  if (!use_local_file)
  {
    cat("Loading data directly from ChessDigits.com is very time consuming.\nWe recommend saving the data locally and setting use_local_file to TRUE.\n")
    dir <- "https://chessdigits.com/data/"
  } else dir <- "d:/Chess/databases/lichess_May2019/out/"
  fpath <- paste0(dir, k_games, "k_blitz_rapid_classical_bullet.csv")
  df <- read.csv(fpath, stringsAsFactors = T)
  return(df)
}


#### clocks ####

# helper fn: make the Clock_ply_ vars into seconds
make_clocks_in_seconds <- function(df)
{
  "
  input: df
  output: df with Clock_ply_ vars reformatted into seconds
  "
  
  # select vars
  v <- (c <- colnames(df))[substr(c, 1, 10)=="Clock_ply_"]
  
  # as char
  df[,v] <- sapply(df[,v], as.character)
  
  # as seconds
  times <- sapply(df[,v], strsplit, split=":", fixed = T)
  times <- apply(times, 2, lapply, as.numeric)
  times <- lapply(times, sapply, \(t) t[1]*60*60 + t[2]*60 + t[3]) # list of length length(v)
  times <- as.data.frame(Map(cbind, times)) # as matrix, then as df
  
  # replace columns
  df[,v] <- times
  
  # out
  print("Clocks now in seconds")
  return(df)
}


# helper fn: add increment
add_increment <- function(df)
{
  "
  input: df
  output: df with new variable 'increment'
  "
  # placeholder
  df$increment <- df$TimeControl
  
  # get inc
  split_levels <- strsplit(levels(df$increment), split = "+", fixed = TRUE)
  levels(df$increment) <- sapply(split_levels, \(l) l[2])
  
  # make numeric
  df$increment <- as.numeric(as.character(df$increment))
  
  # out
  print("Added column 'increment'")
  return(df)
}



# add time_taken variables
add_time_taken <- function(df)
{
  "
  input: df
  output: df with time_taken_ vars added; clocks in seconds; var increment added
  "
  
  # make clocks in seconds
  df <- make_clocks_in_seconds(df) # problem here: this function changes the clocks
  
  # add increment to df
  df <- add_increment(df) # problem here: this function adds a column
  
  # remove inc from clocks where it is added
  v <- (c <- colnames(df))[substr(c, 1, 10)=="Clock_ply_"]
  v_inc <- v[-c(1:2)] # plies 1-2 not affected, time doesn't go down or up; the clocks indicate time remaining AFTER each move
  #df[,v_inc] <- sapply(df[,v_inc], \(t) t-df$increment)
  
  # take difference between pairs of plies
  # could subtract inc here
  pairs <- sapply(1:(length(v)-2), \(i) c(v[i], v[i+2]))
  time_taken <- apply(pairs, 2, \(p) df[,p[1]] - df[,p[2]]) # could add inc on this
  time_taken <- time_taken + df$increment
  
  # set col names
  # move numbers for now; can change here to use plies instead (with or without player color)
  #colnames(time_taken) <- paste0("Time_taken_", c("white", "black"), rep(2:((ncol(time_taken)+2)/2), each=2))
  colnames(time_taken) <- paste0("Time_taken_ply_", 3:(ncol(time_taken)+2))
  
  # as df and cbind
  df <- as.data.frame(cbind(df, time_taken))
  
  # out
  print("Added variables Time_taken_ply_[ply]")
  return(df)
}


# remove negative time taken
# lag (e.g. -1); added time in losing position (e.g. -1819 in a 10+0 game)
remove_negative_time_taken <- function(df, replace_value=NA)
{
  "input: df with time_taken
  output: df with negative time takens replaced with replace_value
  "
  
  v <- grep(pattern = "Time_taken_ply_", colnames(df), value = TRUE)
  df[v][df[v] < 0] <- replace_value
  
  # out
  print(paste0("Replaced negative time taken with ", replace_value))
  return(df)
}



#### evals ####

# helper fn
# replace mates with extreme evaluations
replace_mates_with_extreme_evaluations <- function(df)
{
  eval_cols <- grep(pattern = "Eval_ply_", x=colnames(df), value=TRUE)
  for (c in eval_cols)
  {
    # get row numbers at which eval is mate
    ix_mate <- grep(pattern="#", x=df[,c], fixed = T)
    if (length(ix_mate)==0) # no mate
    {
      df[,c] <- as.numeric(as.character(df[,c]))
      next 
    }
    
    # remove mate sign and make var numeric
    new_col <- gsub(pattern = "#", replacement="", x = df[,c], fixed=T)
    new_col <- as.numeric(as.character(new_col))
    
    # replace mate eval with extreme val
    for (ix in ix_mate)
    {
      new_col[ix] <- ifelse(new_col[ix] < 0, BLACK_MATE_EVAL, WHITE_MATE_EVAL)
    }
    
    # replace in df
    df[,c] <- new_col
  }
  
  
  # out
  print(paste("Replaced mate evaluations with", WHITE_MATE_EVAL, "or", BLACK_MATE_EVAL))
  return(df)
}


# create var eval_change

add_eval_change_at_each_ply <- function(df)
{
  "
  input: df
  output: df with mate evals replaced with 200/-200; added 'Eval_change_ply_' vars
  "
  # replace mates by extreme evals
  df <- replace_mates_with_extreme_evaluations(df)
  
  # select eval vars
  v <- grep(pattern = "Eval_ply_", x=colnames(df), value=TRUE)
  
  # calculate diff at each ply
  eval_diffs <- t(apply(df[,v], 1, diff))
  colnames(eval_diffs) <- paste0("Eval_change_ply_", (1:ncol(eval_diffs))+1)
  
  # merge
  df <- as.data.frame(cbind(df, eval_diffs))
  
  # out
  print("Added columns Eval_change_ply_")
  return(df)
  
}



#### analysis datasets ####

# pivot longer to get time_taken and eval_change
get_one_row_per_ply_with_time_taken_and_eval_change <- function(df, vars_to_keep=NULL)
{
  "
  input: df
  output: ...
  "
  # https://tidyr.tidyverse.org/articles/pivot.html
  # names_to with vector with names_sep or names_pattern
  
  # pivot time taken for each ply
  df_long_time <- pivot_longer(
    df %>% select(all_of(vars_to_keep), Site, starts_with("Time_taken_ply_")),
    cols=starts_with("Time_taken_ply_"),
    names_to="Ply",
    names_prefix="Time_taken_ply_",
    names_transform = list(Ply = as.integer),
    values_to="Time_taken",
    values_drop_na = TRUE
  )
  
  # pivot eval change for each ply
  df_long_eval <- pivot_longer(
    df %>% select(all_of(vars_to_keep), Site, starts_with("Eval_change_ply_")),
    cols=starts_with("Eval_change_ply_"),
    names_to="Ply",
    names_prefix="Eval_change_ply_",
    names_transform = list(Ply = as.integer),
    values_to="Eval_change",
    values_drop_na = TRUE
  )
  
  # inner join
  df <- inner_join(df_long_time, df_long_eval)
  
  # out
  print("df now in long format with Ply, Time_taken, and Eval_change variables")
  return(df)
  
}



#### plots ####
get_plot_eval_change_by_time_taken <- function(df)
{
  "
  input: df long
  ouput: plot
  "
  p <- ggplot(df %>% filter(TimeControl == "180+0") %>% slice_sample(n=10000), aes(x=Time_taken, y=Eval_change)) +
    geom_jitter()
  return(p)
}

get_plot_blunder_by_time_taken <- function(df)
{
  "
  input: df long
  output: plot
  "
  
  p <- df %>%
    filter(TimeControl == "600+0") %>% 
    slice_sample(n=10000) %>% 
    mutate(blunder = abs(Eval_change) >= 3) %>%
    ggplot(aes(x=blunder, y=Time_taken)) + geom_boxplot()
  return(p)
}







#### analyses ####

get_mlm_blunder_by_time_taken <- function(df, scale_time_within_each_game=TRUE)
{
  "
  input: df long
  output: glmer object
  "

  ana <- glmer(blunder ~ Time_taken + (1|Site), family="binomial", 
                data=df %>% 
                  filter(TimeControl == "600+0") %>% 
                  mutate(blunder = abs(Eval_change) >= 3) %>% 
                  { if (scale_time_within_each_game) group_by(., Site) else . } %>% 
                  mutate(Time_taken = as.numeric(scale(Time_taken)))
                )
  return(ana)
}



