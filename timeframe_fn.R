"
2021.08.19
Chess Digits
Timeframe

Functions for article:
[URL]

"

#### imports ####
library(dplyr)


#### helper fn ####
view <- utils::View


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
  colnames(time_taken) <- paste0("Time_taken_", c("white", "black"), rep(2:((ncol(time_taken)+2)/2), each=2))
  
  # as df and cbind
  df <- as.data.frame(cbind(df, time_taken))
  
  # out
  print("Added variables Time_taken_[white, black][move number]")
  return(df)
}


#### evals ####

# create var eval_change











