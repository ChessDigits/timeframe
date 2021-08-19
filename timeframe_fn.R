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

# create var time_taken
# class(df$Clock_ply_1)
# x <- as.character(df$Clock_ply_1)
# x <- strsplit(x, split=":", fixed = T)
# x <- lapply(x, as.numeric) 
# x <- sapply(x, \(t) t[1]*60*60 + t[2]*60 + t[3])


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
  return(df)
}



# add time_taken variables
add_time_taken <- function(df)
{
  "
  
  "
  # make clocks in seconds
  df <- make_clocks_in_seconds(df)
  
  # add increment to df
  df <- add_increment(df)
  
  
  # out
  print("")
  return(df)
}


#### evals ####

# create var eval_change











