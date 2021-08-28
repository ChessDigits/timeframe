# use time taken vars
v <- paste0("Time_taken_ply_", 3:200)

# do white and black for a single game
dw <- (dw<-df[8,v[seq(1, length(v), by=2)]])[!is.na(dw)] # white
db <- (db<-df[8,v[seq(2, length(v), by=2)]])[!is.na(db)] # black

# make equal length
l <- min(length(dw), length(db))
dw <- dw[1:l]; db <- db[1:l]

# two series
ts.plot(cbind(dw, db))
plot(dw, db)

# diff log
dw_dl <- diff(log(dw))
db_dl <- diff(log(db))
ts.plot(cbind(dw_dl, db_dl))
plot(dw_dl, db_dl)


# cor
cor(dw, db) # <0 --> penser aux valeurs comparées (plies), est-ce que ce sont les bonnes? white vs black (pense que oui)
dl <- list(dw_dl, db_dl)
# replace ±Inf with extreme value
dl <- lapply(dl, \(t) { t[is.infinite(t)] <- NA; t})
cor(dl[[1]], dl[[2]], use="pairwise") # <0

# distribution of correlations/acf across games
# posterior distro bayesian style
# outliers could be interesting as well


# acf
# acf lag 1
cor(dw[-length(dw)], dw[-1]) # .11
cor(db[-length(db)], db[-1]) # .25

acf(dw, lag.max = 1, plot = F) # .11
acf(dw, lag.max = 2, plot = F) # .157

acf(db, lag.max = 1, plot = F) # .25
acf(db, lag.max = 2, plot = F) # .296

# plots for all lags
acf(dw, plot=T)
acf(db, plot=T)


# find sequences of moves by finding large autocorrelations and then drop?
# though looking at plots, doesn't seem too interesting with time taken





