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
