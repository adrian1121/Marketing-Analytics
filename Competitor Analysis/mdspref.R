library(cmdpref)

pref<-read.table(file ="c://users/kazaa/downloads/mdspref.csv", sep = ",", header = TRUE, row.names = 1)

fit <- cmdpref(pref, monotone = T, maxit = 3200)
par(mar=c(5.1, 0, 4.1, 2))
plot(fit)