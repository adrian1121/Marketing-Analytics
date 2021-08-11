library(ca)

cora <- read.csv("C://users/kazaa/desktop/ca.csv", header=TRUE, row.names = 1)

chisq.test(cora)

fit <- ca(obj=cora)
summary(fit)
par(mar=c(5,2,0,2))
plot(fit, labels = 1)
