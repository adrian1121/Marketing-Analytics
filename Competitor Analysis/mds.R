library(smacof)

responses <- read.csv("c://users/kazaa/downloads/responses100.csv", header = TRUE)

groups <- t(responses[,53:59])

#standardization
grpst <- scale(groups)

#squared euclidean distance
dist<- dist(grpst,method = "euclidean")^2
dist

mds.fit<- mds(dist,ndim=2,type="ordinal")
summary(mds.fit)
mds.fit

plot(mds.fit, xlim = c(-1,1), ylim = c(-0.4,0.5), main="Fast Food Restaurants")
abline(h=0)
abline(v=0)
