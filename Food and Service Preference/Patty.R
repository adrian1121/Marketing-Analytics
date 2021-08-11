#install.packages("psych")
#install.packages("GPArotation")
library(psych)
library(GPArotation)
library(reshape)
library(ggplot2)

#attributes
df <- read.csv("Patty.csv")

#scree
sc <- scree(df[,5:10], factor=F)
fit <- principal(r=df[,5:10],nfactors=6,rotate="none")
fit

fit2 <- principal(r=df[,5:10],nfactors=3,rotate="varimax")
fit2

f <- fit2$scores
#Ward's method
dist <- dist(f, method="euclidean")^2
fit3 <- hclust(dist, method="ward.D")
history <- cbind(fit3$merge,fit3$height)
par(mar=c(1,4,1,1))
plot(fit3,hang=-1,sub="",xlab="",main="")
#(side=2,at=seq(0,100,20))
ggplot(mapping=aes(x=1:length(fit3$height),y=fit3$height))+
         geom_line()+
         geom_point()+
         labs(x="stage",y="height")

#4-cluster
cluster <- cutree(fit3,k=5)
tb <- aggregate(x=f, by=list(cluster=cluster),FUN=mean)
print(tb,digits=2)
tb <- tb[,-1]
kfit <- kmeans(f,centers=tb)
kfit

#profileplot
tbm <- melt(kfit$centers,id.vars=1:5)
colnames(tbm) <- c("cluster","variable","value")
tbm$cluster <- factor(tbm$cluster)
ggplot(tbm,
       aes(x=variable, y=value,group=cluster,colour=cluster))+
  geom_line(aes(linetype=cluster))+
  geom_point(aes(shape=cluster)) +
  geom_hline(yintercept=0) +
  labs(x=NULL,y="mean")

#relate to background
#Gender
count <- table(cluster,df$Gender)
prop <- prop.table(count,2)
prop
par(mar=c(2,2,2,4))
barplot(prop,legend=rownames(count))
#Year of Study
count2 <- table(cluster,df$Year.of.Study)
prop2 <- prop.table(count2,2)
prop2
par(mar=c(2,2,2,2))
barplot(prop2,legend=rownames(count2))
#Age and Total Monthly Expense
aggregate(df[,c("ï..Age","Total.Monthly.Expense.in.HKD")],FUN=mean,by=list(kfit$cluster))


#Without Factor Analysis
#Standardization
dfs <- scale(df[,5:10])

#Squared Euclidean Distance
dista <- dist(dfs,method="euclidean")^2

#Ward's Method
fita <- hclust(dista, method="ward.D")
historya <- cbind(fita$merge,fita$height)
par(mar=c(1,4,1,1))
plot(fita,hang=-1,sub="",xlab="",main="")
axis(side=2,at=seq(0,100,20))
ggplot(mapping=aes(x=1:length(fita$height),y=fita$height))+
  geom_line()+
  geom_point()+
  labs(x="stage",y="height")
