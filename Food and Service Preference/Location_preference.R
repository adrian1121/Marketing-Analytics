library(reshape)
library(ggplot2)


#attributes
df <- read.csv("Location_preference .csv")


counts <- table(df$location)
print(counts)
par(mar=c(2,2,2,4))
barplot(counts, main="Location_preference", col=counts,
         legend=rownames(counts))
