## Clustering - not very useful results!
options(scipen = 999)
library(tidyverse)

load("input/complete.RData")

str(data)

# We want to reduce the number of levels (65) of the attribute "sport"
# Define the variables to be used for clustering
fac.var <- c("Sex", "Season")
num.var <- c("Age", "Height", "Weight")
# We start with few variables
d <- data[c(fac.var, num.var)]
# Remove missings
d <- d[complete.cases(d),]
# Use numerical features first
x <- scale(d[, num.var])

maxclus <- 40 # max number of clusters
results <- vector("list", maxclus) # container of K-means results
# compute K-means for 2 to maxclus clusters
for (i in 1:maxclus) {
  cat("N. of clusters:", i, "\n")
  results[[i]] <- kmeans(x = x, centers = i)
}

# within sum of squares vs. num. of clusters
wt <- sapply(results, function(x) x$tot.withinss)
plot(x = 1:maxclus, y = wt)
ggplot(mapping = aes(x = 1:maxclus, y = wt)) +
  geom_line() + geom_point() + geom_vline(xintercept = c(7, 11)) +
  xlab("Number of clusters") +
  ylab("Within sum of square") +
  ggtitle("Clustering on numerical attributes", subtitle = "Vertical lines indicate a potential elbow")

# Try with mixed attributes
library(clustMixType) #import library to use kproto function for mixed attributes' type
x <- cbind(x, d[, fac.var]) # bind previous numeric attributes and factor attributes
ks <- 2:30 # number of clusters we want to try
mmw <- numeric(length(ks)) # vector for the mixed weighted-matching distances
for (i in seq_along(ks)) {
  cat("n. of clusters", i+1, "\n")
  set.seed(45654)
  # Lambda is the importance of categorical with respect to numerical attributes
  mmw[i] <- kproto(x, k = ks[i], lambda = 0.5, iter.max = 100, keep.data = F)$tot.withinss
}
ggplot(mapping = aes(x = ks, y = mmw)) +
  geom_line() + geom_point() + geom_vline(xintercept = c(7, 11)) +
  xlab("Number of clusters") +
  ylab("Within sum of square + simple-matching distance") +
  ggtitle("Clustering on mixed attributes", subtitle = "Vertical lines indicate a potential elbow")
