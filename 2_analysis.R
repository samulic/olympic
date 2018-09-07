library(dplyr)

load("input/complete.RData")
head(data)
colnames(data)
str(data)
dim(data)

distinct(data[c("ID", "Event")])

