library(dplyr)
library(funModeling)

load("input/complete.RData")
head(data)
colnames(data)
str(data)
dim(data)

load("input/datasets.rdata")
head(data)
colnames(data)
str(data)
dim(data)

filter(data, Silver == 0 & Gold == 0 & Bronze == 0 & NoMedal == 1)
unique(data$Year_avg) %>% sort()
# tipi di variabili
status = df_status(data, print_results = F)
status

missing.loc <- filter(data, is.na(Continent))
# Manually set missing regions for "Kosovo"
data$Continent[which(data$Country == "Kosovo")] <- "Europe"
data$Sub.region[which(data$Country == "Kosovo")] <- "Southern Europe"
# Remove 55 rows of the "Individual Olympic Athletes" because they have no provenance
# Btw, they didn't win anything...
data <- data[!data$Country == "Individual Olympic Athletes",]

status = df_status(data, print_results = F)
status

missing.age <- filter(data, is.na(Age))
missing.body <- filter(data, is.na(Weight) | is.na(Height))
# Could it be worth to impute Age? Let's check how many body missing with age
# 8608 missing ages
dim(missing.age[!is.na(missing.age$Weight) & !is.na(missing.age$Height),])[1]
# 653 case missing only Age, not worth imputing (with model)
# Count number of missing either height or weight
CountPerGroup <- function(x, groups) {
  data.set <- subset(x, Year_avg %in% groups)
  ans <- sapply(split(data.set, data.set$Year_avg), 
                function(y) sum(!complete.cases(y)))
  return(data.frame(Year_avg = names(ans), nrow_missing = unname(ans)))
}
missing.per.year <- CountPerGroup(data, unique(data$Year_avg)) %>% mutate(Year_avg = (as.character(Year_avg)))
nrow.per.year <- group_by(data, Year_avg) %>% summarise(nrow = n()) %>% mutate(Year_avg = as.character(Year_avg))
missing.per.year <- left_join(missing.per.year, nrow.per.year, by = "Year_avg")
missing.per.year$missing_ratio <- missing.per.year$nrow_missing / missing.per.year$nrow
missing.per.year$Year_avg <- as.integer(missing.per.year$Year_avg)
missing.per.year %>% group_by(Year_avg) %>% summarise(missing_ratio = weighted.mean(missing_ratio, nrow), nrow = sum(nrow)) %>%
  ggplot(aes(x = Year_avg, y = missing_ratio)) +
  geom_point(aes(size = nrow)) + geom_vline(xintercept = 1956, color = "red") +
  xlab("Year") + ylab("Ratio of missing weight or height rows") +
  ggtitle("Missing body details over time", subtitle = "Dot size indicates the number of rows")

data <- filter(data, Year_avg > 1956)
status = df_status(data, print_results = F)
status
data <- data[complete.cases(data),]
save(data, file = "input/noMissing.rdata")
