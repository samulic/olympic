options(scipen = 999)
library(dplyr)
library(caret)

#load("input/noMissing.rdata")
load("input/datasets.rdata")
data <- events.df
head(data)
colnames(data)
str(data)

status <- funModeling::df_status(data)


### Analysis target: predict if athlete wins a medal or not
## COLUMN SELECTION
# (1) Remove Event' (489 levels) but leave 'isTeamEvent'
# Save ID details to be joined later
id.details <- unique(data[c("ID", "Sex", "Age", "Weight", "Height", "Year", "Country", "Sub.region", "Continent")]) # 95.152 obs


d <- group_by(data, ID, Sport, isTeamEvent) %>% 
  summarise(Gold = sum(Gold), Silver = sum(Silver), Bronze = sum(Bronze),
            NoMedal = sum(NoMedal), winMedal = as.factor(max(as.numeric(as.character(winMedal)))))

data <- left_join(d, id.details, by = "ID")
# (2) Remove "ID"
data$ID <- NULL

# (3) Remove two out of these three: "Country", "Sub.region", "Continent"
# We keep "Country" for this analysis
data$Continent <- NULL
#data$Sub.region <- NULL
data$Country <- NULL

## Recode target if not will get following # Error: At least one of the class levels is not a valid R variable name; 
data$winMedal <- car::recode(data$winMedal, "0 = 'no'; 1 = 'yes';", as.factor = TRUE)

status <- funModeling::df_status(data)

(fac.var <- filter(status,  type %in% c("factor", "character","ordered-factor")) %>% .$variable)
(quant.var <- filter(status,  type %in% c("numeric", "integer")) %>% .$variable)
# remove collinear
quant.var.toremove <- c("Gold", "Silver", "Bronze", "NoMedal")
quant.var <- quant.var[! quant.var %in% quant.var.toremove]

(fac.df <- data[fac.var])
(quant.df <- data[quant.var])

(data <- data[c(fac.var, quant.var)])

# see if zero variance 
nearZeroVar(data, saveMetrics = TRUE) # Ok
# find variance for numeric vars..
sapply(quant.df, function(x) var(x)) # Ok

### LOGISTIC MODEL: 
# # prob to win a medal (event = high level: win vs noWin, 1 vs 0, etc) using athlete info like body/sport/origin
# fit <- glm(winMedal ~ . , data = data, family = "binomial"(link = 'logit'))
# summary(fit)
# # predict the event and find confusionmatrix
# predWM <- predict(fit, type = "response", newdata = data)
# head(predWM)
# predWM <- ifelse(predWM > 0.5, "win" ,"no win")
# 
# (conf_matrix <- table(predWM, data$winMedal))
# 
# (err <- sum(conf_matrix[row(conf_matrix) != col(conf_matrix)]) / nrow(data))
# (acc <- 1 - err)
# # Use cross validatin for reliable statistics
# set.seed(1)
# ctrl <- trainControl(method = "cv", number = 10)
# trainModel <- train(winMedal ~ . , data = data, method = "glm", trControl = ctrl)
# trainModel
# 
# predCV <- predict(trainModel, type = "raw", newdata = data)
# confusionMatrix(predCV, data$winMedal) # similar results, low specificity

## ------ ##

# Imbalanced ?
table(data$winMedal)
prop.table(table(data$winMedal)) # Si, 80/20% 

# split train in training e test
set.seed(777)
TrainIndex <- createDataPartition(y = data$winMedal, p = 0.7, list = FALSE)
training <- data[TrainIndex,]
test <- data[-TrainIndex,]

prop.table(table(training$winMedal))
prop.table(table(test$winMedal))

### KNN
## Model variable selection 
# -- rpart
set.seed(1); cvCtrl.acc <- trainControl(method = "cv", number = 10, classProbs = TRUE) # this gives ACC as measure
set.seed(1); cvCtrl.roc <- trainControl(method = "cv", number = 10, classProbs = TRUE, # this gives ROC as measure
                                        summaryFunction = twoClassSummary)

#rpartTune <- train(winMedal ~ ., data = training, method = "rpart", tuneLength = 15, trControl = cvCtrl.acc)
#rpartTune2 <-train(winMedal ~ ., data = training, method = "rpart", tuneLength = 15, trControl = cvCtrl.roc)
rpartTune3 <-train(winMedal ~ ., data = training, method = "rpart", tuneLength = 15, trControl = cvCtrl.roc, metric = "Spec")
# metric = "Spec" because 'positive' class is 'no' therefore try to correctly predict many winners

#getTrainPerf(rpartTune)
#getTrainPerf(rpartTune2)
getTrainPerf(rpartTune3) #same performance as optimizing general ROC statistic.

pred <- predict(rpartTune3, newdata = test)
confusionMatrix(pred, test$winMedal, "no")

# - important factors
ImpMeasure <- data.frame(varImp(rpartTune3, scale = TRUE)$importance)
ImpMeasure$Vars <- row.names(ImpMeasure)
# save top_N important variables (dummizzati)
important.var.25 <- ImpMeasure[order(-ImpMeasure$Overall),][1:25, "Vars"] 
important.var.30 <- ImpMeasure[order(-ImpMeasure$Overall),][1:30, "Vars"]
important.var.35 <- ImpMeasure[order(-ImpMeasure$Overall),][1:35, "Vars"]
# save the full dummized testing and training datasets 
dummy.full.train <- as_tibble(model.matrix(winMedal ~ . , data = training))
dummy.full.test <- as_tibble(model.matrix(winMedal ~ . , data = test))
# select only top_N relevant attributes
dummy.35.train <- select(dummy.full.train, one_of(important.var.35))
dummy.35.test  <- select(dummy.full.test, one_of(important.var.35))
# set standard names
#training <- cbind(dummy.35.train, winMedal = ifelse(training$winMedal == "yes", 1, 0))
#test <- cbind(dummy.35.test, winMedal = ifelse(test$winMedal == "yes", 1, 0))
train <- cbind(dummy.35.train, winMedal = training$winMedal)
test  <- cbind(dummy.35.test,  winMedal = test$winMedal)
# quick check of the structure
funModeling::df_status(training)
nearZeroVar(training, saveMetrics = T)
sapply(training, function(x) var(x)) %>% sort(decreasing = T) 

## Apply KNN
set.seed(1)
ctrl <- trainControl(method = "cv", number = 10 , savePredictions = T, search = "grid", 
                     summaryFunction = twoClassSummary , classProbs = TRUE)
tunegrid <- expand.grid(k = seq(10, 30, 10))
knn <- train(winMedal ~ . , data = train, method = "knn",  
             tuneGrid = tunegrid, trControl = ctrl, tuneLength = 10, # tunelength not needed if grid is provided
             preProcess = c("center", "scale"))
knn_spec <- train(winMedal ~ . , data = train, method = "knn", # optimize specificity: 'no' is the target/positive class 
                  tuneGrid = tunegrid, trControl = ctrl, tuneLength = 10,
                 preProcess = c("center", "scale"), metric = "Spec")
# Waiting time.......
knn
getTrainPerf(knn)
# confusion matrix of knn
pred.tr <- predict(knn, newdata = train)
confusionMatrix(pred.tr, train$winMedal) # even better spec

pred.knn <- predict(knn, newdata = test)
confusionMatrix(pred.knn, test$winMedal) # worse spec..
# KNN = 10? maximized specificity
pred.knn10 <- predict(knn_spec, newdata = test)
confusionMatrix(pred.knn10, test$winMedal)

prob.tr <- predict(knn, newdata = train, type = "prob")
prob.tr10 <- predict(knn_spec, newdata = train, type = "prob")
head(prob.tr10)

#save knn k=30, use k=10
knn.30 <- knn
knn <- knn_spec
prob.tr <- prob.tr10
# Matrice dei profitti, permette di guidare la classifcazione 
## - Multi class classification
# profit.matrix <- ifelse(diag(4) == 1, 0, 1)# matrice dei costi, misclassification cost = 1
# # Sulla diagonale ci sono le predizioni corrette
# profit.matrix[4, 4] <- 50 # Big reward for correctly golden
# profit.matrix[3, 3] <- 23 # Siver profit
# profit.matrix[2, 2] <- 10 # Bronze
# profit.matrix[1, 1] <- 2  # Looser correctly identified (makes sense to change from 1 = miscl cost?)

## - Double class classification
profit.matrix <- ifelse(diag(2) == 1, 0, 1)# matrice dei costi, misclassification cost = 1
# Sulla diagonale ci sono le predizioni corrette
profit.matrix[1, 1] <- 2  # Reward for correctly classifying a loser
profit.matrix[2, 2] <- 15 # Reward for correctly classifying a winner

profit.matrix
rownames(profit.matrix) <- colnames(profit.matrix) <- levels(train$winMedal)
profit.matrix
profit.matrix <- as.matrix(profit.matrix)
prob.tr <- as.matrix(prob.tr)  # prob.tr p(x_j) #probabilita' individuale di essere ogni classe j
# profitto individuale atteso di ciascuna decisione
exp.profit <- data.frame(prob.tr %*% profit.matrix) # e' un prodotto matriciale
head(exp.profit) # classify taking the maximum in each row
# trova il massimo di ogni riga [profitto atteso individuale]
zzz <- data.frame(names(exp.profit)[apply(exp.profit, 1, which.max)])
colnames(zzz) <- "predict"
# predict e' la classe target che massimizza il profitto
exp.p_and_decision <- cbind(exp.profit, zzz)
head(exp.p_and_decision, n = 10)

# matrice di confusione decisionale basata su questa nuova regola decisionale
table(true = train$winMedal, pred = exp.p_and_decision$predict)

# test set
prob.te <- predict(knn, test, type = "prob")
prob.te <- as.matrix(prob.te)
exp.p.te <- data.frame(prob.te %*% profit.matrix)
zzb <- data.frame(names(exp.p.te)[apply(exp.p.te, 1, which.max)])
colnames(zzb) <- "predict"
exp.p.te_decision <- cbind(exp.p.te, zzb)

table(true = test$winMedal, pred = exp.p.te_decision$predict)

# vedi errori di riga
prop.table(table(true = test$winMedal, pred <- exp.p.te_decision$predict), margin = 1)
save(file = "input/02_dataset_knn.rdata", list = c("data", "d", "df.original", "events.df", "sports.df", "id.details", 
                                                   "knn", "knn.30", "dummy.35.test", "dummy.35.train"))

## Rpart performance
# - threshold
pred.prob <- predict(rpartTune3, newdata = training, type = "prob") 

pred.prob.N <- predict(rpartTune3, newdata = training, type = "prob")[,1] # 'no' is the event: take prob to be looser
pred.prob.T <- predict(rpartTune3, newdata = training, type = "prob")[,2] # 'yes' is the event: take prob to 'yes'
head(pred.prob.T)

y <- training$winMedal
#(y <- ifelse(y == "no", 1, 0))
library(ROCR)
pred.ROCR <- prediction(pred.prob.T, y) # Save ROCR object
class(pred.ROCR)
# roc curve
roc.perf <- performance(pred.ROCR, measure = "tpr", x.measure = "fpr") #tnr are winner predicted 
plot(roc.perf)
abline(a = 0, b = 1)

# specificity metrics (y.values) when varying threshold (x.values)
# sensitivity metrics
(perf.R <- performance(pred.ROCR, measure = "sens"))
plot(perf.R)
# x=cutoff/threshold, y=specificity
# example with cutoff = 0.13432836, we expect ~0.90 sens
# example with cutoff = 0.11232309 we expect 0.998416583 sens --> no, too sensitive
# example with cutoff = 0.86567164, we expect 0.905570750 spec.
# see if same results is in caret
pred.p <- predict(rpartTune3, newdata = training, type = "prob")[,2]
predy <- as.factor(ifelse(pred.p > 0.13432836, "yes","no"))
confusionMatrix(predy, training$winMedal, positive = "yes") # ok, veery sensitive threshold

# hence the analysis suggests to choose cutoff very small (<0.134) to have high SENS
# hence new decision will be: if prob(Y='yes' > 0.134 then pred = 'yes', else pred = 'no'
# in other words, is enought a prob 0.14 that athletes will to be predicted as winners

# see results on test set
pred.p <- predict(rpartTune3, newdata = test, type = "prob")[,2]
predy <- as.factor(ifelse(pred.p > 0.13432836, "yes","no"))

confusionMatrix(predy, test$winMedal,positive = "yes")

###################################################
# plot performance of variuos measures when varying threshold

acc.perf <- performance(pred.ROCR, measure = "acc")
plot(acc.perf)

spec.perf <- performance(pred.ROCR, measure = "spec")
plot(spec.perf)

sens.perf <- performance(pred.ROCR, measure = "sens")
plot(sens.perf)

#?performance

#precision
prec.perf <- performance(pred.ROCR, measure = "prec")
plot(prec.perf)

# create a dataset with measures when varying threshold
cut <- as.data.frame(acc.perf@x.values)
colnames(cut) <- "cut"
head(cut)

spec <- as.data.frame(spec.perf@y.values)
colnames(spec) <- "spec"

sens <- as.data.frame(sens.perf@y.values)
colnames(sens) <- "sens"

acc <- as.data.frame(acc.perf@y.values)
colnames(acc) <- "acc"

prec <- as.data.frame(prec.perf@y.values)
colnames(prec) <- "prec"

all <- cbind(cut, spec, sens, acc,prec)

head(all)
dim(all)

# impile vertically 4 measures with a new var called (Measure_type) that defynes the metric
library(reshape2)
metrics <- melt(all, id.vars = "cut", variable.name = "Measure_type", value.name = "Measure")
head(metrics)
dim(metrics) #292 = 73 * (5 - 1)

ggplot(metrics, aes(x = cut, y = Measure, color = Measure_type)) + 
  geom_line(size = 1) + scale_x_continuous(breaks = seq(0, 1, by = 0.1)) +
  ylab("") + xlab("Probability Cutoff") + ggtitle("Win a medal? Threshold plot")
  theme(legend.position = "top")

# - Lift curve
# use as event 'no' = doesn't win any medal
copy <- training
copy$pred.p <- predict(rpartTune3, newdata = copy, type = "prob")[,1]
#sensitivity preche' ora r1
funModeling::gain_lift(data = copy, score = 'pred.p', target = 'winMedal')
# top 20% of our predicted values to be 'no' = loosers captured 27.9% of total true loosers
# gain is a cumulate sensitivity in different deciles

# score.point = threshold: which threshold we must use for P(y = no | x) to capture
# 50% of all loosers? pretty high = 0.8513127: hence IF P(y = yes | x) <= 0.1486873 then predict 'yes'
# hence you will have 50% of specificity (changed target) 

# Instead using as event 'yes' = win atleast a medal (as the above analysis on the threshold)
copy$pred.p <- predict(rpartTune3, newdata = copy, type = "prob")[,2]
# you will have a very big threshold
funModeling::gain_lift(data = copy, score = 'pred.p', target = 'winMedal')
# to capture more than 60% of winners threshold > 0.1425422

####EVENT yes####
#        predict
#true   yes    no
#yes     tp    fn
#no     fp    tn      
################

# assume that a fp (a looser classified as winner)
# is 3 times costing than a fn

#######cost matrix#############
#         predict
#true   r0     r1
#r0     tp     1
#r1     3    tn      
################

# use functionality of pROC package to stydy best cutoff 
# this is a required object want (model, target)
pred.ROCR <- prediction(pred.prob.T, y)
# remember: y <- ifelse(y == "yes", 1, 0)

# which is the best cutoff for a prob P(y=1) = P(lossfreq=r0) which minimize total costs?????
# which is the best cutoff for a prob P(y=1) = P(lossfreq=r0) which minimize total costs?????

cutoff.costs <- list()
for (cost in seq(2, 15)) {
  cost.perf <- performance(pred.ROCR, "cost", cost.fp = cost, cost.fn = 1)
  cutoff.costs[[cost-1]] <- cbind(cost, pred.ROCR@cutoffs[[1]][which.min(cost.perf@y.values[[1]])])
}
cost.perf <- performance(pred.ROCR, "cost", cost.fp = 5, cost.fn = 1)
pred.ROCR@cutoffs[[1]][which.min(cost.perf@y.values[[1]])]
# hence IF P(winMedal = no < 0.8452381 ) then predicted as 'no' else predict winner 'yes'


# -- penalized logistic regression
library(glmnet)
set.seed(1)
# # TODO
# cv.glmnet_model <- cv.glmnet(x = data[, ! names(data) == "winMedal"], y = data$winMedal, family = "binomial")
# # as glm, glmnet use as event highest y value="yes"="winMedal"
# ls(cv.glmnet_model)
# cv.glmnet_model$lambda.min
# cv.glmnet_model$lambda.1se
# # lambda.min, lambda.1se  best tuned lambda forpenalization 
# #predicted prob to be 2
# cv.glmnet_prediction_v <- predict(
#   cv.glmnet_model,   newx = x,
#   s = "lambda.1se", type = "response")
# # confusion matrix of glmnet
# predCV=predict(cv.glmnet_model, x, type="class")
# table(y,predCV)
# #####PROFIT MATRIX###############
# #         predict
# #        2       1
# #true  good     bad
# #good    1      0(fn)
# #bad    -6(fp)  0      
# ################
# 
# 
# # total profit is sum of profit values weighted by elements of the confusion matrix
# total_profit=(115*1-49*6)
# # mean profit is the mean 
# mean_profit=(115*1-49*6)/ nrow(GermanCredit)
# 
# total_profit
# mean_profit
# total_cost= -total_profit
# total_cost






