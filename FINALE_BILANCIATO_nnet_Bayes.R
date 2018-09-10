#metanota: tutta questa prima parte è contenuta nel file FINALE_BILANCIATO.R in cui si definiscono le variabili#

setwd("C:/Users/Massimiliano/Desktop/PROGETTO DATA SCIENCE LAB/appunti Comotto/lezioni Lovaglio/dataLOVAGLIO")
load("complete.RData")

df<-data

library(dplyr)
library(funModeling)
library(car)
library(VIM)
library(caret)
library(pROC)
library(ROCR)

head(df)

# tipi di variabili
status=df_status(df, print_results = F)
status

#ELIMINO ID 
df$ID<-NULL
status=df_status(df, print_results = F)

#TRASFORMO YEAR IN INTEGER
df$Year<-as.integer(df$Year)

#TRASFORMO I character IN FACTOR:

filter(status,type=='character')%>%select(variable)
df$City<-as.factor(df$City)
df$Sport<-as.factor(df$Sport)
df$Event<-as.factor(df$Event)
df$Country<-as.factor(df$Country)



#TRASFORMO MEDAL IN TARGET BINARIO 
unique(df$Medal)

df$Medal<-recode(df$Medal, "'Gold'='r1'; 'Bronze'='r1'; 'Silver'='r1'; NA='r0'", as.factor=TRUE) #binarizza e fattorizza il target
status=df_status(df, print_results = F)
status

lista<-list()

for ( i in 1: length(unique(df$Sport))){
  lista[[i]]<-filter(df,Sport==unique(df$Sport)[i])
}



################################DATASET NON SBILANCIATI########################################

library(nnet)
library(NeuralNetTools)
library(caret)
library(funModeling)
library(dplyr)
library(pROC)


#calcolo la differenza fra le percentuali di valori appartenenti alle 2 classi della variabile target, in ciascun dataset,
#e poi ne cerco il minimo per  trovare il dataset meno sbilanciato:
vet<-c()

for(i in 1:length(lista)){
  vet[i]<-(min(abs(prop.table(table(lista[[i]]$Medal))[1]-prop.table(table(lista[[i]]$Medal))[2])))
  
  }
min(vet)
which((vet)==0)
dfn<-lista[[59]]
dim(dfn)
# piccolo: ne cerco un altro

which((vet==min(vet[-59])))
dim(lista[[60]])
#-->piccolo

which((vet==min(vet[-c(59,60)])))
dim(lista[[58]])
#-->piccolo

which((vet==min(vet[-c(59,60,58)])))
dim(lista[[41]])
#894 13 -> bene

df<-lista[[41]]
prop.table(table(df$Medal))

#SPORT?
describe(df$Sport)

# imputo i dati mancanti
df_status(df)

df$Age=impute(df$Age, mean)  # funzione: impute()
df$Weight=impute(df$Weight,mean)
df$Height=impute(df$Height,mean)

status<-df_status(df)


#ELIMINO VARIABILI FACTOR CON UN UNICO LIVELLO
filter(status,type=='factor' & unique==1)%>%select(variable)
df$Season<-NULL
df$Sport<-NULL
df$Sex<-NULL
df$Event<-NULL
status<-df_status(df)

#nearZeroVariance
nearZeroVar(df[,-6] , saveMetrics = TRUE) 
#OK


# split train in training e test
set.seed(107)
TrainIndex<-createDataPartition(df$Medal ,p=0.7,list=FALSE)
training<-df[TrainIndex,]
test = df[-TrainIndex,]

#controllo che siano bilanciati in modo simile:
prop.table(table(training$Medal))
prop.table(table(test$Medal))
#ok
#########################MODELLAZIONE  ####################################################################################################################
#SELEZIONO LE VARIABILI IMPORTANTI CON UN ALBERO ( NON RICHIEDE PREPROCESSING)

set.seed(1)
cvCtrl <- trainControl(method = "cv", number=10, search="grid", classProbs = TRUE)

rpartTuneCvA <- train(Medal ~ ., data = training, method = "rpart",
                      tuneLength = 10,
                      trControl = cvCtrl)

# miglior accuracy attraverso il miglior cp# cp=0 massima complessità dell'albero
rpartTuneCvA

# final model
getTrainPerf(rpartTuneCvA)#
# accuracy 1 : perchè cercare altri modelli ????
plot(rpartTuneCvA)

#confusion matrix direttamente sul TEST
pred=predict(rpartTuneCvA,test)
confusionMatrix(pred, test$Medal)
# accuracy= 1

#SCELGO UN ALTRO DATASET CON LO STESSO METODO SU DESCRITTO


df<-lista[[49]]

dim(lista[[49]])
#463 13 -> bene

prop.table(table(df$Medal))

#SPORT?
describe(df$Sport)
#sport...

# imputo i dati mancanti
df_status(df)

df$Weight=impute(df$Weight,mean)
df$Height=impute(df$Height,mean)

status<-df_status(df)


#ELIMINO VARIABILI FACTOR CON UN UNICO LIVELLO
filter(status,type=='factor' & unique==1)%>%select(variable)
df$Season<-NULL
df$Sport<-NULL

status<-df_status(df)

#nearZeroVariance
nearZeroVar(df[,-6] , saveMetrics = TRUE) 
#OK


# split train in training e test
set.seed(107)
TrainIndex<-createDataPartition(df$Medal ,p=0.7,list=FALSE)
training<-df[TrainIndex,]
test = df[-TrainIndex,]

#controllo che siano bilanciati in modo simile:
prop.table(table(training$Medal))
prop.table(table(test$Medal))
#ok

#SELEZIONO LE VARIABILI IMPORTANTI CON UN ALBERO ( NON RICHIEDE PREPROCESSING)

set.seed(1)
cvCtrl <- trainControl(method = "cv", number=10, search="grid", classProbs = TRUE)

rpartTuneCvA <- train(Medal ~ ., data = training, method = "rpart",
                      tuneLength = 10,
                      trControl = cvCtrl)

# miglior accuracy attraverso il miglior cp# cp=0 massima complessità dell'albero
rpartTuneCvA

# final model
getTrainPerf(rpartTuneCvA)#
plot(rpartTuneCvA)

#confusion matrix direttamente sul test
pred=predict(rpartTuneCvA,test)
confusionMatrix(pred, test$Medal)
# accuracy= 0.77


#LE VARIABILI PIU' IMPORTATNTI 
varImp(object=rpartTuneCvA)

#seleziono le variabiliimportanti
vim=as.data.frame(rpartTuneCvA$finalModel$variable.importance)
head(vim)

# vettore delle variabili importanti 
vimname=row.names(vim)
vimname

#[1] "CountryCanada"                "Year"                         "CityChamonix"                
#[4] "Sub.regionNorthern Europe"    "Height"                       "CountrySwitzerland"          
#[7] "CitySalt Lake City"           "CountryUK"                    "ContinentEurope"             
#[10] "CountrySweden"                "Age"                          "CountryNorway"               
#[13] "Weight"                       "CountryDenmark"               "CityNagano"                  
#[16] "EventCurling Men's Curling"   "EventCurling Women's Curling" "SexF"                        
#[19] "CityVancouver"                "CountryFinland"


#in vista di nnet

nnet_train<-data.frame(Medal=training$Medal,Sub.regionNorthernEurope=ifelse(training$Sub.region =="Northern Europe", 1,0),CountryCanada=ifelse(training$Country=="Canada", 1,0),
                       Year=training$Year,CountrySweden=ifelse(training$Country=="Sweden", 1,0),Height=training$Height,CityChamonix=ifelse(training$City=="Chamonix", 1,0),
                       Age=training$Age,CityNagano=ifelse(training$City=="Nagano", 1,0),Weight=training$Weight,
                       CountrySwitzerland=ifelse(training$Country=="Switzerland", 1,0),ContinentEurope=ifelse(training$Continent=="Europe", 1,0),
                       CityVancouver=ifelse(training$City=="Vancouver", 1,0),CountryUK=ifelse(training$Country=="UK", 1,0),
                       CitySaltLakeCity=ifelse(training$City=="SaltLakeCity", 1,0),CountryDenmark=ifelse(training$Country=="Denmark", 1,0),
                       CountryFinland=ifelse(training$Country=="Finland", 1,0),CountryNorway=ifelse(training$Country=="Norway", 1,0),
                       EventCurlingMenCurling=ifelse(training$Event=="Curling Men's Curling",1,0),
                       EventCurlingWomenCurling=ifelse(training$Event=="Curling Women's Curling",1,0),
                       SexF=ifelse(training$Sex=="SexF",1,0)
)


nnet_test<-data.frame(Medal=test$Medal,Sub.regionNorthernEurope=ifelse(test$Sub.region =="Northern Europe", 1,0),CountryCanada=ifelse(test$Country=="Canada", 1,0),
                      Year=test$Year,CountrySweden=ifelse(test$Country=="Sweden", 1,0),Height=test$Height,CityChamonix=ifelse(test$City=="Chamonix", 1,0),
                      Age=test$Age,CityNagano=ifelse(test$City=="Nagano", 1,0),Weight=test$Weight,
                      CountrySwitzerland=ifelse(test$Country=="Switzerland", 1,0),ContinentEurope=ifelse(test$Continent=="Europe", 1,0),
                      CityVancouver=ifelse(test$City=="Vancouver", 1,0),CountryUK=ifelse(test$Country=="UK", 1,0),
                      CitySaltLakeCity=ifelse(test$City=="SaltLakeCity", 1,0),CountryDenmark=ifelse(test$Country=="Denmark", 1,0),
                      CountryFinland=ifelse(test$Country=="Finland", 1,0),CountryNorway=ifelse(test$Country=="Norway", 1,0),
                      EventCurlingMenCurling=ifelse(test$Event=="Curling Men's Curling",1,0),
                      EventCurlingWomenCurling=ifelse(test$Event=="Curling Women's Curling",1,0),
                      SexF=ifelse(test$Sex=="SexF",1,0)
)




#variabili zero variance
nzv = nearZeroVar(nnet_train[,-1] , saveMetrics = TRUE)
nzv[order(nzv$zeroVar, decreasing = TRUE), ]

#elimino var zero variace
nnet_train$CitySaltLakeCity<-NULL                  
nnet_train$SexF<-NULL

#Primo modello con CARET per tunare i parametri
set.seed(7)
metric <- "Accuracy"  
ctrl = trainControl(method="cv", number=5, search = "grid")

nnetFit_def <- train(nnet_train[-1], nnet_train$Medal,
                     method = "nnet",
                     preProcess = "range", #METANOTA: QUESTO PER SCALARE I DATI
                     metric=metric, trControl=ctrl,
                     trace = TRUE, # per vedere l'eventuale convergenza
                     maxit = 300)
print(nnetFit_def)
plot(nnetFit_def)
getTrainPerf(nnetFit_def) 
# acc 0.86  size = 3 and decay = 0.1.


# PARAMETRI di ricerca completamente random: non raccomandato
set.seed(7)
metric <- "Accuracy"
ctrl = trainControl(method="cv", number=5, search = "random")#METANOTA: LA DIFFERENZA STA QUI:random

nnetFit_CVrand <- train(nnet_train[-1], nnet_train$Medal,
                        method = "nnet",
                        preProcess = "range", 
                        tuneLength = 10, metric=metric, trControl=ctrl,
                        trace = TRUE,
                        maxit = 400)# converge a con maggiori iterazioni rispetto al modello precedente
print(nnetFit_CVrand)
getTrainPerf(nnetFit_CVrand) 
# acc=0.89 size = 7 and decay = 0.023

#tuning diverso : accuratezza migliore.

# cerca i parametri di default con una discreta lunghezza di tuning (5 combinazioni 'numero neuroni nascosti' * decadimento')
set.seed(7)
metric <- "Accuracy"
ctrl = trainControl(method="cv", number=5)#, search="grid")#METANOTA: NIENTE grid 

nnetFit_deflength <- train(nnet_train[-1], nnet_train$Medal,
                           method = "nnet", 
                           tuneLength = 5,
                           preProcess = "range", metric=metric, trControl=ctrl,
                           trace =T ,
                           maxit = 600)#questo converge a 600

print(nnetFit_deflength)
getTrainPerf(nnetFit_deflength) 
# acc=0.89 size = 9 and decay = 0.01.
## QUI APPAIONO LE 5 COMBINAZIONI 
plot(nnetFit_deflength)
#architettura
print(nnetFit_deflength$finalModel)



# migliore opzione: provo una ricerca suggerendo parametri simili a quelli del modello con risultati migliori(random) = 7, decay = 0.02
set.seed(7)
metric <- "Accuracy"
ctrl = trainControl(method="cv", number=5, search="grid")
tunegrid <- expand.grid(size=c(5:8), decay = c(0.001, 0.02, 0.05 , .1))

nnetFit_defgrid <- train(nnet_train[-1], nnet_train$Medal,
                         method = "nnet",
                         preProcess = c("scale"), 
                         tuneLength = 10, metric=metric, trControl=ctrl, tuneGrid=tunegrid,
                         trace = T,
                         maxit = 250)# converge con minori iterazioni del modello precedente.

print(nnetFit_defgrid)
getTrainPerf(nnetFit_defgrid) 
# acc=0.9 size = 5 and decay = 0.1--> è l'accuracy migliore
plot(nnetFit_defgrid)  


# comparazione dei 3 modelli nnet sul test
nnetFit_def         # model1 size=3
nnetFit_defgrid     # model2 size=5
nnetFit_deflength   # model3 size=9

# modello 1
nnet_test$pred1 = predict(nnetFit_def, nnet_test, "raw")
confusionMatrix(nnet_test$pred1, nnet_test$Medal)

# modello 2
nnet_test$pred2 = predict(nnetFit_defgrid, nnet_test, "raw")
confusionMatrix(nnet_test$pred2,nnet_test$Medal )

# modello 3
nnet_test$pred3 = predict(nnetFit_deflength, nnet_test, "raw")
confusionMatrix(nnet_test$pred3, nnet_test$Medal)

#il migliore in termini di accuracy è il secondo: nnetFit_defgrid, con i parametri 'suggeriti'.




#per vedere l'architettura  del modello migliore rifitto con nnet.
#nnet non vuole stringhe: y

# r0= NO MEDAGLIA--> 1 = NO MEDAGLIA
y=ifelse(nnet_train$Medal=="r0",1,0)

set.seed(7)
mynet1 <- nnet(nnet_train[,-1], y ,  entropy=T, size=5, decay=0.1, maxit=300, trace=T)
plotnet(mynet1, alpha=0.6, cex=.6)

confusionMatrix(nnet_test$pred1, nnet_test$Medal)
confusionMatrix(nnet_test$pred2, nnet_test$Medal)
confusionMatrix(nnet_test$pred3, nnet_test$Medal)

# in termini di accuracy si  ma in termini di ROC?


nnetFit_def         # model1 size=3
nnetFit_defgrid     # model2 size=5 
nnetFit_deflength   # model3 size=9



cvValues <- resamples(list(default= nnetFit_def,default_defgrid=nnetFit_defgrid,   default_length=nnetFit_deflength)) #
summary(cvValues)
bwplot(cvValues, layout = c(1, 3))
#ditribuizione dell'accuracy più concentrata nel modello con i parametri impostati.

#probabilità di NON vittoria della medaglia
nnet_test$p1 = predict(nnetFit_def, nnet_test, "prob")[,1]
nnet_test$p2 = predict(nnetFit_defgrid, nnet_test, "prob")[,1]
nnet_test$p3 = predict(nnetFit_deflength, nnet_test, "prob")[,1]


# roc values
a=roc(Medal ~ p1, data = nnet_test)
b=roc(Medal ~ p3, data = nnet_test)
c=roc(Medal ~ p2, data = nnet_test)
a
b
c

plot(a)
plot(b,add=T,col="red")
plot(c,add=T,col=6)

#quest' ultima è nnetFit_defgrid, con i parametri impostati e l'accuracy migliore: risulta
# migliore anche con la roc.

# altra funzione: pca 

set.seed(7)
metric <- "Accuracy"
ctrl = trainControl(method="cv", number=5, search="grid")
# i parametri sono quelli che hanno dato migliori risultati
tunegrid <- expand.grid(size=c(5:8), decay = c(0.001, 0.01, .1))

nnetFit_pca <- train(nnet_train[,-1], nnet_train$Medal,
                     method = "nnet",
                     preProcess = "pca",
                     tuneLength = 10, metric=metric, trControl=ctrl, tuneGrid=tunegrid,
                     trace = T,
                     maxit = 300)

print(nnetFit_pca)
getTrainPerf(nnetFit_pca) 

# accuracy= 0.88   size = 8 and decay = 0.1.
pre = predict(nnetFit_pca, nnet_test, "raw")
confusionMatrix(pre,nnet_test$Medal )
# per l'accuracy resta migliore il modello nnetFit_defgrid.
# per la roc?


#probabilità di NON vittoria della medaglia
nnet_test$p1 = predict(nnetFit_def, nnet_test, "prob")[,1]
nnet_test$p2 = predict(nnetFit_defgrid, nnet_test, "prob")[,1]
nnet_test$p3 = predict(nnetFit_deflength, nnet_test, "prob")[,1]
nnet_test$p4 = predict(nnetFit_pca, nnet_test, "prob")[,1]


# roc values
a=roc(Medal ~ p1, data = nnet_test)
b=roc(Medal ~ p3, data = nnet_test)
c=roc(Medal ~ p2, data = nnet_test)
d=roc(Medal ~ p4, data = nnet_test)
a
b
c
d
plot(a)
plot(b,add=T,col="red")
plot(c,add=T,col=6)
plot(d,add=T,col=3)
#quest' ultima è nnetFit_pca

#pca
roc(Medal ~ nnet_test$p4, data = nnet_test) 
#tune_grid
roc(Medal ~ nnet_test$p2, data = nnet_test) 



############## ANALISI MULTICLASSE 
#METANOTA: le prime righe sono quelle contenute nel file FINALE_SBILANCIATO.R  in cui si definiscono le variabili e il 
# numero di classi del target.

df3<-data

head(df3)

#ELIMINO ID 
df3$ID<-NULL
status=df_status(df3, print_results = F)


#TRASFORMO YEAR IN INTEGER
df3$Year<-as.integer(df3$Year)

#TRASFORMO I character IN FACTOR:

filter(status,type=='character')%>%select(variable)
df3$City<-as.factor(df3$City)
df3$Sport<-as.factor(df3$Sport)
df3$Event<-as.factor(df3$Event)
df3$Country<-as.factor(df3$Country)

status=df_status(df3, print_results = F)


#TRASFORMO MEDAL IN FACTOR
unique(df3$Medal)

df3$Medal<-recode(df3$Medal, "'Gold'='r3'; 'Bronze'='r2'; 'Silver'='r1'; NA='r0'", as.factor=TRUE) #binarizza e fattorizza il target
status=df_status(df3, print_results = F)

#SUDDIVIDO PER SPORT
lista2<-list()

for ( i in 1: length(unique(df3$Sport))){
  lista2[[i]]<-filter(df3,Sport==unique(df3$Sport)[i])
}

#####################################################################################################

library(dplyr)
library(caret)

# dalla lista creata nell'analisi presedente scelgo lo stesso dataset (49) ma con target a 4 classi:

df1<-lista2[[49]]
levels(df1$Medal)

# STATUS
status=df_status(df1, print_results = F)


#SELEZIONE DEL TIPO DI VARIABILI

Fac2=filter(status,  type %in% c("factor", "character","ordered-factor")) %>% .$variable

Quant2=filter(status,  type %in% c("numeric", "integer")) %>% .$variable

Fac2
Quant2

# salvo in un dataframe
FAc2_df=select(df1, one_of(Fac2))
Quant2_df=select(df1, one_of(Quant2))


#MODELLAZIONE: UTILIZZO UNA STRATEGIA DIVERSA: prima dummizzo e poi escludo le variabili meno importanti.

#DUMMIZZO TUTTI I FACTOR ( meno il target)
dmy = dummyVars(" ~ .", data = FAc2_df[-6], fullRank=TRUE) #permette di dummizzare le variabili factor

predictors_dfC_dummy = data.frame(predict(dmy, newdata = FAc2_df[-6])) #costruzione del dataset con le dummy
head(predictors_dfC_dummy)

# aggiungo variabili quantitative e target
total=cbind(Medal=FAc2_df$Medal, Quant2_df, predictors_dfC_dummy) 

dim(df1)
dim(total) 
head(names(total),10)
#il target è al primo posto: di seguito le 4  var quantitative.

#dataset sparso
total[1:5,6:20] 

dim(total)
#463,1071
# le righe sono atleti
total= total[complete.cases(total),]
dim(total)
#410 1071


table(total$Medal) 
prop.table(table(total$Medal))
# imbalance?


# molti predittori ... analisi accurate su correlazioni e predittori di varianza quasi nulli
y=total$Medal

#variabili importanti
table(total$City.Atlanta) 
#???

#problemi con gli zeri ?
str(total)

# zero variance
nzv = nearZeroVar(total[,-1] , saveMetrics = TRUE) 

head(nzv[order(nzv$zeroVar, decreasing = TRUE), ], n = 10)

# quasi tutte zero variance
table(nzv$zeroVar)
#???

#variabili importanti
table(total$City.Atlanta) 
#???

#variabili con soli zeri
status<-df_status(total,print_results = F)
filter(status,p_zeros==100)%>%select(p_zeros)%>%summarise(n())%>%sum()

#perchè la dummizzazione dovrebbe creare colonne con soli zeri?
table(total$City.Atlanta) 

#conto i livelli dei factor: la loro somma ( escluso un livello per ogni factor:-8) dovrebbe dare il numero di 
#colonne  del dataframe total (escluso  le varibili non factor e il target: +5 ) 
livelli_factor<-sum(length(levels(FAc2_df$Season))+length(levels(FAc2_df$City
))+length(levels(FAc2_df$Sport
))+length(levels(FAc2_df$Event
))+length(levels(FAc2_df$Country
))+length(levels(FAc2_df$Continent
))+length(levels(FAc2_df$Sub.region
  ))+length(levels(FAc2_df$Sex)))-8+5

#differenza
livelli_factor-dim(total)[2]
#quindi la dummizzazione ha creato il giusto numero di colonne, ma:

# ma nel sotto-dataset che analizzo c'è Atlanta fra le variabili City?->si 
levels(df1$City)
#si

# ma nel sotto-dataset che analizzo c'è Atlanta fra le variabili City?->NO 
city<-describe(df1$City)
city$values$value
#"Chamonix"       "Nagano"         "Salt Lake City" "Sochi"          "Torino"         "Vancouver" 

# la suddivisione in sotto-dataset rispetto agli sport è insoddisfacente: ciascun sottodataset si porta dietro 
# i livelli di factor del dataset origiario (nel senso che la dummizzazione li prende in considerazione): escludendo  i zero 
# variance risolvo comunque il problema:le variabili NON facenti parte del sottodataset vengono ovviamente 
# dummmizzate in zerovariance ( tutte con valore zero)


#elimino le zero variance: 
nzvt=data.frame(nzv)
nzvt$var=row.names(nzvt)
head(nzvt)

#escludo le var zero variance
nzvt=filter(nzvt, zeroVar=="FALSE")
dim(nzvt)


#-> siccome ci interessano i row names di nzvt procediamo con la trasposizione 
tt=t(nzvt$var) 

#seleziono le variabili nel dataset
total2=total[, tt]
str(total2)
dim(total2)


# correlazioni X
cor=cor(total2[,-1])
#seleziona solo le variabili con una correlazione >= 0.9
correlatedPredictors = findCorrelation(cor, cutoff = 0.90, names = TRUE) 
correlatedPredictors
#le escludo 

total2$Continent.Americas=NULL         
total2$Continent.Asia=NULL              
total2$Sex.F =NULL                      
total2$Event.Curling.Men.s.Curling=NULL
total2$Sub.region.Eastern.Europe=NULL   
total2$Continent.Oceania=NULL           
total2$Sub.region.Southern.Europe=NULL


#df finale
dff=cbind(y,total2) 

dim(dff)

table(dff$y)
class(dff$y)
colnames(dff)


# split in training e test
set.seed(107)
TrainIndex<-createDataPartition(dff$y ,p=0.7,list=FALSE)
training<-dff[TrainIndex,]
test = dff[-TrainIndex,]

#controllo che siano bilanciati in modo simile:
prop.table(table(training$y))
prop.table(table(test$y))
#ok

########## RF per la scelta delle variabili

#la radice quadrata del numero di predittori è il gold standard per il numero di variabili 
# in ciascun albero per il random forest 
mtry <- floor(sqrt(ncol(training))) 
mtry

set.seed(7)
control <- trainControl(method="cv", number=10, classProbs = TRUE, search="grid")
metric <- "Accuracy"
tunegrid <- expand.grid(.mtry=c(2:10))# intorno di 5
rf_gridsearch <- train(y~., data=training, method="rf", metric=metric, trControl=control, tuneGrid=tunegrid)

rf_gridsearch

res=rf_gridsearch$results
resfin=rf_gridsearch$finalModel
vimp=varImp(rf_gridsearch)
impVar_esco <- data.frame(varImp(rf_gridsearch)$importance)
dim(impVar_esco)
#30 variabili importanti-> praticamente tutte: la scelta fatta sulla matrice iniziale era sufficiente. 
# tra l'altro il modello rf ha stimato un'accuracy di 0.88 con mtry 10: vado avanti con nnet per completezza.

res
ls(resfin) 
resfin$importance 

# nomi delle variabili importanti
impVar_esco$var=row.names(impVar_esco)
head(impVar_esco)
nrow(impVar_esco)

#plot delle var importanti
dotchart(impVar_esco$Overall,labels=impVar_esco$var,cex=.8, xlab="importance", color=2)
# per il Curling è molto importante essere nati in Canada e non essere sovrappeso!


#var poco importanti
impVar_esco_n = impVar_esco%>%arrange(Overall)%>% filter(Overall<4)
dotchart(impVar_esco_n$Overall,labels=impVar_esco_n$var,cex=.6, xlab="importance", color=6)
# in Italia si pensa solo al calcio!


# seleziono solo var con importanza almeno il 10% della var più importante
impVar_esco_s=filter(impVar_esco, Overall>10) 

nrow(impVar_esco)
nrow(impVar_esco_s) #20


# con i nomi le seleziono dal dataset 
tt<-t(impVar_esco_s$var)
train=training[ ,tt]
dim(train)

#aggiungo target
train$y=training$y
dim(train)

names(train)

################################################## NEURAL NETWORK ################################################
set.seed(7)
metric <- "Accuracy"
ctrl = trainControl(method="cv", number=10)
tunegrid <- expand.grid(size=c(5:8), decay = c(0.001, 0.02, 0.05 , .1))# utilizzo i parametri della precedente analisi
nnetFit_esco_ms <- train(y~. , train,
                          method = "nnet",
                          tuneLength = 2, metric=metric, trControl=ctrl, tuneGrid=tunegrid,
                          trace = T,
                          maxit = 500)#converge con 500 (contando con intervalli di 100)

nnetFit_esco_ms
getTrainPerf(nnetFit_esco_ms) 
# size = 7  and decay = 0.02 accuracy: 0.93
nnetFit_esco_ms$results
nnetFit_esco_ms$finalModel
varImp(nnetFit_esco_ms)

# results on test set
predictions<-predict(nnetFit_esco_ms,test)
confusionMatrix(test$y, predictions) 
# buono

############################ classificatore BAYESIANO ###############à

################### TARGET BINARIO

# SCELGO LO STESSO sotto-dataset

dfby<-lista[[49]]

library(klaR)



# split train in training e test
set.seed(107)
TrainIndex<-createDataPartition(dfby$Medal ,p=0.7,list=FALSE)
train<-dfby[TrainIndex,]
test = dfby[-TrainIndex,]

dim(train)

str(train)

#  pre senza di NA
staus<-df_status(train)

# imputo con la media
train$Weight=impute(train$Weight,mean)
train$Height=impute(train$Height,mean)

#zero variance
paste(names(train),collapse = "','")

cross_plot(train, input = c('Sex','Age','Height','Weight','Year','Season','City','Sport','Event','Medal','Country','Continent','Sub.region'), target = "Medal", plot_type = "percentual",auto_binning=TRUE)
# sport e season vanno eliminati


#var zero variance
nzv = nearZeroVar(train[,-10] , saveMetrics = TRUE)
nzv[order(nzv$zeroVar, decreasing = TRUE), ]

#elimino var zero variance
train$Season<-NULL
train$Sport<-NULL
staus<-df_status(train)


# modello con Klar
naive_all <- NaiveBayes(Medal ~ ., data = train, usekernel = FALSE, laplace = 50, na.action=na.pass)


naive_all$tables 
pred <- predict(naive_all, train, type="class")

ls(pred)

head(pred$class)
head(pred$posterior)

# confusion matrix on train
table(pred=pred$class, true=train$Medal)
confusionMatrix(pred$class,train$Medal)

#confusion sul test ( che non c'è)
pred <- predict(naive_all, test, type="class")
confusionMatrix(pred$class,test$Medal)

#acc: 081 e bassa specificity.

######################################## BAYES MULTICLASSE
#SCELGO LO STESSO DATASET DELL'ANALISI PRECEDENTE MA CON TARGET QUADRUPLO


dfby2<-lista2[[49]]


# split train in training e test
set.seed(107)
TrainIndex<-createDataPartition(dfby2$Medal ,p=0.7,list=FALSE)
train2<-dfby2[TrainIndex,]
test2 = dfby2[-TrainIndex,]

dim(train2)

str(train2)


staus<-df_status(train2)

# imputo con la media
train2$Weight=impute(train2$Weight,mean)
train2$Height=impute(train2$Height,mean)


#var zero variance
nzv = nearZeroVar(train2[,-10] , saveMetrics = TRUE)
nzv[order(nzv$zeroVar, decreasing = TRUE), ]

#elimino var zero variace
train2$Season<-NULL
train2$Sport<-NULL
staus<-df_status(dfbc)


naive_all <- NaiveBayes(Medal ~ ., data = train2, usekernel = FALSE, laplace = 50, na.action=na.pass)


naive_all$tables 
pred <- predict(naive_all, train2, type="class")

ls(pred)

head(pred$class)
head(pred$posterior)

# confusion matrix on train
table(pred=pred$class, true=train2$Medal)
confusionMatrix(pred$class,train2$Medal)

#confusion matrix sul test
pred <- predict(naive_all, test2, type="class")
confusionMatrix(pred$class,test2$Medal)

#accuracy 0.7 e predizioni sbilanciate a favore della classe più numerosa

prop.table(table(dfby2$Medal))
#inevitabilmente suddividendo la classe minoritaria del target lo sbilanciamento aumenta.

