options(scipen = 999)
library(dplyr)
library(funModeling)
library(car)
library(VIM)
library(caret)
library(pROC)
library(ROCR)

load("input/complete.RData")

df<-data

head(df)

# tipi di variabili
status=df_status(df, print_results = F)
status

#ELIMINO ID 
df$ID<-NULL
status=df_status(df, print_results = F)

#TRASFORMO YEAR IN INTEGER
df$Year<-as.integer(as.character(df$Year))

#TRASFORMO I character IN FACTOR:

filter(status,type=='character')%>%select(variable)
df$City<-as.factor(df$City)
df$Sport<-as.factor(df$Sport)
df$Event<-as.factor(df$Event)
df$Country<-as.factor(df$Country)



#TRASFORMO MEDAL IN TARGET BINARIO 
unique(df$Medal)
#binarizza e fattorizza il target
df$Medal<-recode(df$Medal, "'Gold'='r1'; 'Bronze'='r1'; 'Silver'='r1'; NA='r0'", as.factor=TRUE) 
status=df_status(df, print_results = F)
status

#presenza di NA
head(status %>% select(variable, type, unique, q_na,p_na) %>% arrange(-q_na,p_na,type))
# in % pochi NA: ma tenere d'occhio.
# NESSUNO ZERO 


#SELEZIONE DEL TIPO DI VARIABILI

Fac2=filter(status,  type %in% c("factor", "character","ordered-factor")) %>% .$variable

Quant2=filter(status,  type %in% c("numeric", "integer")) %>% .$variable

Fac2
Quant2

# salvo in un dataframe
FAc2_df=select(df, one_of(Fac2))
Quant2_df=select(df, one_of(Quant2))

#salvo i nomi di vars come vettore-dataframe
NAmesFac=filter(status,  type %in% c("factor", "character")) %>% select(variable)
NAmesQuant=filter(status,  type %in% c("integer", "numeric")) %>% select(variable)

NAmesFac
NAmesQuant

#IMBALANCE??--->
table(df$Medal)
prop.table(table(df$Medal))

#---> SI 

#RAPPRESENTAZIONE DEI NA:

vars_to_keep=filter(status, p_na ==0)  %>% .$variable
vars_to_keep
data_subset3=select(df, -one_of(vars_to_keep))

# plot delle sole variabili mancanti
missingness<- aggr(data_subset3, col=c('navyblue','yellow'), numbers = TRUE, sortVars = TRUE, 
                   labels = names(data_subset3), cex.axis = .7, gap = 2)

#descrizione dei dati
psych::describe(df)


#decrizione delle variabili numeriche
profiling_num(df)

# salvo in un dataframe
summary=data.frame(profiling_num(df))


# Plot di variabili numeriche
plot_num(df)


#plot di tutte le variabili
freq(data=df)
# Skipping plot for variable 'Event' (more than 100 categories)
# Skipping plot for variable 'Country' (more than 100 categories)

Formula_quant <- paste(names(df[-10]), collapse="','")
Formula_quant# copia e incolla sotto
cross_plot(df, input = c( 'Sex','Age','Height','Weight','Year','Season','City','Sport','Event','Country','Continent','Sub.region'), 
           target = "Medal", plot_type = "percentual", auto_binning = TRUE)
#METANOTA: Skipping "Sport" variable: more than 50 unique values.


# boxplot delle var quantitative vs target binario

Formula1 <- paste(colnames(Quant2_df), collapse="','")
Formula1
vars_to_analyze=c( 'Age','Height','Weight','Year')
plotar(data = df, input=vars_to_analyze , target="Medal", plot_type = "boxplot")
# ci sono atleti ultrasessantenni
summary(df$Age)
#quali sport hanno praticato?
df[which(is.na(df$Sport[df$Age>70])==F),]
unique(df$Sport[which(is.na(df$Sport[df$Age>60])==F)])
#medaglie vinte?
sum(df[which(is.na(df$Sport[df$Age>70])==F),10]=='r1')
#l'importante ? bartecipare!

#densit? rispetto al target
plotar(data=df, input=vars_to_analyze , target="Medal", plot_type = "histdens")


#plot di un numero di anni minore
f<-df[df$Year>1924  & df$Year<1988,]
plotar(data=f, input=vars_to_analyze , target="Medal", plot_type = "histdens")
#si vede che la crescita del numero di medaglie assegnate (r1) ? costante.
#Le medaglie non assegnate (r1) variano in modo meno regolare: segno che cambia il 
# numero di partecipanti: in particolare durante la 2? guerra mondiale (Year=10) e 
# nell'olimpiadi del 1980 in URSS (Year=20), anno del boicottaggio americano.

############### MODELLAZIONE:IMBALANCE #####################

#SUDDIVIDO IL DATASET PER SPORT:

lista<-list()

for ( i in 1: length(unique(df$Sport))){
  lista[[i]]<-filter(df,Sport==unique(df$Sport)[i])
}

head(lista[[1]])

#NUMERO DI RIGHE DEI SINGOLI DATASET
lapply(lista, function(x) dim(x)[1])
for(i in 1:length(lista)){
  print(dim(lista[[i]])[1])
}

#SPORT ASSOCIATI A DATASET CON MENO DI 10 RIGHE ( DA ELIMINARE)
for(i in 1:length(lista)){
  if(dim(lista[[i]][1])<10){ print(describe(lista[[i]]$Sport)$values)}}

#ROQUE, BASQUE PELOTA, AERONAUTICS

# sono i dataset numero:
for(i in 1:length(lista)){
  if(dim(lista[[i]][1])<10){print(head(i,1))}
}

#62 64 65 


# I SINGOLI DATASET SONO ANCORA AFFETTI DA SBILANCIAMENTO?

lapply(lista, function(x) freq(x$Medal))
for(i in 1: length(lista)){
  freq(lista[[i]]$Medal)
}

lapply(lista, function(x) prop.table(table(x$Medal)))
for(i in 1:length(lista)){
  print(prop.table(table(lista[[i]]$Medal)))
}

# ALCUNI SICURAMENTE SI: SONO NECESSARIE 2 ANALISI DISTINTE:
# UNA CHE PRENDA IN CONSIDERAZIONE SOLO DATASET SBILANCIATI.


#STATUS DEI SINGOLI DATASET

length(lista)
#metto i singoli status in coda a partire da lista[[100]]: lo status del datafrane 3 sar? lista[[103]]

for(i in 1:length(lista)){
  lista[[i+100]]<-df_status(lista[[i]],print_results=F)
}



################################################

#METANOTA: COMINCIO AD ANALIZZARE lista[[2]]->sbilanciato

# ESCLUDO LA VARIABILE event: non d? nessuna informazione aggiuntiva.


df2<-lista[[6]]

df2$Event<-NULL

status<-df_status(df2)

#SPORT?
describe(df2$Sport)


#imbalance??
table(df2$Medal)
prop.table(table(df2$Medal))
#si


# imputo i dati mancanti
status<-df_status(df2)
status

# NON IMPUTO
df2 <- na.omit(df2)

df2$Age=impute(df2$Age, mean)  # funzione: impute()
df2$Weight=impute(df2$Weight,mean)
df2$Height=impute(df2$Height,mean)
dim(df2)
#  9133   12
# 7529 12 senza imputare ma con rimozione missings

#ELIMINO VARIABILI FACTOR CON UN UNICO LIVELLO
filter(status,type=='factor' & unique==1)%>%select(variable)
df2$Season<-NULL
df2$Sport<-NULL
status<-df_status(df2)

#nearZeroVariance
nearZeroVar(df2[,-7] , saveMetrics = TRUE) 
#OK

# split train in training e test
set.seed(107)
TrainIndex<-createDataPartition(df2$Medal ,p=0.7,list=FALSE)
training<-df2[TrainIndex,]
test = df2[-TrainIndex,]

#controllo che siano bilanciati in modo simile:
prop.table(table(training$Medal))
prop.table(table(test$Medal))
#ok


# MODELLO: rpart -> ALBERO

#accuracy
set.seed(1)
ctrl <- trainControl(method = "cv" , number=10)
rpartTune <- train(Medal ~ ., data = training, method = "rpart", 
                   tuneLength = 15,trControl=ctrl)
rpartTune
getTrainPerf(rpartTune)
#SEMBRA ANDARE BENE

#VARIABILI IMPORTANTI
vim=as.data.frame(rpartTune$finalModel$variable.importance)
vim
# vettore delle varibili importanti
vimname=row.names(vim)
vimname



# matrice di confusione sul train
PredT <- predict(rpartTune, training)
confusionMatrix(PredT, training$Medal) 
#DIFFIDIAMO DALLA METRICA -> DATI SBILANCIATI
# bassa specificit? che,associando r0 alla classe della NON conquista di medaglie, corrisponde
# a una bassa capacit? di prevedere la classe con numerosit? minore (r1) che ? la classe sulla quale
# vogliamo fare previsioni.

# test set
pred=predict(rpartTune,newdata=test)
confusionMatrix(pred, test$Medal) 
#peggio nel test set 


# cambio tuning con una metrica pi? appropriata:ROC 
set.seed(1)
ctrl <- trainControl(method = "cv" , number=10, summaryFunction = twoClassSummary , classProbs = TRUE) 
rpartTune2 <- train(Medal ~ ., data = training, method = "rpart", 
                    tuneLength = 15,trControl=ctrl)

getTrainPerf(rpartTune2)
#valori bassi 

# predizione sul test set
pred=predict(rpartTune2,newdata=test)
confusionMatrix(pred, test$Medal)
# specificit? ancora bassa

# roc sul test
pred_prob=predict(rpartTune2,newdata=test,type=c("prob"))
head(pred_prob)
# prob di NON vincere la medaglia
pred_prob=predict(rpartTune2,newdata=test,type=c("prob"))[,1]
head(pred_prob)
# valori roc
roc(Medal ~ pred_prob, data = test) 
#METANOTA: AREA UNDER THE CURVE:ACRONIMO AUC
# 0.5 < AUC < 0.7 --> TEST POCO ACCURATO--> 


#provo knn: utilizzo le varibili selezionate dall'albero (prima dummizzo)

#vim
vimname

#[1] "CountryRussia"             "Sub.regionNorthern Europe" "Year"                      "CountrySweden"             "CountryNorway"            
#[6] "CountryFinland"            "Height"                    "CitySarajevo"              "Age"                       "CountryEstonia"           
#[11] "Weight"                    "CityGrenoble"              "CityInnsbruck"             "CountryUK"                 "CityCalgary"              
#[16] "SexF"                      "CityLake Placid"           "CitySapporo"             


#dummizzazione character e dataframe


#dataframe per knn
knn_train<-data.frame(CountryRussia = ifelse(training$Country == "Russia", 1, 0),
                      Sub.regionNorthernEurope=ifelse(training$Sub.region=="Europe", 1,0),
                      Year=training$Year,CountrySweden=ifelse(training$Country=="Sweden", 1,0),
                      CountryNorway=ifelse(training$Country=="Norway", 1,0),
                      CountryFinland=ifelse(training$Country=="Finland", 1,0),
                      CountrySweden=ifelse(training$Country=="Sweden", 1,0),
                      Height=training$Height,CitySarajevo=ifelse(training$City=="Sarajevo", 1,0),
                      Age=training$Age,CountryEstonia=ifelse(training$Country=="Estonia", 1,0),
                      Weight=training$Weight,CityGrenoble=ifelse(training$City=="Grenoble", 1,0),
                      CitySarajevo=ifelse(training$City=="Sarajevo", 1,0),
                      CountryUK=ifelse(training$Country=="UK", 1,0),
                      CityTorino=ifelse(training$City=="Torino", 1,0),
                      "CitySalt Lake City"=ifelse(training$City=="Salt Lake City", 1,0),
                      CityVancouver=ifelse(training$City=="Vancouver", 1,0),
                      SexF=ifelse(training$Sex=="F", 1,0),
                      CitySochi=ifelse(training$City=="Sochi", 1,0),
                      CitySapporo=ifelse(training$City=="Sapporo", 1,0),
                      Medal=training$Medal)
knn_test<-data.frame(CountryRussia = ifelse(test$Country == "Russia", 1, 0),
                     Sub.regionNorthernEurope=ifelse(test$Sub.region=="Europe", 1,0),
                     Year=test$Year,CountrySweden=ifelse(test$Country=="Sweden", 1,0),
                     CountryNorway=ifelse(test$Country=="Norway", 1,0),
                     CountryFinland=ifelse(test$Country=="Finland", 1,0),
                     CountrySweden=ifelse(test$Country=="Sweden", 1,0),
                     Height=test$Height,CitySarajevo=ifelse(test$City=="Sarajevo", 1,0),
                     Age=test$Age,CountryEstonia=ifelse(test$Country=="Estonia", 1,0),
                     Weight=test$Weight,CityGrenoble=ifelse(test$City=="Grenoble", 1,0),
                     CitySarajevo=ifelse(test$City=="Sarajevo", 1,0),
                     CountryUK=ifelse(test$Country=="UK", 1,0),
                     CityTorino=ifelse(test$City=="Torino", 1,0),
                     "CitySalt Lake City"=ifelse(test$City=="Salt Lake City", 1,0),
                     CityVancouver=ifelse(test$City=="Vancouver", 1,0),
                     SexF=ifelse(test$Sex=="F", 1,0),
                     CitySochi=ifelse(test$City=="Sochi", 1,0),
                     CitySapporo=ifelse(test$City=="Sapporo", 1,0),
                     Medal=test$Medal)

#nzv
(nzv<-nearZeroVar(knn_train[,-19] , saveMetrics = TRUE))
#ci sono zero variance
nzv_toremove <- nzv[which(nzv$zeroVar == T | nzv$nzv == T),] %>% rownames()
knn_train <- select(knn_train, -one_of(nzv_toremove))
knn_test <- select(knn_test, -one_of(nzv_toremove))
set.seed(1)
ctrl <- trainControl(method = "cv", number=10 , savePredictions=T,search="grid", summaryFunction = twoClassSummary , classProbs = TRUE)
tunegrid <- expand.grid(k=c(15, 30, 45))
knn <- train(Medal~., data=knn_train, method="knn",  
             tuneGrid=tunegrid, trControl=ctrl, tuneLength=10,
             preProcess = c("center", "scale")) #scalare e centrare covariate -> preprocessing KN

knn
getTrainPerf(knn) 
#pessima performance anche knn

pred=predict(knn,newdata=knn_test)
confusionMatrix(pred, knn_test$Medal) #quasi tutti r0


# anche con  glm
set.seed(1)
ctrl <- trainControl(method = "cv" , number=10, summaryFunction = twoClassSummary , classProbs = TRUE) 
glm <- train(Medal ~ ., data = training, method = "glm", 
             trControl=ctrl)

getTrainPerf(glm)
# 
predglm=predict(glm,newdata=test)
confusionMatrix(predglm, test$Medal)#solo r0:specificit?=0
#
# IL PROBLEMA E' LA SOGLIA (la probabilit?: che va tunata in funzione della grandezza d'interesse, in questo caso: specificit?)

####################
#predict
#true   r0     r1
#r0     a      b
#r1     c      d   spec=d/(c+d)   == TN / (FP + TN)
################

# utilizzo l'albero con la metrica d'interesse: specificit?.


set.seed(1)
ctrl <- trainControl(method = "cv" , number=10, summaryFunction = twoClassSummary , classProbs = TRUE) 
rpartTune3 <- train(Medal~ ., data = training, method = "rpart", 
                    tuneLength = 15,trControl=ctrl,metric="Spec")

getTrainPerf(rpartTune3)
pred=predict(rpartTune3,newdata=test)
table(pred,test$Medal)
confusionMatrix(pred, test$Medal)

# SPECIFICITA' BASSA:

# le soluzioni dipendono dalla regola classificativa usata per predire il target

# VALUTAZIONE DELLA SOGLIA CHE DEFINISCE LA CLASSIFICAZIONE IN FUNZIONE DELLA SPECIFICITA' DESIDERATA:

predProbT=predict(rpartTune3,newdata=training,type="prob")
head(predProbT) 
# r0  #PROBABILITA' DI NON AVERE MEDAGLIE
predProbT=predict(rpartTune3,newdata=training,type="prob")[,1]
head(predProbT)


# convenzioni del pacchetto ROCR:
# y=1 ? r0,  y=0 ? r1
y=training$Medal
y=ifelse(y=="r1",0,1) 
head(y)

# oggetto che ci fornisce le soglie
predR <- prediction(predProbT,y)

# roc curve
roc.perf = performance(predR, measure = "tpr", x.measure = "fpr")#tpr:True positive rate;fpr:False positive rate
plot(roc.perf,col=6)
abline(a=0, b= 1,col=2)

roc.perf
#alpha.values: la soglia
#"y.values": PERCENTUALE DI VERI POSITIVI
#"x.values" : PERCENTUALE DI FALSI POSITIVI-> 1-SPECIFICITA'->1- VERI NEGATIVI 

# per come ? impostato il problema se voglio predire la classe minoritaria ( vincitori di medaglie)
# ho bisogno di specificit? alta: scelta della soglia:


#scelgo cutoff:0.98 a cui corrisponde una specificit? di 1-0.1379=0.86

# r0 #PROBABILITA' DI NON AVERE UNA MEDAGLIA
predP=predict(rpartTune3,newdata=training,type="prob")[,1]
#nuoVa regola classificativa 
predy=ifelse(predP>0.98, "r0","r1") 
preddy<-as.factor(predy)
confusionMatrix(preddy, training$Medal)


# in altre parole, ? sufficiente che la prob sia 0.02 per prevedere di vincere medaglie (r1)

#LA SOGLIA E' A SCALINO: BASTA AUMENTARLA DI 0.002 PER AUMENTARE NOTEVOLMENTE LA SPECIFICITA' E CROLLARE L'ACCURACY:
predP=predict(rpartTune3,newdata=training,type="prob")[,1]
predy=ifelse(predP>0.982, "r0","r1") 
preddy<-as.factor(predy)
confusionMatrix(preddy, training$Medal)
# specificity: 0.9982

# test set
predP=predict(rpartTune3,newdata=test,type="prob")[,1]
#predy=ifelse(predP>0.982, "r0","r1")
predy=ifelse(predP>0.935, "r0","r1") # Sensitivity : 0.786
                                     # Specificity : 0.820 

preddy2<-as.factor(predy)
confusionMatrix(preddy2, test$Medal)
# Specificity ? aumentata a  0.9526 

#plot delle performance delle altre misure
acc.perf = performance(predR, measure = "acc")
plot(acc.perf,col=6)

spec.perf = performance(predR, measure = "spec")
plot(spec.perf,col=6)

sens.perf = performance(predR, measure = "sens")
plot(sens.perf,col=6)


#precisione
prec.perf = performance(predR, measure = "prec")
plot(prec.perf,col=6)

#dataset con soglie
cut=as.data.frame(acc.perf@x.values)
colnames(cut)="cut"
head(cut)

spec=as.data.frame(spec.perf@y.values)
colnames(spec)="spec"

sens=as.data.frame(sens.perf@y.values)
colnames(sens)="sens"

acc=as.data.frame(acc.perf@y.values)
colnames(acc)="acc"

prec=as.data.frame(prec.perf@y.values)
colnames(prec)="prec"

all=cbind(cut, spec, sens, acc,prec)

head(all)
dim(all)
51*5


library(reshape2)
metrics <- melt(all, id.vars = "cut", 
                variable.name = "Measure_type",
                value.name = "Measure") #impilo dati verticalmente 
head(metrics)
dim(metrics)

# plot to a better diagnostic choice
ggplot(metrics, aes(x = cut, y = Measure, color = Measure_type)) + 
  geom_line(size=1) + 
  ylab("") + xlab("Probability Cutoff") +
  theme(legend.position = "top")
#
#METANOTA: IL PUNTO E' MOLTO PROSSIMO A 1 PER TUTTE LE MISURE:oltre questa soglia tutte le 
# grandezze accentuano l'inversione nel senso della loro importanza a favore della classe minoritaria.


############################## LIFT E GAIN CHART


copy=training
#cambio  L'evento => PROBABILITA' DI VINCERE UNA MEDAGLIA
copy$predP=predict(rpartTune3,newdata=copy,type="prob")[,2] 

library(funModeling)
gain_lift(data = copy, score = 'predP', target = 'Medal') 


#il 10% dei nostri valori previsti di r1 = VITTORIA MEDAGLIA(cumulativo!), cattura il 60,9% del totale di r1
#il 20% -> 81,4%


# torno all'evento precedente => PROBABILITA' NON VINCERE UNA MEDAGLIA
copy$predP=predict(rpartTune3,newdata=copy,type="prob")[,1]
gain_lift(data = copy, score = 'predP', target = 'Medal')

# NEL GRAFICO SI VEDE CHE IL CLASSIFICATORE E' PESSIMO, RISPETTO A UN CLASSIFICATORE RANDOM (DIAGONALE), NEL PREVEDERE
#L'APPARTENENZA ALLA CLASSE  r0-> IL FATTO E'GIUSTIFICATO CON LA SCELTA DI AVERE UN BUON CLASSIFICATORE PER LA CLASSE
# MENO NUMEROSA (TARGET SBILANCIATO) OSSIA PER r1
###############################################################################################


#OLTRE ALLA SCELTA DELLA SOGLIA E' POSSIBILE UTILIZZARE UNA MATRICE DEI COSTI /PROFITTI
#MATRICE DEI PROFITTI
# evento = L'ATLETA SU CUI HO SCOMMESSO HA VINTO LA MEDAGLIA:
####profit matrix################
predict
true good   bad
good    1    0   
bad   -10     0
################

# ogni 10 euro scommessi ne guadagno 1 in caso di giiusta predizione o li perdo tutti e 10 in caso contrario.

optimal_bayesian_threshold=10/(10+1)  
optimal_bayesian_threshold 
#0.91-> soglia da inserire per classificare



###########DATI MULTICLASSE######################################################


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

#SCELGO LO STESSO DATASET DELLA PRIMA PARTE DELL'ANALISI

df4<-lista2[[6]]

#ELIMINO EVENT : INUTILE 

df4$Event<-NULL

psych::describe(df4)

#imbalance??
table(df4$Medal)
prop.table(table(df4$Medal))
#si


# imputo i dati mancanti
status<-df_status(df4)
# NON IMPUTO
df4 <- na.omit(df4)
df4$Age=impute(df4$Age, mean)  # funzione: impute()
df4$Weight=impute(df4$Weight,mean)
df4$Height=impute(df4$Height,mean)
dim(df4)
# 9133   12
# 7529   12

prop.table(table(df4$Medal))


#ELIMINO VARIABILI FACTOR CON UN UNICO LIVELLO
filter(status,type=='factor' & unique==1)%>%select(variable)
df4$Season<-NULL
df4$Sport<-NULL
status<-df_status(df4)

#nearZeroVariance
nearZeroVar(df4[,-7] , saveMetrics = TRUE) 

# Variabili che secondo me sono inutili
vars.to.remove <- c("Year", "City", "Continent", "Country") # Si puo' sostituire country con sub.region
df4 <- select(df4, -one_of(vars.to.remove))
set.seed(107)
TrainIndex<-createDataPartition(y=df4$Medal,p=0.7,list=FALSE)

train<-df4[TrainIndex,]
test = df4[-TrainIndex,]



#UTILIZZO KNN MA PRIMA SELEZIONO LE VARIABILI PIU' IMPORTANTI CON UN ALBERO E DUMMIZZO LE character.
set.seed(1)
ctrl <- trainControl(method = "cv" , number=10, summaryFunction = multiClassSummary , classProbs = TRUE) 
rpartTune4 <- train(Medal~ ., data = train, method = "rpart", 
                    tuneLength = 15,trControl=ctrl,metric="Spec")

getTrainPerf(rpartTune4)
pred=predict(rpartTune4,newdata=test)
table(pred,test$Medal)
confusionMatrix(pred, test$Medal)

#VAR IMPORTANTI
Vimportance <- varImp(rpartTune4, scale=TRUE)
Vimportance


# SELEZIONE DELLE VARIABILI E DATAFRAME
vim4=as.data.frame(rpartTune4$finalModel$variable.importance)
head(vim4)
vimname4=row.names(vim4)
vimname4
    
#[1] "CountryRussia"         "Year"                  "Height"                "CitySarajevo"          "Weight"                "CityInnsbruck"        
#[7] "Age"                   "CityCalgary"           "CityLake Placid"       "CitySquaw Valley"      "CityGrenoble"          "CityCortina d'Ampezzo"


CountryRussiaTr=ifelse(train$Country=="Russia", 1,0)
YearTr=train$Year
AgeTr=train$Age
CitySarajevoTr=ifelse(train$City=="Sarajevo", 1,0)
CityInnsbruckTr=ifelse(train$City=="Innsbruck", 1,0)
CityCalgaryTr=ifelse(train$City=="Calgary", 1,0)
CityLakePlacidTr=ifelse(train$City=="Lake Placid", 1,0)
CitySquawValleyTr=ifelse(train$City=="Squaw Valley", 1,0)
CityCortinadAmpezzoTr=ifelse(train$City=="Cortina d'Ampezzo", 1,0)
CityGrenobleTr=ifelse(train$City=="Grenoble", 1,0)
HeightTr=train$Height
WeightTr=train$Weight

CountryRussiaTs=ifelse(test$Country=="Russia", 1,0)
YearTs=test$Year
AgeTs=test$Age
CitySarajevoTs=ifelse(test$City=="Sarajevo", 1,0)
CityInnsbruckTs=ifelse(test$City=="Innsbruck", 1,0)
CityCalgaryTs=ifelse(test$City=="Calgary", 1,0)
CityLakePlacidTs=ifelse(test$City=="Lake Placid", 1,0)
CitySquawValleyTs=ifelse(test$City=="Squaw Valley", 1,0)
CityCortinadAmpezzoTs=ifelse(test$City=="Cortina d'Ampezzo", 1,0)
CityGrenobleTs=ifelse(test$City=="Grenoble", 1,0)
HeightTs=test$Height
WeightTs=test$Weight

#
ForTr <- paste(vimname4, collapse="=, ")
ForTr


#dataframe per knn
knn_train4<-data.frame(CountryRussia=CountryRussiaTr, Year=YearTr, Height=HeightTr, CitySarajevo=CitySarajevoTr, Weight=WeightTr, CityInnsbruck=CityInnsbruckTr, Age=AgeTr, CityCalgary=CityCalgaryTr, CityLakePlacid=CityLakePlacidTr, CitySquawValley=CitySquawValleyTr, CityGrenoble=CityGrenobleTr, CityCortinadAmpezzo= CityCortinadAmpezzoTr  ,Medal=train$Medal)
knn_test4<-data.frame(CountryRussia=CountryRussiaTs, Year=YearTs, Height=HeightTs, CitySarajevo=CitySarajevoTs, Weight=WeightTs, CityInnsbruck=CityInnsbruckTs, Age=AgeTs, CityCalgary=CityCalgaryTs, CityLakePlacid=CityLakePlacidTs, CitySquawValley=CitySquawValleyTs, CityGrenoble=CityGrenobleTs, CityCortinadAmpezzo= CityCortinadAmpezzoTs,Medal=test$Medal)

set.seed(107)
ctrl <- trainControl(method = "cv", number=10, savePredictions=T,search="grid", classProbs = TRUE,
                     summaryFunction = multiClassSummary)#multiClassSummaryPERCHE' LA CLASSE TARGET HA PIU' LIVELLI(4)
tunegrid <- expand.grid(k=c(10,20,30))
knn_grid_c <- train(Medal~., data=knn_train4, method="knn",  metric = "Accuracy",
                    tuneGrid=tunegrid, trControl=ctrl,
                    preProcess = c("center", "scale"))
knn_grid_c

# confusion matrix 
y_Tr=predict(knn_grid_c,newdata=knn_train4)

confusionMatrix(y_Tr,knn_train4$Medal)

#confusion matrix sul test set
table(knn_test4$Medal, predict(knn_grid_c,knn_test4))

#PROBLEMI DI SOGLIA !!!!!!!!


# Bisogna usare una soglia diversa per classificare ogni classe
# 
probTr=predict(knn_grid_c,knn_train4, type = "prob")
head(probTr)


#REGOLA DI PREDIZIONE CHE MASSIMIZZA IL PROFITTO:


# permette di guidare la classifcazione 
profitMatrix <- ifelse(diag(4) == 1, 0, 1)# MATRICE COMPLEMENTARE DELLA MATRICE DIAGONALE
profitMatrix[4, 4] <- 100 #? 100 volte pi? profittevole classificare una medaglia d'oro
profitMatrix[3, 3] <- 70 # METANOTA: SULLA DIAGONALE CI SONO SEMPRE LE GIUSTE PREDIZIONI 
profitMatrix[2, 2] <- 20
profitMatrix[1, 1] <- 1

profitMatrix
rownames(profitMatrix) <- colnames(profitMatrix) <- levels(knn_train4$Medal)
profitMatrix 

# PE: profitto individuale di ciascuna decisione

# PE = Pronostici individuali ponderati per elemento del prodotto profitMatrix di due matrici
# I valori PE i * j sono il profitto individuale per il soggetto i della decisione in classe j:

profitMatrix=as.matrix(profitMatrix)
probTr=as.matrix(probTr)  #probTr->riga 817 p(xj)#probabilit? individuale di essere ogni classe j

EP=data.frame(probTr%*%profitMatrix) #profitti attesi per ogni soggetto # E' UN PRODOTTO MATRICIALE
head(EP)



# DOPO AVER CALCOLATO LA MATRICE EP, LA CLASSIFICAZIONE E' DECISA PRENDENDO IL MASSIMO DI OGNI RIGA: ESEMPIO : 
head(EP,5)# LA PRIMA RIGA PREVEDE CHE IL PRIMO OGGETO DI train APPARTIENE ALLA CLASSE r2; LA SECONDA RIGA DICE CHE 
# IL SECONDO OGGETTO DI train APPARTIENE ALLA CLASSE r3....

zz=data.frame(names(EP)[apply(EP, 1, which.max)] ) #formula per trovare il max [profitto atteso individuale]
colnames(zz)="predict"
# predict ? la classe target che massimizza il profitto
EP_and_decision=cbind(EP,zz)
head(EP_and_decision,n=10)

# matrice di confusione decisionale basata su questa nuova regola decisionale
table(true = knn_train4[,13], pred=EP_and_decision$predict)

# test set
probTe=predict(knn_grid_c, knn_test4, type = "prob")
probTe=as.matrix(probTe)
EP_t=data.frame(probTe%*%profitMatrix)
zzb=data.frame(names(EP_t)[apply(EP_t, 1, which.max)] )
colnames(zzb)="predict"
EP_decision_t=cbind(EP_t,zzb)

table(true = knn_test4[,13], pred=EP_decision_t$predict)

options(scipen=999)

# vedi errori di riga
prop.table(table(true = knn_test4[,13], pred=EP_decision_t$predict), margin=1)


###########################DATASET NON SBILANCIATO################

