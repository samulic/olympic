#setwd("C:/Users/Massimiliano/Desktop/PROGETTO DATA SCIENCE LAB/appunti Comotto/lezioni Lovaglio/dataLOVAGLIO")

library(dplyr)


df<- read.csv("athlete_events.csv", sep=",", dec = ".",  stringsAsFactors=TRUE, na.strings=c("NA","NaN", ""))

noc<-read.csv("noc_regions.csv", sep=",", dec = ".",  stringsAsFactors=TRUE, na.strings=c("NA","NaN", ""))

noc<-select(noc,c('NOC','region'))
  

df <- df%>% left_join(noc, by = "NOC") 

df$region<-as.factor(df$region)

head(df)

library(funModeling)

# SEE types of vars
status=df_status(df, print_results = F)
status

#TRASFORMO MEDAL IN TARGET BINARIO 
unique(df$Medal)

library(car)
df$Medal<-recode(df$Medal, "'Gold'='r0'; 'Bronze'='r0'; 'Silver'='r0'; NA='r1'", as.factor=TRUE) #binarizza e fattorizza il target

#ELIMINO ID e Name
df$ID<-NULL
df$Name<-NULL

# q_zeros: quantity of zeros (p_zeros: in percent)
# q_NA: quantity of NA (p_Na: in percent)
status<-df_status(df)

# arrange by number of na and type var 
head(status %>% select(variable, type, unique, q_na,p_na) %>% arrange(-q_na,p_na,type))
# in % pochi NA: ma tenere d'occhio.
#  NESSUNO ZERO 


#SELEZIONE DEL TIPO DI VARIABILI

Fac2=filter(status,  type %in% c("factor", "character")) %>% .$variable

Quant2=filter(status,  type %in% c("numeric", "integer")) %>% .$variable

#################METANOTA:---> 

# YEAR NON SO SE CONSIDERARLO FACTOR??
filter(status,type=='integer' | type=='numeric')
# 35 POSSIBILI LIVELLI 

##########################



Fac2
class(Fac2)
Quant2

# save them as dataframe
FAc2_df=select(df, one_of(Fac2))
Quant2_df=select(df, one_of(Quant2))

#######################################################
# Può essere utile salvare i nomi di vars come vettore-dataframe
NAmesFac=filter(status,  type %in% c("factor", "character")) %>% select(variable)
NAmesQuant=filter(status,  type %in% c("integer", "numeric")) %>% select(variable)

NAmesFac
NAmesQuant
####################################################
#  IMBALANCE??--->
table(df$Medal)
prop.table(table(df$Medal))

#---> SI ... STELLA DOCET

################################################
#SELECT VARS based on quantity on NA or other
#SELECT VARS

#RAPPRESENTAZIONE DEI NA:

vars_to_remove2=filter(status, p_na ==0)  %>% .$variable
vars_to_remove2
data_subset3=select(df, -one_of(vars_to_remove2))

# plot only var with missing
library(VIM)
missingness<- aggr(data_subset3, col=c('navyblue','yellow'),numbers=TRUE, sortVars=TRUE,labels=names(data_subset3), cex.axis=.7,gap=2)
# see that missing in UNIV and TIPODIP tipolau saturate all obs!!!

#descrizione dei dati 
describe(df)

# describe the target
describe(df$Medal)

# nb describe is used by many package.....
Hmisc::describe(df)

# even better
psych::describe(df)


# description of NUMERIC vars
# description of NUMERIC vars
# description of NUMERIC vars

# Full numerical profiling in one function automatically excludes non-numerical variables
profiling_num(df)

#save it as dataframe..very useful
summary=data.frame(profiling_num(df))


# select  a personalized table
my_profiling_table=profiling_num(df, print_results = FALSE) %>% select(variable, mean, std_dev, skewness, p_01, p_99, range_80)
# see only the first three rows
head(my_profiling_table, 3)

# select some rows in my personalized table
vars_to_remove2=filter(my_profiling_table, skewness > 5 | std_dev==0)  %>% .$variable
vars_to_remove2
#NESSUNA




# Plot num vars
# Plot num vars
# Plot num vars
plot_num(df)


# METANOTA:  NON FACCIAMO REGRESSIONE LINEARE PERO':

# Correlation of num vars with target
correlation_table(data=df, target="Medal")



# relations between YL and quantitative vars
# relations between YL and quantitative vars
# relations between YL and quantitative vars



library(caret)



# factor vars: table and plot
# factor vars: table and plot
# factor vars: table and plot
#METANOTA :TUTTI I DATI
freq(data=df)


# can also put quantitative vars....autobinning
cross_plot(df, input = c( "Sex","Age","Height", "Weight" ,"Team"  , "NOC" ,   "Games" , "Year"  , "Season" ,"City",   "Sport",  "Event"), target = "Medal", plot_type = "percentual",auto_binning=TRUE)
#METANOTA: Skipping "Team" variable: more than 50 unique values.

# boxplot of quantitative x values vs Binary target
# boxplot of quantitative x values vs Binary target
# boxplot of quantitative x values vs Binary target


Formula1 <- paste(colnames(Quant2_df), collapse="','")
Formula1
# copy and paste...
vars_to_analyze=c( "Age","Height" ,"Weight", "Year")
plotar(data=df, input=vars_to_analyze , target="Medal", plot_type = "boxplot")


#densità: area( integrale )=1-->diventa un grafico di probabilità
plotar(data=df, input=vars_to_analyze , target="Medal", plot_type = "histdens")

#plot di un numero di anni minore
f<-df[df$Year> '1940' & df$Year<'1990',]
plotar(data=f, input=vars_to_analyze , target="Medal", plot_type = "histdens")
#si vede che la crescita del numero di medaglie assegnate è costante.
#Le medaglie non assegnate variano in modo meno regolare: segno che cambia il 
# numero di partecipanti: in particolare durante la 2° guerra mondiale e in
# quella del 1980 in URSS, anno del boicottaggio americano.


###############MODELLAZIONE#IMBALANCE #####################

# PRIMO PROBLEMA: FACTOR CON MOLTI LIVELLI-> DUMMIZZO E POI 2 SOLUZIONI 

# 1)



library(caret)
dmy = dummyVars(" ~ .", data = FAc2_df, fullRank=TRUE) 
predictors_Fac2_df_dummy = data.frame(predict(dmy, newdata = FAc2_df)) #costruzione del dataset con le dummy
head(predictors_dfC_dummy)


#  r NON VA PIU': MI DA' QUSTO:
Error in model.matrix.default(Terms, m) : 
  cannot allocate vector of length 688905756


# PROBLEMA DI MEMORIA... NON SO CHE FARE 















