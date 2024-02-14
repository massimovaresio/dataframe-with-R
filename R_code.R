## II Appello : Metodi esplorativi per i big data. Mirrione Silvia ##
# il file originale l'ho salvato nel formato cvs, perch? avevo problemi nel caricarlo

dati<-read.csv2("default_of_credit_card_clients.csv",h=T) 
dati<- dati[-1,]
View(dati)
str(dati) #Tutte le variabili sono state caricate come factor.
dati$X1<-as.numeric(as.character(dati$X1))
dati$X5<-as.numeric(as.character(dati$X5))
dati$X12<-as.numeric(as.character(dati$X12))
dati$X13<-as.numeric(as.character(dati$X13))
dati$X14<-as.numeric(as.character(dati$X14))
dati$X15<-as.numeric(as.character(dati$X15))
dati$X16<-as.numeric(as.character(dati$X16))
dati$X17<-as.numeric(as.character(dati$X17))
dati$X18<-as.numeric(as.character(dati$X18))
dati$X19<-as.numeric(as.character(dati$X19))
dati$X20<-as.numeric(as.character(dati$X20))
dati$X21<-as.numeric(as.character(dati$X21))
dati$X22<-as.numeric(as.character(dati$X22))
dati$X23<-as.numeric(as.character(dati$X23))
dati$Y<-as.numeric(as.character(dati$Y))
dati$Y<-as.factor(dati$Y)
dati$X2<-as.numeric(as.character(dati$X2))
dati$X2<-as.factor(dati$X2)

str(dati)

#Devo unire le classi di EDUCATION (X3) che non rientrano nel range fissato dal testo
dati$X3<-as.numeric(as.character((dati$X3)))
for(i in 1:length(dati$X3)){
   if(dati$X3[i]==0) {dati$X3[i]<- "1"}
   else{if(dati$X3[i]>4){dati$X3[i]<-"4"}}
   }
table(dati$X3)
dati$X3<-as.factor(dati$X3)
## Dobbiamo sistemare anche la variabile stato civile
dati$X4<-as.numeric(as.character(dati$X4))
for(i in 1:length(dati$X4)){
  if(dati$X4[i]==0) {dati$X4[i]<- "1"}
  else{if(dati$X4[i]>3){dati$X4[i]<-"3"}}
}
table(dati$X4)
dati$X4<-as.factor(dati$X4)


#Dobbiamo unire anche le modalit? per le variabili X6,X7,X8,X9,x10,x11.
# le osservazioni con modalit? -2, verranno assegnate a -1
table(dati$X6)
dati$X6<-as.numeric(as.character(dati$X6))
for(i in 1:length(dati$X6)){
  if(dati$X6[i]<(1)) {dati$X6[i]<- "-1"}}
table(dati$X6)
dati$X6<-as.factor(dati$X6)


dati$X7<-as.numeric(as.character(dati$X7))
for(i in 1:length(dati$X7)){
  if(dati$X7[i]<(1)) {dati$X7[i]<- "-1"}}
table(dati$X7)
dati$X7<-as.factor(dati$X7)

dati$X8<-as.numeric(as.character(dati$X8))
for(i in 1:length(dati$X8)){
  if(dati$X8[i]<1) {dati$X8[i]<- "-1"}}
table(dati$X8)
dati$X8<-as.factor(dati$X8)

dati$X9<-as.numeric(as.character(dati$X9))
for(i in 1:length(dati$X9)){
  if(dati$X9[i]<(1)) {dati$X9[i]<- "-1"}}
table(dati$X9)
dati$X9<-as.factor(dati$X9)

dati$X10<-as.numeric(as.character(dati$X10))
for(i in 1:length(dati$X10)){
  if(dati$X10[i]<(1)) {dati$X10[i]<- "-1"}}
table(dati$X10)
dati$X10<-as.factor(dati$X10)

dati$X11<-as.numeric(as.character(dati$X11))
for(i in 1:length(dati$X11)){
  if(dati$X11[i]<(1)) {dati$X11[i]<- "-1"}}
table(dati$X11)


#Carichiamo le libreria per la prima parte
library(MASS)
library(dplyr)
library(ggplot2)
library(vcd)
library(gridExtra)

#L'obiettivo del report ? quello di prevedere customers default payments,
#Analizziamo quindi la variabile

default<-dati$Y
summary(default) #23364 osservazioni per default=0 (No) e 6636 osservazioni per default=1 (SI). in termini assoluti il 78% della popolazione presa in esame non va in default

table(X2,default) #Distribuzione doppia di default con gender
table(dati$X3,default) #distribuzione doppia di default con education
Ammontare<-dati$X1
#analizziamo la variabile default in funzione della variabile X1 che corrisponde all'ammontare del credito
ggplot(data=dati, mapping=aes(x=default,y=Ammontare,col="red" ))+
  geom_boxplot()

#valutiamo se lo stato sociale influenza il default

mosaicplot(structable(default~dati$X4, data=dati),
           main =(""),
           xlab=("Stato sociale"),ylab=("Default"),
color=c("deepskyblue","dodgerblue1","dodgerblue2","dodgerblue3","dodgerblue4"))

# sembra non esserci dipendenza

x11()
par(mfrow=c(3,2))
#Analisi tra Default e X11(Aprile)
mosaicplot(structable(dati$Y~dati$X11, data=dati),
           main = "Aprile 2005",
           xlab=("Pagamento in ritardo"),ylab=("Default"),
           color=c("deepskyblue","dodgerblue1","dodgerblue2","dodgerblue3","dodgerblue4"))
#Analisi tra default e X10
mosaicplot(structable(dati$Y~dati$X10, data=dati),
           main = "Maggio 2005",
           xlab=("Pagamento in ritardo "),ylab=("Default"),
           color=c("deepskyblue","dodgerblue1","dodgerblue2","dodgerblue3","dodgerblue4"))
#Nessuno paga con un mese di ritardo. pochi pagano con piu di due mesi di ritardo
#Analisi tra default e X9
mosaicplot(structable(dati$Y~dati$X9, data=dati),
           main = "Giugno 2005",
           xlab=("Pagamento in ritardo"),ylab=("Default"),
           color=c("deepskyblue","dodgerblue1","dodgerblue2","dodgerblue3","dodgerblue4"))

#Analizziamo la relazione tra default e X8
mosaicplot(structable(dati$Y~dati$X8, data=dati),
           main =("Luglio 2005"),
           xlab=("Pagamento in ritardo"),ylab=("Default"),
           color=c("deepskyblue","dodgerblue1","dodgerblue2","dodgerblue3","dodgerblue4"))


#Analizziamo la relazione tra default e X7
mosaicplot(structable(dati$Y~dati$X7, data=dati),
           main = "Agosto 2005",
           xlab=("Pagamento in ritardo"),ylab=("Default"),
           color=c("deepskyblue","dodgerblue1","dodgerblue2","dodgerblue3","dodgerblue4"))
#Quasi nessuno paga con un mese di ritardo. 
#Analisi tra default e X6
mosaicplot(structable(dati$Y~dati$X6, data=dati),
           main = "Settembre 2005",
           xlab=("Pagamento in ritardo"),ylab=("Default"),
           color=c("deepskyblue","dodgerblue1","dodgerblue2","dodgerblue3","dodgerblue4"))
#si nota che l'andamento di andare in default, aumenta man mano che si paga in ritardo, fino a 3 mesi

#Analizziamo Default con le variabili  X12/X17 che indicano la quantita di estratto conto da Agosto ad Aprile2005
#Possiamo usare i Boxplot condizionati

x11()
par(mfrow=c(3,2))


(a<-ggplot(data=dati) + 
  geom_boxplot(mapping=aes(x=dati$X11,y=dati$X17,col=dati$Y)) +
  labs(
    title = "Aprile 2005",
    x = "Default",
    y = "Ammontare della fattura")+ 
  scale_colour_brewer(palette = "Set2")+
  theme(legend.position = "bottom"))

(b<-ggplot(data=dati) + 
  geom_boxplot(mapping=aes(x=dati$X10,y=dati$X16,col=dati$Y)) +
  labs(
    title = "Maggio 2005",
    x = "Default",
    y = "Ammontare della fattura")+ 
  scale_colour_brewer(palette = "Set2")+
  theme(legend.position = "bottom"))

(c<-ggplot(data=dati) + 
  geom_boxplot(mapping=aes(x=dati$X9,y=dati$X15,col=dati$Y)) +
  labs(
    title = "Giugno 2005",
    x = "Default",
    y = "Ammontare della fattura")+ 
  scale_colour_brewer(palette = "Set2")+
  theme(legend.position = "bottom"))

(d<-ggplot(data=dati) + 
  geom_boxplot(mapping=aes(x=dati$X8,y=dati$X14,col=dati$Y)) +
  labs(
    title = "Luglio 2005",
    x = "Default",
    y = "Ammontare dell'estratto conto")+ 
  scale_colour_brewer(palette = "Set2")+
  theme(legend.position = "bottom"))

(e<-ggplot(data=dati) + 
  geom_boxplot(mapping=aes(x=dati$X7,y=dati$X13,col=dati$Y)) +
  labs(
    title = "Agosto 2005",
    x = "Default",
    y = "Ammontare della fattura")+ 
  scale_colour_brewer(palette = "Set2")+
  theme(legend.position = "bottom"))



(f<-ggplot(data=dati) + 
    geom_boxplot(mapping=aes(x=dati$X6,y=dati$X12,col=dati$Y)) +
    labs(
      title = "Settembre 2005",
      x = "Default",
      y = "Ammontare della fattura")+ 
    scale_colour_brewer(palette = "Set2")+
    theme(legend.position = "bottom"))
x11()
grid.arrange(a,b,ncol=2)
x11()
grid.arrange(c,d,ncol=2)
x11()
grid.arrange(e,f,ncol=2)

# confrontare default con le variabili X18/X23 (ammontare dell'ultimo pagamento)



(a<-ggplot(data=dati) + 
    geom_boxplot(mapping=aes(x=dati$X11,y=dati$X23,col=dati$Y)) +
    labs(
      title = "Aprile 2005",
      x = "Default",
      y = "Ammontare della rata")+
    ylim(0,7000)+ 
    scale_colour_brewer(palette = "Set2")+
    theme(legend.position = "bottom"))

(b<-ggplot(data=dati) + 
    geom_boxplot(mapping=aes(x=dati$X10,y=dati$X22,col=dati$Y)) +
    labs(
      title = "Maggio 2005",
      x = "Default",
      y = "Ammontare della rata")+
    ylim(0,7000)+
    scale_colour_brewer(palette = "Set2")+
    theme(legend.position = "bottom"))

(c<-ggplot(data=dati) + 
    geom_boxplot(mapping=aes(x=dati$X9,y=dati$X21,col=dati$Y)) +
    labs(
      title = "Giugno 2005",
      x = "Default",
      y = "Ammontare della rata")+
    ylim(0,7000)+ 
    scale_colour_brewer(palette = "Set2")+
    theme(legend.position = "bottom"))

(d<-ggplot(data=dati) + 
    geom_boxplot(mapping=aes(x=dati$X8,y=dati$X20,col=dati$Y)) +
    labs(
      title = "Luglio 2005",
      x = "Default",
      y = "Ammontare della rata")+
    ylim(0,7000)+ 
    scale_colour_brewer(palette = "Set2")+
    theme(legend.position = "bottom"))

(e<-ggplot(data=dati) + 
    geom_boxplot(mapping=aes(x=dati$X7,y=dati$X19,col=dati$Y)) +
    labs(
      title = "Agosto 2005",
      x = "Default",
      y = "Ammontare della rata")+
    ylim(0,7000)+ 
    scale_colour_brewer(palette = "Set2")+
    theme(legend.position = "bottom"))

(f<-ggplot(data=dati) + 
    geom_boxplot(mapping=aes(x=dati$X6,y=dati$X18,col=dati$Y)) +
    labs(
      title = "Settembre 2005",
      x = "Default",
      y = "Ammontare della rata")+
ylim(0,7000)+
    scale_colour_brewer(palette = "Set2"))

x11()
grid.arrange(a,b,ncol=2)
x11()
grid.arrange(c,d,ncol=2)
x11()
grid.arrange(e,f,ncol=2)

#### Random Forest ####
detach(package:ggplot2, unload = T)
detach(package:gridExtra, unload = T)
detach(package:dplyr, unload = T)
#rimuoviamo le librerie precedenti.
#carichiamo la libreria random forest
library(MASS)
library(tree)
library(ISLR)
library(randomForest)
#elimino la colonna ID in modo che non mi porti problemi 
dataset<-dati[,-1]
attach(dataset)
str(dataset)
#devo fare la random forest su 1/3 dei dati, quindi mi creo il training set su 2/3 dei dati
train = sample(1:nrow(dati), nrow(dati)*0.67) 

#Devo scegliere il parametro mtry per scegliere tutte le esplicative
#stimo diverse random forest al variare del parametro mtry 

#mtry=5
Defaultforest5=randomForest(Y~.,data=dataset,
                            subset=train,mtry=5,ntree=100,importance=TRUE)
yhat.Default5 = predict(Defaultforest6,newdata=dati[-train,])
plot(Defaultforest5$err.rate[,1],type="l") 

#mtry=6
Defaultforest6=randomForest(Y~.,data=dataset,
                             subset=train,mtry=6,ntree=100,importance=TRUE)
yhat.Default6 = predict(Defaultforest6,newdata=dati[-train,])
plot(Defaultforest6$err.rate[,1],type="l") 

#mtry=7
Defaultforest7=randomForest(Y~.,data=dataset,
                            subset=train,mtry=7,ntree=100,importance=TRUE)
yhat.Default7 = predict(Defaultforest7,newdata=dati[-train,])
plot(Defaultforest7$err.rate[,1],type="l") 



#mtry=10
Defaultforest10=randomForest(Y~.,data=dataset,
                             subset=train,mtry=10,ntree=100,importance=TRUE)
yhat.Default10 = predict(Defaultforest10,newdata=dati[-train,])
plot(Defaultforest10$err.rate[,1],type="l") 


#ripeto i comandi al variare di mtry
#mtry=18

Defaultforest18=randomForest(Y~.,data=dataset,
                             subset=train,mtry=18,ntree=100,importance=TRUE)
yhat.Default18 = predict(Defaultforest18,newdata=dati[-train,])
plot(Defaultforest18$err.rate[,1],type="l") 

#mtry=20

(Defaultforest20=randomForest(Y~.,data=dataset,
                             subset=train,mtry=20,ntree=100,importance=TRUE))
yhat.Default20 = predict(Defaultforest20,newdata=dati[-train,])
plot(Defaultforest20$err.rate[,1],type="l") 


#devo confrontare gli error rate. Quindi li rappresento in un solo grafico
plot(Defaultforest10$err.rate[,1],type="l",ylim = c(0.2,0.3),xlim=c(0,60),
     main="Confronto Default forest",
     xlab="n trees", ylab="OOB estimate of error rate") 
lines(Defaultforest18$err.rate[,1],type="l", col=2)
lines(Defaultforest20$err.rate[,1],type="l", col=3)
legend("topright",legend = c("mtry=10","mtry=18", "mtry=20"),col=c(1:3),lty=1)
#vedo dopo quanti alberi si stabilizza la procedura

varImpPlot(Defaultforest10) #variabili pi? importanti


