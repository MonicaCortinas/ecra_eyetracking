library(tidyverse)
library(here)
library(kableExtra)
library(lme4)
library(nFactors)


basecompleta<- read.csv(file=here("data/otrasbases", "basecompleta.csv"), header = TRUE,
                        sep = "\t" , encoding = "UTF-8")
## QUALTRICS
dataindividuo <- read.csv(file=here("data/otrasbases", "baseindividuo.csv"), header = TRUE,
                          sep = "\t" , encoding = "UTF-8")


dataset<-dataindividuo
dataset$sexo<- dataindividuo$Sexo
dataset$sexo = factor(dataset$sexo, levels = c("Mujer", "Hombre"))

dataset$edad<- dataindividuo$Edad

dataset$educacion<- dataindividuo[,99]
dataset$educacion<- factor(dataset$educacion, levels = c("Enseñanza secundaria (BUP/Bachillerato/FP)", 
                                                         "Estudios universitarios", "Master"))

dataset$ocupacion<- dataindividuo[,100]
dataset$ocupacion<- factor(dataset$ocupacion, levels = c("Estudiante", 
                                                         "Parado", "Trabaja por cuenta ajena"))
dataset$buscaonline<-dataindividuo[,95]
dataset$compraonline<-dataindividuo[,96]

# FACTORIALES

library(nFactors)

implizapa<- dataindividuo[,c("ConoZapatillas","impliZapatillas","interZapatillas")]
d<-factanal(implizapa, factors=1)
a1<-psych::alpha(implizapa)
dataindividuo$consImZapa<- d$loadings[1,]*dataindividuo$ConoZapatillas +d$loadings[2,]*dataindividuo$impliZapatillas+d$loadings[3,]*dataindividuo$impliZapatillas

impliTele<- dataindividuo[,c("ConoTelefonos","impliTelefonos","interTelefonos")]
d<-factanal(impliTele, factors=1)
a2<-psych::alpha(impliTele)
dataindividuo$consImTele<- d$loadings[1,]*dataindividuo$ConoTelefonos +d$loadings[2,]*dataindividuo$impliTelefonos+d$loadings[3,]*dataindividuo$interTelefonos

implibolis<- dataindividuo[,c("Conobolis","implibolis","interbolis")]
d<-factanal(implibolis, factors=1)
a3<-psych::alpha(implibolis)
dataindividuo$consImBolis<- d$loadings[1,]*dataindividuo$Conobolis +d$loadings[2,]*dataindividuo$implibolis+d$loadings[3,]*dataindividuo$interbolis

impliHDs<- dataindividuo[,c("ConoHDs","impliHDs","interHDs")]
a4<-psych::alpha(impliHDs)
d<-factanal(impliHDs, factors=1)
dataindividuo$consImHDs<- d$loadings[1,]*dataindividuo$ConoHDs +d$loadings[2,]*dataindividuo$impliHDs+d$loadings[3,]*dataindividuo$interHDs



#### Tiempos en cada web

# Nuevo data frame:
#sec/n/Task/Cat

DataBase<- data.frame(dataindividuo$n,"ID"=dataindividuo$ID, "secZapa"= dataindividuo$secZapa, "secTele"=dataindividuo$secTele, "secBoli"=dataindividuo$secBoli, "secHd"=dataindividuo$secHd, dataindividuo$Task1, dataindividuo$Task2, dataindividuo$Task3, dataindividuo$Task4, "busca"=dataindividuo[,95], "compra"=dataindividuo[,96], dataindividuo$ConoZapatillas, dataindividuo$ConoTelefonos, dataindividuo$Conobolis, dataindividuo$ConoHDs,dataindividuo$impliZapatillas, dataindividuo$impliTelefonos, dataindividuo$implibolis, dataindividuo$impliHDs, dataindividuo$interZapatillas, dataindividuo$interTelefonos, dataindividuo$interbolis, dataindividuo$interHDs, dataindividuo$precioZapatillas, dataindividuo$marcaZapatillas, dataindividuo$pagoZapatillas, dataindividuo$devolZapatillas, dataindividuo$envioZapatillas, dataindividuo$seguiZapatillas, dataindividuo$precioTelefonos, dataindividuo$marcaTelefonos, dataindividuo$pagoTelefonos, dataindividuo$devolTelefonos, dataindividuo$envioTelefonos, dataindividuo$seguiTelefonos, dataindividuo$preciobolis, dataindividuo$marcabolis, dataindividuo$pagobolis, dataindividuo$devolbolis, dataindividuo$enviobolis, dataindividuo$seguibolis, dataindividuo$precioHDs, dataindividuo$marcaHDs, dataindividuo$pagoHDs, dataindividuo$devolHDs, dataindividuo$envioHDs, dataindividuo$seguiHDs,   dataindividuo$consImZapa, dataindividuo$consImZapa , dataindividuo$consImTele, dataindividuo$consImBolis, dataindividuo$consImHDs       )

# Make sure the subject column is a factor
DataBase$dataindividuo.n <- factor(DataBase$dataindividuo.n)

library(tidyr)



LongDat <- gather(DataBase, category, seconds, secZapa:secHd)

LongDat$Task[LongDat$category=="secZapa"]<-1
LongDat$Task[LongDat$category=="secTele"]<-2
LongDat$Task[LongDat$category=="secBoli"]<-3
LongDat$Task[LongDat$category=="secHd"]<-4

LongDat$IMPGlobal[LongDat$category=="secZapa"]<-LongDat$dataindividuo.consImZapa[LongDat$category=="secZapa"]
LongDat$IMPGlobal[LongDat$category=="secTele"]<-LongDat$dataindividuo.consImTele[LongDat$category=="secTele"]
LongDat$IMPGlobal[LongDat$category=="secBoli"]<-LongDat$dataindividuo.consImBolis[LongDat$category=="secBoli"]
LongDat$IMPGlobal[LongDat$category=="secHd"]<-LongDat$dataindividuo.consImHDs[LongDat$category=="secBoli"]

LongDat$Conoc[LongDat$category=="secZapa"]<-LongDat$dataindividuo.ConoZapatillas[LongDat$category=="secZapa"]
LongDat$Conoc[LongDat$category=="secTele"]<-LongDat$dataindividuo.ConoTelefonos[LongDat$category=="secTele"]
LongDat$Conoc[LongDat$category=="secBoli"]<-LongDat$dataindividuo.Conobolis[LongDat$category=="secBoli"]
LongDat$Conoc[LongDat$category=="secHd"]<-LongDat$dataindividuo.ConoHDs[LongDat$category=="secBoli"]

LongDat$Impli[LongDat$category=="secZapa"]<-LongDat$dataindividuo.impliZapatillas[LongDat$category=="secZapa"]
LongDat$Impli[LongDat$category=="secTele"]<-LongDat$dataindividuo.impliTelefonos[LongDat$category=="secTele"]
LongDat$Impli[LongDat$category=="secBoli"]<-LongDat$dataindividuo.implibolis[LongDat$category=="secBoli"]
LongDat$Impli[LongDat$category=="secHd"]<-LongDat$dataindividuo.impliHDs[LongDat$category=="secBoli"]

LongDat$Interes[LongDat$category=="secZapa"]<-LongDat$dataindividuo.interZapatillas[LongDat$category=="secZapa"]
LongDat$Interes[LongDat$category=="secTele"]<-LongDat$dataindividuo.interTelefonos[LongDat$category=="secTele"]
LongDat$Interes[LongDat$category=="secBoli"]<-LongDat$dataindividuo.interbolis[LongDat$category=="secBoli"]
LongDat$Interes[LongDat$category=="secHd"]<-LongDat$dataindividuo.interHDs[LongDat$category=="secBoli"]

LongDat$Marca[LongDat$category=="secZapa"]<-LongDat$dataindividuo.marcaZapatillas[LongDat$category=="secZapa"]
LongDat$Marca[LongDat$category=="secTele"]<-LongDat$dataindividuo.marcaTelefonos[LongDat$category=="secTele"]
LongDat$Marca[LongDat$category=="secBoli"]<-LongDat$dataindividuo.marcabolis[LongDat$category=="secBoli"]
LongDat$Marca[LongDat$category=="secHd"]<-LongDat$dataindividuo.marcaHDs[LongDat$category=="secBoli"]

LongDat$Precio[LongDat$category=="secZapa"]<-LongDat$dataindividuo.precioZapatillas[LongDat$category=="secZapa"]
LongDat$Precio[LongDat$category=="secTele"]<-LongDat$dataindividuo.precioTelefonos[LongDat$category=="secTele"]
LongDat$Precio[LongDat$category=="secBoli"]<-LongDat$dataindividuo.preciobolis[LongDat$category=="secBoli"]
LongDat$Precio[LongDat$category=="secHd"]<-LongDat$dataindividuo.precioHDs[LongDat$category=="secBoli"]

LongDat$Devol[LongDat$category=="secZapa"]<-LongDat$dataindividuo.devolZapatillas[LongDat$category=="secZapa"]
LongDat$Devol[LongDat$category=="secTele"]<-LongDat$dataindividuo.devolTelefonos[LongDat$category=="secTele"]
LongDat$Devol[LongDat$category=="secBoli"]<-LongDat$dataindividuo.devolbolis[LongDat$category=="secBoli"]
LongDat$Devol[LongDat$category=="secHd"]<-LongDat$dataindividuo.devolHDs[LongDat$category=="secBoli"]

LongDat$Envio[LongDat$category=="secZapa"]<-LongDat$dataindividuo.envioZapatillas[LongDat$category=="secZapa"]
LongDat$Envio[LongDat$category=="secTele"]<-LongDat$dataindividuo.envioTelefonos[LongDat$category=="secTele"]
LongDat$Envio[LongDat$category=="secBoli"]<-LongDat$dataindividuo.enviobolis[LongDat$category=="secBoli"]
LongDat$Envio[LongDat$category=="secHd"]<-LongDat$dataindividuo.envioHDs[LongDat$category=="secBoli"]


LongDat$Segui[LongDat$category=="secZapa"]<-LongDat$dataindividuo.seguiZapatillas[LongDat$category=="secZapa"]
LongDat$Segui[LongDat$category=="secTele"]<-LongDat$dataindividuo.seguiTelefonos[LongDat$category=="secTele"]
LongDat$Segui[LongDat$category=="secBoli"]<-LongDat$dataindividuo.seguibolis[LongDat$category=="secBoli"]
LongDat$Segui[LongDat$category=="secHd"]<-LongDat$dataindividuo.seguiHDs[LongDat$category=="secBoli"]


LongDat$Pago[LongDat$category=="secZapa"]<-LongDat$dataindividuo.pagoZapatillas[LongDat$category=="secZapa"]
LongDat$Pago[LongDat$category=="secTele"]<-LongDat$dataindividuo.pagoTelefonos[LongDat$category=="secTele"]
LongDat$Pago[LongDat$category=="secBoli"]<-LongDat$dataindividuo.pagobolis[LongDat$category=="secBoli"]
LongDat$Pago[LongDat$category=="secHd"]<-LongDat$dataindividuo.pagoHDs[LongDat$category=="secBoli"]


LongDat$Asignada[LongDat$Task==1]<-as.character(LongDat$dataindividuo.Task1[LongDat$Task==1])
LongDat$Asignada[LongDat$Task==2]<-as.character(LongDat$dataindividuo.Task2[LongDat$Task==2])
LongDat$Asignada[LongDat$Task==3]<-as.character(LongDat$dataindividuo.Task3[LongDat$Task==3])
LongDat$Asignada[LongDat$Task==4]<-as.character(LongDat$dataindividuo.Task4[LongDat$Task==4])

LongDat$Asignada<- factor(LongDat$Asignada, levels=c("explor", "search", "purchase", "post" ))
LongDat$category<- factor(LongDat$category, levels=c("secZapa", "secTele", "secBoli", "secHd"))



# Creamos las variables que identifican a las areas:

basecompleta$Tzona<- basecompleta$zona
basecompleta$Tzona<- 0
basecompleta$Tzona[basecompleta$zona==1]<-"A"
basecompleta$Tzona[basecompleta$zona==2]<-"A"

basecompleta$Tzona[basecompleta$zona==3]<-"B"
basecompleta$Tzona[basecompleta$zona==4]<-"B"
basecompleta$Tzona[basecompleta$zona==5]<-"B"
basecompleta$Tzona[basecompleta$zona==6]<-"B"


basecompleta$Tzona[(basecompleta$zona>10)&(basecompleta$zona<90)]<-"C"



# Eliminamos los perdidos

basecompleta$Fixa1 <-basecompleta$fixa1
basecompleta$Fixa1[basecompleta$Fixa1=="Inf"]<-0
basecompleta$Fixa1[is.na(basecompleta$Fixa1)]<-0

#la variable sumFixa1 es las fijaciones totales en esa zona. 


basecompleta$sumFixa1 <-basecompleta$sumFixa1
basecompleta$sumFixa1[basecompleta$sumFixa1=="Inf"]<-0
basecompleta$sumFixa1[is.na(basecompleta$sumFixa1)]<-0

basecompleta$Fixa2 <- basecompleta$fixa2
basecompleta$Fixa2[basecompleta$Fixa2=="Inf"]<-0
basecompleta$Fixa2[is.na(basecompleta$Fixa2)]<-0

basecompleta$sumFixa2 <- basecompleta$sumFixa2
basecompleta$sumFixa2[basecompleta$sumFixa2=="Inf"]<-0
basecompleta$sumFixa2[is.na(basecompleta$sumFixa2)]<-0

basecompleta$Fixa3 <- basecompleta$fixa3
basecompleta$Fixa3[basecompleta$Fixa3=="Inf"]<-0
basecompleta$Fixa3[is.na(basecompleta$Fixa3)]<-0

basecompleta$sumFixa3 <- basecompleta$sumFixa3
basecompleta$sumFixa3[basecompleta$sumFixa3=="Inf"]<-0
basecompleta$sumFixa3[is.na(basecompleta$sumFixa3)]<-0

basecompleta$Fixa4 <- basecompleta$fixa4
basecompleta$Fixa4[basecompleta$Fixa4=="Inf"]<-0
basecompleta$Fixa4[is.na(basecompleta$Fixa4)]<-0

basecompleta$sumFixa4 <- basecompleta$sumFixa4
basecompleta$sumFixa4[basecompleta$sumFixa4=="Inf"]<-0
basecompleta$sumFixa4[is.na(basecompleta$sumFixa4)]<-0


# Area zona 1:

areazonaA<- 226301.63
areazonaB<- 124301.25  
areazonaC_1<-372720.10 
areazonaC_2<-376167.02 
areazonaC_3<-372720.10
areazonaC_4<-375592.53 


poraA1<-areazonaA/(areazonaA+areazonaB+areazonaC_1)
poraA2<-areazonaA/(areazonaA+areazonaB+areazonaC_2)
poraA3<-areazonaA/(areazonaA+areazonaB+areazonaC_3)
poraA4<-areazonaA/(areazonaA+areazonaB+areazonaC_4)

poraB1<-areazonaB/(areazonaA+areazonaB+areazonaC_1)
poraB2<-areazonaB/(areazonaA+areazonaB+areazonaC_2)
poraB3<-areazonaB/(areazonaA+areazonaB+areazonaC_3)
poraB4<-areazonaB/(areazonaA+areazonaB+areazonaC_4)

poraC1<-areazonaC_1/(areazonaA+areazonaB+areazonaC_1)
poraC2<-areazonaC_2/(areazonaA+areazonaB+areazonaC_2)
poraC3<-areazonaC_3/(areazonaA+areazonaB+areazonaC_3)
poraC4<-areazonaC_4/(areazonaA+areazonaB+areazonaC_4)
# Con estas fijaciones totales por area trabajamos en las bases agregadas:

df_fixa <- aggregate(cbind(Fixa1, Fixa2, Fixa3, Fixa4) ~ ID + Tzona, data = basecompleta, sum, na.rm = TRUE)

df_fixa$Fixa1_por[df_fixa$Tzona=="A"]=df_fixa$Fixa1[df_fixa$Tzona=="A"]*poraA1
df_fixa$Fixa1_por[df_fixa$Tzona=="B"]=df_fixa$Fixa1[df_fixa$Tzona=="B"]*poraB1
df_fixa$Fixa1_por[df_fixa$Tzona=="C"]=df_fixa$Fixa1[df_fixa$Tzona=="C"]*poraC1

df_fixa$Fixa2_por[df_fixa$Tzona=="A"]=df_fixa$Fixa2[df_fixa$Tzona=="A"]*poraA2
df_fixa$Fixa2_por[df_fixa$Tzona=="B"]=df_fixa$Fixa2[df_fixa$Tzona=="B"]*poraB2
df_fixa$Fixa2_por[df_fixa$Tzona=="C"]=df_fixa$Fixa2[df_fixa$Tzona=="C"]*poraC2

df_fixa$Fixa3_por[df_fixa$Tzona=="A"]=df_fixa$Fixa3[df_fixa$Tzona=="A"]*poraA3
df_fixa$Fixa3_por[df_fixa$Tzona=="B"]=df_fixa$Fixa3[df_fixa$Tzona=="B"]*poraB3
df_fixa$Fixa3_por[df_fixa$Tzona=="C"]=df_fixa$Fixa3[df_fixa$Tzona=="C"]*poraC3

df_fixa$Fixa4_por[df_fixa$Tzona=="A"]=df_fixa$Fixa4[df_fixa$Tzona=="A"]*poraA4
df_fixa$Fixa4_por[df_fixa$Tzona=="B"]=df_fixa$Fixa4[df_fixa$Tzona=="B"]*poraB4
df_fixa$Fixa4_por[df_fixa$Tzona=="C"]=df_fixa$Fixa4[df_fixa$Tzona=="C"]*poraC4



# La que nos interesa es la que tiene las agregadas:


df_sumFixa <- aggregate(cbind(sumFixa1, sumFixa2, sumFixa3, sumFixa4) ~ ID + Tzona, data = basecompleta, sum, na.rm = TRUE)



df_sumFixa$sumFixa1_por[df_sumFixa$Tzona=="A"]=df_sumFixa$sumFixa1[df_sumFixa$Tzona=="A"]*poraA1
df_sumFixa$sumFixa1_por[df_sumFixa$Tzona=="B"]=df_sumFixa$sumFixa1[df_sumFixa$Tzona=="B"]*poraB1
df_sumFixa$sumFixa1_por[df_sumFixa$Tzona=="C"]=df_sumFixa$sumFixa1[df_sumFixa$Tzona=="C"]*poraC1

df_sumFixa$sumFixa2_por[df_sumFixa$Tzona=="A"]=df_sumFixa$sumFixa2[df_sumFixa$Tzona=="A"]*poraA2
df_sumFixa$sumFixa2_por[df_sumFixa$Tzona=="B"]=df_sumFixa$sumFixa2[df_sumFixa$Tzona=="B"]*poraB2
df_sumFixa$sumFixa2_por[df_sumFixa$Tzona=="C"]=df_sumFixa$sumFixa2[df_sumFixa$Tzona=="C"]*poraC2

df_sumFixa$sumFixa3_por[df_sumFixa$Tzona=="A"]=df_sumFixa$sumFixa3[df_sumFixa$Tzona=="A"]*poraA3
df_sumFixa$sumFixa3_por[df_sumFixa$Tzona=="B"]=df_sumFixa$sumFixa3[df_sumFixa$Tzona=="B"]*poraB3
df_sumFixa$sumFixa3_por[df_sumFixa$Tzona=="C"]=df_sumFixa$sumFixa3[df_sumFixa$Tzona=="C"]*poraC3

df_sumFixa$sumFixa4_por[df_sumFixa$Tzona=="A"]=df_sumFixa$sumFixa4[df_sumFixa$Tzona=="A"]*poraA4
df_sumFixa$sumFixa4_por[df_sumFixa$Tzona=="B"]=df_sumFixa$sumFixa4[df_sumFixa$Tzona=="B"]*poraB4
df_sumFixa$sumFixa4_por[df_sumFixa$Tzona=="C"]=df_sumFixa$sumFixa4[df_sumFixa$Tzona=="C"]*poraC4


basecompleta$sumFixa1[is.na(basecompleta$sumFixa1)]<-0
basecompleta$sumFixa2[is.na(basecompleta$sumFixa2)]<-0
basecompleta$sumFixa3[is.na(basecompleta$sumFixa3)]<-0
basecompleta$sumFixa4[is.na(basecompleta$sumFixa4)]<-0

df_TotalFixa <- aggregate(cbind(sumFixa1, sumFixa2, sumFixa3, sumFixa4) ~ ID , data = basecompleta, sum, na.rm = TRUE)

# Con respecto al orden funcionamos un poco diferente
# En el archivo de lectura, una vez leídos los cuatro archivos, 
#para cada individuo se creavariable que recoja, para cada zona y tarea, 
# cual es el orden en el que aparece por primera vez. 

# podemos calcular cuales son los máximos y en el archivo sustituir 
# los valores perdidos por un valor muy superior (999)


## Creamos un DF con el mínimo valor para cada zona, es cuando aparece por primera vez:
basecompleta$ordenT1[is.na(basecompleta$ordenT1)]<-999
basecompleta$ordenT2[is.na(basecompleta$ordenT2)]<-999
basecompleta$ordenT3[is.na(basecompleta$ordenT3)]<-999
basecompleta$ordenT4[is.na(basecompleta$ordenT4)]<-999

df_orden <- aggregate(cbind(ordenT1, ordenT2, ordenT3, ordenT4) ~ (ID + Tzona), 
                      data = basecompleta, min, na.rm = FALSE)

df_orden<-df_orden[order(df_orden$ID),]




# Calculamos el minimo para cada individuo y tarea
orden_min <- aggregate(cbind("minT1"=ordenT1, "minT2"=ordenT2, "minT3"=ordenT3, "minT4"=ordenT4) ~ ID , data = df_orden, min, na.rm = TRUE)
# Calculamos el maximo para cada individo y tarea
orden_max <- aggregate(cbind("maxT1"=ordenT1, "maxT2"=ordenT2, "maxT3"=ordenT3, "maxT4"=ordenT4) ~ ID , data = df_orden, max, na.rm = TRUE)


#llevamos los datos de minimo y max a la base de orden
df_orden <- merge(x=df_orden, y=orden_min, by=c("ID"))
df_orden <- merge(x=df_orden, y=orden_max, by=c("ID"))

# Si el valor del orden es igual al mínimo, el orden es igual a 1, 
# si el orden es igual al máximo es 3 
# si no es el mínimo ni el máximo es 2
df_orden$O1[df_orden$ordenT1==df_orden$minT1]<-1
df_orden$O1[df_orden$ordenT1==df_orden$maxT1]<-3
df_orden$O1[(df_orden$ordenT1!=df_orden$maxT1)&(df_orden$ordenT1!=df_orden$minT1)]<-2

df_orden$O2[df_orden$ordenT2==df_orden$minT2]<-1
df_orden$O2[df_orden$ordenT2==df_orden$maxT2]<-3
df_orden$O2[(df_orden$ordenT2!=df_orden$maxT2)&(df_orden$ordenT2!=df_orden$minT2)]<-2

df_orden$O3[df_orden$ordenT3==df_orden$minT3]<-1
df_orden$O3[df_orden$ordenT3==df_orden$maxT3]<-3
df_orden$O3[(df_orden$ordenT3!=df_orden$maxT3)&(df_orden$ordenT3!=df_orden$minT3)]<-2

df_orden$O4[df_orden$ordenT4==df_orden$minT4]<-1
df_orden$O4[df_orden$ordenT4==df_orden$maxT4]<-3
df_orden$O4[(df_orden$ordenT4!=df_orden$maxT4)&(df_orden$ordenT4!=df_orden$minT4)]<-2

basecompleta$TAREA1 <- basecompleta$ordenT1
basecompleta$TAREA1[as.character(basecompleta$Task1)=="explor"] <- 1
basecompleta$TAREA1[as.character(basecompleta$Task1)=="search"] <- 2
basecompleta$TAREA1[as.character(basecompleta$Task1)=="purchase"] <- 3
basecompleta$TAREA1[as.character(basecompleta$Task1)=="post"] <- 4


basecompleta$TAREA2 <- basecompleta$ordenT2
basecompleta$TAREA2[as.character(basecompleta$Task2)=="explor"] <- 1
basecompleta$TAREA2[as.character(basecompleta$Task2)=="search"] <- 2
basecompleta$TAREA2[as.character(basecompleta$Task2)=="purchase"] <- 3
basecompleta$TAREA2[as.character(basecompleta$Task2)=="post"] <- 4

basecompleta$TAREA3 <- basecompleta$ordenT3
basecompleta$TAREA3[as.character(basecompleta$Task3)=="explor"] <- 1
basecompleta$TAREA3[as.character(basecompleta$Task3)=="search"] <- 2
basecompleta$TAREA3[as.character(basecompleta$Task3)=="purchase"] <- 3
basecompleta$TAREA3[as.character(basecompleta$Task3)=="post"] <- 4

basecompleta$TAREA4 <- basecompleta$ordenT4
basecompleta$TAREA4[as.character(basecompleta$Task4)=="explor"] <- 1
basecompleta$TAREA4[as.character(basecompleta$Task4)=="search"] <- 2
basecompleta$TAREA4[as.character(basecompleta$Task4)=="purchase"] <- 3
basecompleta$TAREA4[as.character(basecompleta$Task4)=="post"] <- 4

df_task<- aggregate(cbind(TAREA1, TAREA2, TAREA3, TAREA4) ~ ID + Tzona, data = basecompleta, FUN=mean)


FIXA<- data.frame(df_task, df_fixa[7:10], df_sumFixa[7:10],df_orden[15:18])



library(tidyr)

## FIJACIONES TOTALES

# base df_TotalFixa
df_TotalFixa$ID <- factor(df_TotalFixa$ID)

TotalFixa_long <- gather(df_TotalFixa, category, totalFixa, sumFixa1:sumFixa4)
TotalFixa_long$category_rec[TotalFixa_long$category == "sumFixa1"] <- "1"
TotalFixa_long$category_rec[TotalFixa_long$category == "sumFixa2"] <- "2"
TotalFixa_long$category_rec[TotalFixa_long$category == "sumFixa3"] <- "3"
TotalFixa_long$category_rec[TotalFixa_long$category == "sumFixa4"] <- "4"
TotalFixa_long$Task<- TotalFixa_long$category_rec

LongDat <- merge(x=LongDat, y=TotalFixa_long[,c(1,3,5)], by=c("ID", "Task" ), all.x=TRUE)


## HACEMOS LA BASE PARA LA ZONA A

zonaA <- FIXA[FIXA$Tzona=="A",]
zonaA$ID <- factor(zonaA$ID)
LongDat$ID <- factor(LongDat$ID)

zonaA_long <- gather(zonaA, category, fijacZonaA, sumFixa1_por:sumFixa4_por)

zonaA_long$category_rec[zonaA_long$category == "sumFixa1_por"] <- "1"
zonaA_long$category_rec[zonaA_long$category == "sumFixa2_por"] <- "2"
zonaA_long$category_rec[zonaA_long$category == "sumFixa3_por"] <- "3"
zonaA_long$category_rec[zonaA_long$category == "sumFixa4_por"] <- "4"
zonaA_long$Task<- zonaA_long$category_rec

LongDat <- merge(x=LongDat, y=zonaA_long[,c(1,16,18)], by=c("ID", "Task" ))

#  ORDEN Zona 1

zonaA_orlong <- gather(zonaA, category, ordenZonaA, O1:O4)

zonaA_orlong$category_rec[zonaA_orlong$category == "O1"] <- "1"
zonaA_orlong$category_rec[zonaA_orlong$category == "O2"] <- "2"
zonaA_orlong$category_rec[zonaA_orlong$category == "O3"] <- "3"
zonaA_orlong$category_rec[zonaA_orlong$category == "O4"] <- "4"
zonaA_orlong$Task<- zonaA_orlong$category_rec

LongDat <- merge(x=LongDat, y=zonaA_orlong[,c(1,16,18)], by=c("ID", "Task" ))

# PARA LAS FIJACIONES EN LA ZONA 2

zonaB <- FIXA[FIXA$Tzona=="B",]
zonaB$ID <- factor(zonaB$ID)
zonaB_long <- gather(zonaB, category, fijacZonaB, sumFixa1_por:sumFixa4_por)

zonaB_long$category_rec[zonaB_long$category == "sumFixa1_por"] <- "1"
zonaB_long$category_rec[zonaB_long$category == "sumFixa2_por"] <- "2"
zonaB_long$category_rec[zonaB_long$category == "sumFixa3_por"] <- "3"
zonaB_long$category_rec[zonaB_long$category == "sumFixa4_por"] <- "4"
zonaB_long$Task<- zonaB_long$category_rec

LongDat <- merge(x=LongDat, y=zonaB_long[,c(1,16, 18)], by=c("ID", "Task" ))

#  ORDEN Zona 2

zonaB_orlong <- gather(zonaB, category, ordenZonaB, O1:O4)

zonaB_orlong$category_rec[zonaB_orlong$category == "O1"] <- "1"
zonaB_orlong$category_rec[zonaB_orlong$category == "O2"] <- "2"
zonaB_orlong$category_rec[zonaB_orlong$category == "O3"] <- "3"
zonaB_orlong$category_rec[zonaB_orlong$category == "O4"] <- "4"
zonaB_orlong$Task<- zonaB_orlong$category_rec

LongDat <- merge(x=LongDat, y=zonaB_orlong[,c(1,16, 18)], by=c("ID", "Task" ))

# PARA LAS FIJACIONES EN LA ZONA 3

zonaC <- FIXA[FIXA$Tzona=="C",]
zonaC$ID <- factor(zonaC$ID)
zonaC_long <- gather(zonaC, category, fijacZonaC, sumFixa1_por:sumFixa4_por)

zonaC_long$category_rec[zonaC_long$category == "sumFixa1_por"] <- "1"
zonaC_long$category_rec[zonaC_long$category == "sumFixa2_por"] <- "2"
zonaC_long$category_rec[zonaC_long$category == "sumFixa3_por"] <- "3"
zonaC_long$category_rec[zonaC_long$category == "sumFixa4_por"] <- "4"
zonaC_long$Task<- zonaC_long$category_rec

LongDat <- merge(x=LongDat, y=zonaC_long[,c(1,16, 18)], by=c("ID", "Task" ))


#  ORDEN Zona 3

zonaC_orlong <- gather(zonaC, category, ordenZonaC, O1:O4)

zonaC_orlong$category_rec[zonaC_orlong$category == "O1"] <- "1"
zonaC_orlong$category_rec[zonaC_orlong$category == "O2"] <- "2"
zonaC_orlong$category_rec[zonaC_orlong$category == "O3"] <- "3"
zonaC_orlong$category_rec[zonaC_orlong$category == "O4"] <- "4"
zonaC_orlong$Task<- zonaC_orlong$category_rec

LongDat <- merge(x=zonaC_orlong[,c(1,16, 18)], y=LongDat, by=c("ID", "Task" ))

library(xlsx)
#Entropía
entropia<- xlsx::read.xlsx("data/entropias.xls",sheetIndex=1, header=FALSE, encoding = "UTF-8")
names(entropia)<-c("ID","entroexp", "entrosearch", "entropur", "entropost")
# Añadir a LONGDAT
entropia_long <- gather(entropia, asignada, entropia, entroexp:entropost)
entropia_long$Asignada[entropia_long$asignada == "entroexp"] <- "explor"
entropia_long$Asignada[entropia_long$asignada == "entrosearch"] <- "search"
entropia_long$Asignada[entropia_long$asignada == "entropur"] <- "purchase"
entropia_long$Asignada[entropia_long$asignada == "entropost"] <- "post"


LongDat <- merge(x=LongDat, y=entropia_long[,c(1,3,4)], by=c("ID", "Asignada" ), all.x=TRUE)

#Probabilidades

p_explor<- xlsx::read.xlsx("data/p_explore.xls",sheetIndex=1, header=TRUE, encoding = "UTF-8")
names(p_explor)<- c("ID", "Ep11", "Ep12", "Ep13", "Ep21", "Ep22", "Ep23", "Ep31", "Ep32", "Ep33")


p_search<-xlsx::read.xlsx("data/p_search.xls",sheetIndex=1, header=TRUE, encoding = "UTF-8")
names(p_search)<- c("ID", "Sp11", "Sp12", "Sp13", "Sp21", "Sp22", "Sp23", "Sp31", "Sp32", "Sp33")


p_purchase<- xlsx::read.xlsx("data/p_purchase.xls",sheetIndex=1, header=TRUE, encoding = "UTF-8")
names(p_purchase)<- c("ID", "Pup11", "Pup12", "Pup13", "Pup21", "Pup22", "Pup23", "Pup31", "Pup32", "Pup33")


p_post<-xlsx::read.xlsx("data/p_post.xls",sheetIndex=1, header=TRUE, encoding = "UTF-8")
names(p_post)<- c("ID", "Post11", "Post12", "Post13", "Post21", "Post22", "Post23", "Post31", "Post32", "Post33")

## Primer variable p11
parea1<-cbind(p_explor[,c(1:5,8)], p_search[,c(2:5,8)], p_purchase[,c(2:5,8)], p_post[,c(2:5,8)])

parea1_11<-parea1[,c(1,2,7,12,17)]
# Añadir a LONGDAT
parea1_11_long <- gather(parea1_11, asignada, p11, Ep11:Post11)
parea1_11_long$Asignada[parea1_11_long$asignada == "Ep11"] <- "explor"
parea1_11_long$Asignada[parea1_11_long$asignada == "Sp11"] <- "search"
parea1_11_long$Asignada[parea1_11_long$asignada == "Pup11"] <- "purchase"
parea1_11_long$Asignada[parea1_11_long$asignada == "Post11"] <- "post"


LongDat <- merge(x=LongDat, y=parea1_11_long[,c(1,3,4)], by=c("ID", "Asignada" ), all.x=TRUE)

## Segunda variable p12
parea1_12<-parea1[,c(1,3,8,13,18)]

# Añadir a LONGDAT
parea1_12_long <- gather(parea1_12, asignada, p12, Ep12:Post12)
parea1_12_long$Asignada[parea1_12_long$asignada == "Ep12"] <- "explor"
parea1_12_long$Asignada[parea1_12_long$asignada == "Sp12"] <- "search"
parea1_12_long$Asignada[parea1_12_long$asignada == "Pup12"] <- "purchase"
parea1_12_long$Asignada[parea1_12_long$asignada == "Post12"] <- "post"

LongDat <- merge(x=LongDat, y=parea1_12_long[,c(1,3,4)], by=c("ID", "Asignada" ), all.x=TRUE)

## Tercera variable p13
parea1_13<-parea1[,c(1,4,9,14,19)]

# Añadir a LONGDAT
parea1_13_long <- gather(parea1_13, asignada, p13, Ep13:Post13)
parea1_13_long$Asignada[parea1_13_long$asignada == "Ep13"] <- "explor"
parea1_13_long$Asignada[parea1_13_long$asignada == "Sp13"] <- "search"
parea1_13_long$Asignada[parea1_13_long$asignada == "Pup13"] <- "purchase"
parea1_13_long$Asignada[parea1_13_long$asignada == "Post13"] <- "post"

LongDat <- merge(x=LongDat, y=parea1_13_long[,c(1,3,4)], by=c("ID", "Asignada" ), all.x=TRUE)

## Para el area 2

parea2<-cbind(p_explor[,c(1, 5, 6, 7)], p_search[,c(5:7)], p_purchase[,c(5:7)], p_post[,c(5:7)])

parea2_22<-parea2[,c(1,3, 6, 9,12)]
# Añadir a LONGDAT
parea2_22_long <- gather(parea2_22, asignada, p22, Ep22:Post22)
parea2_22_long$Asignada[parea2_22_long$asignada == "Ep22"] <- "explor"
parea2_22_long$Asignada[parea2_22_long$asignada == "Sp22"] <- "search"
parea2_22_long$Asignada[parea2_22_long$asignada == "Pup22"] <- "purchase"
parea2_22_long$Asignada[parea2_22_long$asignada == "Post22"] <- "post"

LongDat <- merge(x=LongDat, y=parea2_22_long[,c(1,3,4)], by=c("ID", "Asignada" ), all.x=TRUE)

parea2_21<-parea2[,c(1,2, 5, 8,11)]
# Añadir a LONGDAT
parea2_21_long <- gather(parea2_21, asignada, p21, Ep21:Post21)
parea2_21_long$Asignada[parea2_21_long$asignada == "Ep21"] <- "explor"
parea2_21_long$Asignada[parea2_21_long$asignada == "Sp21"] <- "search"
parea2_21_long$Asignada[parea2_21_long$asignada == "Pup21"] <- "purchase"
parea2_21_long$Asignada[parea2_21_long$asignada == "Post21"] <- "post"

LongDat <- merge(x=LongDat, y=parea2_21_long[,c(1,3,4)], by=c("ID", "Asignada" ), all.x=TRUE)

parea2_23<-parea2[,c(1,4, 7, 10,13)]

# Añadir a LONGDAT

parea2_23_long <- gather(parea2_23, asignada, p23, Ep23:Post23)
parea2_23_long$Asignada[parea2_23_long$asignada == "Ep23"] <- "explor"
parea2_23_long$Asignada[parea2_23_long$asignada == "Sp23"] <- "search"
parea2_23_long$Asignada[parea2_23_long$asignada == "Pup23"] <- "purchase"
parea2_23_long$Asignada[parea2_23_long$asignada == "Post23"] <- "post"

LongDat <- merge(x=LongDat, y=parea2_23_long[,c(1,3,4)], by=c("ID", "Asignada" ), all.x=TRUE)


## Para el area 3

parea3<-cbind(p_explor[,c(1, 8:10)], p_search[,c( 8:10)], p_purchase[,c( 8:10)], p_post[,c( 8:10)])

parea3_33<-parea3[,c(1,4, 7, 10,13)]
# Añadir a LONGDAT
parea3_33_long <- gather(parea3_33, asignada, p33, Ep33:Post33)
parea3_33_long$Asignada[parea3_33_long$asignada == "Ep33"] <- "explor"
parea3_33_long$Asignada[parea3_33_long$asignada == "Sp33"] <- "search"
parea3_33_long$Asignada[parea3_33_long$asignada == "Pup33"] <- "purchase"
parea3_33_long$Asignada[parea3_33_long$asignada == "Post33"] <- "post"

LongDat <- merge(x=LongDat, y=parea3_33_long[,c(1,3,4)], by=c("ID", "Asignada" ), all.x=TRUE)

parea3_31<-parea3[,c(1,2, 5, 8,11)]
# Añadir a LONGDAT
parea3_31_long <- gather(parea3_31, asignada, p31, Ep31:Post31)
parea3_31_long$Asignada[parea3_31_long$asignada == "Ep31"] <- "explor"
parea3_31_long$Asignada[parea3_31_long$asignada == "Sp31"] <- "search"
parea3_31_long$Asignada[parea3_31_long$asignada == "Pup31"] <- "purchase"
parea3_31_long$Asignada[parea3_31_long$asignada == "Post31"] <- "post"

LongDat <- merge(x=LongDat, y=parea3_31_long[,c(1,3,4)], by=c("ID", "Asignada" ), all.x=TRUE)


parea3_32<-parea3[,c(1,3, 6, 9,12)]

# Añadir a LONGDAT
parea3_33_long <- gather(parea3_32, asignada, p32, Ep32:Post32)
parea3_33_long$Asignada[parea3_33_long$asignada == "Ep32"] <- "explor"
parea3_33_long$Asignada[parea3_33_long$asignada == "Sp32"] <- "search"
parea3_33_long$Asignada[parea3_33_long$asignada == "Pup32"] <- "purchase"
parea3_33_long$Asignada[parea3_33_long$asignada == "Post32"] <- "post"

LongDat <- merge(x=LongDat, y=parea3_33_long[,c(1,3,4)], by=c("ID", "Asignada" ), all.x=TRUE)


### Table 2. Model Seconds


dependentvariables<- c("Intercept", "Search", "Purchase", "PostPurchase",
                       "Phones", "Pens", "HDs", "Involvement", "Search Online", "Purchase Online")
LongDat$AsTask<- LongDat$Asignada
LongDat$Category<- LongDat$category
LongDat$Time<-LongDat$seconds

LongDat$subject<- LongDat$dataindividuo.n

model1 <- lmer(Time ~ AsTask+Category+IMPGlobal+ busca+compra 
               +(1 | subject), data=LongDat, na.action = na.exclude, REML = FALSE)

## Coeficientes
ctable <- coef(summary(model1))
## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
## combined table
ctable <- cbind(ctable, "p value" = p)
row.names(ctable)<- c("Intercept", "Search", "Purchase", "After Sale",
                      "Mobile phones", "Ball-point pens", "Hard disks", "Involvement", "Search Online", "Purchase Online")
coeffpart<-ctable[,c(1:2,4)]
coeffpart<-format(round(coeffpart, 3), nsmall = 3)




## Parte random del modelo

library(tidyverse)
Random<- summary(model1)$varcor
rand2<-as.data.frame(Random)
names(rand2)<-c("Group", "var1", "var2","Variance","Std.Dev.")
randompart<-rand2[,c(4,5)]
randompart<- format(round(randompart, 3), nsmall = 3)
r<-rbind(c("Variance", "Std.Dev."), as.character(randompart[,1]),as.character(randompart[,2]))
r2<- cbind(r," "=c(" ", " "))
row.names(r2)<-c(" ", "Subject", "Residual")
coeficientes<- rbind(coeffpart, r2)

# Ajuste del modelo

Ajuste<-summary(model1)$AICtab
n<- summary(model1)$ngrps
ajusteb<-as.data.frame(Ajuste[c(1:2,5)])
ajusteb<- rbind(ajusteb,n)
row.names(ajusteb)<- c("AIC", "BIC", "Residual DF", "Subjects")
ajusteb<- format(round(ajusteb, 3), nsmall = 3)
ajuste2<- cbind(as.character(ajusteb[,1])," "=c(" "," "," "," ")," "=c(" "," "," "," "))
row.names(ajuste2)<- c("AIC", "BIC", "Residual DF", "Subjects")
names(ajuste2)<-names(coeffpart)
tabla<- rbind(coeficientes, ajuste2)


library(kableExtra)

knitr::kable(tabla,format = "latex", booktabs = T, digits = c(3,3,3),
             align=c("r")) %>% 
  kable_styling() %>%
  group_rows("Task (base exploration)", 2, 4) %>%
  group_rows("Category (base sport shoes)", 5, 7) %>%
  group_rows("Random part", 11, 13) %>%
  group_rows("Fit", 14, 17) %>%
  footnote(general = "Linear mixed model fit by maximum likelihood", general_title = "Note: ", footnote_as_chunk = T)

knitr::kable(tabla,format = "html", booktabs = T, digits = c(3,3,3),
             align=c("r")) %>% 
  kable_styling() %>%
  group_rows("Task (base exploration)", 2, 4) %>%
  group_rows("Category (base sport shoes)", 5, 7) %>%
  group_rows("Random part", 11, 13) %>%
  group_rows("Fit", 14, 17) %>%
  footnote(general = "Linear mixed model fit by maximum likelihood", general_title = "Note: ", footnote_as_chunk = T)
