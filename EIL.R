# Librarys
library(rJava)
library(dplyr)
library(tidyverse)
library(sparklyr)
library(pryr)
library(ggplot2)
library(trelliscopejs)
library(tidyr)
library(plotly)
library(purrr)
library(beepr)
library(lubridate)

#cargamos la base de datos:
#load("Muestra.Longitudinal.Empleo.Rda")

##Encuesta de Indicadores Laborales (EIL): perìdo 01/2015
##Cantidad de relaciones laborales:

Muestra.Longitudinal.Empleo.Registrado %>%
  distinct(pondera)

table(Muestra.Longitudinal.Empleo.Registrado$pondera)

##relaciones laborales 08/2001 - Año base
x1<-Muestra.Longitudinal.Empleo.Registrado%>%
  select(rt200108)%>%
  filter(rt200108!="NA")%>%
  summarize(relaciones_laborales_200108=n())


##relaciones laborales 01/2015
Muestra.Longitudinal.Empleo.Registrado%>%
  select(rt201501)%>%
  filter(rt201501!="NA")%>%
  summarize(relaciones_laborales_201501=n())

#Para todos los períodos:
rt<-Muestra.Longitudinal.Empleo.Registrado%>%
  select(starts_with("rt"))


##Calculamos el salario promedio por período
relaciones.prom<- apply(rt, 2, mean, na.rm = TRUE )

plot(xt, type = "l")

ggplot(xt, aes(t,indice))+
  geom_line()

relaciones$relaciones[[1]]

indice<- relaciones%>%
  mutate(indice=relaciones/relaciones[[1]])

t<-seq(ISOdate(1996,1,1), by = "month", length.out = 240)
t<-as_date(t)
xt<-cbind(t,indice)



#calculamos la antidad de relaciones por período
relaciones<-sapply(rt, function(x) sum(!is.na(x)))
relaciones<-as.data.frame(relaciones)
plot(relaciones)

#t1<-seq(ISOdate(1996,1,1), by = "month", length.out = 240)
#t1<-as_date(t1)
#t1<-format(as.Date(t1), "%Y-%m")
#relaciones1<-cbind(t1,relaciones)
#relaciones1<-as.data.frame(relaciones1)
#str(relaciones1)

##altas y bajas
rt<-Muestra.Longitudinal.Empleo.Registrado%>%
  select(starts_with("rt"))

xj<-table(c("alta","baja","sigue"))

for(i in 1:239){
x<-ifelse(is.na(rt[[i]]) & rt[[i+1]]!="NA","alta",ifelse(is.na(rt[[i+1]]) & rt[[i]]!="NA","baja","sigue"))
xi<-table(x)
xj<-rbind(xj,xi)
}

xm<-as.data.frame(xj)
xm=xm[-1,]

t<-seq(ISOdate(1996,2,1), by = "month", length.out = 239)
t<-as_date(t)
#t<-format(as.Date(t), "%Y-%m")
xt<-cbind(t,xm)

xt%>%
select(t,sigue)%>%
plot(xt)

##comprobación
#xt$sigue[1]+xt$alta[1]
#relaciones1$relaciones[2]

##tasas e índices

DI<-relaciones[-240]
todo<-cbind(xt,DI)

todo<-todo%>%
  mutate(tasa.entrada=alta/DI, tasa.salida=baja/DI, var.empleo=(alta-baja)/DI)
  
todo<-todo%>%
  mutate(DF=DI+alta-baja)

todo$t<-t

todo$t<-as.Date(todo$t)

#format(as.Date(todo$t), "%Y-%m")

todo2 <- todo%>%
  filter(t >= (as.Date("2001-08-01")))

todo2$var.empleo[1]=0

##generamor el indice de empleo
todo.indice<-todo2%>%
  mutate(indice.empleo=1+cumsum(var.empleo))


plot(todo.indice$t,todo.indice$indice.empleo, type = "line") 
#graficamos desde 10/2007 como el informe

grafico<-todo.indice%>%
  filter(t >= (as.Date("2007-10-01")))
  
plot(grafico$t,grafico$indice.empleo, type = "line") 
##, ylim = c(1,1.55))

#write.csv(todo.indice, "todo.indice.csv")
  
##empleo registrado del sector privado para el total de los aglomerados


##Gráfico 1: Evolución mensual del nivel de empleo del total de aglomerados
####El nivel de empleo es la construcción de una evolución acumulada a partir del período base,
####en el caso de la Encuesta de Indicadores Laborales desde agosto de 2001.


##1. Tasas de entrada, de salida, de despidos y suspensiones para el total de
####aglomerados urbanos


##2. Empleo por aglomerados

Muestra.Longitudinal.Empleo.Registrado%>%
  select(rt201501, provi)%>%
  group_by(provi)%>%
  filter(rt201501!="NA")%>%
  summarize(relaciones_laborales_201501=n())




##3. Empleo por rama de actividad
Muestra.Longitudinal.Empleo.Registrado%>%
  select(rt201501, letra)%>%
  group_by(letra)%>%
  filter(rt201501!="NA")%>%
  summarize(relaciones_laborales_201501=n())


##4. Empleo por tamaño de empresa
Muestra.Longitudinal.Empleo.Registrado%>%
  select(rt201501, tr_0415)%>%
  group_by(tr_0415)%>%
  filter(rt201501!="NA")%>%
  summarize(relaciones_laborales_201501=n())


##5. Empleo por modalidad contractual y calificación de la tarea
##no hay datos

##6. Puestos vacantes y demanda laboral
##no hay datos

##Tabla 1: Tasas de entrada y de salida para el total de aglomerados urbanos.

xj<-table(c("alta","baja","sigue"))

for(i in 1:239){
  x<-ifelse(is.na(rt[[i]]) & rt[[i+1]]!="NA","alta",ifelse(is.na(rt[[i+1]]) & rt[[i]]!="NA","baja","sigue"))
  xi<-table(x)
  xj<-rbind(xj,xi)
}

xj


##Tabla 2: Tasa de despidos y suspensiones, empresas que aplicaron suspensiones y proporción de
####suspensiones registradas cada 100 trabajadores para el total de aglomerados urbanos.



##Tabla 3A: Evolución del empleo y principales tasas por aglomerado


##Tabla 3B: Evolución del empleo trimestral y anual trimestral


##Tabla 4: Evolución del empleo por rama de actividad y tamaño de empresa


##Tabla 8: Universo EIL, empresas y empleo.




