library(ggplot2)
library(dplyr)
library(foreign)

setwd("C:/Users/Geronimo/Desktop/muestra longitudinal empleo registrado")

(WD <- getwd())

Muestra.Longitudinal.Empleo.Registrado <- read.csv("Muestra Longitudinal Empleo Registrado.csv", header = TRUE, sep = ",")

#geom bar
ggplot(Muestra.Longitudinal.Empleo.Registrado, aes(sexo)) +
  geom_bar()

#histograma
ggplot(Muestra.Longitudinal.Empleo.Registrado, aes(rt199707)) +
  geom_histogram()

#vemos los nombres de las variables
head(Muestra.Longitudinal.Empleo.Registrado)
#vemos cuales son factor=numeros
lista<-sapply(Muestra.Longitudinal.Empleo.Registrado, is.factor)

#vemos las dimensiones de la tabla   
dim(Muestra.Longitudinal.Empleo.Registrado)
dim(ingrt)

##vemos las remuneraciones promedio por edades en el periodo 07/1997 pra hombres y mujeres

rm(by_year)
rm(Muestra_sexo)

by_year <- Muestra.Longitudinal.Empleo.Registrado %>%
  select(fnac_anu, sexo, rt199707)%>%
  filter(!sexo=="Sin dato", !is.na(rt199707))%>%
  group_by(fnac_anu, sexo) %>%
  summarise(medianrt199707 = median(rt199707, na.rm = TRUE))


by_year <- Muestra.Longitudinal.Empleo.Registrado %>%
  select(fnac_anu, sexo, rt199707)%>%
  filter(!is.na(rt199707))%>%
  group_by(fnac_anu, sexo) %>%
  summarise(medianrt199707 = median(rt199707, na.rm = TRUE))



ggplot(by_year, aes(fnac_anu, medianrt199707, color=sexo))+
geom_line()+
  facet_trelliscope(~ sexo)





