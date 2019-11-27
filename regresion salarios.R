##regresión salarios con edad


regresion<-Muestra.Longitudinal.Empleo.Registrado%>%
  select(rt201501, fnac_anu, sexo)%>%
  filter(!is.na(rt201501))%>%
  filter(sexo!="Sin dato")%>%
  mutate(edad=2015-fnac_anu)%>%
  mutate(mujer= ifelse (sexo=="Mujer",1,0))%>%
  mutate(em=edad*mujer)

reg <- lm(regresion$rt201501 ~ regresion$edad + regresion$mujer + regresion$em)

summary(reg)


Muestra.Longitudinal.Empleo.Registrado%>%
  select(rt199601)%>%
  filter(!is.na(rt199601))%>%
 min(rt199601)

names(Muestra.Longitudinal.Empleo.Registrado)

unique(Muestra.Longitudinal.Empleo.Registrado$letra)
unique(Muestra.Longitudinal.Empleo.Registrado$tr_0496)

regresion1<-Muestra.Longitudinal.Empleo.Registrado%>%
  select(rt199601, fnac_anu, sexo, letra, provi, tr_0496, relacion, pondera)%>%
  filter(!is.na(rt199601))%>%
  filter(rt199601>10)%>%
  filter(sexo!="Sin dato")%>%
  mutate(lg_rt= log(rt199601))%>%
  mutate(edad=1996-fnac_anu)%>%
  mutate(mujer= ifelse (sexo=="Mujer",1,0))%>%
  mutate(em=edad*mujer)%>%
  mutate(pampa= ifelse((provi == "Cordoba")|(provi==  "Santa Fe")|(provi=="Buenos Aires") |(provi=="La Pampa") |(provi=="Capital Federal") |(provi=="Gran Buenos Aires"), 1,0))%>%
  mutate(cuyo= ifelse ((provi=="Mendoza") | (provi== "San Juan") | (provi=="san luis"), 1, 0))%>%
  mutate(patagonia= ifelse((provi=="Neuquén")| (provi=="Río Negro")| (provi=="Chubut")|(provi=="Santa Cruz")|(provi=="Tierra del Fuego"),1,0))%>%
  mutate(noroeste= ifelse ((provi=="Jujuy") | (provi=="Salta") | (provi=="La Rioja")| (provi=="Tucumán")| (provi=="Catamarca")| (provi== "Santiago del Estero"),1,0)) 


unique(regresion1$edad)

(regresion1$edad)^2

reg1 <- lm(regresion1$lg_rt ~ regresion1$edad + regresion1$em + regresion1$mujer), weights = regresion1$pondera)

reg2 <- lm(regresion1$lg_rt ~ regresion1$edad  + regresion1$mujer + regresion1$letra + regresion1$provi + regresion1$tr_0496)

reg2 <- lm(regresion1$rt199601 ~ regresion1$edad  + regresion1$mujer + regresion1$letra + regresion1$provi + regresion1$tr_0496)

reg2 <- lm(regresion1$lg_rt ~ regresion1$edad  + regresion1$mujer + regresion1$em + regresion1$letra + regresion1$noroeste + regresion1$pampa + regresion1$cuyo + regresion1$patagonia) , weights = regresion1$pondera)     + regresion1$tr_0496)

reg3 <- lm(scale(regresion1$lg_rt) ~ scale(regresion1$edad)  + scale(regresion1$em)  + scale(regresion1$mujer) + regresion1$relacion + regresion1$letra + regresion1$provi + regresion1$tr_0496), weights = regresion1$pondera)

reg4 <- lm(scale(regresion1$lg_rt) ~ scale(regresion1$edad)  + scale(regresion1$mujer) + regresion1$relacion + (regresion1$letra-"M") + regresion1$provi + regresion1$tr_0496)


summary(reg1)

summary(reg2)

summary(reg3)

summary(reg4)


library(stargazer)

comparacion<-stargazer(reg1, reg2, type = "text", title = "salarios", align =TRUE, out="table1.txt" )

comparacion<-stargazer(reg1, reg2, reg3, type = "html", title = "salarios", align =TRUE, out="table1.htm" )

provincias<-Muestra.Longitudinal.Empleo.Registrado%>%
  group_by(provi)%>%
  tally

tamaño.emp<-regresion1%>%
  group_by(tr_0496)%>%
  tally

fnac<-regresion1%>%
  group_by(provi)%>%
  tally
