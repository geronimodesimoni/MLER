
##2. Empleo por aglomerados

Muestra.Longitudinal.Empleo.Registrado%>%
  select(rt201501, provi)%>%
  group_by(provi)%>%
  filter(rt201501!="NA")%>%
  summarize(relaciones_laborales_201501=n())

#Para todos los períodos:
rt<-Muestra.Longitudinal.Empleo.Registrado%>%
  select(starts_with("rt"))

rp<-Muestra.Longitudinal.Empleo.Registrado%>%
  select(provi)

##calculamos el salario promedio por provincia


relaciones.p.prom<-apply(rt, 2, function(x) tapply(x, rp, mean, na.rm = TRUE ))

##calculamos las relaciones por provincia

sum.na<-function(x){ 
  tapply(x, rp, function(x) sum(!is.na(x)))
}

relaciones.p<-apply(rt, 2, sum.na)

colSums(relaciones.p)

##write.csv(relaciones.p, "relaciones.prov.csv")

##altas y bajas

##agrupamos la muestra
Muestra.p<- Muestra.Longitudinal.Empleo.Registrado %>%
  group_by(provi) %>%
  nest(starts_with("rt"))

#glimpse(Muestra.p)
#glimpse(Muestra.p$data)


#vemos en qué lugar está cada provincia
distinct(rp)

##calculamos para Cordoba
Muestra.p$provi[5]

#promedio de remuneraciones
sum.d<-map_dbl(Muestra.p$data[[5]], mean ,na.rm= TRUE)

view(sum.d)


##rotaciones

xj.p<-table(c("alta","baja","sigue"))

for(i in 1:239){
    x<-ifelse(is.na(Muestra.p$data[[5]][i]) & Muestra.p$data[[5]][i+1]!="NA","alta",ifelse(is.na(Muestra.p$data[[5]][i+1]) & Muestra.p$data[[5]][i]!="NA","baja","sigue"))
    x<-table(x)
    xj.p<-rbind(xj.p,x)
  }


xj.p<-as.data.frame(xj.p)
xj.p=xj.p[-1,]

t<-seq(ISOdate(1996,1,1), by = "month", length.out = 239)
t<-as_date(t)
#t<-format(as.Date(t), "%Y-%m")
xj.p<-cbind(t,xj.p)

xj.p%>%
  select(t,sigue)%>%
  plot(xj.p, type = "line")

##tasas e índices

DIcba<-relaciones.p["Cordoba",]


DIcba=DIcba[-240]

todocba<-cbind(xj.p,DIcba)

todocba<-todocba%>%
  mutate(tasa.entrada=alta/DIcba, tasa.salida=baja/DIcba, var.empleo=(alta-baja)/DIcba)

todocba<-todocba%>%
  mutate(DF=DIcba+alta-baja)

todocba$t<-t
todocba$t<-as.Date(todocba$t)
#format(as.Date(todo$t), "%Y-%m")

todocba.indice <- todocba%>%
  filter(t >= (as.Date("2001-08-01")))

todocba.indice$var.empleo[1]=0

##generamor el indice de empleo
todocba.indice<-todocba.indice%>%
  mutate(indice.empleo=1+cumsum(var.empleo))


plot(todocba.indice$t,todocba.indice$indice.empleo, type = "line") 

##write.csv(todocba.indice, "todocba.indice.csv")



