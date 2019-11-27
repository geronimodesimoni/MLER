
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

##calculamos para Buenos Aires
Muestra.p$provi[2]

#promedio de remuneraciones
sum.d<-map_dbl(Muestra.p$data[[2]], mean ,na.rm= TRUE)

view(sum.d)


##rotaciones

xj.p<-table(c("alta","baja","sigue"))

for(i in 1:239){
    x<-ifelse(is.na(Muestra.p$data[[2]][i]) & Muestra.p$data[[2]][i+1]!="NA","alta",ifelse(is.na(Muestra.p$data[[2]][i+1]) & Muestra.p$data[[2]][i]!="NA","baja","sigue"))
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

DIba<-relaciones.p["Buenos Aires",]


DIba=DIba[-240]

todoba<-cbind(xj.p,DIba)

todoba<-todoba%>%
  mutate(tasa.entrada=alta/DIba, tasa.salida=baja/DIba, var.empleo=(alta-baja)/DIba)

todoba<-todoba%>%
  mutate(DF=DIba+alta-baja)

todoba$t<-t
todoba$t<-as.Date(todoba$t)
#format(as.Date(todo$t), "%Y-%m")

todoba.indice <- todoba%>%
  filter(t >= (as.Date("2001-08-01")))

todoba.indice$var.empleo[1]=0

##generamor el indice de empleo
todoba.indice<-todoba.indice%>%
  mutate(indice.empleo=1+cumsum(var.empleo))


plot(todoba.indice$t,todoba.indice$indice.empleo, type = "line") 

##write.csv(todoba.indice, "todoba.indice.csv")



