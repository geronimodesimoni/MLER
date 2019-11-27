
##2. Empleo por aglomerados

Muestra.Longitudinal.Empleo.Registrado%>%
  select(rt201501, provi)%>%
  group_by(provi)%>%
  filter(rt201501!="NA")%>%
  summarize(relaciones_laborales_201501=n())

#Para todos los períodos:
rt.c<-Muestra.Longitudinal.Empleo.Registrado%>%
  filter(provi=="Chaco")%>%
  select(starts_with("rt"))

rs.c<-Muestra.Longitudinal.Empleo.Registrado%>%
  filter(provi=="Chaco")%>%
  select(sexo)

##calculamos el salario promedio por provincia
relaciones.p.prom<-apply(rt, 2, function(x) tapply(x, rp, mean, na.rm = TRUE ))

##calculamos las relaciones por provincia

sum.na<-function(x){ 
  tapply(x, rs.c, function(x) sum(!is.na(x)))
}


relaciones.ch.s<-apply(rt.c, 2, sum.na)

colSums(relaciones.ch.s)

##write.csv(relaciones.ch.s, "relaciones.chaco.sex.csv")

##altas y bajas

##agrupamos la muestra
Muestra.ch.sex<- Muestra.Longitudinal.Empleo.Registrado %>%
  filter(provi=="Chaco")%>%
  group_by(sexo) %>%
  nest(starts_with("rt"))

#glimpse(Muestra.p)
#glimpse(Muestra.p$data)


#vemos en qué lugar está cada provincia
distinct(rs.c)

##calculamos
Muestra.ch.sex$sexo[1]

#promedio de remuneraciones
#sum.d<-map_dbl(Muestra.p$data[[5]], mean ,na.rm= TRUE)

view(sum.d)


##rotaciones

xj.ch.sm<-table(c("alta","baja","sigue"))


for(i in 1:239){
    x<-ifelse(is.na(Muestra.ch.sex$data[[2]][i]) & Muestra.ch.sex$data[[2]][i+1]!="NA","alta",ifelse(is.na(Muestra.ch.sex$data[[2]][i+1]) & Muestra.ch.sex$data[[2]][i]!="NA","baja","sigue"))
    x<-table(x)
    xj.ch.sm<-rbind(xj.ch.sm,x)
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

DIch<-relaciones.p["Chaco",]


DIch=DIch[-240]

todoch<-cbind(xj.p,DIch)

todoch<-todoch%>%
  mutate(tasa.entrada=alta/DIch, tasa.salida=baja/DIch, var.empleo=(alta-baja)/DIch)

todoch<-todoch%>%
  mutate(DF=DIch+alta-baja)

todoch$t<-t
todoch$t<-as.Date(todoch$t)
#format(as.Date(todo$t), "%Y-%m")

todoch.indice <- todoch%>%
  filter(t >= (as.Date("2008-08-01")))

todoch.indice$var.empleo[1]=0

##generamor el indice de empleo
todoch.indice<-todoch.indice%>%
  mutate(indice.empleo=1+cumsum(var.empleo))


plot(todoch.indice$t,todoch.indice$indice.empleo, type = "line") 

##write.csv(xj.ch.sm, "xj.ch.sm.csv")



