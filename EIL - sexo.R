##2. Empleo por sexo

Muestra.Longitudinal.Empleo.Registrado%>%
  select(rt201501, sexo)%>%
  group_by(sexo)%>%
  filter(rt201501!="NA")%>%
  summarize(relaciones_laborales_201501=n())

#Para todos los períodos:
rt<-Muestra.Longitudinal.Empleo.Registrado%>%
  select(starts_with("rt"))

rs<-Muestra.Longitudinal.Empleo.Registrado%>%
  select(sexo)

##calculamos el salario promedio por sexo
relaciones.s.prom<-apply(rt, 2, function(x) tapply(x, rs, mean, na.rm = TRUE ))

##calculamos las relaciones por sexo

sum.na.s<-function(x){ 
  tapply(x, rs, function(x) sum(!is.na(x)))
}

relaciones.s<-apply(rt, 2, sum.na.s)

colSums(relaciones.s)

##write.csv(relaciones.s, "relaciones.sex.csv")




