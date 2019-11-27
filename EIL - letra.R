##3. Empleo por rama de actividad
Muestra.Longitudinal.Empleo.Registrado%>%
  select(rt201501, letra)%>%
  group_by(letra)%>%
  filter(rt201501!="NA")%>%
  summarize(relaciones_laborales_201501=n())

#Para todos los períodos:
rt<-Muestra.Longitudinal.Empleo.Registrado%>%
  select(starts_with("rt"))

rl<-Muestra.Longitudinal.Empleo.Registrado%>%
  select(letra)

##calculamos el salario promedio por sexo
#relaciones.s.prom<-apply(rt, 2, function(x) tapply(x, rs, mean, na.rm = TRUE ))

##calculamos las relaciones por rubro

sum.na.s<-function(x){ 
  tapply(x, rl, function(x) sum(!is.na(x)))
}

relaciones.l<-apply(rt, 2, sum.na.s)

colSums(relaciones.l)

#write.csv(relaciones.l, "relaciones.rubro.csv")

distinct(rs)


table(Muestra.Longitudinal.Empleo.Registrado$letra)



