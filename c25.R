# CREACION DE LA VARIABLE: CATEGORIA OCUPACIONAL 
# (1:Empleador; 2:Cuenta Propia; 3:Trabajador dependiente)
info2$variables$cat.ocup = recode(info2$variables$categoria_ocupacion, `1`= 1, 
                                  `2`= 2, `3` =3, `4`=3, `5`=3, `6`=3,
                                  `7`=4, .default = 0, .missing = 99)

info2$variables = mutate(info2$variables, cat.ocup = ifelse(cat.ocup==3 & b8==2,3,
                                      ifelse(cat.ocup==3 & b8==1 & b9==1,4, 
                                      ifelse(cat.ocup==3 & b8==1 & b9==2,5, 
                                      ifelse(cat.ocup==1,1, 
                                      ifelse(cat.ocup==2,2, 
                                      ifelse(cat.ocup==4,6,NA)))))))

info2$variables$cat.ocup = factor(info2$variables$cat.ocup, 
                                  levels=c(1,2,3,4,5,6),
                                  labels=c("empleador","cuenta propia","asalariado sin contrato", 
                                           "asalariado con contrato definido",
                                           "asalariado con contrato indefinido",
                                           "no remunerado"))


# CREACION DE LA VARIABLE: CATEGORIA OCUPACIONAL 
# (1:Empleador; 2:Cuenta Propia; 3:Trabajador dependiente)
for (i in 1:length(info)){
  info[[i]]$variables$cat.ocup = recode(info[[i]]$variables$categoria_ocupacion, `1`= 1, 
                                        `2`= 2, `3` =3, `4`=3, `5`=3, `6`=3,
                                        `7`=4, .default = 0, .missing = 99)
}

for (i in 1:length(info)){
  info[[i]]$variables = mutate(info[[i]]$variables, cat.ocup = ifelse(cat.ocup==3 & b8==2,3,
                                              ifelse(cat.ocup==3 & b8==1 & b9==1,4, 
                                              ifelse(cat.ocup==3 & b8==1 & b9==2,5, 
                                              ifelse(cat.ocup==1,1, 
                                              ifelse(cat.ocup==2,2, 
                                              ifelse(cat.ocup==4,6,NA)))))))
}

for (i in 1:length(info)){
  info[[i]]$variables$cat.ocup = factor(info[[i]]$variables$cat.ocup, 
                                        levels=c(1,2,3,4,5,6),
                                        labels=c("empleador","cuenta propia","asalariado sin contrato", 
                                                 "asalariado con contrato definido",
                                                 "asalariado con contrato indefinido",
                                                 "no remunerado"))
}

#-------------------------------------------------------------------------------
### CUADRO 25: Trabajadores con jornada completa en nuble segun categoria ocupacional
#-------------------------------------------------------------------------------
###################################################################
### Tasa Trabajadores con jornada completa en NACIONAL segun categoria ocupacional
###################################################################
#
tasa.ocupados.nacional.jornada.completa = svyby(~I(cae_general=="Ocupado" &  c1==1), by=~cat.ocup,
                                           denominator=~I(cae_general=="Ocupado"),info2, svyratio,
                                           multicore = TRUE, drop.empty.groups = FALSE, 
                                           na.rm=TRUE)

freq= xtabs(~I(cae_general=="Ocupado" &  c1==1)+cat.ocup, data=info2$variables)[2,]

tasa.ocupados.nacional.jornada.completa$cv = 
  tasa.ocupados.nacional.jornada.completa$`se.I(cae_general == \"Ocupado\" & c1 == 1)/I(cae_general == \"Ocupado\")`/tasa.ocupados.nacional.jornada.completa$`I(cae_general == \"Ocupado\" & c1 == 1)/I(cae_general == \"Ocupado\")`

tasa.ocupados.nacional.jornada.completa$frecuencia = freq

names(tasa.ocupados.nacional.jornada.completa) = 
  c("cat.ocup", "tasa.ocupados.nacional.jornada.completa", "error estandar","cv","frecuencia")

write.csv(tasa.ocupados.nacional.jornada.completa, "tasa_ocupados_nacional_jornada_completa.csv")

#
tasa.ocupados.nacional.jornada.completa.promedio = list()
for (i in 1:length(info)){
  tasa.ocupados.nacional.jornada.completa.promedio[[i]] = svyratio(~I(cae_general=="Ocupado" & c1==1), 
                                              denominator=~I(cae_general=="Ocupado"),
                                              info[[i]],multicore = TRUE, 
                                              drop.empty.groups = FALSE, 
                                              na.rm=TRUE)
}

tasa.ocupados.nacional.jornada.completa.promedio. = unlist(lapply(tasa.ocupados.nacional.jornada.completa.promedio, '[[', 1) ) 

ee.tasa.ocupados.nacional.jornada.completa.promedio = unlist(lapply(tasa.ocupados.nacional.jornada.completa.promedio, SE))

tasa.ocupados.nacional.jornada.completa.promedio.. = data.frame(tasa.ocupados.nacional.jornada.completa.promedio., ee.tasa.ocupados.nacional.jornada.completa.promedio)

write.csv(tasa.ocupados.nacional.jornada.completa.promedio.., "tasa.ocupados_nacional_jornada_completa_promedio.csv")

###################################################################
# Tasa Trabajadores con jornada completa en NUBLE segun categoria ocupacional
###################################################################
#
tasa.ocupados.nuble.jornada.completa = svyby(~I(prov_e==84 & c1==1), by=~cat.ocup,
                                                denominator=~I(prov_e==84),info2, svyratio,
                                                multicore = TRUE, drop.empty.groups = FALSE, 
                                                na.rm=TRUE)

freq= xtabs(~I(prov_e==84 & c1==1)+cat.ocup, data=info2$variables)[2,]

tasa.ocupados.nuble.jornada.completa$cv = 
  tasa.ocupados.nuble.jornada.completa$`se.I(prov_e == 84 & c1 == 1)/I(prov_e == 84)`/tasa.ocupados.nuble.jornada.completa$`I(prov_e == 84 & c1 == 1)/I(prov_e == 84)`

tasa.ocupados.nuble.jornada.completa$frecuencia = freq

names(tasa.ocupados.nuble.jornada.completa) = 
  c("cat.ocup", "tasa.ocupados.nuble.jornada.completa", "error estandar","cv","frecuencia")

write.csv(tasa.ocupados.nuble.jornada.completa, "tasa_ocupados_nuble_jornada_completa.csv")

#
tasa.ocupados.nuble.jornada.completa.promedio = list()
for (i in 1:length(info)){
  tasa.ocupados.nuble.jornada.completa.promedio[[i]] = svyratio(~I(prov_e==84 & c1==1), 
                                                     denominator=~I(prov_e==84),
                                                     info[[i]],multicore = TRUE, 
                                                     drop.empty.groups = FALSE, 
                                                     na.rm=TRUE)
}

tasa.ocupados.nuble.jornada.completa.promedio. = unlist(lapply(tasa.ocupados.nuble.jornada.completa.promedio, '[[', 1) ) 

ee.tasa.ocupados.nuble.jornada.completa.promedio = unlist(lapply(tasa.ocupados.nuble.jornada.completa.promedio, SE))

tasa.ocupados.nuble.jornada.completa.promedio.. = data.frame(tasa.ocupados.nuble.jornada.completa.promedio., ee.tasa.ocupados.nuble.jornada.completa.promedio)

write.csv(tasa.ocupados.nuble.jornada.completa.promedio.., "tasa_ocupados_nuble_jornada_completa_promedio.csv")

### FIN CUADRO 25
