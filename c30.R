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
### CUADRO 30: 
#-------------------------------------------------------------------------------
###################################################################
# Total de ocupados nuble por ocupaciones a 1 digito
###################################################################
#
total.ocupados.nuble.1digito = list()
for (i in 1:length(info)){
  total.ocupados.nuble.1digito[[i]] = svyby(~I(prov_e==84),
                                            by=~b1, info[[i]], svytotal,
                                            multicore = TRUE, 
                                            drop.empty.groups = FALSE, na.rm=TRUE)
}

total.ocupados.nuble.1digito. = 
  do.call(rbind, total.ocupados.nuble.1digito)

freq = list()
for (i in 1:length(info)){
  freq[[i]]= xtabs(~I(prov_e==84)+b1, data=info[[i]]$variables) %>% data.frame() %>%
    filter(`I.prov_e....84.`== TRUE)
}

freq = do.call(rbind, freq)

total.ocupados.nuble.1digito.$cv = 
  total.ocupados.nuble.1digito.$`se.I(prov_e == 84)TRUE`/total.ocupados.nuble.1digito.$`I(prov_e == 84)TRUE`

total.ocupados.nuble.1digito.$frecuencia = freq$Freq

total.ocupados.nuble.1digito. = 
  total.ocupados.nuble.1digito.[,c("b1", "I(prov_e == 84)TRUE", "se.I(prov_e == 84)TRUE","cv","frecuencia")]

write.csv(total.ocupados.nuble.1digito., "total_ocupados_nuble_1digito.csv")

################################################################
# tasa de dependiente sin contrato del total de ocupados en nuble by clasificación a 1 digito
################################################################
#
tasa.dep.sin.contrato.nuble.1digito = svyby(~I(cat.ocup=="asalariado sin contrato" & prov_e==84),
                                            by=~b1, denominator=~I(cae_general=="Ocupado" & prov_e==84), 
                                            info2, svyratio, multicore=TRUE, drop.empty.groups = FALSE, na.rm=TRUE)

freq= xtabs(~I(cat.ocup=="asalariado sin contrato" & prov_e==84)+b1, data=info2$variables)[2,]

tasa.dep.sin.contrato.nuble.1digito$cv = 
  tasa.dep.sin.contrato.nuble.1digito$`se.I(cat.ocup == \"asalariado sin contrato\" & prov_e == 84)/I(cae_general == \"Ocupado\" & prov_e == 84)`/tasa.dep.sin.contrato.nuble.1digito$`I(cat.ocup == \"asalariado sin contrato\" & prov_e == 84)/I(cae_general == \"Ocupado\" & prov_e == 84)`

tasa.dep.sin.contrato.nuble.1digito$frecuencia = freq

names(tasa.dep.sin.contrato.nuble.1digito) = 
  c("b1", "tasa.dep.sin.contrato.nuble.1digito", "error estandar", "cv", "frecuencia")

write.csv(tasa.dep.sin.contrato.nuble.1digito, "tasa_dep_sin_contrato_nuble_1digito.csv")


#
tasa.promedio.dep.sin.contrato.nuble = list()
for (i in 1:length(info)){
  tasa.promedio.dep.sin.contrato.nuble[[i]] = svyratio(~I(cat.ocup=="asalariado sin contrato" & prov_e==84),
                                                       denominator=~I(cae_general=="Ocupado" & prov_e==84), info[[i]],
                                                       multicore=TRUE, drop.empty.groups = FALSE, na.rm=TRUE)
}

tasa.promedio.dep.sin.contrato.nuble. = unlist(lapply(tasa.promedio.dep.sin.contrato.nuble, '[[', 1) ) 

ee.tasa.promedio.dep.sin.contrato.nuble = unlist(lapply(tasa.promedio.dep.sin.contrato.nuble, SE))

tasa.promedio.dep.sin.contrato.nuble.. = data.frame(tasa.promedio.dep.sin.contrato.nuble., ee.tasa.promedio.dep.sin.contrato.nuble)

write.csv(tasa.promedio.dep.sin.contrato.nuble.., "tasa_promedio_dep_sin_contrato_nuble.csv")

################################################################
# tasa de cuenta propia del total de ocupados en nuble por categoria a 1 digito
################################################################
#
tasa.cuentap.nuble.1digito= svyby(~I(cat.ocup=="cuenta propia" & prov_e==84),
                                  by=~b1, denominator=~I(cae_general=="Ocupado" & prov_e==84), 
                                  info2, svyratio, multicore=TRUE, drop.empty.groups = FALSE, na.rm=TRUE)

freq= xtabs(~I(cat.ocup=="cuenta propia" & prov_e==84)+b1, data=info2$variables)[2,]

tasa.cuentap.nuble.1digito$cv = 
  tasa.cuentap.nuble.1digito$`se.I(cat.ocup == \"cuenta propia\" & prov_e == 84)/I(cae_general == \"Ocupado\" & prov_e == 84)`/tasa.cuentap.nuble.1digito$`I(cat.ocup == \"cuenta propia\" & prov_e == 84)/I(cae_general == \"Ocupado\" & prov_e == 84)`

tasa.cuentap.nuble.1digito$frecuencia = freq

names(tasa.cuentap.nuble.1digito) = 
  c("b1", "tasa.cuentap.nuble.1digito", "error estandar", "cv", "frecuencia")

write.csv(tasa.cuentap.nuble.1digito, "tasa_cuentap_nuble_1digito.csv")

#
tasa.promedio.cuentap.nuble = list()
for (i in 1:length(info)){
  tasa.promedio.cuentap.nuble[[i]] = svyratio(~I(categoria_ocupacion==2 & cae_general=="Ocupado" & prov_e==84),
                                              denominator=~I(cae_general=="Ocupado" & prov_e==84), info[[i]],
                                              multicore=TRUE, drop.empty.groups = FALSE, na.rm=TRUE)
}

tasa.promedio.cuentap.nuble. = unlist(lapply(tasa.promedio.cuentap.nuble, '[[', 1) ) 

ee.tasa.promedio.cuentap.nuble = unlist(lapply(tasa.promedio.cuentap.nuble, SE))

tasa.promedio.cuentap.nuble.. = data.frame(tasa.promedio.cuentap.nuble., ee.tasa.promedio.cuentap.nuble)

write.csv(tasa.promedio.cuentap.nuble.., "tasa_promedio_cuentap_nuble.csv")


### FIN CUADRO 30
