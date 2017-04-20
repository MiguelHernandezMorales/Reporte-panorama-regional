# CREACION DE LA VARIABLE: CONMUTANTE A NIVEL REGIONAL
for (i in 1:length(info)){
  info[[i]]$variables = mutate(info[[i]]$variables, conmutante1 =
                                 ifelse(region!=region_e,1,0))
}

#-------------------------------------------------------------------------------
# CREACION DE LA VARIABLE: CONMUTANTE A NIVEL PROVINCIAL
for (i in 1:length(info)){
  info[[i]]$variables = mutate(info[[i]]$variables, conmutante2 =
                                 ifelse(prov!=prov_e,1,0))
}


# CREACION DE LA VARIABLE: CONMUTANTE A NIVEL NACIONAL
for (i in 1:length(info)){
  info[[i]]$variables = mutate(info[[i]]$variables, conmuta.nacional1 =
                                    ifelse(region!=1 & region_e==1,1,
                                    ifelse(region!=2 & region_e==2,1,
                                    ifelse(region!=3 & region_e==3,1,
                                    ifelse(region!=4 & region_e==4,1,                
                                    ifelse(region!=5 & region_e==5,1,
                                    ifelse(region!=6 & region_e==6,1,
                                    ifelse(region!=7 & region_e==7,1,
                                    ifelse(region!=8 & region_e==8,1,
                                    ifelse(region!=9 & region_e==9,1,
                                    ifelse(region!=10 & region_e==10,1,
                                    ifelse(region!=11 & region_e==11,1,
                                    ifelse(region!=12 & region_e==12,1,
                                    ifelse(region!=13 & region_e==13,1,
                                    ifelse(region!=14 & region_e==14,1,
                                    ifelse(region!=15 & region_e==15,1,0))))))))))))))))
}

#-------------------------------------------------------------------------------
# CREACION DE LA VARIABLE: CONMUTANTE A NIVEL NACIONAL
for (i in 1:length(info)){
  info[[i]]$variables = mutate(info[[i]]$variables, conmuta.nacional2 =
                                    ifelse(region==1 & region_e!=1,1,
                                    ifelse(region==2 & region_e!=2,1,
                                    ifelse(region==3 & region_e!=3,1,
                                    ifelse(region==4 & region_e!=4,1,                
                                    ifelse(region==5 & region_e!=5,1,
                                    ifelse(region==6 & region_e!=6,1,
                                    ifelse(region==7 & region_e!=7,1,
                                    ifelse(region==8 & region_e!=8,1,
                                    ifelse(region==9 & region_e!=9,1,
                                    ifelse(region==10 & region_e!=10,1,
                                    ifelse(region==11 & region_e!=11,1,
                                    ifelse(region==12 & region_e!=12,1,
                                    ifelse(region==13 & region_e!=13,1,
                                    ifelse(region==14 & region_e!=14,1,
                                    ifelse(region==15 & region_e!=15,1,0))))))))))))))))
}

#-------------------------------------------------------------------------------
### CUADRO 22: Caracteristicas generales de los trabajadores residentes de nuble que trabajan en otra region 2016
#-------------------------------------------------------------------------------
###################################################################
# Edad de los trabajadores residente que trabajan fuera de Nuble
###################################################################
#
edad.promedio.residentes.trabaja.otra.nuble = list()
for (i in 1:length(info)){
  edad.promedio.residentes.trabaja.otra.nuble[[i]]=svyby(~edad,
                                                         by=~I(conmutante2==1 & prov==84), info[[i]], 
                                                         svymean, multicore = TRUE, 
                                                         drop.empty.groups = FALSE, na.rm=TRUE) %>% 
                                                        filter( `I(conmutante2 == 1 & prov == 84)`==TRUE)
}

edad.promedio.residentes.trabaja.otra.nuble. = 
  do.call(rbind, edad.promedio.residentes.trabaja.otra.nuble)

freq = list()
for (i in 1:length(info)){
  freq[[i]]= xtabs(~I(conmutante2==1 & prov==84), data=info[[i]]$variables) %>% data.frame() %>% 
    filter(I.conmutante2....1...prov....84.==TRUE)
}

freq = do.call(rbind, freq)

edad.promedio.residentes.trabaja.otra.nuble.$cv = 
  edad.promedio.residentes.trabaja.otra.nuble.$`se`/edad.promedio.residentes.trabaja.otra.nuble.$`edad`

edad.promedio.residentes.trabaja.otra.nuble.$frecuencia = freq$Freq

edad.promedio.residentes.trabaja.otra.nuble. = 
  edad.promedio.residentes.trabaja.otra.nuble.[, c("I(conmutante2 == 1 & prov == 84)", "edad", "se", "cv", "frecuencia")]

write.csv(edad.promedio.residentes.trabaja.otra.nuble., "edad_promedio_residentes_trabaja_otra_nuble.csv")

###################################################################
# Edad de los trabajadores que residen y trabajan en la region
###################################################################
#
edad.promedio.residentes.y.trabajan = list()
for (i in 1:length(info)){
  edad.promedio.residentes.y.trabajan[[i]]=svyby(~edad,
                                                            by=~I(prov_e==84 & prov==84), info[[i]], svymean,
                                                            multicore = TRUE, drop.empty.groups = FALSE, na.rm=TRUE)
}

edad.promedio.residentes.y.trabajan. = 
  do.call(rbind, edad.promedio.residentes.y.trabajan)

edad.promedio.residentes.y.trabajan. = 
  edad.promedio.residentes.y.trabajan.[, c("I(prov_e == 84 & prov == 84)", "edad", "se")]

write.csv(edad.promedio.residentes.y.trabajan., "edad_promedio_residentes_y_trabajan.csv")

###################################################################
# Escolaridad de los trabajadores no residentes de Nuble
###################################################################
#
esc.promedio.residentes.trabaja.otra.nuble = list()
for (i in 1:length(info)){
  esc.promedio.residentes.trabaja.otra.nuble[[i]]=svyby(~esc,
                                                        by=~I(conmutante2==1 & prov==84), info[[i]], 
                                                        svymean, multicore = TRUE, 
                                                        drop.empty.groups = FALSE, na.rm=TRUE) %>% 
    filter( `I(conmutante2 == 1 & prov == 84)`==TRUE)
}

esc.promedio.residentes.trabaja.otra.nuble. = 
  do.call(rbind, esc.promedio.residentes.trabaja.otra.nuble)

freq = list()
for (i in 1:length(info)){
  freq[[i]]= xtabs(~I(conmutante2==1 & prov==84), data=info[[i]]$variables) %>% data.frame() %>% 
    filter(I.conmutante2....1...prov....84.==TRUE)
}

freq = do.call(rbind, freq)

esc.promedio.residentes.trabaja.otra.nuble.$cv = 
  esc.promedio.residentes.trabaja.otra.nuble.$`se`/esc.promedio.residentes.trabaja.otra.nuble.$`esc`

esc.promedio.residentes.trabaja.otra.nuble.$frecuencia = freq$Freq

esc.promedio.residentes.trabaja.otra.nuble. = 
  esc.promedio.residentes.trabaja.otra.nuble.[, c("I(conmutante2 == 1 & prov == 84)", "esc", "se", "cv", "frecuencia")]

write.csv(esc.promedio.residentes.trabaja.otra.nuble., "esc_promedio_residentes_trabaja_otra_nuble.csv")

###################################################################
# Escolaridad de los trabajadores residentes y trabajan en nuble
###################################################################
#
esc.promedio.residentes.y.trabajan = list()
for (i in 1:length(info)){
  esc.promedio.residentes.y.trabajan[[i]]=svyby(~esc,
                                                           by=~I(prov_e==84 & prov==84), info[[i]], 
                                                           svymean, multicore = TRUE, 
                                                           drop.empty.groups = FALSE, na.rm=TRUE)
}

esc.promedio.residentes.y.trabajan. = 
  do.call(rbind, esc.promedio.residentes.y.trabajan)

esc.promedio.residentes.y.trabajan. = 
  esc.promedio.residentes.y.trabajan.[, c("I(prov_e == 84 & prov == 84)", "esc", "se")]

write.csv(esc.promedio.residentes.y.trabajan., "esc_promedio_residentes_y_trabajan.csv")

####################################################################################
# Porcentaje de mujeres promedio residentes que trabaja en otra a nivel de nuble. Se calcula sobre el total de residentes en otra
####################################################################################
#
tasa.mujeres.residente.trabaja.otra.nuble = list()
for (i in 1:length(info)){
  tasa.mujeres.residente.trabaja.otra.nuble[[i]] = svyratio(~I(conmutante2==1 & prov==84 & sexo==2), 
                                                            denominator=~I(conmutante2==1 & prov==84),
                                                            info[[i]],multicore = TRUE, 
                                                            drop.empty.groups = FALSE, na.rm=TRUE)
}

tasa.mujeres.residente.trabaja.otra.nuble. = unlist(lapply(tasa.mujeres.residente.trabaja.otra.nuble, '[[', 1) ) 

ee.tasa.mujeres.residente.trabaja.otra.nuble = unlist(lapply(tasa.mujeres.residente.trabaja.otra.nuble, SE))

tasa.mujeres.residente.trabaja.otra.nuble.. = data.frame(tasa.mujeres.residente.trabaja.otra.nuble., ee.tasa.mujeres.residente.trabaja.otra.nuble)

write.csv(tasa.mujeres.residente.trabaja.otra.nuble.., "tasa_promedio_mujeres_residente_trabaja_otra_nuble.csv")

####################################################################################
# Porcentaje de mujeres promedio residentes que trabaja en otra a nivel nacional. Se calcula sobre el total de residentes ytrabajan en nuble
####################################################################################
#
tasa.mujeres.residente.y.trabajan = list()
for (i in 1:length(info)){
  tasa.mujeres.residente.y.trabajan[[i]] = svyratio(~I(prov_e==84 & prov==84 & sexo==2), 
                                                       denominator=~I(prov_e==84 & prov==84),
                                                       info[[i]],multicore = TRUE, 
                                                       drop.empty.groups = FALSE, na.rm=TRUE)
}

tasa.mujeres.residente.y.trabajan. = unlist(lapply(tasa.mujeres.residente.y.trabajan, '[[', 1) ) 

ee.tasa.mujeres.residente.y.trabajan = unlist(lapply(tasa.mujeres.residente.y.trabajan, SE))

tasa.mujeres.residente.y.trabajan.. = data.frame(tasa.mujeres.residente.y.trabajan., ee.tasa.mujeres.residente.y.trabajan)

write.csv(tasa.mujeres.residente.y.trabajan.., "tasa_promedio_mujeres_residente_y_trabajan.csv")

####################################################################################
# Porcentaje de los ocupados con educación superior completa:  residen, pero trabajan fuera
####################################################################################
#
tasa.ocupados.profesional.residente.y.trabajan.otra = list()
for (i in 1:length(info)){
  tasa.ocupados.profesional.residente.y.trabajan.otra[[i]] = svyratio(~I(conmutante2==1 & prov==84 & educ>=7), 
                                                    denominator=~I(conmutante2==1 & prov==84),
                                                    info[[i]],multicore = TRUE, 
                                                    drop.empty.groups = FALSE, na.rm=TRUE)
}

tasa.ocupados.profesional.residente.y.trabajan.otra. = unlist(lapply(tasa.ocupados.profesional.residente.y.trabajan.otra, '[[', 1) ) 

ee.tasa.ocupados.profesional.residente.y.trabajan.otra = unlist(lapply(tasa.ocupados.profesional.residente.y.trabajan.otra, SE))

tasa.ocupados.profesional.residente.y.trabajan.otra.. = data.frame(tasa.ocupados.profesional.residente.y.trabajan.otra., ee.tasa.ocupados.profesional.residente.y.trabajan.otra)

write.csv(tasa.ocupados.profesional.residente.y.trabajan.otra.., "tasa_ocupados_profesional_residente_y_trabajan_otra.csv")

####################################################################################
# Porcentaje de los ocupados con educación superior completa:  residen y trabajan 
####################################################################################
#
tasa.ocupados.profesional.residen.y.trabajan = list()
for (i in 1:length(info)){
  tasa.ocupados.profesional.residen.y.trabajan[[i]] = svyratio(~I(prov_e==84 & prov==84 & educ>=7), 
                                                    denominator=~I(prov_e==84 & prov==84),
                                                    info[[i]],multicore = TRUE, 
                                                    drop.empty.groups = FALSE, na.rm=TRUE)
}

tasa.ocupados.profesional.residen.y.trabajan. = unlist(lapply(tasa.ocupados.profesional.residen.y.trabajan, '[[', 1) ) 

ee.tasa.ocupados.profesional.residen.y.trabajan = unlist(lapply(tasa.ocupados.profesional.residen.y.trabajan, SE))

tasa.ocupados.profesional.residen.y.trabajan.. = data.frame(tasa.ocupados.profesional.residen.y.trabajan., ee.tasa.ocupados.profesional.residen.y.trabajan)

write.csv(tasa.ocupados.profesional.residen.y.trabajan.., "tasa_ocupados_profesional_residen_y_trabajan.csv")

### FIN CUADRO 22