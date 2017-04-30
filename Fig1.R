#-------------------------------------------------------------------------------
# CREACION DE LA VARIABLE: POBLACION EN EDAD DE TRABAJAR
for (i in 1:length(todas)){
  info[[i]]$variables = mutate(info[[i]]$variables, pet =
                                 ifelse(edad>=15,1, 0)) 
}

#-------------------------------------------------------------------------------
# CREACION DE LA VARIABLE: CONMUTANTE A NIVEL PROVINCIAL
for (i in 1:length(info)){
  info[[i]]$variables = mutate(info[[i]]$variables, conmutante =
                                 ifelse(prov!=prov_e,1,0))
}

#-------------------------------------------------------------------------------
# CREACION DE LA VARIABLE: CONMUTANTE A NIVEL NACIONAL
for (i in 1:length(info)){
  info[[i]]$variables = mutate(info[[i]]$variables, conmuta.nacional = 
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
# Figura 1. Esquema sobre los principales indicadores del mercado laboral para nuble 
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# poblacion en edad de trabajar a nivel nacional y Nuble
#-------------------------------------------------------------------------------
#
pet.nacional. = list()
for (i in 1:length(info)){
  pet.nacional.[[i]] = svytotal(~pet, info[[i]], multicore= TRUE, 
                        drop.empty.groups = FALSE, na.rm=TRUE)
}

pet.nuble. = list()
for (i in 1:length(info)){
  pet.nuble.[[i]] = svytotal(~pet, subset(info[[i]],prov==84), multicore= TRUE, 
                        drop.empty.groups = FALSE, na.rm=TRUE)
}

pet.nacional = unlist(lapply(pet.nacional., '[[', 1) ) 
pet.nuble = unlist(lapply(pet.nuble., '[[', 1) ) 

ee.pet.nacional = unlist(lapply(pet.nacional., SE))
ee.pet.nuble = unlist(lapply(pet.nuble., SE))

pet = data.frame(pet.nacional, ee.pet.nacional, pet.nuble, ee.pet.nuble)

write.csv(pet, "pet_nacional_nuble.csv")

#-------------------------------------------------------------------------------
# Numero y tasa de inactivos a nivel nacional
#-------------------------------------------------------------------------------
# 
inactivos.nacional. = list()
for (i in 1:length(info)){
  inactivos.nacional.[[i]] = svytotal(~I(cae_general=="Inactivo"), info[[i]], multicore= TRUE, 
                              drop.empty.groups = FALSE, na.rm=TRUE)
}

tasa.inactivos.nacional. = list()
for (i in 1:length(info)){
  tasa.inactivos.nacional.[[i]] = svyratio(~I(cae_general=="Inactivo"),
                                   denominator=~pet,info[[i]], multicore= TRUE, 
                                   drop.empty.groups = FALSE, na.rm=TRUE)
}

inactivos.nacional = unlist(lapply(inactivos.nacional., '[[', 2) ) 
tasa.inactivos.nacional = unlist(lapply(tasa.inactivos.nacional., '[[', 1) ) 

ee.inactivos.nacional = unlist(lapply(inactivos.nacional., SE))[seq(2, 2*length(inactivos.nacional),2)]
ee.tasa.inactivos.nacional = unlist(lapply(tasa.inactivos.nacional., SE))

tasa.inactivos.nacional = data.frame(inactivos.nacional, ee.inactivos.nacional, 
                            tasa.inactivos.nacional, ee.tasa.inactivos.nacional)

write.csv(tasa.inactivos.nacional, "tasa_inactivos_nacional.csv")

#-------------------------------------------------------------------------------
# Numero y tasa de inactivos a nivel de nuble
#-------------------------------------------------------------------------------
# 
inactivos.nuble. = list()
for (i in 1:length(info)){
  inactivos.nuble.[[i]] = svytotal(~I(cae_general=="Inactivo"), subset(info[[i]],prov==84), 
                              multicore= TRUE, drop.empty.groups = FALSE, na.rm=TRUE)
}

tasa.inactivos.nuble. = list()
for (i in 1:length(info)){
  tasa.inactivos.nuble.[[i]] = svyratio(~I(cae_general=="Inactivo"),
                                   denominator=~pet, subset(info[[i]],prov==84), 
                                   multicore= TRUE, drop.empty.groups = FALSE, na.rm=TRUE)
}

inactivos.nuble = unlist(lapply(inactivos.nuble., '[[', 2) ) 
tasa.inactivos.nuble = unlist(lapply(tasa.inactivos.nuble., '[[', 1) ) 

ee.inactivos.nuble = unlist(lapply(inactivos.nuble., SE))[seq(2, 2*length(inactivos.nuble.),2)]
ee.tasa.inactivos.nuble = unlist(lapply(tasa.inactivos.nuble., SE))

tasa.inactivos.nuble = data.frame(inactivos.nuble, ee.inactivos.nuble, 
                            tasa.inactivos.nuble, ee.tasa.inactivos.nuble)

write.csv(tasa.inactivos.nuble, "tasa_inactivos_nuble.csv")

#-------------------------------------------------------------------------------
# numero de activos: Nacional y Nuble
#-------------------------------------------------------------------------------
activos.nacional. = list()
for (i in 1:length(info)){
  activos.nacional.[[i]] = svytotal(~I(cae_general=="Ocupado" | cae_general=="Desocupado" | 
                                 cae_general=="Busca trabajo Primera vez"), info[[i]], 
                            multicore= TRUE, drop.empty.groups = FALSE, na.rm=TRUE)
}

activos.nuble. = list()
for (i in 1:length(info)){
  activos.nuble.[[i]] = svytotal(~I(cae_general=="Ocupado" | cae_general=="Desocupado" | 
                                 cae_general=="Busca trabajo Primera vez"),
                            subset(info[[i]],prov==84), multicore= TRUE, drop.empty.groups = FALSE, na.rm=TRUE)
}

activos.nacional = unlist(lapply(activos.nacional., '[[', 2) ) 
activos.nuble = unlist(lapply(activos.nuble., '[[', 2) )

ee.activos.nacional = unlist(lapply(activos.nacional., SE)) [seq(2, 2*length(activos.nacional.),2)]
ee.activos.nuble = unlist(lapply(activos.nuble., SE)) [seq(2, 2*length(activos.nuble.),2)]

activos = data.frame(activos.nacional, ee.activos.nacional, 
                     activos.nuble, ee.activos.nuble)

write.csv(activos, "activos_nacional_nuble.csv")

#-------------------------------------------------------------------------------
# tasa de participacion: Nacional y Nuble
#-------------------------------------------------------------------------------
#
tasa.participacion.nacional. = list()
for (i in 1:length(info)){
  tasa.participacion.nacional.[[i]] = svyratio(~I(cae_general=="Ocupado" | 
                                            cae_general=="Desocupado" | 
                                            cae_general=="Busca trabajo Primera vez"), 
                                       denominator=~pet, info[[i]], multicore= TRUE, 
                                       drop.empty.groups = FALSE, na.rm=TRUE)
}

tasa.participacion.nuble. = list()
for (i in 1:length(info)){
  tasa.participacion.nuble.[[i]] = svyratio(~I(cae_general=="Ocupado" | 
                                          cae_general=="Desocupado" | 
                                          cae_general=="Busca Trabajo Primera Vez"), 
                                       denominator=~pet, subset(info[[i]],prov==84), 
                                       multicore= TRUE, drop.empty.groups = FALSE, na.rm=TRUE)
}

tasa.participacion.nacional = unlist(lapply(tasa.participacion.nacional., '[[', 1) ) 
tasa.participacion.nuble = unlist(lapply(tasa.participacion.nuble., '[[', 1) ) 

ee.tasa.participacion.nacional = unlist(lapply(tasa.participacion.nacional., SE))
ee.tasa.participacion.nuble = unlist(lapply(tasa.participacion.nuble., SE))

tasa.participacion = data.frame(tasa.participacion.nacional, ee.tasa.participacion.nacional,
                                   tasa.participacion.nuble, ee.tasa.participacion.nuble)

write.csv(tasa.participacion, "tasa_participacion_nacional_nuble.csv")

#-------------------------------------------------------------------------------
# Numero y tasa de ocupados a nivel nacional
#-------------------------------------------------------------------------------
#
ocupados.nacional. = list()
for (i in 1:length(info)){
  ocupados.nacional.[[i]] = svytotal(~I(cae_general=="Ocupado"), info[[i]], 
                             multicore= TRUE, drop.empty.groups = FALSE, na.rm=TRUE)
}

tasa.ocupados.nacional. = list()
for (i in 1:length(info)){
  tasa.ocupados.nacional.[[i]] = svyratio(~I(cae_general=="Ocupado"),
                                   denominator=~pet,info[[i]], 
                                  multicore= TRUE, drop.empty.groups = FALSE, 
                                  na.rm=TRUE)
}

ocupados.nacional = unlist(lapply(ocupados.nacional., '[[', 2) ) 
tasa.ocupados.nacional = unlist(lapply(tasa.ocupados.nacional., '[[', 1) ) 

ee.ocupados.nacional = unlist(lapply(ocupados.nacional., SE)) [seq(2, 2*length(ocupados.nacional),2)]
ee.tasa.ocupados.nacional = unlist(lapply(tasa.ocupados.nacional., SE))

Ocupados = data.frame(ocupados.nacional, ee.ocupados.nacional, 
                      tasa.ocupados.nacional, ee.tasa.ocupados.nacional)

write.csv(Ocupados, "Ocupados_tasa_nacional.csv")

#-------------------------------------------------------------------------------
# Numero y tasa de ocupados a nivel de nuble
#-------------------------------------------------------------------------------
#
ocupados.nuble. = list()
for (i in 1:length(info)){
  ocupados.nuble.[[i]] = svytotal(~I(cae_general=="Ocupado"), subset(info[[i]],prov==84), 
                             multicore= TRUE, drop.empty.groups = FALSE, na.rm=TRUE)
}

tasa.ocupados.nuble. = list()
for (i in 1:length(info)){
  tasa.ocupados.nuble.[[i]] = svyratio(~I(cae_general=="Ocupado"),
                                  denominator=~pet,subset(info[[i]],prov==84), 
                                  multicore= TRUE, drop.empty.groups = FALSE, na.rm=TRUE)
}

ocupados.nuble = unlist(lapply(ocupados.nuble., '[[', 2) ) 
tasa.ocupados.nuble = unlist(lapply(tasa.ocupados.nuble., '[[', 1) ) 

ee.ocupados.nuble = unlist(lapply(ocupados.nuble., SE)) [seq(2, 2*length(ocupados.nuble),2)]
ee.tasa.ocupados.nuble = unlist(lapply(tasa.ocupados.nuble., SE))

Ocupados.nuble = data.frame(ocupados.nuble, ee.ocupados.nuble, 
                      tasa.ocupados.nuble, ee.tasa.ocupados.nuble)

write.csv(Ocupados.nuble, "Ocupados_tasa_nuble.csv")

#-------------------------------------------------------------------------------
# Numero y tasa de desempleo a nivel nacional
#-------------------------------------------------------------------------------
#
desocupados.nacional. = list()
for (i in 1:length(info)){
  desocupados.nacional.[[i]] = svytotal(~I(cae_general=="Desocupado" | 
                                     cae_general=="Busca trabajo Primera vez"), info[[i]], 
                                multicore= TRUE, drop.empty.groups = FALSE, na.rm=TRUE)
}

tasa.desempleo.nacional. = list()
for (i in 1:length(info)){
  tasa.desempleo.nacional.[[i]] = svyratio(~I(cae_general=="Desocupado" | 
                                        cae_general=="Busca trabajo Primera vez"),
                                   denominator=~I(cae_general=="Ocupado"),info[[i]], 
                                   multicore= TRUE, drop.empty.groups = FALSE, na.rm=TRUE)
}

desocupados.nacional = unlist(lapply(desocupados.nacional., '[[', 2) ) 
tasa.desempleo.nacional = unlist(lapply(tasa.desempleo.nacional., '[[', 1) ) 

ee.desocupados.nacional = unlist(lapply(desocupados.nacional., SE)) [seq(2, 2*length(desocupados.nacional),2)]
ee.tasa.desempleo.nacional = unlist(lapply(tasa.desempleo.nacional., SE))

desocupados = data.frame(desocupados.nacional, ee.desocupados.nacional, 
                            tasa.desempleo.nacional, ee.tasa.desempleo.nacional)

write.csv(desocupados, "desocupados_tasa_nacional.csv")

#-------------------------------------------------------------------------------
# Numero y tasa de desempleo a nivel de Nuble
#-------------------------------------------------------------------------------
#
desocupados.nuble. = list()
for (i in 1:length(info)){
  desocupados.nuble.[[i]] = svytotal(~I(cae_general=="Desocupado" | 
                                     cae_general=="Busca trabajo Primera vez"), 
                                subset(info[[i]],prov==84), multicore= TRUE, 
                                drop.empty.groups = FALSE, na.rm=TRUE)
}

tasa.desempleo.nuble. = list()
for (i in 1:length(info)){
  tasa.desempleo.nuble.[[i]] = svyratio(~I(cae_general=="Desocupado" | 
                                        cae_general=="Busca trabajo Primera vez"),
                                   denominator=~I(cae_general=="Ocupado"),
                                   subset(info[[i]],prov==84), multicore= TRUE, 
                                   drop.empty.groups = FALSE, na.rm=TRUE)
}

desocupados.nuble = unlist(lapply(desocupados.nuble., '[[', 2) ) 
tasa.desempleo.nuble = unlist(lapply(tasa.desempleo.nuble., '[[', 1) ) 

ee.desocupados.nuble = unlist(lapply(desocupados.nuble., SE)) [seq(2, 2*length(desocupados.nuble),2)]
ee.tasa.desempleo.nuble = unlist(lapply(tasa.desempleo.nuble., SE))

desocupados.nuble = data.frame(desocupados.nuble, ee.desocupados.nuble, 
                         tasa.desempleo.nuble, ee.tasa.desempleo.nuble)

write.csv(desocupados.nuble, "desocupados_tasa_nuble.csv")

#-------------------------------------------------------------------------------
# inactivos residentes a nivel nacional
#-------------------------------------------------------------------------------
#
inactivos.residentes.nacional. = list()
for (i in 1:length(info)){
  inactivos.residentes.nacional.[[i]] = svytotal(~I(cae_general=="Inactivo" & nacionalidad==0),
                                        info[[i]], multicore= TRUE, 
                                       drop.empty.groups = FALSE, na.rm=TRUE)
}

inactivos.migrantes.nacional. = list()
for (i in 1:length(info)){
  inactivos.migrantes.nacional.[[i]] = svytotal(~I(cae_general=="Inactivo" & nacionalidad!=0),
                                                 info[[i]], multicore= TRUE, 
                                                 drop.empty.groups = FALSE, na.rm=TRUE)
}

inactivos.residentes.nacional = unlist(lapply(inactivos.residentes.nacional., '[[', 2) ) 
inactivos.migrantes.nacional = unlist(lapply(inactivos.migrantes.nacional., '[[', 2) ) 

ee.inactivos.residentes.nacional = unlist(lapply(inactivos.residentes.nacional., SE))[c(2,4,6,8)]
ee.inactivos.migrantes.nacional = unlist(lapply(inactivos.migrantes.nacional., SE))[c(2,4,6,8)]

inactivos.residentes.migrantes.nacional = data.frame(inactivos.residentes.nacional, ee.inactivos.residentes.nacional,
                                                     inactivos.migrantes.nacional, ee.inactivos.migrantes.nacional)

write.csv(inactivos.residentes.migrantes.nacional, "inactivos_residentes_migrantes_nacional.csv")

#-------------------------------------------------------------------------------
# inactivos residentes a nivel nuble
#-------------------------------------------------------------------------------
#
inactivos.residentes.nuble. = list()
for (i in 1:length(info)){
  inactivos.residentes.nuble.[[i]] = svytotal(~I(cae_general=="Inactivo" & nacionalidad==0),
                                                 subset(info[[i]],prov==84), multicore= TRUE, 
                                                 drop.empty.groups = FALSE, na.rm=TRUE)
}

inactivos.migrantes.nuble. = list()
for (i in 1:length(info)){
  inactivos.migrantes.nuble.[[i]] = svytotal(~I(cae_general=="Inactivo" & nacionalidad!=0),
                                                subset(info[[i]],prov==84), multicore= TRUE, 
                                                drop.empty.groups = FALSE, na.rm=TRUE)
}


inactivos.residentes.nuble = unlist(lapply(inactivos.residentes.nuble., '[[', 2) ) 
inactivos.migrantes.nuble = unlist(lapply(inactivos.migrantes.nuble., '[[', 2) ) 

ee.inactivos.residentes.nuble = unlist(lapply(inactivos.residentes.nuble., SE))[c(2,4,6,8)]
ee.inactivos.migrantes.nuble = unlist(lapply(inactivos.migrantes.nuble., SE))[c(2,4,6,8)]

inactivos.residentes.migrantes.nuble = data.frame(inactivos.residentes.nuble, ee.inactivos.residentes.nuble,
                                           inactivos.migrantes.nuble, ee.inactivos.migrantes.nuble)

write.csv(inactivos.residentes.migrantes.nuble, "inactivos_residentes_migrantes_nuble.csv")

#-------------------------------------------------------------------------------
# trabajadores residentes a nivel nacional
#-------------------------------------------------------------------------------
#
ocupado.residentes.nacional. = list()
for (i in 1:length(info)){
  ocupado.residentes.nacional.[[i]] = svytotal(~I(cae_general=="Ocupado" & nacionalidad==0 & conmuta.nacional==0),
                                       info[[i]], multicore= TRUE, 
                                       drop.empty.groups = FALSE, na.rm=TRUE)
}

ocupado.conmutante.nacional. = list()
for (i in 1:length(info)){
  ocupado.conmutante.nacional.[[i]] = svytotal(~I(cae_general=="Ocupado" & nacionalidad==0 & conmuta.nacional==1),
                                       info[[i]], multicore= TRUE, 
                                    drop.empty.groups = FALSE, na.rm=TRUE)
}

ocupado.migrantes.nacional. = list()
for (i in 1:length(info)){
  ocupado.migrantes.nacional.[[i]] = svytotal(~I(cae_general=="Ocupado" & nacionalidad!=0),
                                                info[[i]], multicore= TRUE, 
                                                drop.empty.groups = FALSE, na.rm=TRUE)
}

ocupado.residentes.nacional = unlist(lapply(ocupado.residentes.nacional., '[[', 2) ) 
ocupado.conmutante.nacional = unlist(lapply(ocupado.conmutante.nacional., '[[', 2) ) 
ocupado.migrantes.nacional  = unlist(lapply(ocupado.migrantes.nacional., '[[', 2) ) 


ee.ocupado.residentes.nacional = unlist(lapply(ocupado.residentes.nacional., SE))[c(2,4,6,8)]
ee.ocupado.conmutante.nacional = unlist(lapply(ocupado.conmutante.nacional., SE)) [c(2,4,6,8)]
ee.ocupado.migrantes.nacional = unlist(lapply(ocupado.migrantes.nacional., SE))[c(2,4,6,8)]

ocupado.residentes.conmutante.migrantge.nacional = 
                        data.frame(ocupado.residentes.nacional, ee.ocupado.residentes.nacional,
                                    ocupado.conmutante.nacional, ee.ocupado.conmutante.nacional,
                                    ocupado.migrantes.nacional, ee.ocupado.migrantes.nacional)

write.csv(ocupado.residentes.conmutante.migrantge.nacional, "ocup_residentes_conmutante_migrante_nacional.csv")

#-------------------------------------------------------------------------------
# desocupado residente a nivel nacional
#-------------------------------------------------------------------------------
#
desocupado.residente.nacional. = list()
for (i in 1:length(info)){
  desocupado.residente.nacional.[[i]] = svytotal(~I((cae_general=="Desocupado" | 
                                                       cae_general=="Busca Trabajo Primera Vez") & 
                                                   nacionalidad==0),
                                              info[[i]], multicore= TRUE, 
                                              drop.empty.groups = FALSE, na.rm=TRUE)
}

desocupado.migrante.nacional. = list()
for (i in 1:length(info)){
  desocupado.migrante.nacional.[[i]] = svytotal(~I((cae_general=="Desocupado" | 
                                                      cae_general=="Busca Trabajo Primera Vez") & 
                                                  nacionalidad!=0),
                                             info[[i]], multicore= TRUE, 
                                             drop.empty.groups = FALSE, na.rm=TRUE)
}

desocupado.residente.nacional = unlist(lapply(desocupado.residente.nacional., '[[', 2) ) 
desocupado.migrante.nacional = unlist(lapply(desocupado.migrante.nacional., '[[', 2) ) 


ee.desocupado.residente.nacional = unlist(lapply(desocupado.residente.nacional., SE))[c(2,4,6,8)]
ee.desocupado.migrante.nacional = unlist(lapply(desocupado.migrante.nacional., SE))[c(2,4,6,8)]


desocupado.residente.migrante.nacional = data.frame(desocupado.residente.nacional, ee.desocupado.residente.nacional,
                                               desocupado.migrante.nacional, ee.desocupado.migrante.nacional)

write.csv(desocupado.residente.migrante.nacional, "desocupado_residente_migrante_nacional.csv")

#-------------------------------------------------------------------------------
# residentes ocupados dentro de nuble
#-------------------------------------------------------------------------------
#
ocupado.residentes.dentro.nuble. = list()
for (i in 1:length(info)){
  ocupado.residentes.dentro.nuble.[[i]] = svytotal(~I(prov==84 & prov_e==84 & nacionalidad==0),
                                             info[[i]], multicore= TRUE, 
                                       drop.empty.groups = FALSE, na.rm=TRUE)
}

ocupado.residentes.fuera.nuble. = list()
for (i in 1:length(info)){
  ocupado.residentes.fuera.nuble.[[i]] = svytotal(~I(conmutante & prov==84 & nacionalidad==0),
                                                   info[[i]], multicore= TRUE, 
                                             drop.empty.groups = FALSE, na.rm=TRUE)
}

ocupado.migrantes.nuble. = list()
for (i in 1:length(info)){
  ocupado.migrantes.nuble.[[i]] = svytotal(~I(cae_general=="Ocupado" & nacionalidad!=0),
                                              subset(info[[i]],prov==84), multicore= TRUE, 
                                              drop.empty.groups = FALSE, na.rm=TRUE)
}

ocupado.residentes.dentro.nuble = unlist(lapply(ocupado.residentes.dentro.nuble., '[[', 2) ) 
ocupado.residentes.fuera.nuble = unlist(lapply(ocupado.residentes.fuera.nuble., '[[', 2) ) 
ocupado.migrantes.nuble = unlist(lapply(ocupado.migrantes.nuble., '[[', 2) ) 

ee.ocupado.residentes.dentro.nuble = unlist(lapply(ocupado.residentes.dentro.nuble., SE)) [c(2,4,6,8)]
ee.ocupado.residentes.fuera.nuble = unlist(lapply(ocupado.residentes.fuera.nuble., SE)) [c(2,4,6,8)]
ee.ocupado.migrantes.nuble = unlist(lapply(ocupado.migrantes.nuble., SE))[c(2,4,6,8)]


ocupado.conmutante.migrante.nuble = data.frame(ocupado.residentes.dentro.nuble, ee.ocupado.residentes.dentro.nuble,
                                               ocupado.residentes.fuera.nuble, ee.ocupado.residentes.fuera.nuble,
                                               ocupado.migrantes.nuble, ee.ocupado.migrantes.nuble)

write.csv(ocupado.conmutante.migrante.nuble, "ocupado_conmutante_migrante_nuble.csv")

#-------------------------------------------------------------------------------
# desocupado residente a nivel nuble
#-------------------------------------------------------------------------------
#
desocupado.residente.nuble. = list()
for (i in 1:length(info)){
  desocupado.residente.nuble.[[i]] = svytotal(~I((cae_general=="Desocupado" | 
                                                    cae_general=="Busca Trabajo Primera Vez") & 
                                                  nacionalidad==0),
                                              subset(info[[i]],prov==84), multicore= TRUE, 
                                        drop.empty.groups = FALSE, na.rm=TRUE)
}

desocupado.migrante.nuble. = list()
for (i in 1:length(info)){
  desocupado.migrante.nuble.[[i]] = svytotal(~I((cae_general=="Desocupado" | 
                                                   cae_general=="Busca Trabajo Primera Vez") & 
                                                   nacionalidad!=0),
                                              subset(info[[i]],prov==84), multicore= TRUE, 
                                              drop.empty.groups = FALSE, na.rm=TRUE)
}

desocupado.residente.nuble = unlist(lapply(desocupado.residente.nuble., '[[', 2) ) 
desocupado.migrante.nuble = unlist(lapply(desocupado.migrante.nuble., '[[', 2) ) 

ee.desocupado.residente.nuble = unlist(lapply(desocupado.residente.nuble., SE))[c(2,4,6,8)]
ee.desocupado.migrante.nuble = unlist(lapply(desocupado.migrante.nuble., SE))[c(2,4,6,8)]

desocupado.residente.migrante.nuble = data.frame(desocupado.residente.nuble, ee.desocupado.residente.nuble,
                                                 desocupado.migrante.nuble, ee.desocupado.migrante.nuble)

write.csv(desocupado.residente.migrante.nuble, "desocupado_residente_migrante_nuble.csv")

# FIN FIGURA 1
