#-------------------------------------------------------------------------------
### CUADRO 24: Distribucion de la jornada laboral nacional
#-------------------------------------------------------------------------------
####################################################################
# tasa de hombres ocupados por jornada a nivel nacional
####################################################################
#
jornada= names(table(info[[1]]$variables$c1))
jornada_sexo_nacional_hombre2 = list()
for (i in 1:length(info)){
  jornada_sexo_nacional_hombre2[[i]] = lapply(jornada, function(x) 
    svyratio(~I(cae_general=="Ocupado" & c1==x & sexo==1),
             denominator=~I(cae_general=="Ocupado" & sexo==1), 
             na.rm=TRUE, info[[i]]))
}

#
jornada_sexo_nacional_hombre2_=list()
for (i in 1:length(info)){
  jornada_sexo_nacional_hombre2_[[i]] = unlist(lapply(jornada_sexo_nacional_hombre2[[i]], '[[', 1)) 
}

jornada_sexo_nacional_hombre2. = do.call(rbind, jornada_sexo_nacional_hombre2_)
rownames(jornada_sexo_nacional_hombre2.) = c("EFM","AMJ","JAS","OND")
colnames(jornada_sexo_nacional_hombre2.) = c("Jornada completa", "jornada parcial")

var.jornada_sexo_nacional_hombre2_=list()
for (i in 1:length(info)){
  var.jornada_sexo_nacional_hombre2_[[i]] = unlist(lapply(jornada_sexo_nacional_hombre2[[i]], '[[', 2)) 
}

var.jornada_sexo_nacional_hombre2_ = do.call(rbind, var.jornada_sexo_nacional_hombre2_)
rownames(var.jornada_sexo_nacional_hombre2_) = c("EFM","AMJ","JAS","OND")
colnames(var.jornada_sexo_nacional_hombre2_) = c("Jornada completa", "jornada parcial")

write.csv(jornada_sexo_nacional_hombre2., "jornada_sexo_nacional_hombre.csv")

# mujeres nacional
#
jornada_sexo_nacional_mujer2 = list()
for (i in 1:length(info)){
  jornada_sexo_nacional_mujer2[[i]] = lapply(jornada, function(x) 
    svyratio(~I(cae_general=="Ocupado" & c1==x & sexo==2),
             denominator=~I(cae_general=="Ocupado" & sexo==2), 
             na.rm=TRUE, info[[i]]))
}

#
jornada_sexo_nacional_mujer2_=list()
for (i in 1:length(info)){
  jornada_sexo_nacional_mujer2_[[i]] = unlist(lapply(jornada_sexo_nacional_mujer2[[i]], '[[', 1)) 
}

jornada_sexo_nacional_mujer2. = do.call(rbind, jornada_sexo_nacional_mujer2_)
rownames(jornada_sexo_nacional_mujer2.) = c("EFM","AMJ","JAS","OND")
colnames(jornada_sexo_nacional_mujer2.) = c("Jornada completa", "jornada parcial")

var.jornada_sexo_nacional_mujer2_=list()
for (i in 1:length(info)){
  var.jornada_sexo_nacional_mujer2_[[i]] = unlist(lapply(jornada_sexo_nacional_mujer2[[i]], '[[', 2)) 
}

var.jornada_sexo_nacional_mujer2_ = do.call(rbind, var.jornada_sexo_nacional_mujer2_)
rownames(var.jornada_sexo_nacional_mujer2_) = c("EFM","AMJ","JAS","OND")
colnames(var.jornada_sexo_nacional_mujer2_) = c("Jornada completa", "jornada parcial")

write.csv(jornada_sexo_nacional_mujer2., "jornada_sexo_nacional_mujer.csv")


## Ocupados Nacional por jornada y sexo
#
jornada_sexo_nacional2 = list()
for (i in 1:length(info)){
  jornada_sexo_nacional2[[i]] = lapply(jornada, function(x) 
    svyratio(~I(cae_general=="Ocupado" & c1==x),
             denominator=~I(cae_general=="Ocupado"), 
             na.rm=TRUE, info[[i]]))
}

#
jornada_sexo_nacional2_=list()
for (i in 1:length(info)){
  jornada_sexo_nacional2_[[i]] = unlist(lapply(jornada_sexo_nacional2[[i]], '[[', 1)) 
}

jornada_sexo_nacional2. = do.call(rbind, jornada_sexo_nacional2_)
rownames(jornada_sexo_nacional2.) = c("EFM","AMJ","JAS","OND")
colnames(jornada_sexo_nacional2.) = c("Jornada completa", "jornada parcial")

var.jornada_sexo_nacional2_=list()
for (i in 1:length(info)){
  var.jornada_sexo_nacional2_[[i]] = unlist(lapply(jornada_sexo_nacional2[[i]], '[[', 2)) 
}

var.jornada_sexo_nacional2_ = do.call(rbind, var.jornada_sexo_nacional2_)
rownames(var.jornada_sexo_nacional2_) = c("EFM","AMJ","JAS","OND")
colnames(var.jornada_sexo_nacional2_) = c("Jornada completa", "jornada parcial")

write.csv(jornada_sexo_nacional2., "jornada_sexo_nacional.csv")

####################################################################
# tasa de hombres ocupados por jornada a nivel de nuble
####################################################################
#
jornada_sexo_nuble_hombre2 = list()
for (i in 1:length(info)){
  jornada_sexo_nuble_hombre2[[i]] = lapply(jornada, function(x) 
    svyratio(~I(cae_general=="Ocupado" & c1==x & sexo==1 & prov_e==84),
             denominator=~I(cae_general=="Ocupado" & sexo==1 & prov_e==84), 
             na.rm=TRUE, info[[i]]))
}

#
jornada_sexo_nuble_hombre2_=list()
for (i in 1:length(info)){
  jornada_sexo_nuble_hombre2_[[i]] = unlist(lapply(jornada_sexo_nuble_hombre2[[i]], '[[', 1)) 
}

jornada_sexo_nuble_hombre2. = do.call(rbind, jornada_sexo_nuble_hombre2_)
rownames(jornada_sexo_nuble_hombre2.) = c("EFM","AMJ","JAS","OND")
colnames(jornada_sexo_nuble_hombre2.) = c("Jornada completa", "jornada parcial")

var.jornada_sexo_nuble_hombre2_=list()
for (i in 1:length(info)){
  var.jornada_sexo_nuble_hombre2_[[i]] = unlist(lapply(jornada_sexo_nuble_hombre2[[i]], '[[', 2)) 
}

var.jornada_sexo_nuble_hombre2_ = do.call(rbind, var.jornada_sexo_nuble_hombre2_)
rownames(var.jornada_sexo_nuble_hombre2_) = c("EFM","AMJ","JAS","OND")
colnames(var.jornada_sexo_nuble_hombre2_) = c("Jornada completa", "jornada parcial")

write.csv(jornada_sexo_nuble_hombre2., "jornada_sexo_nuble_hombre.csv")

# mujeres nuble
#
jornada_sexo_nuble_mujer2 = list()
for (i in 1:length(info)){
  jornada_sexo_nuble_mujer2[[i]] = lapply(jornada, function(x) 
    svyratio(~I(cae_general=="Ocupado" & c1==x & sexo==2 & prov_e==84),
             denominator=~I(cae_general=="Ocupado" & sexo==2 & prov_e==84), 
             na.rm=TRUE, info[[i]]))
}

#
jornada_sexo_nuble_mujer2_=list()
for (i in 1:length(info)){
  jornada_sexo_nuble_mujer2_[[i]] = unlist(lapply(jornada_sexo_nuble_mujer2[[i]], '[[', 1)) 
}

jornada_sexo_nuble_mujer2. = do.call(rbind, jornada_sexo_nuble_mujer2_)
rownames(jornada_sexo_nuble_mujer2.) = c("EFM","AMJ","JAS","OND")
colnames(jornada_sexo_nuble_mujer2.) = c("Jornada completa", "jornada parcial")

var.jornada_sexo_nuble_mujer2_=list()
for (i in 1:length(info)){
  var.jornada_sexo_nuble_mujer2_[[i]] = unlist(lapply(jornada_sexo_nuble_mujer2[[i]], '[[', 2)) 
}

var.jornada_sexo_nuble_mujer2_ = do.call(rbind, var.jornada_sexo_nuble_mujer2_)
rownames(var.jornada_sexo_nuble_mujer2_) = c("EFM","AMJ","JAS","OND")
colnames(var.jornada_sexo_nuble_mujer2_) = c("Jornada completa", "jornada parcial")

write.csv(jornada_sexo_nuble_mujer2., "jornada_sexo_nuble_mujer.csv")


## Ocupados Nacional por jornada y sexo
#
jornada_sexo_nuble2 = list()
for (i in 1:length(info)){
  jornada_sexo_nuble2[[i]] = lapply(jornada, function(x) 
    svyratio(~I(cae_general=="Ocupado" & c1==x & prov_e==84),
             denominator=~I(cae_general=="Ocupado"& prov_e==84), 
             na.rm=TRUE, info[[i]]))
}

#
jornada_sexo_nuble2_=list()
for (i in 1:length(info)){
  jornada_sexo_nuble2_[[i]] = unlist(lapply(jornada_sexo_nuble2[[i]], '[[', 1)) 
}

jornada_sexo_nuble2. = do.call(rbind, jornada_sexo_nuble2_)
rownames(jornada_sexo_nuble2.) = c("EFM","AMJ","JAS","OND")
colnames(jornada_sexo_nuble2.) = c("Jornada completa", "jornada parcial")

var.jornada_sexo_nuble2_=list()
for (i in 1:length(info)){
  var.jornada_sexo_nuble2_[[i]] = unlist(lapply(jornada_sexo_nuble2[[i]], '[[', 2)) 
}

var.jornada_sexo_nuble2_ = do.call(rbind, var.jornada_sexo_nuble2_)
rownames(var.jornada_sexo_nuble2_) = c("EFM","AMJ","JAS","OND")
colnames(var.jornada_sexo_nuble2_) = c("Jornada completa", "jornada parcial")

write.csv(jornada_sexo_nuble2., "jornada_sexo_nuble.csv")

### FIN CUADRO 24
