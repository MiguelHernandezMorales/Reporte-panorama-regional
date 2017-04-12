#-------------------------------------------------------------------------------
# CUADRO 19: Ocupados de nuble por tramo de edad y sexo
#-------------------------------------------------------------------------------
## hombres nacional
tramos = levels(info[[1]]$variables$tramos)
tramos.etarios.nacional2 = list()
for (i in 1:length(info)){
  tramos.etarios.nacional2[[i]] = lapply(tramos, function(x) svyratio(~I(cae_general=="Ocupado" & tramos==x & sexo==1),
                                                                      denominator=~I(cae_general=="Ocupado" & sexo==1), 
                                                                      na.rm=TRUE, info[[i]]))
}

#
tramos.etarios.nacional2.=list()
for (i in 1:length(info)){
  tramos.etarios.nacional2.[[i]] = unlist(lapply(tramos.etarios.nacional2[[i]], '[[', 1)) 
}

tramos.etarios.nacional. = do.call(rbind, tramos.etarios.nacional2.)
rownames(tramos.etarios.nacional.) = c("EFM","AMJ","JAS","OND")
colnames(tramos.etarios.nacional.) = c("15-19", "20-24", "25-34", "35-44", "45-54", "55-64", "65 y mas")

var.tramos.etarios.nacional2.=list()
for (i in 1:length(info)){
  var.tramos.etarios.nacional2.[[i]] = unlist(lapply(tramos.etarios.nacional2[[i]], '[[', 2)) 
}

var.tramos.etarios.nacional2. = do.call(rbind, var.tramos.etarios.nacional2.)
rownames(var.tramos.etarios.nacional2.) = c("EFM","AMJ","JAS","OND")
colnames(var.tramos.etarios.nacional2.) = c("15-19", "20-24", "25-34", "35-44", "45-54", "55-64", "65 y mas")

write.csv(tramos.etarios.nacional., "tramos_etarios_nacional_sexo_hombre.csv")

# mujeres nacional
tramos = levels(info[[1]]$variables$tramos)
tramos.etarios.mujer.nacional2 = list()
for (i in 1:length(info)){
  tramos.etarios.mujer.nacional2[[i]] = 
    lapply(tramos, function(x) svyratio(~I(cae_general=="Ocupado" & tramos==x & sexo==2),
                                        denominator=~I(cae_general=="Ocupado" & sexo==2), 
                                        na.rm=TRUE, info[[i]]))
}

tramos.etarios.mujer.nacional2.=list()
for (i in 1:length(info)){
  tramos.etarios.mujer.nacional2.[[i]] = unlist(lapply(tramos.etarios.mujer.nacional2[[i]], '[[', 1)) 
}

tramos.etarios.mujer.nacional. = do.call(rbind, tramos.etarios.mujer.nacional2.)
rownames(tramos.etarios.mujer.nacional.) = c("EFM","AMJ","JAS","OND")
colnames(tramos.etarios.mujer.nacional.) = c("15-19", "20-24", "25-34", "35-44", "45-54", "55-64", "65 y mas")

var.tramos.etarios.mujer.nacional2.=list()
for (i in 1:length(info)){
  var.tramos.etarios.mujer.nacional2.[[i]] = unlist(lapply(tramos.etarios.mujer.nacional2[[i]], '[[', 2)) 
}

var.tramos.etarios.mujer.nacional. = do.call(rbind, var.tramos.etarios.mujer.nacional2.)
rownames(var.tramos.etarios.mujer.nacional.) = c("EFM","AMJ","JAS","OND")
colnames(var.tramos.etarios.mujer.nacional.) = c("15-19", "20-24", "25-34", "35-44", "45-54", "55-64", "65 y mas")

write.csv(tramos.etarios.mujer.nacional., "tramos_etarios_nacional_sexo_mujer.csv")

## Occupados Nacional por tramos
tramos = levels(info[[1]]$variables$tramos)
tramos.etarios.nacional = list()
for (i in 1:length(info)){
  tramos.etarios.nacional[[i]] = 
    lapply(tramos, function(x) svyratio(~I(cae_general=="Ocupado" & tramos==x),
                                        denominator=~I(cae_general=="Ocupado"), 
                                        na.rm=TRUE, info[[i]]))
}

#
tramos.etarios.nacional.=list()
for (i in 1:length(info)){
  tramos.etarios.nacional.[[i]] = unlist(lapply(tramos.etarios.nacional[[i]], '[[', 1)) 
}

tramos.etarios.nacional2 = do.call(rbind, tramos.etarios.nacional.)
rownames(tramos.etarios.nacional2) = c("EFM","AMJ","JAS","OND")
colnames(tramos.etarios.nacional2) = c("15-19", "20-24", "25-34", "35-44", "45-54", "55-64", "65 y mas")

var.tramos.etarios.nacional2=list()
for (i in 1:length(info)){
  var.tramos.etarios.nacional2[[i]] = unlist(lapply(tramos.etarios.nacional[[i]], '[[', 2)) 
}

var.tramos.etarios.nacional2. = do.call(rbind, var.tramos.etarios.nacional2)
rownames(var.tramos.etarios.nacional2.) = c("EFM","AMJ","JAS","OND")
colnames(var.tramos.etarios.nacional2.) = c("15-19", "20-24", "25-34", "35-44", "45-54", "55-64", "65 y mas")

write.csv(tramos.etarios.nacional2, "tramos_etarios_nacional.csv")

# Hombre en nuble
tramos = levels(info[[1]]$variables$tramos)
tramos.etarios_nuble2 = list()
for (i in 1:length(info)){
  tramos.etarios_nuble2[[i]] = 
    lapply(tramos, function(x) svyratio(~I(cae_general=="Ocupado" & tramos==x & prov_e==84 & sexo==1),
                                        denominator=~I(cae_general=="Ocupado" & prov_e==84 & sexo==1), 
                                        na.rm=TRUE, info[[i]]))
}

tramos.etarios_nuble2.=list()
for (i in 1:length(info)){
  tramos.etarios_nuble2.[[i]] = unlist(lapply(tramos.etarios_nuble2[[i]], '[[', 1)) 
}

tramos.etarios_nuble. = do.call(rbind, tramos.etarios_nuble2.)
rownames(tramos.etarios_nuble.) = c("EFM","AMJ","JAS","OND")
colnames(tramos.etarios_nuble.) = c("15-19", "20-24", "25-34", "35-44", "45-54", "55-64", "65 y mas")

var.tramos.etarios_nuble2.=list()
for (i in 1:length(info)){
  var.tramos.etarios_nuble2.[[i]] = unlist(lapply(tramos.etarios_nuble2[[i]], '[[', 2)) 
}

var.tramos.etarios_nuble. = do.call(rbind, var.tramos.etarios_nuble2.)
rownames(var.tramos.etarios_nuble.) = c("EFM","AMJ","JAS","OND")
colnames(var.tramos.etarios_nuble.) = c("15-19", "20-24", "25-34", "35-44", "45-54", "55-64", "65 y mas")

write.csv(tramos.etarios_nuble., "tramos_etarios_nuble_sexo_hombre.csv")

# mujeres nuble
tramos = levels(info[[1]]$variables$tramos)
tramos.etarios_nuble_mujeres_nuble2 = list()
for (i in 1:length(info)){
  tramos.etarios_nuble_mujeres_nuble2[[i]] = 
    lapply(tramos, function(x) svyratio(~I(cae_general=="Ocupado" & tramos==x & prov_e==84 & sexo==2),
                                        denominator=~I(cae_general=="Ocupado" & prov_e==84 & sexo==2), 
                                        na.rm=TRUE, info[[i]]))
}

tramos.etarios_nuble_mujeres_nuble2.=list()
for (i in 1:length(info)){
  tramos.etarios_nuble_mujeres_nuble2.[[i]] = unlist(lapply(tramos.etarios_nuble_mujeres_nuble2[[i]], '[[', 1)) 
}

tramos.etarios_nuble_mujeres_nuble. = do.call(rbind, tramos.etarios_nuble_mujeres_nuble2.)
rownames(tramos.etarios_nuble_mujeres_nuble.) = c("EFM","AMJ","JAS","OND")
colnames(tramos.etarios_nuble_mujeres_nuble.) = c("15-19", "20-24", "25-34", "35-44", "45-54", "55-64", "65 y mas")

var.tramos.etarios_nuble_mujeres_nuble2.=list()
for (i in 1:length(info)){
  var.tramos.etarios_nuble_mujeres_nuble2.[[i]] = unlist(lapply(tramos.etarios_nuble_mujeres_nuble2[[i]], '[[', 2)) 
}

var.tramos.etarios_nuble_mujeres_nuble. = do.call(rbind, var.tramos.etarios_nuble_mujeres_nuble2.)
rownames(var.tramos.etarios_nuble_mujeres_nuble.) = c("EFM","AMJ","JAS","OND")
colnames(var.tramos.etarios_nuble_mujeres_nuble.) = c("15-19", "20-24", "25-34", "35-44", "45-54", "55-64", "65 y mas")

write.csv(tramos.etarios_nuble_mujeres_nuble., "tramos_etarios_nuble_sexo_mujer.csv")

## Ocupados Nuble por tramos
tramos = levels(info[[1]]$variables$tramos)
tramos.etarios.nuble = list()
for (i in 1:length(info)){
  tramos.etarios.nuble[[i]] = 
    lapply(tramos, function(x) svyratio(~I(cae_general=="Ocupado" & tramos==x & prov_e==84),
                                        denominator=~I(cae_general=="Ocupado" & prov_e==84), 
                                        na.rm=TRUE, info[[i]]))
}

tramos.etarios.nuble.=list()
for (i in 1:length(info)){
  tramos.etarios.nuble.[[i]] = unlist(lapply(tramos.etarios.nuble[[i]], '[[', 1)) 
}

tramos.etarios.nuble2 = do.call(rbind, tramos.etarios.nuble.)
rownames(tramos.etarios.nuble2) = c("EFM","AMJ","JAS","OND")
colnames(tramos.etarios.nuble2) = c("15-19", "20-24", "25-34", "35-44", "45-54", "55-64", "65 y mas")

var.tramos.etarios.nuble.=list()
for (i in 1:length(info)){
  var.tramos.etarios.nuble.[[i]] = unlist(lapply(tramos.etarios.nuble[[i]], '[[', 2)) 
}

var.tramos.etarios.nuble2. = do.call(rbind, var.tramos.etarios.nuble.)
rownames(var.tramos.etarios.nuble2.) = c("EFM","AMJ","JAS","OND")
colnames(var.tramos.etarios.nuble2.) = c("15-19", "20-24", "25-34", "35-44", "45-54", "55-64", "65 y mas")

write.csv(tramos.etarios.nuble2, "tramos_etarios_nuble.csv")

# FIN CUADRO 19
