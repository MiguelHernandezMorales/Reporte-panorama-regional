for(i in 1:length(info)){
  info[[i]]$variables = mutate(info[[i]]$variables,
                      region2 = ifelse((prov==81 | prov==82 | prov==83),8, 
                               ifelse(prov==84,16, region)))
}


for(i in 1:length(info)){
  info[[i]]$variables = mutate(info[[i]]$variables,
                               region_e2 = ifelse((prov_e==81 | prov_e==82 | prov_e==83),8, 
                                                ifelse(prov_e==84,16, region_e)))
}


for(i in 1:length(info)){
  info[[i]]$variables = mutate(info[[i]]$variables,
                        conmutante = ifelse(region2!=region_e2,1,0))
}

peso_conmutantes1 = list()
for(i in 1:length(info)){
peso_conmutantes[[i]] = svyby(~I(conmutante==1), denominator = ~cae_general=="Ocupado", 
                         design = info[[i]], svyratio, by=~region_e2, na.rm = TRUE, na.rm.all = TRUE)
}

peso_conmutantes2 = list()
for(i in 1:length(info)){
  peso_conmutantes[[i]] = svyby(~I(conmutante==1), denominator = ~cae_general=="Ocupado", 
                                design = info[[i]], svyratio, by=~region2, na.rm = TRUE, na.rm.all = TRUE)
}
