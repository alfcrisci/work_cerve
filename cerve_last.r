#################################################################################################################
# Please install required R packages before.

# install.packages("vcd")
# install.packages("vcdExtra")
# install.packages("caret")


library(MASS)
library(caret)
library(vcd)
library(vcdExtra)

setwd("/home/alf/Scrivania/lav_cerve")
mat_cerve=read.csv("mat_cerve_new.csv")
mat_square_distance=read.csv("mat_square_distance.csv")

# [1] "giorno"   "mese"     "anno"     "stagione" "animale"  "Altacq"   "quadr"    "dist"     "bosco"    "tmina"    "tmaxa"    "tmeda"   
# [13] "tminr"    "tmaxr"    "tmediar"  "deltmin"  "deltmac"  "deltmed"  "altcmin"  "altcmax"  "altcmed"  "delqmed"  "delqmax"  "delqmin" 
#  [25] "oreluce"  "activore"          



#################################################################################################################
# Purging variables e preparing diffrent work matrix

matcerve_purged_tmediar=mat_cerve[,-which(names(mat_cerve) %in% c("giorno","mese","anno","Altacq","activore","quadr","tminr","tmaxr" ))]
matcerve_purged_tmaxr=mat_cerve[,-which(names(mat_cerve) %in% c("giorno","mese","anno","Altacq","activore","quadr","tminr","tmediar" ))]
matcerve_purged_tminr=mat_cerve[,-which(names(mat_cerve) %in% c("giorno","mese","anno","Altacq","activore","quadr","tmaxr","tmediar" ))]

#################################################################################################################
# Factorisation

factorisation=function(x) {
  
  
x$dist=as.factor(x$dist)
x$bosco=as.factor(x$bosco)
x$animale=as.factor(x$animale)
x$stagione=as.factor(x$stagione)
x$altcmin=as.factor(x$altcmin)
x$altcmax=as.factor(x$altcmax)
x$altcmed=as.factor(x$altcmed)
x$delqmed=as.factor(x$delqmed)
x$delqmax=as.factor(x$delqmax)
x$delqmin=as.factor(x$delqmin)
x$delqmed=as.factor(x$delqmed)

return(x)
}

matcerve_purged_tmediar=factorisation(matcerve_purged_tmediar)
matcerve_purged_tmaxr=factorisation(matcerve_purged_tmaxr)
matcerve_purged_tminr=factorisation(matcerve_purged_tminr)


#################################################################################################################
# guess modelisation


res=list()


res[[1]]=summary(stepAIC(glm(tmediar ~ . ,data=na.omit(matcerve_purged_tmediar))))
res[[2]]=summary(stepAIC(glm(tmaxr ~ . ,data=na.omit(matcerve_purged_tmaxr))))
res[[3]]=summary(stepAIC(glm(tminr ~ . ,data=na.omit(matcerve_purged_tminr))))


#################################################################################################################
# Esporta i risultati

cat(res,file="risultati_modelli.txt")



############################################################################################################àà
# Preparazione matrice dati fattoriale 



mat_cerve_F=mat_cerve[c("stagione","bosco","delqmed","altcmed","quadr","anno","animale")]
mat_cerve_F=as.data.frame(apply(mat_cerve_F,2,as.factor))

############################################################################################################àà
# Presenza in bosco per stagione e anno.

season_factor_full <- structable(stagione ~bosco+anno,data = mat_cerve_F)

mosaic(season_factor_full, shaded=T,legend=T) 

png("accoc_stagione_bosco_anno.png",width = 900, height = 1280)
assoc(season_factor_full, shade=TRUE)
dev.off()

############################################################################################################àà
# Presenza in bosco per stagione e quota.

season_factor_full <- structable(stagione ~ bosco,data = mat_cerve_F)

mosaic(season_factor_full, shaded=T,legend=T) 

png("accoc_stagione_bosco.png",width = 680, height = 980)
assoc(season_factor_full, shade=TRUE)
dev.off()






############################################################################################################àà
# Presenza in bosco per stagione e per salto quota

season_factor_full <- structable(stagione ~ bosco+delqmed,data = mat_cerve_F)

mosaic(season_factor_full, shaded=T,legend=T) 

png("accoc_stagione_bosco_saltoquota.png",width = 680, height = 980)
assoc(season_factor_full, shade=TRUE)
dev.off()




###############################################################################################
# Quadrante per stagione

season_factor_full <- structable(stagione ~ quadr , data = mat_cerve_F)

mosaic(season_factor_full, shaded=T,legend=T) 
png("accoc_bosco_stagione.png",width = 680, height = 1280)
assoc(season_factor_full, shade=TRUE) 
dev.off()