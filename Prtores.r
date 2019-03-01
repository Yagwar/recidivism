
library(ggplot2)
library(MLmetrics)
library(FactoMineR)
library(party) #install.packages("") 
library(rpart) # install.packages("ggplot2", repos='https://cloud.r-project.org')


entropy_y= function(y_values){
    frecs=prop.table(table(y_values))
    p_val=frecs["SI"]+1e-16# este decimal pequeño es para corregir el error logaritmo cero, da NAN y no deja avanzar
    q_val=frecs["NO"]+1e-16# este decimal pequeño es para corregir el error logaritmo cero, da NAN y no deja avanzar
    return (as.numeric(-(q_val*log2(q_val))-(p_val*log2(p_val))))}

entropy_xy=function(true_condition,reactivo){ #La formula actualizada para calcular más de dos valores
    df_dat=data.frame("true_condition"=true_condition,"reactivo"=reactivo)
    df_dat=df_dat[rowSums(!is.na(df_dat))==2,]
    entr_xy=0
    for (val in unique(df_dat$reactivo)){
        entr_y=entropy_y(df_dat[df_dat$reactivo==val,"true_condition"])
        p_val=sum(df_dat$reactivo==val)/length(df_dat$reactivo)
        entr_xy=(entr_y*p_val)+entr_xy
    }
    return (entr_xy)
}    
info_gain=function(true_condition,reactivo){entropy_y(true_condition)-entropy_xy(true_condition,reactivo)}

cuestionario_1=read.csv(file="SOLO IVIC.csv", header = F, encoding="UTF-8", row.names=1)
print(paste("Archivo de datos cargado ", nrow(cuestionario_1)," filas y ",ncol(cuestionario_1),"columnas"))
variables_1=read.csv("vars.csv", header=T)
print(paste("Archivo variables cargado ", nrow(variables_1)," filas y ",ncol(variables_1),"columnas"))
colnames(cuestionario_1)=variables_1$ident
#row.names(cuestionario_1)=cuestionario_1$INTERNO
variables_1$procesar=as.logical(variables_1$procesar)

cuestionario_1$var_35=as.Date(as.character.factor(cuestionario_1$var_35),format = "%m/%d/%Y")

## Separar variables a procesar, de las que no.
cuestionario_2=cuestionario_1[,variables_1$procesar]

variables_2=variables_1[variables_1$procesar,1:3]
 # variables_2$tipo[5]="CATEGORICA" # la variable reincidenciareco es la variable dependiente
##

for (i in 1:ncol(cuestionario_2)){
    if(variables_2$tipo[i]=="CATEGORICA"){
#         cuestionario_2[is.na(cuestionario_2[,i]),i]=9999
        cuestionario_2[,i]=factor(cuestionario_2[,i])
    }else{
        cuestionario_2[,i]=as.numeric(cuestionario_2[,i])
    }
}
cuestionario_2$var_5=factor(cuestionario_1$var_5, labels=c("NO","SI"))
cuestionario_3=cuestionario_2[!colnames(cuestionario_2)=="var_5"]
cuestionario_3=data.frame("REINCIDENCIARECO"=cuestionario_2$var_5,cuestionario_3)
print(paste("Variables a procesar separadas y transformados sus valores. Forma final del cuestionario:", list(dim(cuestionario_3))))

str(cuestionario_3)

# cuestionario_3b=cuestionario_3
# # table(rowSums(is.na(cuestionario_3))==0)
# table(cuestionario_3$REINCIDENCIARECO)
# summary(cuestionario_3$REINCIDENCIARECO)
dim(cuestionario_1)

tbl=table(
cuestionario_3$var_8==7
);print(tbl)
prop.table(tbl)





# cuestionario_3=cuestionario_3b
################
cuestionario_3=cuestionario_3[cuestionario_1$var_35<as.Date("12/31/2012",format = "%m/%d/%Y"),] # eliminar registros posteriores a 2012
cuestionario_3=cuestionario_3[cuestionario_3$var_12>17&
                              cuestionario_3$var_12<66,
                              1:ncol(cuestionario_3)]# rango limitado de edad

cuestionario_3=cuestionario_3[cuestionario_3$var_6==1,
                              1:ncol(cuestionario_3)]#Solo hombres

cuestionario_3=cuestionario_3[,c(1,3:ncol(cuestionario_3))]# Se elimina la variable género

cuestionario_3$var_7[is.na(cuestionario_3$var_7)]=0# Na's Se reemplazan por cero hijos
cuestionario_3=cuestionario_3[cuestionario_3$var_7<13,
                              1:ncol(cuestionario_3)]# Menos de 13 hijos

cuestionario_3=cuestionario_3[!(cuestionario_3$var_8==7),
                              1:ncol(cuestionario_3)]# Eliminar los religiosos

# Unificar estado civil
cuestionario_3$var_8[cuestionario_3$var_8==0]=1 # sin datos se pasó a soltero como valor por defecto
cuestionario_3$var_8[cuestionario_3$var_8==4]=3 # Divorciado se pasó a separado
cuestionario_3$var_8[cuestionario_3$var_8==6]=3 # Viudo se pasó a separado

variables_2=variables_2[c(1,3:nrow(variables_2)),]# se elimina la variable género por quedar solo hombres
###############

# cuestionario_3=cuestionario_3[rowSums(is.na(cuestionario_3))==0,]# Eliminar los casos con datos vacíos
summary(cuestionario_3$REINCIDENCIARECO)

reincidencia_año=data.frame(año=as.numeric(format(as.Date(cuestionario_1$var_35),"%Y")),Reincidente=cuestionario_1$var_5)
barplot(
    t(
        table(reincidencia_año)
    ),beside=T
)
plot(table(reincidencia_año))
# summary(as.numeric(format(as.Date(cuestionario_1$var_35),"%Y")))

tbl_df=data.frame(table(reincidencia_año))
reincid_año=data.frame(rowSums(table(reincidencia_año)))
reincid_año$no_rein= tbl_df[tbl_df$Reincidente==0,]$Freq
reincid_año$si_rein= tbl_df[tbl_df$Reincidente==1,]$Freq
reincid_año$no_rein_cumsum=cumsum(tbl_df[tbl_df$Reincidente==0,]$Freq)
reincid_año$si_rein_cumsum=cumsum(tbl_df[tbl_df$Reincidente==1,]$Freq)
colnames(reincid_año)[1]="n_internos"
reincid_año$n_internos_cumsum=cumsum(reincid_año$n_internos)

reincid_año

plot(rownames(reincid_año),reincid_año$n_internos, type="l")#, col=1,ylim=c(0,30000))
lines(rownames(reincid_año),reincid_año$no_rein, col=3)
lines(rownames(reincid_año),reincid_año$si_rein, col=2)

plot(rownames(reincid_año),reincid_año$n_internos_cumsum, type="l")#, col=1,ylim=c(0,30000))
lines(rownames(reincid_año),reincid_año$no_rein_cumsum, col=3)
lines(rownames(reincid_año),reincid_año$si_rein_cumsum, col=2)


tree_dat=ctree(Reincidente ~ año, data=reincidencia_año)
plot(tree_dat)
# reincidencia_año

results=data.frame("ident"=rep(NA,ncol(cuestionario_3)),
                   "nom_var"=rep(NA,ncol(cuestionario_3)),
                   "info_gain"=rep(NA,ncol(cuestionario_3)),
                   "chisq"=rep(NA,ncol(cuestionario_3)),
                   "t_test"=rep(NA,ncol(cuestionario_3)),
                   "decis"=rep(NA,ncol(cuestionario_3))
)
for(i in 2:ncol(cuestionario_3)){
    print (paste("variable***",i))
    ident_var=variables_2[variables_2$ident==colnames(cuestionario_3)[i],"ident"]
    print(paste("Identificador de la variable***",ident_var))
    results$ident[i]=as.character(ident_var)
    nombr_var=variables_2[variables_2$ident==colnames(cuestionario_3)[i],"nombre"]
    print(paste("Nombre variable***",nombr_var))
    results$nom_var[i]=as.character(nombr_var)
    print(colnames(cuestionario_3)[i])
    tipo_var=variables_2[variables_2$ident==colnames(cuestionario_3)[i],"tipo"]
    decis=NULL

    if(tipo_var=="CATEGORICA"){
        print(tipo_var)
        info_gain_var=info_gain(cuestionario_3$REINCIDENCIARECO,cuestionario_3[,i])
        print(paste("info Gain",info_gain_var))
        results$info_gain[i]=info_gain_var
        test_var=chisq.test(table(cuestionario_3$REINCIDENCIARECO,cuestionario_3[,i]))
        valor_p_chi=test_var$p.value
        if (valor_p_chi=="NaN"){valor_p_chi=0.99}
        results$chisq[i]=valor_p_chi
#         fisher_test_var=fisher.test(table(cuestionario_3$REINCIDENCIARECO,cuestionario_3[,i]))
#         fisher_valor_p=round(test_var$p.value,2)
        print(paste("prueba chi",valor_p_chi))
#         print(paste("prueba fisher",fisher_valor_p))
        if (valor_p_chi<0.05){
            decis="Si existen diferencias"
            if(info_gain_var>0.003){results$decis[i]=2}else{results$decis[i]=1}
        }else{
            decis="No existen diferencias"
            results$decis[i]=0
        }
    }else{
        print(tipo_var)
        t_test=t.test(cuestionario_3[,i]~cuestionario_3$REINCIDENCIARECO,var.equal=T)
        valor_p=t_test$p.value
        results$t_test[i]=valor_p
        print(paste("prueba t",valor_p))
        if (valor_p<0.05){
            decis="Si existen diferencias"
            results$decis[i]=1
        }else{
            decis="No existen diferencias"
            results$decis[i]=0
        }
    }
    print (decis)
}

?ctree



tree_dat=ctree(REINCIDENCIARECO ~ var_7, data=cuestionario_3)
print(unique(cuestionario_3$var_7))
plot(tree_dat)
# table(cuestionario_3$REINCIDENCIARECO)

tree_dat=ctree(REINCIDENCIARECO ~ var_10, data=cuestionario_3)
print(unique(cuestionario_3$var_10))
plot(tree_dat)
# table(cuestionario_3$REINCIDENCIARECO)

tree_dat=ctree(REINCIDENCIARECO ~ var_11, data=cuestionario_3)
print(unique(cuestionario_3$var_11))
plot(tree_dat)
# table(cuestionario_3$REINCIDENCIARECO)

tree_dat=ctree(REINCIDENCIARECO ~ var_12, data=cuestionario_3)
print(unique(cuestionario_3$var_12))
plot(tree_dat)
# table(cuestionario_3$REINCIDENCIARECO)

tree_dat=ctree(REINCIDENCIARECO ~ var_13, data=cuestionario_3)
print(unique(cuestionario_3$var_13))
plot(tree_dat)
# table(cuestionario_3$REINCIDENCIARECO)

tree_dat=ctree(REINCIDENCIARECO ~ var_34, data=cuestionario_3)
print(unique(cuestionario_3$var_34))
plot(tree_dat)
# table(cuestionario_3$REINCIDENCIARECO)

cuestionario_4=cuestionario_3
cuestionario_4$var_7 =factor((cuestionario_4$var_7 >3)*1)
cuestionario_4$var_10=factor((cuestionario_4$var_10>3)*1)
cuestionario_4$var_11=factor((cuestionario_4$var_11>0)*1)
cuestionario_4$var_12=factor((cuestionario_4$var_12>30)*1)
cuestionario_4$var_13=factor((cuestionario_4$var_13>1)*1)
cuestionario_4$var_34=factor((cuestionario_4$var_34>0)*1)
str(cuestionario_4)

rownames(results)=colnames(cuestionario_3)
results
# hist(results$info_gain, col="gray75")

results_2=data.frame(#"ident"=rep(NA,ncol(cuestionario_4)),
                   "nom_var"=rep(NA,ncol(cuestionario_4)),
                   "info_gain"=rep(NA,ncol(cuestionario_4)),
                   "chisq"=rep(NA,ncol(cuestionario_4)),
                   "decis"=rep(NA,ncol(cuestionario_4))
)
for(i in 2:ncol(cuestionario_4)){
#     ident_var=variables_2[variables_2$ident==colnames(cuestionario_4)[i],"ident"]
#     results_2$ident[i]=as.character(ident_var)
    nombr_var=variables_2[variables_2$ident==colnames(cuestionario_4)[i],"nombre"]
    results_2$nom_var[i]=as.character(nombr_var)
    decis=NULL
    info_gain_var=info_gain(cuestionario_4$REINCIDENCIARECO,cuestionario_4[,i])
    results_2$info_gain[i]=info_gain_var
    f1_var=F1_Score((cuestionario_4$REINCIDENCIARECO=="SI")*1,(cuestionario_4[,i]=="1")*1)
    test_var=chisq.test(table(cuestionario_4$REINCIDENCIARECO,cuestionario_4[,i]))
    valor_p_chi=test_var$p.value
    if (valor_p_chi=="NaN"){valor_p_chi=0.99}
    results_2$chisq[i]=valor_p_chi
    if (valor_p_chi<0.05){
        decis="Si existen diferencias"
        if(info_gain_var>0.003){results_2$decis[i]=2}else{results_2$decis[i]=1}
    }else{
        decis="No existen diferencias"
        results_2$decis[i]=0
    }
}

rownames(results_2)=colnames(cuestionario_4)

plot(results_2$chisq, results_2$info_gain, pch=16, col=results_2$decis+2)

write.csv(cuestionario_4,file="cuestionario_4.csv")
write.csv(cuestionario_3,file="cuestionario_3.csv")
write.csv(variables_2,file="variables_2.csv")
write.csv(results_2,file="results.csv")

cuestionario_4=read.csv(file="cuestionario_4.csv", header=T)#, stringsAsFactors=F)#, row.names=1
variables_2=read.csv(file="variables_2.csv", header=T, row.names=1)
results_2=read.csv(file="results.csv", header=T, row.names=1)

best_predicts=read.csv("best_predicts.csv", header=T, row.names=1)
df_results=read.csv("results_hot.csv", header=T, row.names=1)
predicts_dat=read.csv("predicts.csv", header=T, row.names=1)

for (i in c(1:length(colnames(predicts_dat)))){
    predicts_dat[,i]=factor(predicts_dat[,i])
}
str(predicts_dat)

predicts_dat$var_10    =factor(predicts_dat$var_10,    labels=c("Menor que Ejemplar", "Ejemplar"))
predicts_dat$var_12    =factor(predicts_dat$var_12,    labels=c("30 o Menos", "Mayor a 30"))
predicts_dat$var_13    =factor(predicts_dat$var_13,    labels=c("Uno", "Mas de Uno"))
predicts_dat$var_14    =factor(predicts_dat$var_14,    labels=c("NO", "SI"))
predicts_dat$var_175   =factor(predicts_dat$var_175,   labels=c("NO", "SI"))
predicts_dat$var_189   =factor(predicts_dat$var_189,   labels=c("NO", "SI"))
predicts_dat$var_254   =factor(predicts_dat$var_254,   labels=c("NO", "SI"))
predicts_dat$var_27    =factor(predicts_dat$var_27,    labels=c("NO", "SI"))
predicts_dat$var_176_1 =factor(predicts_dat$var_176_1, labels=c("Alguna SPA", "Ninguna"))
predicts_dat$var_203_1 =factor(predicts_dat$var_203_1, labels=c("Por una causa","Niega cometer el delito"))
predicts_dat$var_212_1 =factor(predicts_dat$var_212_1, labels=c("Individual/Conjunto","Niega cometer el delito"))
predicts_dat$var_217_3 =factor(predicts_dat$var_217_3, labels=c("SI/NO", "No Consume"))
predicts_dat$var_203_3 =factor(predicts_dat$var_203_3, labels=c("NO", "SI"))

varnames=c("REINCIDENCIARECO",# REINCIDENCIARECO
           "Calificacion",#var_10 
           "Edad 1er Ingreso",#var_12
           "Numero de Delitos",#var_13
           "Homicidio",#var_14
           "Consumo alguna vez SPA",#var_175  
           "Forma Vida Delictiva",#var_189 
           "Confesó el delito",#var_254 
           "Hurto",#var_27
           "Tipo de SPA consumida",#var_176_1
           "Motivación del Delito",#var_203_1
           "Cometió delito con",#var_212_1
           "Violar la Ley Bajo SPA",#var_217_3
           "Delito por Circunstancias "#var_203_3
          )
colnames(predicts_dat)=varnames

write.csv2(predicts_dat,file="select_20_codes.csv")

log_reg_model <- glm(REINCIDENCIARECO ~.,family=binomial(link='logit'),data=predicts_dat)
summary(log_reg_model)

names(log_reg_model)







tree_dat_sel=ctree(REINCIDENCIARECO ~.,
               data=predicts_dat)

jpeg(filename="arbol_select.jpg",width = 3500, height = 1200, units = "px",quality = 90)
plot(tree_dat_sel)
dev.off()

Reincdnts_factor=cuestionario_4[cuestionario_4$REINCIDENCIARECO=="SI",2:ncol(cuestionario_4)]

dend_test4=hclust(dist(t(Reincdnts_factor)),method="ward.D")

k_groups=2
# jpeg(file = "dendrograma.jpeg",width = 16400, height = 6400, units = "px", quality = 95)
plot(dend_test4)#,hang=-1)
groups<- factor(cutree(dend_test4, k=k_groups) )
rect.hclust(dend_test4, k=k_groups, border="darkblue") 
# dev.off()

(groups)

Reincdnts_factor_25=predicts_dat[predicts_dat$REINCIDENCIARECO=="SI",2:ncol(predicts_dat)]

dend_test4_25=hclust(dist((Reincdnts_factor_25)),method="ward.D")

k_groups=3
# jpeg(file = "dendrograma_25.jpeg",width = 16400, height = 6400, units = "px", quality = 95)
plot(dend_test4_25)#,hang=-1)
groups_25<- factor(cutree(dend_test4_25, k=k_groups) )
rect.hclust(dend_test4_25, k=k_groups, border="darkblue") 
# dev.off()

dend_predicts_dat=hclust(dist((predicts_dat)),method="ward.D")

k_groups=2
# jpeg(file = "dendrograma_25.jpeg",width = 16400, height = 6400, units = "px", quality = 95)
plot(dend_predicts_dat)#,hang=-1)
groups_dend_predicts_dat<- factor(cutree(dend_predicts_dat, k=k_groups) )
rect.hclust(dend_predicts_dat, k=k_groups, border="darkblue") 
# dev.off()

table(groups_dend_predicts_dat,predicts_dat$REINCIDENCIARECO)

predicts_dat_2=predicts_dat
predicts_dat_2["grupo"]=groups_dend_predicts_dat

tree_predicts_dat_2=ctree(grupo ~.,
               data=predicts_dat_2)
# (predicts_dat_2)

plot(tree_predicts_dat_2)

best_predicts[20:25,]

(table(data.frame(cuestionario_5$var_212,cuestionario_5$REINCIDENCIARECO)))

barplot(table(cuestionario_5$var_212))

str(predicts_dat)



predicts_dat$var_176_1


