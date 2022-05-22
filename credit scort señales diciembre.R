# ============================
# Sesión 01: Credit Scoring I 
# ============================
rm(list = ls())
library(data.table)
library(ggplot2)
library(rstudioapi)

# =================================
# Importación de datos 
# =================================
ruta <- dirname(getActiveDocumentContext()$path)
setwd(ruta)

dt_base <- fread(file = "BBDD_CS.csv",
                 sep = ",",
                 header = T)

colnames(CS.PM.DIC.21)

# Age: Edad 
# D1_MaxD12m: Deuda del último mes respecto al máximo de la deuda de los u12m
# DiasAtrasos_12m: Número de meses con atrasos en los u12m
# AntiguedadSF_u24m: Antiguedad en el sistema financiero en los u24m
# flg_garantias: Si el cliente tiene o no garantías
# flg_Situacionlaboral: Si el cliente es dependiente e independiente.
# Default: Si alguna vez ha tenido más de 30 días de atrasos en los s6m 

# =================================
# Desarrollo del Scorecard: Parte I
# Análisis Univariado
# =================================

# Análisis Univariado 
f_Ana_Uni_Cuan <- function(CS.PM.DIC.21,variable){
  # Histograma
  g1 <- ggplot(data = CS.PM.DIC.21)
  g1 <- g1 + geom_histogram(mapping = aes(x = get(variable)))
  g1 <- g1 + xlab(variable)
  g1 <- g1 + ylab("N")
  
  # Boxplot
  g2 <- ggplot(data = CS.PM.DIC.21)
  g2 <- g2 + geom_boxplot(mapping = aes(y = get(variable)))
  g2 <- g2 + stat_boxplot(mapping = aes(y = get(variable)),geom = "errorbar")
  
  library(ggpubr)
  g <- ggarrange(g1,g2,labels = c("Histograma","Boxplot"),ncol = 2,nrow = 1)
  return(g)
}

 # t(as.matrix(summary(CS.PM.DIC.21$EDAD)))
   #Estadísticos Descriptivos 
  #dt_EstDescr <- t(as.matrix(summary(CS.PM.DIC.21[,get(variable)])))
  #dt_EstDescr[,Variable := variable]
  #return(list(Grafico = g,
              #EstDscr = (dt_EstDescr)))
#

f_Ana_Uni_Cuan(CS.PM.DIC.21,"EDAD")
f_Ana_Uni_Cuan(CS.PM.DIC.21,"ABONO_PROM_AM")
f_Ana_Uni_Cuan(CS.PM.DIC.21,"TC_LA")
f_Ana_Uni_Cuan(CS.PM.DIC.21,"SF_REVOLVENTE_D")

# Análisis Univariado para variables cualitativas
f_Ana_Uni_Cual <- function(CS.PM.DIC.21,variable){
  dt_freq <- CS.PM.DIC.21[,list(N =.N,
                            PCT_N = .N/nrow(CS.PM.DIC.21)),
                         by = variable]
  g <- ggplot(CS.PM.DIC.21)
  g <- g + geom_bar(aes(x = as.factor(get(variable))), stat = "count")
  g <- g + xlab(variable)
  g <- g + ylab("N")
  
  return(list(Grafico = g, Frecuencia = dt_freq))
}
CS.PM.DIC.21[,list(N =.N,
                   PCT_N = .N/nrow(CS.PM.DIC.21)),
             by = C]
f_Ana_Uni_Cual(CS.PM.DIC.21,"C")
f_Ana_Uni_Cual(dt_base,"flg_SituacionLaboral")

# Número de Missings (Valores Perdidos)
sum(is.na(CS.PM.DIC.21$EDAD))
dt_missings <- as.data.table(sapply(CS.PM.DIC.21,FUN = function(x){sum(is.na(x))}),
                             keep.rownames = T)
setnames(dt_missings,c("Variables","#Missing"))

# Split de la muestra 
set.seed(20220520)
v_ids <- sample(x = 1:nrow(CS.PM.DIC.21),
                size = 0.70*nrow(CS.PM.DIC.21),
                replace = FALSE)
dt_train <- CS.PM.DIC.21[v_ids,]

CS.PM.DIC.21[,list(BR= sum(DEFAULT)/nrow(CS.PM.DIC.21))]
dt_train[,list(BR= sum(DEFAULT)/nrow(dt_train))]


# =================================
# Desarrollo del Scorecard: Parte II
# Análisis Características
# =================================

# Variables Cualitatitas y Cuantitativas
library(party)
v_variables_cuantitativas <- c("EDAD","NRO_EF_D",
                               "PM_SALDO")

v_variables_cualitativas <- c("TC_LA","SF_REVOLVENTE_D")


# Binning de variables en R
# Variable EDAD: 
formula1 <- formula(DEFAULT ~ EDAD)
parms <- ctree_control(mincriterion = 0.99,
                       minbucket = 0.05*nrow(dt_train))
ctree1 <- ctree(formula = formula1,
                controls = parms,
                data = dt_train)
plot(ctree1,type = "simple")


# Variable SALDO_TOTAL_BNSF: 
formula1 <- formula(DEFAULT ~ PM_SALDO)
parms <- ctree_control(mincriterion = 0.99,
                       minbucket = 0.05*nrow(dt_train))
ctree2 <- ctree(formula = formula1,
                controls = parms,
                data = dt_train)
plot(ctree2,type = "simple")

# Variable ABONO_PROM_AM : 
formula1 <- formula(DEFAULT ~ NRO_EF_D)
parms <- ctree_control(mincriterion = 0.95,
                       minbucket = 0.05*nrow(dt_train))
ctree3 <- ctree(formula = formula1,
                controls = parms,
                data = dt_train)
plot(ctree3,type = "simple") 


# Variable AntiguedadSF_u24m: 
#formula1 <- formula(Default ~ AntiguedadSF_u24m)
#parms <- ctree_control(mincriterion = 0.99,
                       #minbucket = 0.05*nrow(dt_train))
#ctree4 <- ctree(formula = formula1,
                #controls = parms,
                #data = dt_train)
#plot(ctree4,type = "simple") 

# Categorizar las variables en la BBDD.

# ========================================
# Calcular del WoE e Information Value
# ========================================
dt_train[,list(N = .N, 
               BR = sum(Default)/.N),
         by = EDAD]

dt_train[,list(N = .N, 
               Bads = sum(Default)),
         by = EDAD]

f_IV <- function(dt_datos,var_default, var_analisis){
  dt_temp <- dt_datos[,list(N=.N,
                            Bads = sum(get(var_default)),
                            Goods = .N - sum(get(var_default))),
                      by = var_analisis][order(get(var_analisis))]
  dt_temp[,Bads_pct:= Bads/sum(dt_temp$Bads)]
  dt_temp[,Goods_pct:= Goods/sum(dt_temp$Goods)]
  dt_temp[,WoE:= log(Goods_pct/Bads_pct)]
  dt_temp[,PartialIV:= WoE*(Goods_pct-Bads_pct)]
  IV <- sum(dt_temp$PartialIV)
  return(IV)
}      

f_IV(dt_datos = dt_train,
     var_default = "Default",
     var_analisis = "AntiguedadSF_u24m_cat")

dt_IV <- data.table()
for (i in c(paste0(v_variables_cuantitativas,"_cat"),v_variables_cualitativas)){
  dt_temp <- data.table(Variable = i ,
                        IV = f_IV(dt_datos = dt_train,
                                  var_default = "Default",
                                  var_analisis = i))
  dt_IV <- rbind(dt_IV,dt_temp)
}
