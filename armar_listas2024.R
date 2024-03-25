setwd("/home/FedericoYU/Documentos/Chamba/Olimpiada/Estatal 2024/estatal2024_Rproject/")

library(dplyr)
library(stringr)

source("funciones_adhoc.R")

independientes<-
   read.csv("../listas_crudas/RegistrosIndependientes2024.csv",encoding = "UTF-8")%>%
   mutate(Sede=trimba(Sede))%>%
   filter(Estatus%in%c("Completo","Acta","Enviado"))
 print("lista de sedes con registro independiente")
 print(unique(independientes$Sede))
 independientes$Escuela<-"Participante Independiente"
 independientes$Clave_escuela <- NA
 
sedes_indy<-sort(unique(independientes$Sede))

indy_por_sede<-list()    #lista para bases de datos de independientes por sede, completos
escuela_por_sede<-list() #lista para bases de datos de equipos de escuela por sede, completos
basica_sede <- list()    #lista para bases de datos por sede con variables basicas

archivos<-dir("../listas_crudas/sedes_escuelas/")

#Se generan las tres listas arriba mencionadas, en primera version

for (i in 1:length(sedes_indy)){
  temp<-sedes_indy[[i]]
  x<-which(independientes$Sede==temp)
  indy_por_sede[[temp]] <- independientes[x, ]
  checksede <- paste(sedes_indy[i],"_",sep="")
  x <- grep(checksede,archivos)
  if (length(x)>1){
    print("Error: archivos excedentes.")
    break
  }
  path <- paste("../listas_crudas/sedes_escuelas/",archivos[x],sep="")
  if (length(x)==1){
    escuela_por_sede[[temp]]<-read.csv(path,skip=2)
    x<-which(names(indy_por_sede[[i]])%in%names(escuela_por_sede[[i]]))
    aux <- indy_por_sede[[i]][x]
    x<-which(names(escuela_por_sede[[i]])%in%names(aux))
    aux2 <- escuela_por_sede[[i]][x]
    basica_sede[[temp]] <- rbind(aux,aux2)%>%
      arrange(CURP)
    basica_sede[[temp]]<-unique(basica_sede[[temp]])
  }
  else{
    escuela_por_sede[[temp]]<-data.frame()
    basica_sede[[temp]]<-data.frame()
  }
  if (i>9){
    basica_sede[[temp]]$numero_sede<-rep(i,nrow(basica_sede[[temp]]))
  }
  else{
    basica_sede[[temp]]$numero_sede<-rep(20+i,nrow(basica_sede[[temp]]))
  }
  if ("CURP"%in%names(basica_sede[[temp]])){
    basica_sede[[temp]]$CURP<-trimws(basica_sede[[temp]]$CURP)
    aux<-paste(as.character(basica_sede[[temp]]$numero_sede),
               substring(basica_sede[[temp]]$CURP,5,10),
               substring(basica_sede[[temp]]$CURP,18,18),sep="")
    aux2<-nchar(basica_sede[[temp]]$Nombre)%%10
    basica_sede[[temp]]$clave<-paste(aux,aux2,sep="")
    basica_sede[[temp]]$sede<-temp
  }
}

rm(i,path,x,temp,checksede,aux,aux2,archivos)
