setwd("/home/FedericoYU/Documentos/Chamba/Olimpiada/Estatal 2023/estatal2023_Rproject/")

library(dplyr)
library(stringr)

source("funciones_adhoc.R")

independientes<-
    read.csv("../listas_crudas/RegistrosIndependientes2023_final.csv",encoding = "UTF-8")%>%
    mutate(Sede=trimba(Sede))
print(unique(independientes$Sede))
independientes$Escuela<-"Participante Independiente"
independientes$Clave_escuela <- NA

sedes_indy<-sort(unique(independientes$Sede))

indy_por_sede<-list()   #lista para bases de datos de independientes por sede, completos
escuela_por_sede<-list() #lista para bases de datos de equipos de escuela por sede, completos
basica_sede <- list()  #lista para bases de datos por sede con variables basicas

archivos<-dir("../listas_crudas/sedes_escuelas/")

#Se generan las tres listas arriba mencionadas, en primera version
#NOTA> para versiones posteriores hacer una clave que solo dependa de la
#variable no del orden.

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
      basica_sede[[temp]]$orden<-1:nrow(basica_sede[[temp]])
      for (j in 1:9){
          basica_sede[[temp]]$orden[j]<-430+17*basica_sede[[temp]]$orden[j]
      }
      for (j in 10:min(99,nrow(basica_sede[[i]]))){
        basica_sede[[temp]]$orden[j]<-500+13*basica_sede[[temp]]$orden[j]
      }
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
                 substring(basica_sede[[temp]]$CURP,5,10),sep="")
      aux2<-substring(as.character(basica_sede[[temp]]$orden),2,3)
      aux<-paste(aux,aux2,sep="")
      basica_sede[[temp]]$clave<-aux
      basica_sede[[temp]]$sede<-temp
      basica_sede[[temp]]<-select(basica_sede[[i]],-orden)
    }
}

lista_general_registro <- juntar_bases(basica_sede)

sedes<-names(basica_sede)

# hacer las listas de asistencia desde las listas basicas

asistencia<-list()

for (i in sedes){
  if (length ( names ( basica_sede [[i]] ) ) > 1) {
    basica_sede[[i]] <- arrange(basica_sede[[i]],Nombre)
    asistencia[[i]] <-select ( basica_sede[[i]], Nombre, Primer_apellido, Segundo_apellido,
                               Escuela, clave,sede)
    orden<-1:nrow(asistencia[[i]])
    asistencia[[i]]<-cbind(orden,asistencia[[i]])
  }
}

# #-------capturar correos en general ---------

correos<- ""
for (i in sedes){
  correos<-c(correos,unique(basica_sede[[i]]$Correo),
             unique(basica_sede[[i]]$Correo_escuela))
}
correos<-unique(correos)
x <- grep("@",correos)
correos<-correos[x]

write.csv(correos,"../listas_generadas/correos.csv")

#--------------------------------------------

#exportar algunas listas  de cada sede a un csv

for (sede in sedes){
  if ("Nombre"%in%names(basica_sede[[sede]])){
    print(sede)
  }
  path1 <- paste("../listas_generadas/lista_basica_",sede,".csv",sep="")
  path2 <- paste("../listas_generadas/lista_asistencia_",sede,".csv",sep="")

  print(dim(basica_sede[[sede]]))
  write.csv(basica_sede[[sede]],file = path1,row.names = FALSE)
  write.csv(asistencia[[sede]],file = path2,row.names = FALSE)
}

#-----Incorporar a los morosos---------------

tardios<-read.csv("../listas_crudas/RegistrosTardios2023.csv")
x<-which(names(tardios)%in%names(basica_sede[[1]]))
tardios<-tardios[ ,x]
tardios$clave<-paste("3758",as.character(substr(tardios$CURP,5,10)),
                     sep="")
write.csv(select(tardios,-c(Escuela,CURP,Correo,Correo_escuela)),
            "../listas_generadas/agregar_registros.csv",
            row.names = FALSE)

#----------------------------------


rm(i,j,path,x,temp,checksede,aux,aux2,archivos,sede,orden,path1,path2)