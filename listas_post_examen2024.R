setwd("/home/FedericoYU/Documentos/Chamba/Olimpiada/Estatal 2024/estatal2024_Rproject/")

library(dplyr)
library(readr)
library(stringr)
library(tools)

source("funciones_adhoc.R")

#---------- Agregar los registros tardios independientes y de escuela 

porescuela_extra<-
  read.csv("../listas_crudas/extras_tarde/extra_escuelas.csv",encoding = "UTF-8",skip=2)

independientes_extra<-
  read.csv("../listas_crudas/extras_tarde/Independientes_extras.csv",encoding = "UTF-8")%>%
  mutate(Sede=trimba(sede))
independientes_extra$Escuela<-"Participante Independiente"
independientes_extra$Clave_escuela <- NA
independientes_extra$Correo_escuela <- NA

aux <- intersect(names(porescuela_extra),names(independientes_extra))
lista_registros_tardios <- 
  rbind(select(porescuela_extra,all_of(aux)),select(independientes_extra,all_of(aux)))
lista_registros_tardios$clave <- paste("7381",substr(lista_registros_tardios$CURP,5,10),sep="")

aux <- intersect(names(lista_general_registro),names(lista_registros_tardios))

lista_general_registro <- read.csv("../listas_generadas/lista_general_registro.csv")

lista_general_posterior <- 
  rbind(select(lista_general_registro,all_of(aux)),select(lista_registros_tardios,
        all_of(aux)))


#---------- Limpiar listas de respuestas por sede 

respuestas_sedes<-list()  

vars <- c("Identificacion","Aciertos")

for (i in c(1:10,12)){
  temp <- paste("Resp_",as.character(i),sep="")
  vars<-c(vars,temp)
}

for (sede in sedes){
  path <- paste("../listas_crudas/respuestas_examen/",
                sede,"2024.csv", sep="")
  print(c("leyendo",sede))
  respuestas_sedes[[sede]]<-read.csv(path)%>%
    select(all_of(vars))
  respuestas_sedes[[sede]]<-unique(respuestas_sedes[[sede]])
  respuestas_sedes[[sede]]$sede<-sede
}

dn1<-(read.csv("../listas_crudas/respuestas_examen/denuez1.csv"))%>%select(all_of(vars))
dn2<-(read.csv("../listas_crudas/respuestas_examen/denuez1.csv"))%>%select(all_of(vars))

denuez<-rbind(dn1,dn2)
denuez$sede<-"sin datos"


#--------------------------------------------Agregar la puntuación a la 
# lista de respuestas y exportar las listas con puntos y juntarlas 

lista_respuestas <- juntar_bases(respuestas_sedes)
lista_respuestas <- rbind(lista_respuestas,denuez)

# for (sede in sedes){
#     print(c("puntuando",sede))
#     respuestas_sedes[[sede]]$Puntos<-NA
#     for (i in 1:nrow(respuestas_sedes[[sede]])){
#         p_1 <- 3*length(which(respuestas_sedes[[sede]]
#               [i,c("Resp_1","Resp_2","Resp_3","Resp_4")]==c("C","B","B","C")))
#         p_2 <- 4*length(which(respuestas_sedes[[sede]]
#               [i,c("Resp_5","Resp_6","Resp_7","Resp_8","Resp_9")]==c("C","D","B","C","E")))
#         p_3 <- 5*length(which(respuestas_sedes[[sede]]
#                               [i,c("Resp_10","Resp_12")]==c("D","D")))
#         respuestas_sedes[[sede]]$Puntos[i] <- p_1 + p_2 + p_3 + 5
#     }   
#  
#     print(c("exportando respuestas",sede))
#     path <- paste("../listas_generadas/respuestas_depuradas/",sede,
#                   "_resp2024.csv",sep="")
#     write.csv(respuestas_sedes[[sede]],path,row.names = FALSE)
# }  

     for (i in 1:nrow(lista_respuestas)){
         p_1 <- 3*length(which(lista_respuestas
               [i,c("Resp_1","Resp_2","Resp_3","Resp_4")]==c("C","B","B","C")))
         p_2 <- 4*length(which(lista_respuestas
               [i,c("Resp_5","Resp_6","Resp_7","Resp_8","Resp_9")]==c("C","D","B","C","E")))
         p_3 <- 5*length(which(lista_respuestas
                               [i,c("Resp_10","Resp_12")]==c("D","D")))
         lista_respuestas$Puntos[i] <- p_1 + p_2 + p_3 + 5
     }   




#------------- Crear lista para revisar examanes con claves no registradas

x<-which(!lista_respuestas$Identificacion%in%lista_general_posterior$clave)

claves_revisar <- lista_respuestas[x, c("Identificacion","Aciertos","Puntos","sede")]

#--------------la lista completa y la lista depurada

x<-which(names(lista_respuestas)=="Identificacion")
names(lista_respuestas)[x]<-"clave"

lista_completa <- merge(lista_general_posterior,lista_respuestas,by ="clave",all.y=TRUE)

lista_depurada <- filter(lista_completa,!is.na(Nombre))


#-------------- varias listas para entregar: comite de examen, constancias de participacion

lista_secundaria<-filter(lista_completa,Grado.de.estudios<4)
lista_preparatoria<-filter(lista_completa,Grado.de.estudios>3)

comite_examen_secundaria <- select(lista_secundaria,clave,Aciertos,Puntos)%>%
  arrange(desc(Puntos))
comite_examen_preparatoria <- select(lista_preparatoria,clave,Aciertos,Puntos)%>%
  arrange(desc(Puntos))

write.csv(comite_examen_secundaria,"../listas_generadas/comite_examen_sec.csv")
write.csv(comite_examen_preparatoria,"../listas_generadas/comite_examen_prepa.csv")

rm(aux,i,p_1,p_2,p_3,sede,temp,vars,x)