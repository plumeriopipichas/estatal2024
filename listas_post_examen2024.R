setwd("/home/FedericoYU/Documentos/Chamba/Olimpiada/Estatal 2024/estatal2024_Rproject/")

library(dplyr)
library(readr)
library(stringr)
library(tools)

source("funciones_adhoc.R")

#---------- Limpiar listas de respuestas por sede 

respuestas_sedes<-list()  

vars <- c("Identificacion","Aciertos")

for (i in c(1:10,12)){
  temp <- paste("Resp_",as.character(i),sep="")
  vars<-c(vars,temp)
}

for (sede in sedes[c(-5,-6,-12,-15,-17)]){
  path <- paste("../listas_crudas/respuestas_examen/",
                sede,"2024.csv", sep="")
  print(c("leyendo",sede))
  respuestas_sedes[[sede]]<-read.csv(path)%>%
    select(all_of(vars))
  respuestas_sedes[[sede]]<-unique(respuestas_sedes[[sede]])
}

#--------------------------------------------Agregar la puntuación a la 
# lista de respuestas y exportar las listas con puntos y juntarlas 


for (sede in sedes[c(-5,-6,-12,-15,-17)]){
    print(c("puntuando",sede))
    respuestas_sedes[[sede]]$Puntos<-NA
    for (i in 1:nrow(respuestas_sedes[[sede]])){
        p_1 <- 3*length(which(respuestas_sedes[[sede]]
              [i,c("Resp_1","Resp_2","Resp_3","Resp_4")]==c("C","B","B","C")))
        p_2 <- 4*length(which(respuestas_sedes[[sede]]
              [i,c("Resp_5","Resp_6","Resp_7","Resp_8","Resp_9")]==c("C","D","B","C","E")))
        p_3 <- 3*length(which(respuestas_sedes[[sede]]
                              [i,c("Resp_10","Resp_12")]==c("D","D")))
        respuestas_sedes[[sede]]$Puntos[i] <- p_1 + p_2 + p_3 + 5
    }   
 
    print(c("exportando respuestas",sede))
    path <- paste("../listas_generadas/respuestas_depuradas/",sede,
                  "_resp2024.csv",sep="")
    write.csv(respuestas_sedes[[sede]],path,row.names = FALSE)
}  

lista_respuestas <- juntar_bases(respuestas_sedes) 



rm(i,p_1,p_2,p_3sede,temp,vars)