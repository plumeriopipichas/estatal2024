setwd("/home/FedericoYU/Documentos/Chamba/Olimpiada/Estatal 2023/estatal2023_Rproject/")

library(dplyr)
library(readr)
library(stringr)
library(tools)

source("funciones_adhoc.R")

###-------Hacer la lista general de participacion
basica_sedes_oficial<-list()
path<-"../listas_generadas/participacion_oficial/basica/"
m<-nchar(dir(path))
sedes <- substr(dir(path),14,m-4)
for (sede in sedes){
  x<-grep(sede,dir(path))
  basica_sedes_oficial[[sede]]<-read.csv(
    paste(path,dir(path)[x],sep=""))%>%
    select(-sede)
}

lista_general_participacion<-juntar_bases(basica_sedes_oficial)
lista_general_participacion$Escuela<-
  subte(lista_general_participacion$Escuela)

###---------- Cotejar las claves con la lista de asistencia de donde
### se hicieron las portadas. 

# asistencia_oficial<-list()
# path<-"../listas_generadas/participacion_oficial/asistencia/"
# m<-nchar(dir(path))
# sedes <- substr(dir(path),18,m-4)
# for (sede in sedes){
#   x<-grep(sede,dir(path))
#   asistencia_oficial[[sede]]<-read.csv(
#     paste(path,dir(path)[x],sep=""))
#   print(dim(asistencia_oficial[[sede]]))
# }
# 
# asistencia<-juntar_bases(asistencia_oficial)
# 
# distintos <- character()
# repetidas <- character()
# ausentes <- character()
# 
# for (clave in asistencia$clave[1:5]){
#   x <- which(lista_general_participacion$clave==clave)
#   y <- which(asistencia$clave==clave)
#   if (!asistencia$Nombre[x]==lista_general_participacion$Nombre[x]){
#     distintos<-c(distintos,clave)
#   }
#   if (length(x)==0){
#     ausentes <- c(ausentes,clave)
#   }
#   if (length(x)>1){
#     repetidas<-c(repetidas,clave)
#   }
# }


#-------------- agregar los registros extras y los que tienen cambios en 
# la clave

lista_general_participacion <-rbind(lista_general_participacion,
          read.csv("../listas_crudas/modificados_y_extras.csv"))

#-------------un poco de limpieza a la lista general ----------

lista_general_participacion$Primer_apellido<-
  limpiar(lista_general_participacion$Primer_apellido)
lista_general_participacion$Segundo_apellido<-
  limpiar(lista_general_participacion$Segundo_apellido)
lista_general_participacion$Nombre<-
  limpiar(lista_general_participacion$Nombre)

#---------- para las constancias de entrenadores ----------

# entrenadores_sedes <-list()
# path<-"../listas_crudas/entrenadores"
# m<-nchar(dir(path))
# sedes <- substr(dir(path),14,m-4)
# for (sede in sedes){
#   x<-grep(sede,dir(path))
#   entrenadores_sedes[[sede]]<-read.csv(
#     paste(path,"/",dir(path)[x],sep=""))
#   entrenadores_sedes[[sede]]$sede<-sede
#   print(c("entrenadores",sede))
#   print(dim(entrenadores_sedes[[sede]]))
# }
# 
# lista_entrenadores<-juntar_bases(entrenadores_sedes)
# lista_entrenadores$Institucion<-subte(lista_entrenadores$Institucion)
# lista_entrenadores$Nombre<-notitle(lista_entrenadores$Nombre)
# lista_entrenadores$Nombre<-limpiar(lista_entrenadores$Nombre)
# lista_entrenadores<-unique(lista_entrenadores)
# 
# write.csv(lista_entrenadores,"../listas_generadas/lista_entrenadores.csv",row.names = FALSE)
#rm(entrenadores_sedes)

#----------Depurar listas con las respuestas

respuestas_sedes<-list()  #donde se guardaran las respuestas por sede, 
#depuradas, sin mas datos
sin_registro<-list() #donde se guardaran las claves que no aparecen 
#en la lista del registro

vars <- c("Identificacion","Aciertos")
nr <- 12

for (i in 1:12){
  temp <- paste("Resp_",as.character(i),sep="")
  vars<-c(vars,temp)
}

for (sede in sedes){
  path <- paste("../listas_crudas/respuestas_examen/",
                sede,"2023.csv", sep="")
  respuestas_sedes[[sede]]<-read.csv(path)%>%
    select(all_of(vars))%>%
    unique()
  path <- paste("../listas_generadas/respuestas_depuradas/",sede,
    "_resp2023.csv",sep="")
  write.csv(respuestas_sedes[[sede]],path,row.names = FALSE)
  x<-which(respuestas_sedes[[sede]]$Identificacion%in%
             lista_general_participacion$clave)
  if (length(x)<nrow(respuestas_sedes[[sede]])){
    sin_registro[[sede]]<-respuestas_sedes[[sede]][-x, ]
    sin_registro[[sede]]$Sede<-sede
  }
}


sin_registro_todos <- juntar_bases(sin_registro)
respuestas_examen <- juntar_bases(respuestas_sedes)
posterior<-read.csv("../listas_crudas/respuestas_examen/posterior.csv")%>%
  select(all_of(vars))


respuestas_examen <- rbind(respuestas_examen,posterior)
                           

write.csv(sin_registro_todos,"../listas_generadas/
          examenes_sin_registro.csv",row.names = FALSE)

#-----------Juntar las puntuaciones con los datos personales y hacer
# listas asicociadas: comite examen,constancias de participacion --------


for (k in 1:nrow(respuestas_examen)){
  if (respuestas_examen$Aciertos[k]>67){
    respuestas_examen$Aciertos[k]<-respuestas_examen$Aciertos[k]-68
  }
}

x <- which(names(respuestas_examen)=="Identificacion")
names(respuestas_examen)[x] <- "clave"

lista_completa <- merge(lista_general_participacion,respuestas_examen,
                        by ="clave",all.y=TRUE)%>%
                  select(-numero_sede)
          
lista_completa$nacio_en <- as.integer(
  paste("20",substr(lista_completa$CURP,5,6),sep=""))

comite_examen <- select(lista_completa,clave,nacio_en,nivel,Nombre,
                             Aciertos)%>%
                      filter(!is.na(Nombre))%>%
                      arrange(desc(Aciertos),Nombre)
x<-which(comite_examen$nacio_en<2003)
comite_examen$nacio_en[x]<-""
x<-grep("ecun",comite_examen$nivel)
y<-grep("ille",comite_examen$nivel)
comite_examen_secundarias<-arrange(comite_examen[x, ],desc(Aciertos))
comite_examen_prepas<-arrange(comite_examen[y, ],desc(Aciertos))

para_constancias <- select(lista_completa,Nombre,Primer_apellido,Segundo_apellido,
                           Escuela,Correo,Correo_escuela)

x<-grep("@",para_constancias$Correo)
y<-grep("@",para_constancias$Correo_escuela)
para_constancias<-para_constancias[union(x,y), ]
para_constancias$Escuela<-subte(para_constancias$Escuela)
para_constancias$Nombre_completo<-paste(para_constancias$Nombre,
            para_constancias$Primer_apellido,para_constancias$Segundo_apellido)

para_constancias<-select(para_constancias,Nombre_completo,Escuela,Correo,Correo_escuela)
para_constancias$Correo<-subte_correo(para_constancias$Correo)
para_constancias$Correo_escuela<-subte_correo(para_constancias$Correo_escuela)

para_constancias<-arrange(para_constancias,Nombre_completo)

inicio <- 1100
para_constancias$folio <- inicio:(inicio+nrow(para_constancias)-1)  

# write.csv(comite_examen,"../listas_generadas/puntuaciones_primer_examen/
#           puntos_primer_examen_2023.csv",row.names=FALSE)
# write.csv(comite_examen_secundarias,"../listas_generadas/puntuaciones_primer_examen/
#           puntos_primer_examen_sec2023.csv",row.names=FALSE)
# write.csv(comite_examen_prepas,"../listas_generadas/puntuaciones_primer_examen/
#           puntos_primer_examen_bach2023.csv",row.names=FALSE)


#write.csv(para_constancias,"../listas_generadas/lista_constancias_participacion.csv",
 #         row.names = FALSE)

#---------- lista para publicar quienes pasan a segunda etapa -------

segunda_etapa_sec <- filter(lista_completa,Aciertos>7,!is.na(Nombre),
                            nivel=="Secundaria")%>%
                      select(Nombre,Primer_apellido,Segundo_apellido,Escuela,
                             nivel,Correo,Correo_escuela)

segunda_etapa_ms <- filter(lista_completa,Aciertos>8,!is.na(Nombre),
                            nivel=="Bachillerato")%>%
  select(Nombre,Primer_apellido,Segundo_apellido,Escuela,nivel,Correo,Correo_escuela)


segunda_etapa<-rbind(segunda_etapa_sec,segunda_etapa_ms)%>%
  mutate(Nombre_completo=paste(Nombre,Primer_apellido,Segundo_apellido))%>%
  select(Nombre_completo,Equipo=Escuela,nivel,Correo,Correo_escuela)

correos_segunda_etapa<-unique(c(segunda_etapa$Correo,
                                segunda_etapa$Correo_escuela))

print(c("A",length(correos_segunda_etapa),
        class(correos_segunda_etapa)))


print(c("B",dim(segunda_etapa),
        class(segunda_etapa)))

segunda_etapa<-rbind(segunda_etapa,read.csv("../listas_crudas/directos_segunda.csv"))%>%
  arrange(Nombre_completo)

print(c("C",dim(segunda_etapa),
        class(segunda_etapa)))

correos_segunda_etapa<-c(segunda_etapa$Correos,
                                segunda_etapa$Correo_escuela)

print(c("D",length(correos_segunda_etapa),
        class(correos_segunda_etapa)))

x<-grep("@",correos_segunda_etapa)
correos_segunda_etapa<-correos_segunda_etapa[x]

print(c("E",length(correos_segunda_etapa),
        class(correos_segunda_etapa)))

rm(segunda_etapa_ms,segunda_etapa_sec)

write.csv(select(segunda_etapa,-Correo),"../listas_generadas/publicar_segunda_etapa.csv",
          row.names = FALSE)


write.csv(correos_segunda_etapa,"../listas_generadas/correos_2e.csv",
          row.names = FALSE)
#----------para sacar puntuaciones por escuela

revision_escuelas <-select(lista_completa,-c(CURP,Sede,nivel,clave))%>%
  filter(!is.na(Clave_escuela))

revision_escuelas$Clave_escuela<-trimws(revision_escuelas$Clave_escuela,which = "both")
x<-which(revision_escuelas$Clave_escuela=="")
revision_escuelas<-revision_escuelas[-x, ]
revision_escuelas<-unique(revision_escuelas)
revision_escuelas<-group_by(revision_escuelas,Clave_escuela)

revisar_escuelas<-summarise(revision_escuelas,cuantos=n(),suma_puntos=sum(Aciertos),
                            varianza=var(Aciertos),)%>%
  arrange(desc(suma_puntos),varianza)

revision_escuelas<-ungroup(revision_escuelas)

#--------------------------------------------

rm(i,k,m,nr,path,sede,temp,vars,x,y)