###################################################################
###Modelo de prediccion de los estados de desarrollo de plagas#####
###SPA - SENAMHI
###Abril - 2022
###################################################################
getwd()
setwd("C:/Users/usuario/Desktop/MOSCA_FRUTA_ORIGINAL")

#Especificar parámetros de la plaga

spp<-"Mosca de la fruta"
umbral<-9.7 #T umbral minima de desarrollo
huevo_pupa<-142.8 #Constante termica(°D)
pupa_adulto<-325.2 #Constante termica(°D)
FechaInicio<-"2006-07-06" #Fecha de inicio del cálculo
Estación<-"Tongorrape"
Adulto<-16.6

#Cargar librerías
library(tidyverse)
library(lubridate)
library(ggplot2)

#Cargar archivo de datos: primera columna fecha, segunda y tercera con temperatura de aire
#cuarta y quinta con temperatura de suelo.
MF <- tibble(read.csv("Datos/Data diaria_Tongorrape.csv", sep = ";"))%>%
  mutate(FECHA= dmy(FECHA))

MF[which(MF$FECHA >= FechaInicio),]->MF


#Calculando promedio de temperaturas de aire
MdA <- (MF[,2]+MF[,3])/2
colnames(MdA)<-"Promedio_Aire"


#Calculando promedio de temperaturas de suelo
MdS <- (MF[,4]+MF[,5])/2
colnames(MdS)<-"Promedio_Suelo"

#Descontando temperatura umbral
DA <- (MdA - umbral)
colnames(DA)<-"Grado_Dia_Aire"
DS <- (MdS - umbral)
colnames(DS)<-"Grado_Dia_Suelo"

#Juntando todas las columnas
MF <- cbind(MF, MdA, DA, MdS, DS)

#Calculando valores acumulados
SumA <- cumsum (MF$Grado_Dia_Aire)
SumS <- cumsum (MF$Grado_Dia_Suelo)

#Integrando estos datos en una nueva tabla
MF <- data.frame(MF,Acumulado_Aire=SumA,Acumulado_Suelo=SumS)

#Agregando columna para resultados finales
MF$Origen_Temp<-NA
MF$T_Max<-NA
MF$T_Min<-NA
MF$T_Promedio<-NA
MF$Grado_Dia<-NA
MF$T_Umbral<-umbral
MF$Acumulado <- NA
MF$Estadio <- NA
MF$Generación<-0
MF$Origen_Temp<-NA
MF$Estación<-Estación
MF$Control<-NA

for (i in 1:nrow(MF)){
  if(is.na(MF$Acumulado[i])){
    if(i==1){
      MF$Grado_Dia_Aire[i]->  MF$Acumulado[i]
      MF$Estadio[i]<-"Huevo_Larva"
      MF$Origen_Temp[i]<-"Aire"
      MF$T_Max[i]<- MF$T_Max_Aire[i]
      MF$T_Min[i]<-MF$T_Min_Aire[i]
      MF$T_Promedio[i]<-MF$Promedio_Aire[i]
      MF$Grado_Dia[i]<-MF$Grado_Dia_Aire[i]
    } else {
      if(MF$Acumulado[i-1]<huevo_pupa){
        MF$Acumulado[i-1]+MF$Grado_Dia_Aire[i]-> MF$Acumulado[i]
        MF$Estadio[i]<-"Huevo_Larva"
        MF$Origen_Temp[i]<-"Aire"
        MF$T_Max[i]<- MF$T_Max_Aire[i]
        MF$T_Min[i]<-MF$T_Min_Aire[i]
        MF$T_Promedio[i]<-MF$Promedio_Aire[i]
        MF$Grado_Dia[i]<-MF$Grado_Dia_Aire[i]
      } else if (MF$Acumulado[i-1]<pupa_adulto){
        MF$Acumulado[i-1]+MF$Grado_Dia_Suelo[i]-> MF$Acumulado[i]
        MF$Estadio[i]<-"Pupa"
        MF$Origen_Temp[i]<-"Suelo"
        MF$T_Max[i]<- MF$T_Max_Suelo[i]
        MF$T_Min[i]<-MF$T_Min_Suelo[i]
        MF$T_Promedio[i]<-MF$Promedio_Suelo[i]
        MF$Grado_Dia[i]<-MF$Grado_Dia_Suelo[i]
        
        if(i>30){
          mean(MF$Promedio_Aire[(i-29):i])->tiempoV
          if(mean(MF$Promedio_Aire[(i-29):i])>Adulto){
            tiempo<-"verano"
            limiteProm<-0
            limiteMax<-0
          } else { 
            tiempo <- "invierno"
            limiteProm<-0
            limiteMax<-0
          }
        }
      } else {
        MF$Estadio[i:nrow(MF)]<-"Adulto"
        MF$Origen_Temp[i]<-"Aire"
        MF$T_Max[i]<- MF$T_Max_Aire[i]
        MF$T_Min[i]<-MF$T_Min_Aire[i]
        MF$T_Promedio[i]<-MF$Promedio_Aire[i]
        MF$Grado_Dia[i]<-MF$Grado_Dia_Aire[i]
        MF$Acumulado[i-1]+MF$Grado_Dia_Aire[i]-> MF$Acumulado[i]
        
        if(length(unique(MF$Estadio[(i-4):i]))==1){
          if(unique(MF$Estadio[(i-4):i])=="Adulto" & MF$Estadio[(i-5)]=="Pupa"){
            for(l in (i-4):i){
              if (MF$T_Promedio[l]>Adulto){
                limiteProm<-limiteProm+1
              }
              if (MF$T_Max[l]>Adulto){
                limiteMax<-limiteMax+1
              }
            }
            
            if(limiteProm==5 & tiempo=="verano"){
              MF$Control[(i-4):i]<-paste0("verano (",round(tiempoV,2)," °C - ",limiteProm," días >",Adulto,"), con 5 días contínuos")
              MF$Generación[(i+1):nrow(MF)]<-MF$Generación[i]+1
              0->  MF$Acumulado[i]
            } else if(limiteMax==5){
              MF$Control[(i-4):(i+5)]<-paste0("invierno (",round(tiempoV,2)," °C - ",limiteProm," días >",Adulto,"), con 5 días contínuos y 10 días acumulados")
              MF$Generación[(i+6):nrow(MF)]<-MF$Generación[i]+1
              MF$Origen_Temp[i+5]<-"Aire"
              MF$T_Max[i+5]<- MF$T_Max_Aire[i+5]
              MF$T_Min[i+5]<-MF$T_Min_Aire[i+5]
              MF$T_Promedio[i+5]<-MF$Promedio_Aire[i+5]
              MF$Grado_Dia[i+5]<-MF$Grado_Dia_Aire[i+5]
              0->  MF$Acumulado[i+5]
            }else{
              MF$Control[(i-4):(i+10)]<-paste0("invierno (",round(tiempoV,2)," °C - ",limiteProm," días >",Adulto,"), con 15 días acumulados")
              MF$Generación[(i+11):nrow(MF)]<-MF$Generación[i]+1
              MF$Origen_Temp[i+10]<-"Aire"
              MF$T_Max[i+10]<- MF$T_Max_Aire[i+10]
              MF$T_Min[i+10]<-MF$T_Min_Aire[i+10]
              MF$T_Promedio[i+10]<-MF$Promedio_Aire[i+10]
              MF$Grado_Dia[i+10]<-MF$Grado_Dia_Aire[i+10]
              0->  MF$Acumulado[i+10]
            }
          }
        }
      }
    }
  }
}


for(i in 1:nrow(MF)){
  if(MF$Estadio[i]=="Adulto"){
    MF$T_Umbral[i]<-NA
    MF$Grado_Dia[i]<-NA
    MF$Acumulado[i]<-NA
  }
}

#Presentando la Tabla

MF[,c(1,21,12:20,22)]->MF_final

#### Tabla final ############
View(MF_final)

###Resultado final

write.csv(MF_final, file = "Resultados/Data diaria_Tongorrape.csv", row.names = FALSE)

### Gráfico de Número de generaciones de la plaga

ggplot(MF_final, aes(x = FECHA, y = Acumulado)) + 
  geom_point() +
  ggtitle(label = paste("Simulación de fenología de", spp),sub=paste("Fecha de Inicio:",FechaInicio)) +
  ylab(label = "Grados Día Acumulados")+
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(breaks = seq(0, 700, by = 100)) +
  scale_x_date(date_labels = "%h-%m",
               date_breaks = "10 days")

### Perfil de generaciones de la plaga
col_stade=c(Huevo_Larva="yellow", Pupa="brown", `Adulto`="orange")

plot(MF_final$FECHA,MF_final$Generación, type="p",xlab="Dia del año",ylab="Número de generaciones", col=col_stade[MF_final$Estadio],lwd=3,pch=1.9)
legend("topleft", cex = 0.98, bty = "n", col= col_stade, legend=names(col_stade),pch=15)
title(main=paste("Simulación del Ciclo de", spp),sub=paste("Fecha de inicio:", FechaInicio))

### Gráfico interactivo en Dygraphs

library(dygraphs)
library(xts)

Grado_día <- xts(x =MF_final$Acumulado, order.by = MF_final$FECHA)
Generación <- xts(x =MF_final$Generación, order.by = MF_final$FECHA)
Est <- xts(x =as.numeric(as.factor(MF_final$Estadio)), order.by = MF_final$FECHA)
dun <- cbind(Grado_día,Generación,Est)

p <- dygraph(dun, main=paste("Simulación del Ciclo de",spp, "<br> <small> Fecha de Inicio:", FechaInicio,"</small>")) %>%
  dyOptions(labelsUTC = TRUE, fillGraph=TRUE, fillAlpha=0.1, drawGrid = FALSE) %>%
  dyRangeSelector() %>%
  dyLimit(huevo_pupa,"Transición Huevo-Pupa",
          strokePattern = "solid", color = "purple") %>%
  dyCrosshair(direction = "vertical") %>%
  dyAxis("y", valueRange = c(0, max(MF_final$Acumulado)+100)) %>% 
  dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
  dyRoller(rollPeriod = 1)
p


