library(shiny)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(dplyr)
library(dygraphs)
library(xts)

main_page <- tabPanel(title = "Análisis")
about_page <- tabPanel(title = "Acerca de")
 
about_page <- tabPanel(
  title = "Acerca de",
  titlePanel("Acerca de la aplicación"),
  "Creado en R Shiny y revisado por Wilmer Ll, Edson A, Alexis M. & Ruth V. Basado en: Tassan, R. L., K. S. Hagen, A. Cheng, T. K. Palmer, G. Feliciano and T. L. Bough. 1982. Mediterranean fruit fly life cycle estimations for the California eradication program. CEC/IOBC Symposium Athens November 1982. pp. 564-570.
 - ", "Asimismo: Informacion proporcionada por el Ing. Glen Quintanilla - SENASA",
  br(),
    "Abril 2022"
)



main_page <- tabPanel(
  title = "Análisis",
  titlePanel("Análisis"),
  sidebarLayout(
    sidebarPanel(
      title = "Inputs",
      fileInput("csv_input","Cargue el archivo CSV con los registros de T° de aire y suelo",accept=".csv"),
      textInput("Estación","Nombre de la Estación",value="Tongorrape - Motupe"),
      textInput("spp","Nombre de la plaga",value="Mosca de la fruta"),
      textInput("umbral","Escriba el valor de la T° umbral (°C). Por ejemplo: 9.7",value="9.7"),
      textInput("huevo_pupa","Escriba el valor para pasar de huevo/larva a pupa. Por ejemplo: 142.8", value=142.8),
      textInput("pupa_adulto","Escriba el valor límite para pasar de pupa a adulto. Por ejemplo: 325.2",value=325.2),
      textInput("Adulto","Escriba la temperatura umbral del adulto (°C). Por ejemplo: 16.6°C",value=16.6),
      dateInput("FechaInicio","Fecha de Inicio del conteo", value="2020-06-07"),
      actionButton("run_button","Correr análisis",icon=icon("play"))
    ),
  
   mainPanel(
      tabsetPanel(
        tabPanel(
          title = "Verificación de datos",
          textOutput("verif"),
          textOutput("verif2")
          #tableOutput("table")
        ),
        tabPanel(
          title = "Tabla de Resultados",
          tableOutput("resultado"),
          textOutput("salidas")
        ),
        tabPanel(
          title = "Gráficos Regulares",
          plotOutput("regular"),
          plotOutput("regular2")
        ),
          tabPanel(
            title = "Gráfico Interactivo",
            dygraphOutput("inter")
        )
        
      )
    )
  )
)

ui <- navbarPage(
  title = "Simulación del Ciclo Biológico de plagas",
  img(src="https://i.ibb.co/5hLCRtS/Mosca-de-la-fruta.png|.png",  width='70%', height=70),
  main_page, 
  about_page
  )

server <- function(input, output){
  
  
 
  filedata <- reactive({
    infile <- input$csv_input
    if (is.null(infile)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    read.csv(infile$datapath, sep=";")
  })
  
  observeEvent(input$run_button,{
    if(is.null(filedata())){
      renderText({"Por favor, anexe un archivo CSV con los registros de T° de aire y suelo"})->output$verif
    }else{
    
      spp<-input$spp
      umbral<-as.numeric(input$umbral)
      huevo_pupa<-as.numeric(input$huevo_pupa)
      pupa_adulto<-as.numeric(input$pupa_adulto)
      Adulto<-as.numeric(input$Adulto)
      FechaInicio<-input$FechaInicio
      Estación<-input$Estación
      
    #Cargar archivos de datos: primera columna fecha, segunda y tercera temperaturas de aire
    #cuarta y quinta con temperaturas de suelo.
    MF <- tibble(filedata())%>%
      mutate(FECHA= dmy(FECHA))
    
    MF[which(MF$FECHA >= FechaInicio),]->MF
    
    
    
    if(ncol(MF)==5){
      if(is.Date(MF$FECHA)){
        renderText({"La tabla anexa presenta datos completos"})->output$verif 
        } else {
          renderText({"Tenemos datos incompletos, por favor revise la tabla"})->output$verif
        }  
      }else{
        renderText({"La tabla no presenta los datos completos. La tabla input debe contener 5 columnas: Fecha, Temperatura mínima y máxima del aire, Temperatura mínima y máxima del suelo"})->output$verif
      }
    

   if(FechaInicio>=min(as.Date(MF$FECHA))){
    if(FechaInicio<max(MF$FECHA)){
renderText({"La fecha de inicio se encuentra dentro de los valores de la tabla, podemos continuar"})->output$verif2
      } else {
       renderText({"La fecha de inicio no se encuentra dentro de los valores de la tabla"})->output$verif2
     }
   }else {      renderText({"La fecha de inicio no se encuentra dentro de los valores de la tabla"})->output$verif2
   }
      

    
    
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
    MF_final2 <- MF_final %>% 
      mutate(FECHA = as.character(FECHA))
    
    renderTable({MF_final2})->output$resultado
    
    #Gráfico convencional
    
    p <- ggplot(MF_final, aes(x = FECHA, y = Acumulado)) + 
      geom_point() +
      ggtitle(label = paste("Simulación de fenología de", spp),sub=paste("Fecha de Inicio:",FechaInicio)) +
      ylab(label = "Grados Día Acumulados")+
      xlab(label = "")+
      theme(plot.title = element_text(hjust = 0.5)) +
      scale_y_continuous(breaks = seq(0, 700, by = 100)) +
      theme(axis.text.x=element_text(angle=30, vjust = 0.5,hjust=1))+
      scale_x_date(date_labels = "%h-%m",
                   date_breaks = "10 days")
    
    renderPlot({p},res=70)->output$regular
    
    ### Perfil de generaciones
    col_stade=c(Huevo_Larva="yellow", Pupa="brown", `Adulto`="orange")
    
    renderPlot({ plot(MF_final$FECHA,MF_final$Generación, type="p",xlab="Dia del año",ylab="Número de generaciones", col=col_stade[MF_final$Estadio],lwd=3,pch=1.5)
      legend("topleft", cex = 0.98, bty = "n", col= col_stade, legend=names(col_stade),pch=15)
      title(main=paste("Simulación del Ciclo de", spp),sub=paste("Fecha de inicio:", FechaInicio))},res=70)->output$regular2
    
    
    ### Gráfico en Dygraphs
    
    
    Grado_día <- xts(x =MF_final$Acumulado, order.by = MF_final$FECHA)
    Generación <- xts(x =MF_final$Generación, order.by = MF_final$FECHA)
    Est <- xts(x =as.numeric(as.factor(MF_final$Estadio)), order.by = MF_final$FECHA)
    dun <- cbind(Grado_día,Generación,Est)
    
    p2 <- dygraph(dun, main=paste("Simulación del Ciclo de",spp, "<br> <small> Fecha de Inicio:", FechaInicio,"</small>")) %>%
      dyOptions(labelsUTC = TRUE, fillGraph=TRUE, fillAlpha=0.1, drawGrid = FALSE) %>%
      dyRangeSelector() %>%
      dyLimit(huevo_pupa,"Transición Huevo-Pupa",
              strokePattern = "solid", color = "purple") %>%
      dyCrosshair(direction = "vertical") %>%
      dyAxis("y", valueRange = c(0, max(MF_final$Acumulado)+100)) %>% 
      dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
      dyRoller(rollPeriod = 1)
    
    
    renderDygraph({p2})->output$inter
    }
    
  })
  
  
  
  
  
  
  
}

shinyApp(ui = ui, server = server)
