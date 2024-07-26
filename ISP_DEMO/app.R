#librerias para shiny y password
library(shiny)
library(shinydashboard)
library(DT)
library(shinyjs)
library(sodium)
#librerias para manipulacion de la Data
library(tidyverse)
library(readxl)
library(echarts4r)
#library(googlesheets4) #tengo que ver como subo el archiv a un pinboard
library(dplyr)
library(highcharter)
#library(gapminder)
#library(forcats)
#library(purrr)
library(stringr)
#library(googledrive)
#library(pins)
library(plotly)
library(shinycssloaders)

# options(
#   # whenever there is one account token found, use the cached token
#   gargle_oauth_email = TRUE,
#   # specify auth tokens should be stored in a hidden directory ".secrets"
#   gargle_oauth_cache = "~/.secrets"
# )
# 
# 
# #### script para leer los archivos de los boards en el folder "Pins_Boards" en el drive de ISP_DEMO
# coneccion a la carpeta 
# board <- board_gdrive(googledrive::as_id("https://drive.google.com/drive/u/1/folders/1TVihHB5mprgsQEv3Po6nF8WhNJo-mlqU")) #coneccion al folder de "Bases de Datos ISP_DEMO"
# a <- board %>% pin_read("DB_Facturas") #leeo del board el archivo "DB_Facturas"

# "G:/My Drive/DeepForest Analytics/ISP_DEMO/ISP_DEMO/CLIENTES_ACTIVOS_POR_MES_ISP_DEMO.RData"
# ###  Se usa solo para desarrollo local
# Tickets_DB <-  readRDS(file ="G:/My Drive/DeepForest Analytics/ISP_DEMO/ISP_DEMO/Tickets_DB_ISP_DEMO.RData")
# Clientes_ISP_DEMO <-  readRDS(file ="G:/My Drive/DeepForest Analytics/ISP_DEMO/ISP_DEMO/Clientes_ISP_DEMO.RData")
# DB_Facturas <- readRDS(file ="G:/My Drive/DeepForest Analytics/ISP_DEMO/ISP_DEMO/DB_Facturas_ISP_DEMO.RData")
# CLIENTES_ACTIVOS_POR_MES <- readRDS("G:/My Drive/DeepForest Analytics/ISP_DEMO/ISP_DEMO/CLIENTES_ACTIVOS_POR_MES_ISP_DEMO.RData")
# Tickets_sin_servicio <- c("Antena Desalineada" , "Cable de Fibra Dañado", "Cable Fibra Dañado", "Cambio de Domicilio", "Poe Dañado", "Potencia Alta", "No tiene Internet")
# # ##


### BASES DE DATOS al la hora de hacer el deploy####
Clientes_ISP_DEMO <-  readRDS(file ="CLIENTES_ISP_DEMO.RData")
Clientes_ISP_DEMO <- Clientes_ISP_DEMO %>% 
  dplyr::filter(!is.na(Ciudad.Municipio))

indices <- sample(nrow(Clientes_ISP_DEMO), 8000)
# Seleccionar las filas correspondientes
Clientes_ISP_DEMO <- Clientes_ISP_DEMO[indices, ]
Clientes_a_filtrar <- Clientes_ISP_DEMO$Nombre

Tickets_DB <-  readRDS(file ="Tickets_DB_ISP_DEMO.RData")
Tickets_DB <- Tickets_DB %>% 
  dplyr::filter(!is.na(Ciudad.Municipio))
Tickets_DB <- Tickets_DB %>% 
  dplyr::filter(Nombre %in% Clientes_a_filtrar)


DB_Facturas <- readRDS(file ="DB_Facturas_ISP_DEMO.RData")
DB_Facturas <- DB_Facturas %>% 
  dplyr::filter(Nombre %in% Clientes_a_filtrar)

CLIENTES_ACTIVOS_POR_MES <- readRDS(file ="CLIENTES_ACTIVOS_POR_MES_ISP_DEMO.RData")
Tickets_sin_servicio <- c("Antena Desalineada" , "Cable de Fibra Dañado", "Cable Fibra Dañado", "Cambio de Domicilio", "Poe Dañado", "Potencia Alta", "No tiene Internet")


#### FUNCIONES ####
verde <- "forestgreen"
amarillo <- "gold"
rojo <- "firebrick"
naranja <- "orange"
azul <- "royalblue4"
celeste <- "cornflowerblue"
color_spinner <- "#0dc5c1"
Mes_en_curso <- as.Date(lubridate::floor_date(today(), unit = "month"))

grafica_mttr_QyA <- function(df_mttr_QyA){
  #ggplot(df_mttr_QyA, aes(fill=Rango_duracion_ticket, y=porcentaje, x=Inicio_Semana, label = paste0(porcentaje*100,"%"))) +
  # ggplot(df_mttr_QyA, aes(fill= factor(Rango_duracion_ticket, levels = c("Menor 24 Hrs", "Menor 48 Hrs", "Menor 72 Hrs", "Mayor 72 Hrs", NA)) , y=porcentaje, x=Inicio_Semana, label = paste0(porcentaje*100,"%"))) +
  ggplot(df_mttr_QyA, aes(fill= factor(Rango_duracion_ticket, levels = c("Mayor 72 Hrs", "Menor 72 Hrs", "Menor 48 Hrs", "Menor 24 Hrs", NA)),
                          y=porcentaje,
                          x=Inicio_Semana,
                          label = paste0(porcentaje*100,"%"))) +  
    geom_bar(stat="identity") + # position="stack", 
    geom_text(size = 3, position = position_stack(vjust = 0.5)) +
    scale_x_date(date_labels = "%b-%d", breaks = unique(df_mttr_QyA$Inicio_Semana)) +
    theme_bw(base_size = 15) +
    labs(fill = "MTTR QyA") +
    theme(#strip.text = element_text(size = 14),
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank(),
      axis.title.y = element_blank(),
      axis.title.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.text.x = element_text(angle = 90),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.background = element_blank(),
      panel.border = element_blank(),
      legend.position = "bottom") +
    scale_fill_manual(values = c("tomato", "tan2", "lightgreen", verde, rojo))
}# cierre funcion grafica MTTR

grafica_barras_averias_sobre_activo <- function(df_in){
  df_in %>% 
    ggplot() +
    aes(x = Mes, y = Cantidad, fill = name) +
    geom_bar(stat = "identity", position = "dodge") + #stat = "summary"
    scale_fill_hue(direction = 1) +
    theme_minimal() +
    theme(strip.text = element_text(size = 11),
          axis.ticks.y = element_blank(), #quito tick eje Y
          panel.grid.major.y = element_blank(), #quito grilla Y mayor
          axis.text.y = element_blank(), # quito los numeros del eje Y
          axis.text.x = element_text(angle = 45),
          panel.grid.minor.x = element_blank(), #quito grilla X menor
          panel.grid.major.x = element_blank(), #quito grilla X mayor
          panel.grid.minor.y = element_blank(),
          legend.position= "none")
}

#### ETL DATA ####
#### INSTALACIONES ####
Clientes_activos <- Clientes_ISP_DEMO %>% 
  dplyr::distinct(ID, Nombre, Fecha.Instalación, .keep_all = TRUE) %>% 
  dplyr::group_by(Estado) %>% 
  dplyr::summarise(Clientes= n(), .groups = "drop") 
Clientes_activos <- as.numeric(Clientes_activos[1,2])

Instalaciones_por_mes <- Clientes_ISP_DEMO %>% 
  dplyr::select(ID, Nombre, Fecha.Instalación, Barrio.Localidad, Mes_Instalacion) %>% 
  dplyr::distinct(ID, Nombre, Fecha.Instalación, .keep_all = TRUE) %>% 
  dplyr::group_by(Mes_Instalacion) %>%
  dplyr::summarise(Intstalaciones_Mensuales =n(), .groups = "drop")

#### TICKETS ####
Tickets_ISP_DEMO_Mes <- Tickets_DB %>%
  dplyr::group_by(Mes, Tipo1) %>% 
  dplyr::summarise(Tickets = n(), .groups = "drop")

Tickets_ISP_DEMO_Inicio_Semana <- Tickets_DB %>%
  dplyr::group_by(Inicio_Semana, Tipo1) %>% 
  dplyr::summarise(Tickets = n(), .groups = "drop")

df_mttr_pais_QyA <- Tickets_DB %>% 
  dplyr::select(X.Ticket, Inicio_Semana, Abierto, Estado, Ticket.cerrado, Duración.del.ticket,
                Barrio.Localidad, Rango_duracion_ticket) %>% 
  dplyr::filter(!is.na(Rango_duracion_ticket)) %>% 
  #dplyr::filter(Estado %in% c("Cerrado")) %>% 
  dplyr::group_by(Rango_duracion_ticket, Inicio_Semana) %>% 
  dplyr::summarise(Cantidad = n(), .groups = "drop") %>% 
  dplyr::group_by(Inicio_Semana) %>% 
  dplyr::mutate(porcentaje = round(Cantidad/sum(Cantidad), digits = 3)) %>% 
  dplyr::ungroup() %>% 
  dplyr::arrange(Inicio_Semana)

df_mttr_pais_QyA_Tipo <- Tickets_DB %>% 
  dplyr::select(X.Ticket, Inicio_Semana, Abierto, Estado, Ticket.cerrado, Duración.del.ticket, Barrio.Localidad, Rango_duracion_ticket, Tipo1) %>% 
  dplyr::filter(!is.na(Rango_duracion_ticket)) %>% 
  #dplyr::filter(Estado %in% c("Cerrado")) %>% 
  dplyr::group_by(Tipo1, Rango_duracion_ticket, Inicio_Semana) %>% 
  dplyr::summarise(Cantidad = n(), .groups = "drop") %>% 
  dplyr::group_by(Inicio_Semana, Tipo1) %>% 
  dplyr::mutate(porcentaje = round(Cantidad/sum(Cantidad), digits = 3)) %>% 
  dplyr::ungroup() %>% 
  dplyr::arrange(Inicio_Semana)

df_mttr_pais_QyA_Ticket_sin_servicio <- Tickets_DB %>% 
  dplyr::select(X.Ticket, Inicio_Semana, Abierto, Estado, Ticket.cerrado, Duración.del.ticket, Barrio.Localidad, Rango_duracion_ticket, Tipo1, Asunto) %>% 
  dplyr::filter(!is.na(Rango_duracion_ticket)) %>% 
  #dplyr::filter(Estado %in% c("Cerrado")) %>% 
  dplyr::filter(Asunto %in% Tickets_sin_servicio) %>% 
  dplyr::group_by(Rango_duracion_ticket, Inicio_Semana) %>% 
  dplyr::summarise(Cantidad = n(), .groups = "drop") %>% 
  dplyr::group_by(Inicio_Semana) %>% 
  dplyr::mutate(porcentaje = round(Cantidad/sum(Cantidad), digits = 3)) %>% 
  dplyr::ungroup() %>% 
  dplyr::arrange(Inicio_Semana)

df_mttr_pais_QyA_cerrados <- Tickets_DB %>% 
  dplyr::select(X.Ticket, Inicio_Semana, Abierto, Estado, Ticket.cerrado, Duración.del.ticket,
                Barrio.Localidad, Rango_duracion_ticket) %>% 
  dplyr::filter(!is.na(Rango_duracion_ticket)) %>% 
  dplyr::filter(Estado %in% c("Cerrado")) %>% 
  dplyr::group_by(Rango_duracion_ticket, Inicio_Semana) %>% 
  dplyr::summarise(Cantidad = n(), .groups = "drop") %>% 
  dplyr::group_by(Inicio_Semana) %>% 
  dplyr::mutate(porcentaje = round(Cantidad/sum(Cantidad), digits = 3)) %>% 
  dplyr::ungroup() %>% 
  dplyr::arrange(Inicio_Semana)

df_mttr_pais_QyA_Tipo_cerrados <- Tickets_DB %>% 
  dplyr::select(X.Ticket, Inicio_Semana, Abierto, Estado, Ticket.cerrado, Duración.del.ticket, Barrio.Localidad, Rango_duracion_ticket, Tipo1) %>% 
  dplyr::filter(!is.na(Rango_duracion_ticket)) %>% 
  dplyr::filter(Estado %in% c("Cerrado")) %>% 
  dplyr::group_by(Tipo1, Rango_duracion_ticket, Inicio_Semana) %>% 
  dplyr::summarise(Cantidad = n(), .groups = "drop") %>% 
  dplyr::group_by(Inicio_Semana, Tipo1) %>% 
  dplyr::mutate(porcentaje = round(Cantidad/sum(Cantidad), digits = 3)) %>% 
  dplyr::ungroup() %>% 
  dplyr::arrange(Inicio_Semana)

df_mttr_pais_QyA_Ticket_sin_servicio_cerrados <- Tickets_DB %>% 
  dplyr::select(X.Ticket, Inicio_Semana, Abierto, Estado, Ticket.cerrado, Duración.del.ticket, Barrio.Localidad, Rango_duracion_ticket, Tipo1, Asunto) %>% 
  dplyr::filter(!is.na(Rango_duracion_ticket)) %>% 
  dplyr::filter(Estado %in% c("Cerrado")) %>% 
  dplyr::filter(Asunto %in% Tickets_sin_servicio) %>% 
  dplyr::group_by(Rango_duracion_ticket, Inicio_Semana) %>% 
  dplyr::summarise(Cantidad = n(), .groups = "drop") %>% 
  dplyr::group_by(Inicio_Semana) %>% 
  dplyr::mutate(porcentaje = round(Cantidad/sum(Cantidad), digits = 3)) %>% 
  dplyr::ungroup() %>% 
  dplyr::arrange(Inicio_Semana)

df_mttr_pais_QyA_pendientes <- Tickets_DB %>% 
  dplyr::select(X.Ticket, Inicio_Semana, Abierto, Estado, Ticket.cerrado, Duración.del.ticket,
                Barrio.Localidad, Rango_duracion_ticket) %>% 
  dplyr::filter(!is.na(Rango_duracion_ticket)) %>% 
  dplyr::filter(Estado %in% c("Nuevo", "En Progreso")) %>% 
  dplyr::group_by(Rango_duracion_ticket, Inicio_Semana) %>% 
  dplyr::summarise(Cantidad = n(), .groups = "drop") %>% 
  dplyr::group_by(Inicio_Semana) %>% 
  dplyr::mutate(porcentaje = round(Cantidad/sum(Cantidad), digits = 3)) %>% 
  dplyr::ungroup() %>% 
  dplyr::arrange(Inicio_Semana)

df_mttr_pais_QyA_Tipo_pendientes <- Tickets_DB %>% 
  dplyr::select(X.Ticket, Inicio_Semana, Abierto, Estado, Ticket.cerrado, Duración.del.ticket, Barrio.Localidad, Rango_duracion_ticket, Tipo1) %>% 
  dplyr::filter(!is.na(Rango_duracion_ticket)) %>% 
  dplyr::filter(Estado %in% c("Nuevo", "En Progreso")) %>% 
  dplyr::group_by(Tipo1, Rango_duracion_ticket, Inicio_Semana) %>% 
  dplyr::summarise(Cantidad = n(), .groups = "drop") %>% 
  dplyr::group_by(Inicio_Semana, Tipo1) %>% 
  dplyr::mutate(porcentaje = round(Cantidad/sum(Cantidad), digits = 3)) %>% 
  dplyr::ungroup() %>% 
  dplyr::arrange(Inicio_Semana)

df_mttr_pais_QyA_Ticket_sin_servicio_pendientes <- Tickets_DB %>% 
  dplyr::select(X.Ticket, Inicio_Semana, Abierto, Estado, Ticket.cerrado, Duración.del.ticket, Barrio.Localidad, Rango_duracion_ticket, Tipo1, Asunto) %>% 
  dplyr::filter(!is.na(Rango_duracion_ticket)) %>% 
  dplyr::filter(Estado %in% c("Nuevo", "En Progreso")) %>% 
  dplyr::filter(Asunto %in% Tickets_sin_servicio) %>% 
  dplyr::group_by(Rango_duracion_ticket, Inicio_Semana) %>% 
  dplyr::summarise(Cantidad = n(), .groups = "drop") %>% 
  dplyr::group_by(Inicio_Semana) %>% 
  dplyr::mutate(porcentaje = round(Cantidad/sum(Cantidad), digits = 3)) %>% 
  dplyr::ungroup() %>% 
  dplyr::arrange(Inicio_Semana)


## Tickets Sin Servicio por municipio semanal
df_tickets_sin_servicio_municipio_semanal <- Tickets_DB %>% 
  dplyr::distinct(X.Ticket, Nombre, Abierto, .keep_all = TRUE) %>%
  dplyr::select(Asunto, Estado, Ciudad.Municipio, Inicio_Semana) %>% 
  dplyr::filter(Inicio_Semana >= "2024-01-01") %>%
  dplyr::filter(Asunto %in% Tickets_sin_servicio) %>% 
  dplyr::group_by(Inicio_Semana, Ciudad.Municipio) %>% 
  dplyr::summarise(Cnt_Tickets = n(), .groups = "drop")

## Tickets Sin Servicio por municipio Mensual
df_tickets_sin_servicio_municipio_mensual <- Tickets_DB %>% 
  dplyr::distinct(X.Ticket, Nombre, Abierto, .keep_all = TRUE) %>%
  dplyr::select(Asunto, Estado, Ciudad.Municipio, Mes) %>% 
  dplyr::filter(Mes >= "2024-01-01") %>%
  dplyr::filter(Asunto %in% Tickets_sin_servicio) %>% 
  dplyr::group_by(Mes, Ciudad.Municipio) %>% 
  dplyr::summarise(Cnt_Tickets = n(), .groups = "drop")

df_tipo_tickets_sin_servicio <- Tickets_DB %>% 
  dplyr::distinct(X.Ticket, Nombre, Abierto, .keep_all = TRUE) %>% 
  dplyr::select(Asunto, Estado, Ciudad.Municipio, Mes) %>% 
  dplyr::filter(Asunto %in% Tickets_sin_servicio) %>% 
  dplyr::filter(Mes >= "2024-01-01") %>% 
  dplyr::group_by(Mes, Asunto) %>% 
  dplyr::summarise(Cnt_Tickets = n(), .groups = "drop") 

#### Dx, Rx y Netos ####
#Tickets de clientes a cancelar y recuperados
Tickets_Dx_Rx <- Tickets_DB %>% 
  dplyr::filter(Mes >= "2023-05-01") %>% 
  dplyr::filter(Asunto == "Recolección De Equipos") %>% 
  dplyr::filter(Estado == "Cerrado") %>%
  tidyr::separate(
    col = Razón.Falla ,                    # nombre de la columna a separar
    into = c("Razon1", "Razon2"),  # nombres de las columnas a crear
    sep = "-" ,                   # patron a buscar, en este caso se usa \\ por ser un caracter especial
    remove = FALSE                     # sirve para conservar la variable a separar
  ) %>% 
  dplyr::group_by(Mes, Ciudad.Municipio, Tipo_Rx_Dx) %>% 
  dplyr::summarise(Cantidad = n(), .groups = "drop")

#tabla de averias sin servicio por mes por municipio
Tickets_Averias <- Tickets_DB %>% 
  dplyr::filter(Mes >= "2023-05-01") %>% 
  #dplyr::filter(Tipo == "Averia") %>%
  dplyr::filter(Asunto %in% Tickets_sin_servicio) %>%
  dplyr::group_by(Mes, Ciudad.Municipio) %>% 
  dplyr::summarise(Cantidad = n(), .groups = "drop") %>% 
  dplyr::mutate(Tipo_Rx_Dx = "Averias") %>% 
  dplyr::select(Mes, Ciudad.Municipio, Tipo_Rx_Dx, Cantidad)

# tabla de instalaciones por mes por municipio 
Clientes_Meses <- Clientes_ISP_DEMO %>% 
  dplyr::distinct(ID, Nombre, Fecha.Instalación, .keep_all = TRUE) %>%
  dplyr::filter(Mes_Instalacion >= "2023-02-01") %>% 
  dplyr::group_by(Mes_Instalacion, Ciudad.Municipio.1) %>% 
  dplyr::summarise(Cantidad = n(), .groups = "drop") %>% 
  dplyr::mutate(Tipo_Rx_Dx = "Instalacion") %>% 
  dplyr::select(Mes_Instalacion, Ciudad.Municipio.1, Tipo_Rx_Dx, Cantidad)
names(Clientes_Meses)[which(colnames(Clientes_Meses)== "Mes_Instalacion")] <- "Mes"
names(Clientes_Meses)[which(colnames(Clientes_Meses)== "Ciudad.Municipio.1")] <- "Ciudad.Municipio"

#leo el archivo de clientes activos por mes que prepara claudia, el gsheet lo bajo como csv 
#este archivo esta el drive de ISP_DEMO -> "Base de Datos ISP_DEMO"
Clientes_final_mes <- CLIENTES_ACTIVOS_POR_MES %>% 
  tidyr::pivot_longer(!Mes, names_to = "Ciudad.Municipio", values_to = "Cantidad") %>% 
  dplyr::mutate(Tipo_Rx_Dx = "Clientes_Finales") %>% 
  dplyr::select(Mes, Ciudad.Municipio, Tipo_Rx_Dx, Cantidad)
#Clientes_final_mes$Mes <- lubridate::floor_date(Clientes_final_mes$Mes - 10, unit = "month")

Clientes_inicial_mes <- CLIENTES_ACTIVOS_POR_MES %>% 
  tidyr::pivot_longer(!Mes, names_to = "Ciudad.Municipio", values_to = "Cantidad") %>% 
  dplyr::mutate(Tipo_Rx_Dx = "Clientes_Inicial") %>% 
  dplyr::select(Mes, Ciudad.Municipio, Tipo_Rx_Dx, Cantidad)
Clientes_inicial_mes$Mes <- lubridate::floor_date(Clientes_inicial_mes$Mes + 45, unit = "month")


#junto todas las tablas
DF_averias_Dx_Rx_clientesMes <- Tickets_Averias %>% 
  dplyr::bind_rows(Clientes_Meses) %>% 
  dplyr::bind_rows(Clientes_final_mes)  %>%
  dplyr::bind_rows(Clientes_inicial_mes)  %>%
  dplyr::bind_rows(Tickets_Dx_Rx) 

##### Netos y Churn al cuadro total ----
DF_averias_Dx_Rx_clientesMes <- DF_averias_Dx_Rx_clientesMes %>% 
  tidyr::pivot_wider(names_from = Tipo_Rx_Dx, values_from = Cantidad, values_fill = 0) %>% 
  dplyr::mutate(Netos = (Instalacion+Rx)- Dx,
                Churn = ((Dx/Clientes_Inicial)*100)) %>% #definir churn si es Dx/Clientes o (Dx-Rx)/Clientes
  tidyr::pivot_longer(!c(Mes, Ciudad.Municipio), values_to = "Cantidad")

Churn_mes_anterior <- DF_averias_Dx_Rx_clientesMes #%>% 

# Data frame de Dx, Rx, Instalaciones y Netos, Cambiar el mes que se filtra
Dx_Rx_Inst_Netos <- DF_averias_Dx_Rx_clientesMes %>%
  filter(Mes >= "2023-12-22" & Mes <= "2024-09-01") %>%
  filter(!(Ciudad.Municipio %in% c("Mazatenango", 
                                   "Solola", "Quetzaltenango", "Suchitepequez", "Chimaltenango", "TOTAL"))) %>%
  filter(name %in% c("Dx","Instalacion", "Netos", "Rx"))

Dx_Rx_Inst_Netos_Total <- DF_averias_Dx_Rx_clientesMes %>%
  filter(Mes >= "2023-12-22" & Mes <= "2024-09-01") %>%
  filter(!(Ciudad.Municipio %in% c("Mazatenango", 
                                   "Solola", "Quetzaltenango", "Suchitepequez", "Chimaltenango", "TOTAL"))) %>%
  filter(name %in% c("Dx","Instalacion", "Netos", "Rx")) %>% 
  dplyr::group_by(Mes, name) %>% 
  dplyr::summarise(Cantidad = sum(Cantidad), .groups = "drop")

#### Averias Sobre Activos ####

DF_Averias_sobre_activos <- DF_averias_Dx_Rx_clientesMes %>% 
  dplyr::filter(name %in% c("Averias", "Clientes_Inicial")) %>% 
  tidyr::pivot_wider(names_from = name, values_from = Cantidad, values_fill = 0) %>% 
  dplyr::mutate(Av_Activo = round((Averias/ Clientes_Inicial)*100, 2) ) %>% 
  tidyr::pivot_longer(!c(Mes, Ciudad.Municipio), values_to = "Cantidad")

DF_Averias_sobre_activos_total <- DF_Averias_sobre_activos %>%
  dplyr::filter(!is.na(Ciudad.Municipio)) %>% #filtro municipios NA
  dplyr::filter(name == "Averias") %>% 
  dplyr::group_by(Mes) %>% 
  dplyr::summarise(Averias = sum(Cantidad), .groups = "drop") 

DF_Averias_sobre_activos_total <- dplyr::left_join(DF_Averias_sobre_activos_total,
                                                   DF_Averias_sobre_activos %>% #obtengo la columna de los clientes totales por mes
                                                     dplyr::filter(!is.na(Ciudad.Municipio)) %>% #filtro municipios NA
                                                     dplyr::filter(name == "Clientes_Inicial") %>% 
                                                     dplyr::filter(!Ciudad.Municipio %in% c("TOTAL", "Mazatenango", "Solola", #filtro municpios que no aportan por ser especiales
                                                                                            "Chimaltenango", "Quetzaltenango",
                                                                                            "Suchitepequez")) %>% 
                                                     dplyr::group_by(Mes) %>% 
                                                     dplyr::summarise(Clientes = sum(Cantidad, na.rm = TRUE), .groups = "drop"),
                                                   by = "Mes") %>% 
  dplyr::mutate(Averias_Activo = (Averias / Clientes)*100) %>% 
  dplyr::filter(Mes >= "2023-05-01")


#### Facturacion ----
##### ARPU ----
DB_Facturas <- DB_Facturas %>% 
  dplyr::mutate(Mes_facturacion = lubridate::floor_date(DB_Facturas$Fecha.Emisión, unit = "month")) %>% 
  dplyr::left_join(Clientes_ISP_DEMO %>% 
                     dplyr::select(Nombre, Latitud, Longitud),
                   by = "Nombre") %>% 
  dplyr::left_join(Clientes_ISP_DEMO %>% 
                     dplyr::select(Nombre, Plan.Precio),
                   by = "Nombre") %>% 
  dplyr::left_join(Clientes_ISP_DEMO %>% 
                     dplyr::select(Nombre, Estado),
                   by = "Nombre")
names(DB_Facturas)[which(colnames(DB_Facturas)== "Estado.x")] <- "Estado_Factura"
names(DB_Facturas)[which(colnames(DB_Facturas)== "Estado.y")] <- "Estado_Cliente"


# DB_Facturas %>% 
#   dplyr::filter(Mes_facturacion == Mes_en_curso) %>% 
#   dplyr::filter(Estado_Cliente == "Activo") %>% 
#   dplyr::pull(Plan.Precio) %>% 
#   max()
# 
# DB_Facturas %>% 
#   dplyr::filter(Mes_facturacion == Mes_en_curso) %>% 
#   dplyr::filter(Estado_Cliente == "Activo") %>% 
#   dplyr::pull(Plan.Precio) %>% 
#   min()
# 
# DB_Facturas %>% 
#   dplyr::filter(Mes_facturacion == Mes_en_curso) %>% 
#   dplyr::filter(Estado_Cliente == "Activo") %>% 
#   dplyr::pull(Plan.Precio) %>% 
#   mean()
# 
# DB_Facturas %>% 
#   dplyr::filter(Mes_facturacion == Mes_en_curso) %>% 
#   dplyr::filter(Estado_Cliente == "Activo") %>% 
#   dplyr::pull(Plan.Precio) %>% 
#   sd()

ARPU_Mes <- DB_Facturas %>% 
  dplyr::filter(Estado_Cliente == "Activo") %>% 
  dplyr::group_by(Mes_facturacion) %>% 
  dplyr::summarise(Arpu = round(mean(Plan.Precio, na.rm = TRUE), digits = 2) , .groups = "drop")

ARPU_Mes_actual <- ARPU_Mes %>% 
  dplyr::filter(Mes_facturacion == Mes_en_curso) %>% 
  dplyr::pull(Arpu) 

## Main login screen ----
loginpage <- div(id = "loginpage", style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
                 wellPanel(
                   tags$h2("LOG IN", class = "text-center", style = "padding-top: 0;color:#333; font-weight:600;"),
                   textInput("userName", placeholder="Username", label = tagList(icon("user"), "Username")),
                   passwordInput("passwd", placeholder="Password", label = tagList(icon("unlock-alt"), "Password")),
                   br(),
                   div(
                     style = "text-align: center;",
                     actionButton("login", "SIGN IN", style = "color: white; background-color:#3c8dbc;
                                 padding: 10px 15px; width: 150px; cursor: pointer;
                                 font-size: 18px; font-weight: 600;"),
                     shinyjs::hidden(
                       div(id = "nomatch",
                           tags$p("Oops! Incorrect username or password!",
                                  style = "color: red; font-weight: 600; 
                                            padding-top: 5px;font-size:16px;", 
                                  class = "text-center"))),
                     br(),
                     br(),
                     tags$code("Username: usuario1@ISP_DEMO  Password: usuario1@ISP_DEMO"),
                     br(),
                     tags$code("Username: usuario2@ISP_DEMO  Password: usuario2@ISP_DEMO"),
                     br(),
                     tags$code("Username: usuario3@ISP_DEMO  Password: usuario3@ISP_DEMO")
                   ))
)

credentials = data.frame(
  username_id = c("usuario1@ISP_DEMO", "usuario2@ISP_DEMO", "usuario3@ISP_DEMO", ".."),
  passod   = sapply(c("usuario1@ISP_DEMO", "usuario2@ISP_DEMO", "usuario3@ISP_DEMO", ".."),password_store), # le asigna un hash a cada password
  permission  = c("advanced", "advanced", "advanced", "advanced"), 
  stringsAsFactors = F
)


header <- dashboardHeader( title = "ISP_DEMO Dashboard", uiOutput("logoutbtn"))

sidebar <- dashboardSidebar(uiOutput("sidebarpanel")) 
body <- dashboardBody(shinyjs::useShinyjs(), uiOutput("body"))
ui <- dashboardPage(header, sidebar, body, skin = "blue")

# SERVER SIDE ----

server <- function(input, output, session) {
  
  #inicializando valores
  login = FALSE
  USER <- reactiveValues(login = login)
  
  ##verificacion de user vrs password y asignamos TRUE o FALSE a la variable reactiva "login"
  observe({ 
    if (USER$login == FALSE) {
      if (!is.null(input$login)) {
        if (input$login > 0) {
          Username <- isolate(input$userName) #extrae el usuario que introdujo el usuario
          Password <- isolate(input$passwd) #extrae el pasword que introdujo el usuario
          if(length(which(credentials$username_id==Username))==1) { # verifica si el usuario existe, si existe entra al if
            pasmatch  <- credentials["passod"][which(credentials$username_id==Username),] # extrae el password que le corresponde al user
            pasverify <- password_verify(pasmatch, Password) # verifica 
            if(pasverify) { #si pasverify es verdadero asigna TRUE a login
              USER$login <- TRUE
            } else {  #si no pasa la verificacion 
              shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
              shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
            }
          } else {
            shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
            shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
          }
        } 
      }
    }    
  })
  
  ##### boton de log out ####
  output$logoutbtn <- renderUI({ 
    req(USER$login)
    tags$li(a(icon("sign-out"), "Logout", 
              href="javascript:window.location.reload(true)"),
            class = "dropdown", 
            style = "background-color: #eee !important; border: 0;
                    font-weight: bold; margin:5px; padding: 10px;")
  })
  
  
  output$sidebarpanel <- renderUI({
    if (USER$login == TRUE ){ 
      sidebarMenu(
        menuItem("Comercial", tabName = "Comercial", icon = icon("dashboard")),
        menuItem("Tec Domiciliar", tabName = "Tec_Domiciliar", icon = icon("th")),
        menuItem("Dx, Rx, Insta", tabName = "Dx_Rx_Insta", icon = icon("th")),
        menuItem("Averias Sobre Activos", tabName = "Averia_Sobre_activo", icon = icon("th"))
      )
    }
  })
  
  ##### UI SIDE  ----
  # aqui estan metido todo el body layout de la app 
  output$body <- renderUI({
    if (USER$login == TRUE ) {
      tabItems(
        
        ###### UI Tab Comercial ----
        tabItem(tabName ="Comercial", class = "active", #solo un tab puede ser "class = active" este tab es el que se despliega al inicio
                fluidRow(
                  valueBox(Clientes_activos, "Clientes Activos", icon = icon("user")),
                  valueBox(paste0("Q ", ARPU_Mes_actual), "ARPU", icon = icon("user"), color = "green"),
                  valueBox(" ", "Suspendidos", icon = icon("user"), color = "orange"),
                  #),
                  #fluidRow(
                  #box(width = 12, dataTableOutput('results'))
                  box(width = 6, echarts4rOutput("Clientes_Municipio", height = 300) %>% withSpinner(color=color_spinner)),
                  box(width = 6, echarts4rOutput("Instalaciones_mes", height = 300) %>% withSpinner(color=color_spinner)),
                  box(width = 12, echarts4rOutput("Clientes_Municipio_ultimo_mes") %>% withSpinner(color=color_spinner)),
                  box(width = 12, echarts4rOutput("instalaciones_por_municipio_2023_2024") %>% withSpinner(color=color_spinner))
                  #)
                )
        ),
        
        ##### UI Tab Tec_Domiciliar ----
        tabItem(tabName = "Tec_Domiciliar",
                fluidRow(
                  box(width = 12, plotOutput("mttr_sin_servivio") %>% withSpinner(color=color_spinner)),
                  box(width = 12, echarts4rOutput("tickets_sin_servicio_municipio_semanal") %>% withSpinner(color=color_spinner)),
                  box(width = 12, echarts4rOutput("tickets_sin_servicio_municipio_mensual") %>% withSpinner(color=color_spinner)),
                  box(width = 12, echarts4rOutput("tipo_tickets_sin_Servicio") %>% withSpinner(color=color_spinner)),
                  box(width = 4, selectInput("municipio_asunto_tickets_sin_servicio", "Seleccione Municipio", choices = unique(Tickets_DB$Ciudad.Municipio))),
                  box(width = 12, echarts4rOutput("tipo_tickets_sin_servicio_municipio") %>% withSpinner(color=color_spinner)),
                  box(width = 12, echarts4rOutput("tipo_tickets_sin_servicio_barrio") %>% withSpinner(color=color_spinner))
                )
        ),
        ##### UI Tab Dx, Rx, Netos, Insta ----
        tabItem(tabName = "Dx_Rx_Insta",
                fluidRow(
                  box(width = 12, plotlyOutput("Dx_Rx_Inst_Netos_Graf_total") %>% withSpinner(color=color_spinner)),
                  box(width = 12, plotlyOutput("Dx_Rx_Inst_Netos_Graf_Region_01") %>% withSpinner(color=color_spinner)),
                  box(width = 12, plotlyOutput("Dx_Rx_Inst_Netos_Graf_Region_02") %>% withSpinner(color=color_spinner)),
                  box(width = 12, plotlyOutput("Dx_Rx_Inst_Netos_Graf_Region_03") %>% withSpinner(color=color_spinner)),
                  box(width = 12, plotlyOutput("Dx_Rx_Inst_Netos_Graf_Region_04") %>% withSpinner(color=color_spinner)),
                  box(width = 12, plotlyOutput("Dx_Rx_Inst_Netos_Graf_Region_05") %>% withSpinner(color=color_spinner)),
                  box(width = 12, plotlyOutput("Dx_Rx_Inst_Netos_Graf_Region_06") %>% withSpinner(color=color_spinner)),
                  box(width = 12, plotlyOutput("Dx_Rx_Inst_Netos_Graf_Region_07") %>% withSpinner(color=color_spinner)),
                  box(width = 12, plotlyOutput("Dx_Rx_Inst_Netos_Graf_Region_08") %>% withSpinner(color=color_spinner)),
                  box(width = 12, plotlyOutput("Dx_Rx_Inst_Netos_Graf_Region_09") %>% withSpinner(color=color_spinner)),
                  box(width = 12, plotlyOutput("Dx_Rx_Inst_Netos_Graf_Region_10") %>% withSpinner(color=color_spinner)),
                  box(width = 12, plotlyOutput("Dx_Rx_Inst_Netos_Graf_Region_14") %>% withSpinner(color=color_spinner)),
                  box(width = 12, plotlyOutput("Dx_Rx_Inst_Netos_Graf_Region_17") %>% withSpinner(color=color_spinner))
                  # box(width = 12, echarts4rOutput("x"))
                  # box(width = 12, echarts4rOutput("tickets_sin_servicio_municipio_mensual")),
                  # box(width = 12, echarts4rOutput("tipo_tickets_sin_Servicio")),
                  # box(width = 4, selectInput("municipio_asunto_tickets_sin_servicio", "Seleccione Municipio", choices = unique(Tickets_DB$Ciudad.Municipio))),
                  # box(width = 12, echarts4rOutput("tipo_tickets_sin_servicio_municipio")),
                  # box(width = 12, echarts4rOutput("tipo_tickets_sin_servicio_barrio"))
                )
        ),
        ##### UI Tab Averia_Sobre_activo ----
        tabItem(tabName = "Averia_Sobre_activo",
                fluidRow(
                  box(width = 12, plotlyOutput("Averias_sobre_activo_grafica_Total") %>% withSpinner(color=color_spinner)),
                  box(width = 12, plotlyOutput("Averias_sobre_activo_grafica_Mes_actual_municipios") %>% withSpinner(color=color_spinner)),
                  box(width = 12, plotlyOutput("Averias_sobre_activo_grafica_Region_01") %>% withSpinner(color=color_spinner)),
                  box(width = 12, plotlyOutput("Averias_sobre_activo_grafica_Region_02") %>% withSpinner(color=color_spinner)),
                  box(width = 12, plotlyOutput("Averias_sobre_activo_grafica_Region_03") %>% withSpinner(color=color_spinner)),
                  box(width = 12, plotlyOutput("Averias_sobre_activo_grafica_Region_04") %>% withSpinner(color=color_spinner)),
                  box(width = 12, plotlyOutput("Averias_sobre_activo_grafica_Region_05") %>% withSpinner(color=color_spinner)),
                  box(width = 12, plotlyOutput("Averias_sobre_activo_grafica_Region_06") %>% withSpinner(color=color_spinner)),
                  box(width = 12, plotlyOutput("Averias_sobre_activo_grafica_Region_07") %>% withSpinner(color=color_spinner)),
                  box(width = 12, plotlyOutput("Averias_sobre_activo_grafica_Region_08") %>% withSpinner(color=color_spinner)),
                  box(width = 12, plotlyOutput("Averias_sobre_activo_grafica_Region_09") %>% withSpinner(color=color_spinner)),
                  box(width = 12, plotlyOutput("Averias_sobre_activo_grafica_Region_10") %>% withSpinner(color=color_spinner)),
                  box(width = 12, plotlyOutput("Averias_sobre_activo_grafica_Region_14") %>% withSpinner(color=color_spinner)),
                  box(width = 12, plotlyOutput("Averias_sobre_activo_grafica_Region_17") %>% withSpinner(color=color_spinner))
                ))
      )
      
    }
    else {
      loginpage
    }
  })
  
  ##
  #value Box de clientes totales en ISP_DEMO
  ##
  
  # observe({   
  #   updateSelectInput(session,
  #                     "municipio_asunto_tickets_sin_servicio",
  #                     choices = unique(Tickets_DB$Ciudad.Municipio))
  # })
  
  
  # Outputs Comercial ----
  output$Clientes_Municipio <- echarts4r::renderEcharts4r({
    Clientes_ISP_DEMO %>% 
      dplyr::filter(Estado == "Activo") %>% 
      dplyr::distinct(ID, Nombre, Fecha.Instalación, .keep_all = TRUE) %>% 
      dplyr::group_by(Ciudad.Municipio.1) %>% 
      dplyr::summarise(Clientes = n(), .groups = "drop") %>% 
      echarts4r::e_chart(x = Ciudad.Municipio.1) %>% 
      echarts4r::e_bar(serie = Clientes, smooth = TRUE) %>% 
      echarts4r::e_title("Clientes Activos 2024", "Clientes por municipio") %>% # Add title & subtitle
      echarts4r::e_tooltip(trigger = "axis") %>%  # tooltip
      #echarts4r::e_legend(bottom = 0) %>% # move legend to the bottom
      echarts4r::e_labels(fontSize = 10) %>% 
      echarts4r::e_y_axis(axisLabel = list(fontSize = 10)) %>%  #coloco tamano de letra a cero para hacer desaparecer los valor del eje Y
      echarts4r::e_x_axis(axisLabel = list(rotate = 25)) %>%  # rotate
      echarts4r::e_hide_grid_lines()
  })
  
  output$Instalaciones_mes <- echarts4r::renderEcharts4r({
    Instalaciones_por_mes %>% 
      dplyr::filter(Mes_Instalacion >= "2021-09-01") %>% 
      echarts4r::e_charts(x = Mes_Instalacion) %>% 
      echarts4r::e_line(serie = Intstalaciones_Mensuales, smooth = TRUE) %>% 
      echarts4r::e_title("Instalaciones Mensuales Historico", " ") %>% # Add title & subtitle
      echarts4r::e_tooltip(trigger = "axis") %>%  # tooltip
      echarts4r::e_legend(bottom = 0) %>% # move legend to the bottom
      echarts4r::e_labels(fontSize = 10) %>% 
      echarts4r::e_y_axis(axisLabel = list(fontSize = 10)) %>%  #coloco tamano de letra a cero para hacer desaparecer los valor del eje Y
      echarts4r::e_hide_grid_lines()
  })
  
  output$Clientes_Municipio_ultimo_mes <- echarts4r::renderEcharts4r({
    Clientes_ISP_DEMO %>% 
      dplyr::filter(Estado == "Activo") %>%
      dplyr::distinct(ID, Nombre, Fecha.Instalación, .keep_all = TRUE) %>% 
      dplyr::group_by(Mes_Instalacion, Ciudad.Municipio.1) %>%
      dplyr::filter(Mes_Instalacion >= "2024-06-01") %>% #View()
      dplyr::summarise(Clientes = n(), .groups = "drop") %>% 
      dplyr::group_by(Mes_Instalacion) %>% 
      echarts4r::e_charts(x = Ciudad.Municipio.1) %>% 
      echarts4r::e_bar(serie = Clientes, smooth = TRUE) %>% 
      echarts4r::e_title("Clientes Nuevos Junio vrs Julio 2024", "Comparativo de clientes instalados nuevos por municipio Junio vrs Julio") %>% # Add title & subtitle
      echarts4r::e_tooltip(trigger = "axis") %>%  # tooltip
      echarts4r::e_legend(bottom = 0) %>% # move legend to the bottom
      echarts4r::e_labels(fontSize = 10) %>% 
      echarts4r::e_y_axis(axisLabel = list(fontSize = 10)) %>%  #coloco tamano de letra a cero para hacer desaparecer los valor del eje Y
      echarts4r::e_x_axis(axisLabel = list(rotate = 25)) %>%  # rotate
      echarts4r::e_hide_grid_lines()
  })
  
  #
  output$instalaciones_por_municipio_2023_2024 <- echarts4r::renderEcharts4r({
    Clientes_ISP_DEMO %>% 
      dplyr::filter(Mes_Instalacion >= "2023-01-01") %>%
      dplyr::distinct(ID, Nombre, Fecha.Instalación, .keep_all = TRUE) %>% 
      dplyr::select(Nombre, Fecha.Instalación, Ciudad.Municipio.1, Mes_Instalacion) %>% 
      dplyr::group_by(Mes_Instalacion, Ciudad.Municipio.1) %>%
      dplyr::summarise(Instalaciones_Mensuales =n(), .groups = "drop") %>% 
      #dplyr::filter(Instalaciones_Mensuales > 2) %>% 
      dplyr::filter(!is.na (Ciudad.Municipio.1)) %>%
      dplyr::group_by(Ciudad.Municipio.1) %>% 
      echarts4r::e_charts(x = Mes_Instalacion) %>% 
      echarts4r::e_line(serie = Instalaciones_Mensuales, smooth = TRUE) %>% 
      echarts4r::e_title("Instalaciones 2023 y 2024", "Mensual por Municipio") %>% # Add title & subtitle
      echarts4r::e_tooltip(trigger = "axis") %>%  # tooltip
      echarts4r::e_legend(bottom = 0) %>% # move legend to the bottom
      echarts4r::e_labels(fontSize = 10) %>% 
      echarts4r::e_y_axis(axisLabel = list(fontSize = 10)) %>%  #coloco tamano de letra a cero para hacer desaparecer los valor del eje Y
      echarts4r::e_hide_grid_lines()
  })
  #
  
  # Outputs Tec Domiciliar ----
  output$mttr_sin_servivio <-  renderPlot({
    df_mttr_pais_QyA_Ticket_sin_servicio %>% 
      dplyr::filter(Inicio_Semana >= "2024-01-01") %>% 
      grafica_mttr_QyA() + 
      labs(title = "MTTR Tickets Sin Servicio")
  })
  
  
  output$tickets_sin_servicio_municipio_semanal <- echarts4r::renderEcharts4r({
    df_tickets_sin_servicio_municipio_semanal %>%
      dplyr::filter(!is.na(Ciudad.Municipio)) %>% 
      dplyr::group_by(Ciudad.Municipio) %>% 
      echarts4r::e_charts(x = Inicio_Semana) %>% 
      echarts4r::e_line(serie = Cnt_Tickets, smooth = TRUE) %>% 
      echarts4r::e_title("Averias 2024", "Semanal por Municipio") %>% # Add title & subtitle
      echarts4r::e_tooltip(trigger = "axis") %>%  # tooltip
      echarts4r::e_legend(bottom = 0) %>% # move legend to the bottom
      echarts4r::e_labels(fontSize = 10) %>% 
      echarts4r::e_y_axis(axisLabel = list(fontSize = 10)) %>%  #coloco tamano de letra a cero para hacer desaparecer los valor del eje Y
      echarts4r::e_hide_grid_lines()
  })
  
  output$tickets_sin_servicio_municipio_mensual <- echarts4r::renderEcharts4r({
    df_tickets_sin_servicio_municipio_mensual %>%
      dplyr::filter(!is.na(Ciudad.Municipio)) %>% 
      dplyr::group_by(Ciudad.Municipio) %>% 
      echarts4r::e_charts(x = Mes) %>% 
      echarts4r::e_line(serie = Cnt_Tickets, smooth = TRUE) %>% 
      echarts4r::e_title("Averias 2024", "Mensual por Municipio") %>% # Add title & subtitle
      echarts4r::e_tooltip(trigger = "axis") %>%  # tooltip
      echarts4r::e_legend(bottom = 0) %>% # move legend to the bottom
      echarts4r::e_labels(fontSize = 10) %>% 
      echarts4r::e_y_axis(axisLabel = list(fontSize = 10)) %>%  #coloco tamano de letra a cero para hacer desaparecer los valor del eje Y
      echarts4r::e_hide_grid_lines()
  })
  
  output$tipo_tickets_sin_Servicio <- echarts4r::renderEcharts4r({
    df_tipo_tickets_sin_servicio %>% 
      #dplyr::filter(Ciudad.Municipio == "Retalhuleu") %>% 
      dplyr::filter(Mes >= "2024-01-01") %>% 
      dplyr::group_by(Mes) %>% 
      echarts4r::e_chart(x = Asunto) %>% 
      echarts4r::e_bar(serie = Cnt_Tickets, smooth = TRUE) %>% 
      echarts4r::e_title("Tickets Sin Servicio 2024") %>% # Add title & subtitle
      echarts4r::e_tooltip(trigger = "axis") %>%  # tooltip
      #echarts4r::e_legend(bottom = 0) %>% # move legend to the bottom
      echarts4r::e_labels(fontSize = 10) %>% 
      echarts4r::e_y_axis(axisLabel = list(fontSize = 10)) %>%  #coloco tamano de letra a cero para hacer desaparecer los valor del eje Y
      echarts4r::e_x_axis(axisLabel = list(rotate = 25)) %>%  # rotate
      echarts4r::e_hide_grid_lines()
  })
  
  output$tipo_tickets_sin_servicio_municipio <- echarts4r::renderEcharts4r({
    if (input$municipio_asunto_tickets_sin_servicio == ""){
      return()
    }
    
    Tickets_DB %>% 
      dplyr::distinct(X.Ticket, Nombre, Abierto, .keep_all = TRUE) %>%
      dplyr::select(Asunto, Estado, Ciudad.Municipio, Mes) %>% 
      dplyr::filter(Mes >= "2024-01-01") %>%
      dplyr::filter(Asunto %in% Tickets_sin_servicio) %>% 
      dplyr::group_by(Mes,Asunto, Ciudad.Municipio) %>% 
      dplyr::summarise(Cnt_Tickets = n(), .groups = "drop") %>% 
      dplyr::filter(Ciudad.Municipio == input$municipio_asunto_tickets_sin_servicio) %>% # 
      dplyr::filter(Mes >= "2024-01-01") %>% 
      dplyr::group_by(Mes) %>% 
      echarts4r::e_chart(x = Asunto) %>% 
      echarts4r::e_bar(serie = Cnt_Tickets, smooth = TRUE) %>% 
      echarts4r::e_title("Tickets Sin Servicio 2024", paste0(input$municipio_asunto_tickets_sin_servicio)) %>% # Add title & subtitle
      echarts4r::e_tooltip(trigger = "axis") %>%  # tooltip
      #echarts4r::e_legend(bottom = 0) %>% # move legend to the bottom
      echarts4r::e_labels(fontSize = 10) %>% 
      echarts4r::e_y_axis(axisLabel = list(fontSize = 10)) %>%  #coloco tamano de letra a cero para hacer desaparecer los valor del eje Y
      echarts4r::e_x_axis(axisLabel = list(rotate = 25)) %>%  # rotate
      echarts4r::e_hide_grid_lines()
  })
  
  output$tipo_tickets_sin_servicio_barrio <- echarts4r::renderEcharts4r({
    if (input$municipio_asunto_tickets_sin_servicio == ""){
      return()
    }
    
    Tickets_DB %>% 
      dplyr::distinct(X.Ticket, Nombre, Abierto, .keep_all = TRUE) %>%
      dplyr::select(Asunto, Estado, Barrio.Localidad, Mes, Ciudad.Municipio) %>% 
      dplyr::filter(Mes >= "2024-01-01") %>%
      dplyr::filter(Asunto %in% Tickets_sin_servicio) %>% 
      dplyr::group_by(Mes,Asunto, Barrio.Localidad, Ciudad.Municipio) %>% 
      dplyr::summarise(Cnt_Tickets = n(), .groups = "drop") %>% 
      dplyr::filter(Ciudad.Municipio == input$municipio_asunto_tickets_sin_servicio) %>% # 
      dplyr::filter(Mes >= "2024-01-01") %>% 
      dplyr::filter(Asunto == "Cable Fibra Dañado") %>% 
      dplyr::group_by(Mes) %>% 
      echarts4r::e_chart(x = Barrio.Localidad) %>% 
      echarts4r::e_bar(serie = Cnt_Tickets, smooth = TRUE) %>% 
      echarts4r::e_title("Tickets Sin Servicio 2024", paste0(input$municipio_asunto_tickets_sin_servicio)) %>% # Add title & subtitle
      echarts4r::e_tooltip(trigger = "axis") %>%  # tooltip
      #echarts4r::e_legend(bottom = 0) %>% # move legend to the bottom
      echarts4r::e_labels(fontSize = 10) %>% 
      echarts4r::e_y_axis(axisLabel = list(fontSize = 10)) %>%  #coloco tamano de letra a cero para hacer desaparecer los valor del eje Y
      echarts4r::e_x_axis(axisLabel = list(rotate = 25)) %>%  # rotate
      echarts4r::e_hide_grid_lines()
  })
  
  # Output Dx, Rx, Instalaciones y Netos ----
  output$Dx_Rx_Inst_Netos_Graf_total <- plotly::renderPlotly({
    d <- Dx_Rx_Inst_Netos_Total %>%
      #dplyr::filter(Ciudad.Municipio == "XX") %>% 
      dplyr::filter(Mes <= Mes_en_curso) %>% 
      #dplyr::filter(Cantidad > 0) %>% 
      ggplot() +
      aes(x = Mes, y = Cantidad, fill = name) +
      geom_bar(stat = "identity", position = "dodge") + #stat = "summary"
      geom_text(aes(label= Cantidad), position=position_dodge(width=25), vjust= -0.5, color = "black", size = 3, group = "name") +
      scale_fill_hue(direction = 1) +
      theme_minimal() +
      #facet_wrap(vars(Ciudad.Municipio), scales = "free", labeller = label_wrap_gen(10)) +
      theme(strip.text = element_text(size = 11),
            axis.ticks.y = element_blank(), #quito tick eje Y
            panel.grid.major.y = element_blank(), #quito grilla Y mayor
            axis.text.y = element_blank(), # quito los numeros del eje Y
            panel.grid.minor.x = element_blank(), #quito grilla X menor
            panel.grid.major.x = element_blank(), #quito grilla X mayor
            panel.grid.minor.y = element_blank()) + #quito grilla Y menor
      scale_x_date(date_labels = "%b-%y", breaks = unique(Dx_Rx_Inst_Netos_Total$Mes)) +
      labs(title = "Dx, Rx, Instalaciones y Netos 2024 (Mensual)")
    ggplotly(d)
  })
  
  output$Dx_Rx_Inst_Netos_Graf_Region_05 <- plotly::renderPlotly({ 
    d <- Dx_Rx_Inst_Netos %>%
      dplyr::filter(Ciudad.Municipio == "Region_05") %>% 
      dplyr::filter(Mes <= Mes_en_curso) %>% 
      #dplyr::filter(Cantidad > 0) %>% 
      ggplot() +
      aes(x = Mes, y = Cantidad, fill = name) +
      geom_bar(stat = "identity", position = "dodge") + #stat = "summary"
      geom_text(aes(label= Cantidad), position=position_dodge(width=25), vjust= -0.5, color = "black", size = 3, group = "name") +
      scale_fill_hue(direction = 1) +
      theme_minimal() +
      #facet_wrap(vars(Ciudad.Municipio), scales = "free", labeller = label_wrap_gen(10)) +
      theme(strip.text = element_text(size = 11),
            axis.ticks.y = element_blank(), #quito tick eje Y
            panel.grid.major.y = element_blank(), #quito grilla Y mayor
            axis.text.y = element_blank(), # quito los numeros del eje Y
            panel.grid.minor.x = element_blank(), #quito grilla X menor
            panel.grid.major.x = element_blank(), #quito grilla X mayor
            panel.grid.minor.y = element_blank()) + #quito grilla Y menor
      scale_x_date(date_labels = "%b-%y", breaks = unique(Dx_Rx_Inst_Netos_Total$Mes)) +
      labs(title = "Dx, Rx, Instalaciones y Netos 2024 (Mensual) Region_05")
    ggplotly(d)
  })
  
  output$Dx_Rx_Inst_Netos_Graf_Region_02 <- plotly::renderPlotly({
    d <- Dx_Rx_Inst_Netos %>%
      dplyr::filter(Ciudad.Municipio == "Region_02") %>% 
      dplyr::filter(Mes <= Mes_en_curso) %>% 
      #dplyr::filter(Cantidad > 0) %>% 
      ggplot() +
      aes(x = Mes, y = Cantidad, fill = name) +
      geom_bar(stat = "identity", position = "dodge") + #stat = "summary"
      geom_text(aes(label= Cantidad), position=position_dodge(width=25), vjust= -0.5, color = "black", size = 3, group = "name") +
      scale_fill_hue(direction = 1) +
      theme_minimal() +
      #facet_wrap(vars(Ciudad.Municipio), scales = "free", labeller = label_wrap_gen(10)) +
      theme(strip.text = element_text(size = 11),
            axis.ticks.y = element_blank(), #quito tick eje Y
            panel.grid.major.y = element_blank(), #quito grilla Y mayor
            axis.text.y = element_blank(), # quito los numeros del eje Y
            panel.grid.minor.x = element_blank(), #quito grilla X menor
            panel.grid.major.x = element_blank(), #quito grilla X mayor
            panel.grid.minor.y = element_blank()) + #quito grilla Y menor
      scale_x_date(date_labels = "%b-%y", breaks = unique(Dx_Rx_Inst_Netos_Total$Mes)) +
      labs(title = "Dx, Rx, Instalaciones y Netos 2024 (Mensual) Region_02")
    ggplotly(d)
  })
  
  output$Dx_Rx_Inst_Netos_Graf_Region_06 <- plotly::renderPlotly({
    d <- Dx_Rx_Inst_Netos %>%
      dplyr::filter(Ciudad.Municipio == "Region_06") %>% 
      dplyr::filter(Mes <= Mes_en_curso) %>% 
      #dplyr::filter(Cantidad > 0) %>% 
      ggplot() +
      aes(x = Mes, y = Cantidad, fill = name) +
      geom_bar(stat = "identity", position = "dodge") + #stat = "summary"
      geom_text(aes(label= Cantidad), position=position_dodge(width=25), vjust= -0.5, color = "black", size = 3, group = "name") +
      scale_fill_hue(direction = 1) +
      theme_minimal() +
      #facet_wrap(vars(Ciudad.Municipio), scales = "free", labeller = label_wrap_gen(10)) +
      theme(strip.text = element_text(size = 11),
            axis.ticks.y = element_blank(), #quito tick eje Y
            panel.grid.major.y = element_blank(), #quito grilla Y mayor
            axis.text.y = element_blank(), # quito los numeros del eje Y
            panel.grid.minor.x = element_blank(), #quito grilla X menor
            panel.grid.major.x = element_blank(), #quito grilla X mayor
            panel.grid.minor.y = element_blank()) + #quito grilla Y menor
      scale_x_date(date_labels = "%b-%y", breaks = unique(Dx_Rx_Inst_Netos_Total$Mes)) +
      labs(title = "Dx, Rx, Instalaciones y Netos 2024 (Mensual) Region_06")
    ggplotly(d)
  })
  
  output$Dx_Rx_Inst_Netos_Graf_Region_04 <- plotly::renderPlotly({
    d <- Dx_Rx_Inst_Netos %>%
      dplyr::filter(Ciudad.Municipio == "Region_04") %>% 
      dplyr::filter(Mes <= Mes_en_curso) %>% 
      #dplyr::filter(Cantidad > 0) %>% 
      ggplot() +
      aes(x = Mes, y = Cantidad, fill = name) +
      geom_bar(stat = "identity", position = "dodge") + #stat = "summary"
      geom_text(aes(label= Cantidad), position=position_dodge(width=25), vjust= -0.5, color = "black", size = 3, group = "name") +
      scale_fill_hue(direction = 1) +
      theme_minimal() +
      #facet_wrap(vars(Ciudad.Municipio), scales = "free", labeller = label_wrap_gen(10)) +
      theme(strip.text = element_text(size = 11),
            axis.ticks.y = element_blank(), #quito tick eje Y
            panel.grid.major.y = element_blank(), #quito grilla Y mayor
            axis.text.y = element_blank(), # quito los numeros del eje Y
            panel.grid.minor.x = element_blank(), #quito grilla X menor
            panel.grid.major.x = element_blank(), #quito grilla X mayor
            panel.grid.minor.y = element_blank()) + #quito grilla Y menor
      scale_x_date(date_labels = "%b-%y", breaks = unique(Dx_Rx_Inst_Netos_Total$Mes)) +
      labs(title = "Dx, Rx, Instalaciones y Netos 2024 (Mensual) Region_04")
    ggplotly(d)
  })
  
  output$Dx_Rx_Inst_Netos_Graf_Region_01 <- plotly::renderPlotly({
    d <- Dx_Rx_Inst_Netos %>%
      dplyr::filter(Ciudad.Municipio == "Region_01") %>% 
      dplyr::filter(Mes <= Mes_en_curso) %>% 
      #dplyr::filter(Cantidad > 0) %>% 
      ggplot() +
      aes(x = Mes, y = Cantidad, fill = name) +
      geom_bar(stat = "identity", position = "dodge") + #stat = "summary"
      geom_text(aes(label= Cantidad), position=position_dodge(width=25), vjust= -0.5, color = "black", size = 3, group = "name") +
      scale_fill_hue(direction = 1) +
      theme_minimal() +
      #facet_wrap(vars(Ciudad.Municipio), scales = "free", labeller = label_wrap_gen(10)) +
      theme(strip.text = element_text(size = 11),
            axis.ticks.y = element_blank(), #quito tick eje Y
            panel.grid.major.y = element_blank(), #quito grilla Y mayor
            axis.text.y = element_blank(), # quito los numeros del eje Y
            panel.grid.minor.x = element_blank(), #quito grilla X menor
            panel.grid.major.x = element_blank(), #quito grilla X mayor
            panel.grid.minor.y = element_blank()) + #quito grilla Y menor
      scale_x_date(date_labels = "%b-%y", breaks = unique(Dx_Rx_Inst_Netos_Total$Mes)) +
      labs(title = "Dx, Rx, Instalaciones y Netos 2024 (Mensual) Region_01")
    ggplotly(d)
  })
  
  output$Dx_Rx_Inst_Netos_Graf_Region_07 <- plotly::renderPlotly({
    d <- Dx_Rx_Inst_Netos %>%
      dplyr::filter(Ciudad.Municipio == "Region_07") %>% 
      dplyr::filter(Mes <= Mes_en_curso) %>% 
      #dplyr::filter(Cantidad > 0) %>% 
      ggplot() +
      aes(x = Mes, y = Cantidad, fill = name) +
      geom_bar(stat = "identity", position = "dodge") + #stat = "summary"
      geom_text(aes(label= Cantidad), position=position_dodge(width=25), vjust= -0.5, color = "black", size = 3, group = "name") +
      scale_fill_hue(direction = 1) +
      theme_minimal() +
      #facet_wrap(vars(Ciudad.Municipio), scales = "free", labeller = label_wrap_gen(10)) +
      theme(strip.text = element_text(size = 11),
            axis.ticks.y = element_blank(), #quito tick eje Y
            panel.grid.major.y = element_blank(), #quito grilla Y mayor
            axis.text.y = element_blank(), # quito los numeros del eje Y
            panel.grid.minor.x = element_blank(), #quito grilla X menor
            panel.grid.major.x = element_blank(), #quito grilla X mayor
            panel.grid.minor.y = element_blank()) + #quito grilla Y menor
      scale_x_date(date_labels = "%b-%y", breaks = unique(Dx_Rx_Inst_Netos_Total$Mes)) +
      labs(title = "Dx, Rx, Instalaciones y Netos 2024 (Mensual) Region_07")
    ggplotly(d)
  })
  
  output$Dx_Rx_Inst_Netos_Graf_Region_08 <- plotly::renderPlotly({
    d <- Dx_Rx_Inst_Netos %>%
      dplyr::filter(Ciudad.Municipio == "Region_08") %>% 
      dplyr::filter(Mes <= Mes_en_curso) %>% 
      #dplyr::filter(Cantidad > 0) %>% 
      ggplot() +
      aes(x = Mes, y = Cantidad, fill = name) +
      geom_bar(stat = "identity", position = "dodge") + #stat = "summary"
      geom_text(aes(label= Cantidad), position=position_dodge(width=25), vjust= -0.5, color = "black", size = 3, group = "name") +
      scale_fill_hue(direction = 1) +
      theme_minimal() +
      #facet_wrap(vars(Ciudad.Municipio), scales = "free", labeller = label_wrap_gen(10)) +
      theme(strip.text = element_text(size = 11),
            axis.ticks.y = element_blank(), #quito tick eje Y
            panel.grid.major.y = element_blank(), #quito grilla Y mayor
            axis.text.y = element_blank(), # quito los numeros del eje Y
            panel.grid.minor.x = element_blank(), #quito grilla X menor
            panel.grid.major.x = element_blank(), #quito grilla X mayor
            panel.grid.minor.y = element_blank()) + #quito grilla Y menor
      scale_x_date(date_labels = "%b-%y", breaks = unique(Dx_Rx_Inst_Netos_Total$Mes)) +
      labs(title = "Dx, Rx, Instalaciones y Netos 2024 (Mensual) Region_08")
    ggplotly(d)
  })
  
  output$Dx_Rx_Inst_Netos_Graf_Region_09 <- plotly::renderPlotly({
    d <- Dx_Rx_Inst_Netos %>%
      dplyr::filter(Ciudad.Municipio == "Region_09") %>% 
      dplyr::filter(Mes <= Mes_en_curso) %>% 
      #dplyr::filter(Cantidad > 0) %>% 
      ggplot() +
      aes(x = Mes, y = Cantidad, fill = name) +
      geom_bar(stat = "identity", position = "dodge") + #stat = "summary"
      geom_text(aes(label= Cantidad), position=position_dodge(width=25), vjust= -0.5, color = "black", size = 3, group = "name") +
      scale_fill_hue(direction = 1) +
      theme_minimal() +
      #facet_wrap(vars(Ciudad.Municipio), scales = "free", labeller = label_wrap_gen(10)) +
      theme(strip.text = element_text(size = 11),
            axis.ticks.y = element_blank(), #quito tick eje Y
            panel.grid.major.y = element_blank(), #quito grilla Y mayor
            axis.text.y = element_blank(), # quito los numeros del eje Y
            panel.grid.minor.x = element_blank(), #quito grilla X menor
            panel.grid.major.x = element_blank(), #quito grilla X mayor
            panel.grid.minor.y = element_blank()) + #quito grilla Y menor
      scale_x_date(date_labels = "%b-%y", breaks = unique(Dx_Rx_Inst_Netos_Total$Mes)) +
      labs(title = "Dx, Rx, Instalaciones y Netos 2024 (Mensual) Region_09")
    ggplotly(d)
  })
  
  output$Dx_Rx_Inst_Netos_Graf_Region_03 <- plotly::renderPlotly({
    d <- Dx_Rx_Inst_Netos %>%
      dplyr::filter(Ciudad.Municipio == "Region_03") %>% 
      dplyr::filter(Mes <= Mes_en_curso) %>% 
      #dplyr::filter(Cantidad > 0) %>% 
      ggplot() +
      aes(x = Mes, y = Cantidad, fill = name) +
      geom_bar(stat = "identity", position = "dodge") + #stat = "summary"
      geom_text(aes(label= Cantidad), position=position_dodge(width=25), vjust= -0.5, color = "black", size = 3, group = "name") +
      scale_fill_hue(direction = 1) +
      theme_minimal() +
      #facet_wrap(vars(Ciudad.Municipio), scales = "free", labeller = label_wrap_gen(10)) +
      theme(strip.text = element_text(size = 11),
            axis.ticks.y = element_blank(), #quito tick eje Y
            panel.grid.major.y = element_blank(), #quito grilla Y mayor
            axis.text.y = element_blank(), # quito los numeros del eje Y
            panel.grid.minor.x = element_blank(), #quito grilla X menor
            panel.grid.major.x = element_blank(), #quito grilla X mayor
            panel.grid.minor.y = element_blank()) + #quito grilla Y menor
      scale_x_date(date_labels = "%b-%y", breaks = unique(Dx_Rx_Inst_Netos_Total$Mes)) +
      labs(title = "Dx, Rx, Instalaciones y Netos 2024 (Mensual) Region_03")
    ggplotly(d)
  })
  
  output$Dx_Rx_Inst_Netos_Graf_Region_10 <- plotly::renderPlotly({
    d <- Dx_Rx_Inst_Netos %>%
      dplyr::filter(Ciudad.Municipio == "Region_10") %>% 
      dplyr::filter(Mes <= Mes_en_curso) %>% 
      #dplyr::filter(Cantidad > 0) %>% 
      ggplot() +
      aes(x = Mes, y = Cantidad, fill = name) +
      geom_bar(stat = "identity", position = "dodge") + #stat = "summary"
      geom_text(aes(label= Cantidad), position=position_dodge(width=25), vjust= -0.5, color = "black", size = 3, group = "name") +
      scale_fill_hue(direction = 1) +
      theme_minimal() +
      #facet_wrap(vars(Ciudad.Municipio), scales = "free", labeller = label_wrap_gen(10)) +
      theme(strip.text = element_text(size = 11),
            axis.ticks.y = element_blank(), #quito tick eje Y
            panel.grid.major.y = element_blank(), #quito grilla Y mayor
            axis.text.y = element_blank(), # quito los numeros del eje Y
            panel.grid.minor.x = element_blank(), #quito grilla X menor
            panel.grid.major.x = element_blank(), #quito grilla X mayor
            panel.grid.minor.y = element_blank()) + #quito grilla Y menor
      scale_x_date(date_labels = "%b-%y", breaks = unique(Dx_Rx_Inst_Netos_Total$Mes)) +
      labs(title = "Dx, Rx, Instalaciones y Netos 2024 (Mensual) Region_10")
    ggplotly(d)
  })
  
  output$Dx_Rx_Inst_Netos_Graf_Region_14 <- plotly::renderPlotly({
    d <- Dx_Rx_Inst_Netos %>%
      dplyr::filter(Ciudad.Municipio == "Region_14") %>% 
      dplyr::filter(Mes <= Mes_en_curso) %>% 
      #dplyr::filter(Cantidad > 0) %>% 
      ggplot() +
      aes(x = Mes, y = Cantidad, fill = name) +
      geom_bar(stat = "identity", position = "dodge") + #stat = "summary"
      geom_text(aes(label= Cantidad), position=position_dodge(width=25), vjust= -0.5, color = "black", size = 3, group = "name") +
      scale_fill_hue(direction = 1) +
      theme_minimal() +
      #facet_wrap(vars(Ciudad.Municipio), scales = "free", labeller = label_wrap_gen(10)) +
      theme(strip.text = element_text(size = 11),
            axis.ticks.y = element_blank(), #quito tick eje Y
            panel.grid.major.y = element_blank(), #quito grilla Y mayor
            axis.text.y = element_blank(), # quito los numeros del eje Y
            panel.grid.minor.x = element_blank(), #quito grilla X menor
            panel.grid.major.x = element_blank(), #quito grilla X mayor
            panel.grid.minor.y = element_blank()) + #quito grilla Y menor
      scale_x_date(date_labels = "%b-%y", breaks = unique(Dx_Rx_Inst_Netos_Total$Mes)) +
      labs(title = "Dx, Rx, Instalaciones y Netos 2024 (Mensual) Region_14")
    ggplotly(d)
  })
  
  output$Dx_Rx_Inst_Netos_Graf_Region_17 <- plotly::renderPlotly({
    d <- Dx_Rx_Inst_Netos %>%
      dplyr::filter(Ciudad.Municipio == "Region_17") %>% 
      dplyr::filter(Mes <= Mes_en_curso) %>% 
      #dplyr::filter(Cantidad > 0) %>% 
      ggplot() +
      aes(x = Mes, y = Cantidad, fill = name) +
      geom_bar(stat = "identity", position = "dodge") + #stat = "summary"
      geom_text(aes(label= Cantidad), position=position_dodge(width=25), vjust= -0.5, color = "black", size = 3, group = "name") +
      scale_fill_hue(direction = 1) +
      theme_minimal() +
      #facet_wrap(vars(Ciudad.Municipio), scales = "free", labeller = label_wrap_gen(10)) +
      theme(strip.text = element_text(size = 11),
            axis.ticks.y = element_blank(), #quito tick eje Y
            panel.grid.major.y = element_blank(), #quito grilla Y mayor
            axis.text.y = element_blank(), # quito los numeros del eje Y
            panel.grid.minor.x = element_blank(), #quito grilla X menor
            panel.grid.major.x = element_blank(), #quito grilla X mayor
            panel.grid.minor.y = element_blank()) + #quito grilla Y menor
      scale_x_date(date_labels = "%b-%y", breaks = unique(Dx_Rx_Inst_Netos_Total$Mes)) +
      labs(title = "Dx, Rx, Instalaciones y Netos 2024 (Mensual) Region_17")
    ggplotly(d)
  })
  
  
  # Output graficas averias sobre activos ----
  
  output$Averias_sobre_activo_grafica_Total <- plotly::renderPlotly({
    d <- DF_Averias_sobre_activos_total %>%
      dplyr::filter(Mes <= lubridate::floor_date(today(), unit = "month")) %>% 
      ggplot() +
      aes(x = Mes, y = Averias_Activo) +
      geom_bar(stat = "identity", fill = verde) + #stat = "summary"
      geom_hline(aes(yintercept = 8),linetype = "dashed", color = rojo) +
      scale_fill_hue(direction = 1) +
      theme_minimal() +
      theme(strip.text = element_text(size = 11),
            plot.title = element_text(size = 18),
            axis.ticks.y = element_blank(), #quito tick eje Y
            panel.grid.major.y = element_blank(), #quito grilla Y mayor
            axis.text.y = element_blank(), # quito los numeros del eje Y
            axis.text.x = element_text(angle = 45),
            panel.grid.minor.x = element_blank(), #quito grilla X menor
            panel.grid.major.x = element_blank(), #quito grilla X mayor
            panel.grid.minor.y = element_blank(),
            legend.position= "none") +
      labs(title = "Total Averias Sobre Activo ISP_DEMO") +
      scale_x_date(date_labels = "%b-%y", breaks = unique(DF_Averias_sobre_activos_total$Mes)) +
      geom_text(aes(label= paste0(round(Averias_Activo, digits = 2), "%")), position=position_dodge(width=25), vjust= -0.5, color = "black", size = 3, group = "name")
    
    ggplotly(d)
  })
  
  output$Averias_sobre_activo_grafica_Mes_actual_municipios <- plotly::renderPlotly({
    d <- DF_Averias_sobre_activos %>%
      dplyr::filter(name == "Av_Activo") %>% 
      dplyr::filter(Mes == Mes_en_curso) %>% 
      dplyr::filter(!is.na(Ciudad.Municipio)) %>% 
      dplyr::filter(!Ciudad.Municipio %in% c("TOTAL", "Region_12", "Region_15", #filtro municpios que no aportan por ser especiales
                                             "Region_11", "Region_13",
                                             "Region_16")) %>% 
      ggplot() +
      aes(x = Ciudad.Municipio, y = Cantidad) +
      geom_bar(stat = "identity", fill = verde) + #stat = "summary"
      geom_hline(aes(yintercept = 8),linetype = "dashed", color = rojo) +
      scale_fill_hue(direction = 1) +
      theme_minimal() +
      theme(strip.text = element_text(size = 11),
            plot.title = element_text(size = 18),
            axis.ticks.y = element_blank(), #quito tick eje Y
            panel.grid.major.y = element_blank(), #quito grilla Y mayor
            axis.text.y = element_blank(), # quito los numeros del eje Y
            axis.text.x = element_text(angle = 45),
            panel.grid.minor.x = element_blank(), #quito grilla X menor
            panel.grid.major.x = element_blank(), #quito grilla X mayor
            panel.grid.minor.y = element_blank(),
            legend.position= "none") +
      labs(title = "Total Averias Sobre Activo ISP_DEMO por Municipio mes Actual") +
      #scale_x_date(date_labels = "%b-%y", breaks = unique(DF_Averias_sobre_activos_total$Mes)) +
      geom_text(aes(label= paste0(round(Cantidad, digits = 2), "%")), vjust= -0.5, color = "black", size = 3, group = "name")
    
    ggplotly(d)
  })
  
  
  output$Averias_sobre_activo_grafica_Region_05 <- plotly::renderPlotly({
    d <- DF_Averias_sobre_activos %>%
      dplyr::filter(Ciudad.Municipio == "Region_05") %>%
      dplyr::filter(name == "Av_Activo") %>% 
      dplyr::filter(Mes <= Mes_en_curso) %>%
      dplyr::filter(Mes >= "2023-05-01")
    
    d1 <- d %>% 
      grafica_barras_averias_sobre_activo() +
      labs(title = "Region_05, Averias Sobre Activo") +
      scale_x_date(date_labels = "%b-%y", breaks = unique(d$Mes)) +
      geom_text(aes(label= paste0(Cantidad, "%")), position=position_dodge(width=25), vjust= -0.5, color = "black", size = 3, group = "name") +
      geom_hline(aes(yintercept = 8),linetype = "dashed", color = rojo)
    
    ggplotly(d1)
  })
  
  
  output$Averias_sobre_activo_grafica_Region_02 <- plotly::renderPlotly({
    d <- DF_Averias_sobre_activos %>%
      dplyr::filter(Ciudad.Municipio == "Region_02") %>%
      dplyr::filter(name == "Av_Activo") %>% 
      dplyr::filter(Mes <= Mes_en_curso) %>%
      dplyr::filter(Mes >= "2023-05-01")
    
    d1 <- d %>% 
      grafica_barras_averias_sobre_activo() +
      labs(title = "Region_02, Averias Sobre Activo") +
      scale_x_date(date_labels = "%b-%y", breaks = unique(d$Mes)) +
      geom_text(aes(label= paste0(Cantidad, "%")), position=position_dodge(width=25), vjust= -0.5, color = "black", size = 3, group = "name") +
      geom_hline(aes(yintercept = 8),linetype = "dashed", color = rojo)
    
    ggplotly(d1)
  })
  
  output$Averias_sobre_activo_grafica_Region_06 <- plotly::renderPlotly({
    d <- DF_Averias_sobre_activos %>%
      dplyr::filter(Ciudad.Municipio == "Region_06") %>%
      dplyr::filter(name == "Av_Activo") %>% 
      dplyr::filter(Mes <= Mes_en_curso) %>%
      dplyr::filter(Mes >= "2023-05-01")
    
    d1 <- d %>% 
      grafica_barras_averias_sobre_activo() +
      labs(title = "Region_06, Averias Sobre Activo") +
      scale_x_date(date_labels = "%b-%y", breaks = unique(d$Mes)) +
      geom_text(aes(label= paste0(Cantidad, "%")), position=position_dodge(width=25), vjust= -0.5, color = "black", size = 3, group = "name") +
      geom_hline(aes(yintercept = 8),linetype = "dashed", color = rojo)
    
    ggplotly(d1)
  })
  
  output$Averias_sobre_activo_grafica_Region_04 <- plotly::renderPlotly({
    d <- DF_Averias_sobre_activos %>%
      dplyr::filter(Ciudad.Municipio == "Region_04") %>%
      dplyr::filter(name == "Av_Activo") %>% 
      dplyr::filter(Mes <= Mes_en_curso) %>%
      dplyr::filter(Mes >= "2023-05-01")
    
    d1 <- d %>% 
      grafica_barras_averias_sobre_activo() +
      labs(title = "Region_04, Averias Sobre Activo") +
      scale_x_date(date_labels = "%b-%y", breaks = unique(d$Mes)) +
      geom_text(aes(label= paste0(Cantidad, "%")), position=position_dodge(width=25), vjust= -0.5, color = "black", size = 3, group = "name") +
      geom_hline(aes(yintercept = 8),linetype = "dashed", color = rojo)
    
    ggplotly(d1)
  })
  
  output$Averias_sobre_activo_grafica_Region_01 <- plotly::renderPlotly({
    d <- DF_Averias_sobre_activos %>%
      dplyr::filter(Ciudad.Municipio == "Region_01") %>%
      dplyr::filter(name == "Av_Activo") %>% 
      dplyr::filter(Mes <= Mes_en_curso) %>%
      dplyr::filter(Mes >= "2023-05-01")
    
    d1 <- d %>% 
      grafica_barras_averias_sobre_activo() +
      labs(title = "Region_01, Averias Sobre Activo") +
      scale_x_date(date_labels = "%b-%y", breaks = unique(d$Mes)) +
      geom_text(aes(label= paste0(Cantidad, "%")), position=position_dodge(width=25), vjust= -0.5, color = "black", size = 3, group = "name") +
      geom_hline(aes(yintercept = 8),linetype = "dashed", color = rojo)
    
    ggplotly(d1)
  })
  
  output$Averias_sobre_activo_grafica_Region_07 <- plotly::renderPlotly({
    d <- DF_Averias_sobre_activos %>%
      dplyr::filter(Ciudad.Municipio == "Region_07") %>%
      dplyr::filter(name == "Av_Activo") %>% 
      dplyr::filter(Mes <= Mes_en_curso) %>%
      dplyr::filter(Mes >= "2023-05-01")
    
    d1 <- d %>% 
      grafica_barras_averias_sobre_activo() +
      labs(title = "Region_07, Averias Sobre Activo") +
      scale_x_date(date_labels = "%b-%y", breaks = unique(d$Mes)) +
      geom_text(aes(label= paste0(Cantidad, "%")), position=position_dodge(width=25), vjust= -0.5, color = "black", size = 3, group = "name") +
      geom_hline(aes(yintercept = 8),linetype = "dashed", color = rojo)
    
    ggplotly(d1)
  })
  
  output$Averias_sobre_activo_grafica_Region_08 <- plotly::renderPlotly({
    d <- DF_Averias_sobre_activos %>%
      dplyr::filter(Ciudad.Municipio == "Region_08") %>%
      dplyr::filter(name == "Av_Activo") %>% 
      dplyr::filter(Mes <= Mes_en_curso) %>%
      dplyr::filter(Mes >= "2023-05-01")
    
    d1 <- d %>% 
      grafica_barras_averias_sobre_activo() +
      labs(title = "Region_08, Averias Sobre Activo") +
      scale_x_date(date_labels = "%b-%y", breaks = unique(d$Mes)) +
      geom_text(aes(label= paste0(Cantidad, "%")), position=position_dodge(width=25), vjust= -0.5, color = "black", size = 3, group = "name") +
      geom_hline(aes(yintercept = 8),linetype = "dashed", color = rojo)
    
    ggplotly(d1)
  })
  
  output$Averias_sobre_activo_grafica_Region_09 <- plotly::renderPlotly({
    d <- DF_Averias_sobre_activos %>%
      dplyr::filter(Ciudad.Municipio == "Region_09") %>%
      dplyr::filter(name == "Av_Activo") %>% 
      dplyr::filter(Mes <= Mes_en_curso) %>%
      dplyr::filter(Mes >= "2023-05-01")
    
    d1 <- d %>% 
      grafica_barras_averias_sobre_activo() +
      labs(title = "Region_09, Averias Sobre Activo") +
      scale_x_date(date_labels = "%b-%y", breaks = unique(d$Mes)) +
      geom_text(aes(label= paste0(Cantidad, "%")), position=position_dodge(width=25), vjust= -0.5, color = "black", size = 3, group = "name") +
      geom_hline(aes(yintercept = 8),linetype = "dashed", color = rojo)
    
    ggplotly(d1)
  })
  
  output$Averias_sobre_activo_grafica_Region_03 <- plotly::renderPlotly({
    d <- DF_Averias_sobre_activos %>%
      dplyr::filter(Ciudad.Municipio == "Region_03") %>%
      dplyr::filter(name == "Av_Activo") %>% 
      dplyr::filter(Mes <= Mes_en_curso) %>%
      dplyr::filter(Mes >= "2023-05-01")
    
    d1 <- d %>% 
      grafica_barras_averias_sobre_activo() +
      labs(title = "Region_03, Averias Sobre Activo") +
      scale_x_date(date_labels = "%b-%y", breaks = unique(d$Mes)) +
      geom_text(aes(label= paste0(Cantidad, "%")), position=position_dodge(width=25), vjust= -0.5, color = "black", size = 3, group = "name") +
      geom_hline(aes(yintercept = 8),linetype = "dashed", color = rojo)
    
    ggplotly(d1)
  })
  
  output$Averias_sobre_activo_grafica_Region_10 <- plotly::renderPlotly({
    d <- DF_Averias_sobre_activos %>%
      dplyr::filter(Ciudad.Municipio == "Region_10") %>%
      dplyr::filter(name == "Av_Activo") %>% 
      dplyr::filter(Mes <= Mes_en_curso) %>%
      dplyr::filter(Mes >= "2023-05-01")
    
    d1 <- d %>% 
      grafica_barras_averias_sobre_activo() +
      labs(title = "Region_10, Averias Sobre Activo") +
      scale_x_date(date_labels = "%b-%y", breaks = unique(d$Mes)) +
      geom_text(aes(label= paste0(Cantidad, "%")), position=position_dodge(width=25), vjust= -0.5, color = "black", size = 3, group = "name") +
      geom_hline(aes(yintercept = 8),linetype = "dashed", color = rojo)
    
    ggplotly(d1)
  })
  
  output$Averias_sobre_activo_grafica_Region_14 <- plotly::renderPlotly({
    d <- DF_Averias_sobre_activos %>%
      dplyr::filter(Ciudad.Municipio == "Region_14") %>%
      dplyr::filter(name == "Av_Activo") %>% 
      dplyr::filter(Mes <= Mes_en_curso) %>%
      dplyr::filter(Mes >= "2023-05-01")
    
    d1 <- d %>% 
      grafica_barras_averias_sobre_activo() +
      labs(title = "Region_14, Averias Sobre Activo") +
      scale_x_date(date_labels = "%b-%y", breaks = unique(d$Mes)) +
      geom_text(aes(label= paste0(Cantidad, "%")), position=position_dodge(width=25), vjust= -0.5, color = "black", size = 3, group = "name") +
      geom_hline(aes(yintercept = 8),linetype = "dashed", color = rojo)
    
    ggplotly(d1)
  })
  
  output$Averias_sobre_activo_grafica_Region_17 <- plotly::renderPlotly({
    d <- DF_Averias_sobre_activos %>%
      dplyr::filter(Ciudad.Municipio == "Region_17") %>%
      dplyr::filter(name == "Av_Activo") %>% 
      dplyr::filter(Mes <= Mes_en_curso) %>%
      dplyr::filter(Mes >= "2023-05-01")
    
    d1 <- d %>% 
      grafica_barras_averias_sobre_activo() +
      labs(title = "Region_17, Averias Sobre Activo") +
      scale_x_date(date_labels = "%b-%y", breaks = unique(d$Mes)) +
      geom_text(aes(label= paste0(Cantidad, "%")), position=position_dodge(width=25), vjust= -0.5, color = "black", size = 3, group = "name") +
      geom_hline(aes(yintercept = 8),linetype = "dashed", color = rojo)
    
    ggplotly(d1)
  })
  
  
}


# Run the application 
shinyApp(ui = ui, server = server)
