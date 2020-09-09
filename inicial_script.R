rm(list=ls())

###
# IMPORTAR LIBRERIAS ----
###
library(tidyverse)
library(dplyr) # More efficient DATA FRAME working
library(reshape) # Rename column
library(data.table) # Modify NA more efficient
library(corrplot)
library(lubridate)
library(ggplot2)

### 
# IMPORTAR DATOS ----
###
setwd('~')
dir1 <- paste(getwd(), "/DEVF/Curso Data Science/Proyecto/devF_proyecto/Assets", sep="")
WD <- normalizePath(dir1, winslash = "\\", mustWork = NA)
file416 <- read.csv(paste(WD, "Abr2019-Jun2019wet.csv", sep="\\"), stringsAsFactors = FALSE, strip.white = TRUE)

###
# Pre-View
###
head(file416)
str(file416)

###
# Create a DataFrame just with necessary columns and modify name
###
f416 <- select(file416, ï..Date2, Textbox70, Product1,Textbox42, Textbox44, Pump1, Cashier1, Name1, MOP11)

###
# Modificacion de nombres de columnas
###
name_head <- colnames(f416)
new_name_head <- c('date', 'hour', 'product', 'qty', 'value', 'pump', 'id_cashier','name_cashier', 'pay')
names(f416)[names(f416) == name_head] <- new_name_head

head(f416)

###
# SECCION DE MANIPULACION DE DATOS Y ANALISIS EXPLORATORIO ########
###

###
# Filas y columnas
###
print(paste('Número de filas en el dataframe', nrow(f416), sep=": "))
print(paste('Número de columnas en el dataframe', ncol(f416), sep=": "))

###
# Descripción del dataset
###
str(f416)

### Bucle para eliminar espacion en blancos al inicio y final de una cadena string
#for(i in 1:nrow(f416)) {
#  f416$pay[i] <- trimws(x = f416$pay[i])
#  f416$id_cashier[i] <- trimws(x = f416$id_cashier[i])
#  f416$product[i] <- trimws(x = f416$product[i])
#}
###  reemplazado por -> strip.white = TRUE en las seccion de carga de BD
# Aporte de IsraAgus

###
# Modificando variables, convirtiendo chr -> num && eliminando chr especiales
###

### Modificacion caracteres especiales y decimales
f416$value = gsub(',00', '', f416$value)
f416$value = gsub('\\.', '', f416$value)
f416$qty = gsub(',', '\\.', f416$qty)

### Modificacion chr -> num
f416 <- transform(f416, value = as.numeric(value), 
          id_cashier = as.numeric(id_cashier),
          qty = as.numeric(qty))
### Warning debido a valores *Auto en variable id_cashier y Negativos en variable qty
sapply(f416, function(x) sum(is.na(x)))
### Verify & Replace NA to 0
f416 <- replace(f416, is.na(f416), 0) # 0 representa asignacion automatica

### Modificacion de valores id_cashier y name_cashier por valores Shellcard cajero default sistema
f416$id_cashier[f416$id_cashier == 0] <- 1000
f416$name_cashier[f416$id_cashier == 1000] <- "ShellCard"

###
# Estadísticas del dataframe
###
stads <- summary(f416)
stads


###
# Modify date
###

# Transformamos fecha en formato R
f416$date <- as.Date(f416$date, format = '%d/%m/%Y')
# Include day_week and event (laboral =0, festivo =1, findesemana =2) in data
f416 <- f416 %>%
  mutate(day_week = lubridate::wday(f416$date, label = TRUE, abbr=FALSE),
         event =0
  )
# Vector festivos 2019 Chile
fes_2019 <- c('01/01/2019','19/04/2019','20/04/2019','01/05/2019','21/05/2019',
              '07/06/2019','29/06/2019','16/07/2019','15/08/2019','20/08/2019',  
              '18/09/2019','19/09/2019','20/09/2019','29/09/2019','12/10/2019',  
              '31/10/2019','01/11/2019','08/12/2019','25/12/2019','31/12/2019')
# Include a number 1 (festivo), in event
f416[f416$date %in% as.Date(fes_2019, format = '%d/%m/%Y'), 'event'] <- 1
# Include a number 2 into event like a findesemana
f416[f416$day_week %in% c('sábado','domingo'), 'event'] <- 2
# Dividimos fecha en dia mes y anio para Analisis Descriptivo
f416 <- f416 %>%
        mutate(day = format(date, "%d"),
               month = format(date, "%m"),
               year = format(date, "%Y")
               ) %>%
        transform(day = as.numeric(day),month = as.numeric(month), year = as.numeric(year))

str(f416)

###
# AGRUPACION Y ANALISIS DESCRIPTIVO -----------------------------------
###


###
# Obtener datos de cantidades y promedios de venta diarias y por cajeros
###

# Cantidad y Promedio litros diarios y mensual
litros <- f416 %>%
            group_by(month,day) %>%
            summarise(litros_dia = sum(qty), promedio_litros = round(mean(qty),2)
            )

print(litros)

bar <- ggplot(litros,aes(x=day,y=litros_dia)) +
  geom_bar(stat = "identity") +
  labs(title="Número de Litros por dia",subtitle='Trimestre Abril-Junio',x="Dias",y="Litros")
bar

###
# Numero de Ventas por Cajero
###

tempo <- f416 %>%
  filter(month != 7) %>%
  group_by(month, id_cashier) %>%
  summarise(total_cashier=n(),
  )
# Crear funcion para etraer iniciales de listas de cajeros
tempo$id_cashier <- factor(tempo$id_cashier,
                           levels = c(1000, 1001, 1002, 1003, 1005,  1006, 1008, 1009,
                                      1011, 1012, 1014, 1016, 1018, 1019, 1020),
                           labels = c("ShCa","ENVA","VICA","GIVA","NN1","MAMA", "NN2","JUCR",
                                      "NN3","JOPI","ALFI","ARCA","EDMA","ALLE","GORA")
)

line <- ggplot(tempo, aes(x=month,y=total_cashier,group=id_cashier,color=id_cashier)) +
  geom_line()

line

###
# Numero de Ventas por Cajero
###

tempo <- f416 %>%
  filter(month == 4) %>%
  group_by(id_cashier) %>%
  summarise(total_cashier=n(),
  )
# Crear funcion para etraer iniciales de listas de cajeros
tempo$id_cashier <- factor(tempo$id_cashier,
                    levels = c(1000, 1001, 1002, 1003, 1005,  1006, 1008, 1009,
                             1011, 1012, 1014, 1016, 1018, 1019, 1020),
                    labels = c("ShCa","ENVA","VICA","GIVA","NN1","MAMA", "NN2","JUCR",
                             "NN3","JOPI","ALFI","ARCA","EDMA","ALLE","GORA")
                    )

bar <- ggplot(tempo, aes(x = id_cashier, y = total_cashier, fill = total_cashier)) +
      geom_bar(stat = 'identity') +
      labs(title="Número de ventas registradas por Cajero",subtitle='Abril',
           x="Identificado Cajero",y="Transacciones")
bar

###
# Matriz de color, Venta cajero por dia de semana
###

tempo <- f416 %>%
  filter(month == 4) %>%
  group_by(day_week, id_cashier) %>%
  summarise(promedio_cajero = mean(qty), litros_cajero = sum(qty)
  )

tempo$id_cashier <- factor(tempo$id_cashier,
                           levels = c(1000, 1001, 1002, 1003, 1005,  1006, 1008, 1009,
                                      1011, 1012, 1014, 1016, 1018, 1019, 1020),
                           labels = c("ShCa","ENVA","VICA","GIVA","NN1","MAMA", "NN2","JUCR",
                                      "NN3","JOPI","ALFI","ARCA","EDMA","ALLE","GORA")
                    )

mt <- ggplot(tempo, aes(id_cashier,day_week)) +
      geom_tile(aes(fill = litros_cajero)) + 
      scale_fill_gradient(low = "green", high = "red") +
      theme_classic()
mt

###
# Grafico litros por cajero por dia en un Mes
###

tempo <- f416 %>%
  filter(month == 4,) %>%
  group_by(day, id_cashier) %>%
  summarise(litros_cajero = sum(qty)) %>% ### numero de transacciones por cajeros x dia
  ungroup() %>%
  group_by(day) %>%
  mutate(litros_dia= sum(litros_cajero),
         ratio = round((litros_cajero/litros_dia)*100, 1)     
  )

tempo$id_cashier <- factor(tempo$id_cashier,
                           levels = c(1000, 1001, 1002, 1003, 1005,  1006, 1008, 1009,
                                      1011, 1012, 1014, 1016, 1018, 1019, 1020),
                           labels = c("ShCa","ENVA","VICA","GIVA","NN1","MAMA", "NN2","JUCR",
                                      "NN3","JOPI","ALFI","ARCA","EDMA","ALLE","GORA")
                    )
print(tempo)

gr <- ggplot(tempo, aes(x=id_cashier, y=ratio)) +
  geom_bar(stat="identity", fill="#41b6c4") +
  geom_text(aes(label=ratio), vjust=-0.3, size=3, angle=0) +
  facet_wrap(~day) +
  labs(title="Promedio por Cajero \n Venta Diaria  Mes de Abril", 
       x="Cajero", y="% por Cajero") +
  theme_bw()
gr

###
# Participacion porcentual de Cajeros X Mes
###

tempo <- f416 %>%
  filter(month != 7,) %>%
  group_by(month, id_cashier) %>%
  summarise(total = sum(qty)) %>% ### litros por cajero
  ungroup() %>%
  group_by(month) %>%
  mutate(total_mes= sum(total),
         ratio = round((total/total_mes)*100, 1) # ratio participacion cajero en total mes
  )

tempo$id_cashier <- factor(tempo$id_cashier,
                           levels = c(1000, 1001, 1002, 1003, 1005,  1006, 1008, 1009,
                                      1011, 1012, 1014, 1016, 1018, 1019, 1020),
                           labels = c("ShCa","ENVA","VICA","GIVA","NN1","MAMA", "NN2","JUCR",
                                      "NN3","JOPI","ALFI","ARCA","EDMA","ALLE","GORA")
                    )

gr <- ggplot(tempo, aes(x=id_cashier, y=ratio)) +
  geom_bar(stat="identity", fill="#41b6c4") +
  geom_text(aes(label=ratio), vjust=-0.3, size=3, angle=0) +
  facet_wrap(~month) +
  labs(title="Participacion en la Venta Mensual\n Por Cada Cajero", 
       x="Identificador Cajero", y="% de Participacion") +
  theme_bw()
gr

####
# Participacion Mensual por cada dia del Mes
###

tempo <- f416 %>%
        filter(month != 7,) %>%
        group_by(month, day) %>%
        summarise(venta_dia = sum(qty)) %>%  ## Venta Diaria
        ungroup() %>%
        group_by(month,) %>%
        mutate(total_mes= sum(venta_dia),
        ratio = round((venta_dia/total_mes)*100, 1) # participacion x dia en total mes
        )

gr <- ggplot(tempo, aes(x=day, y=ratio)) +
  geom_bar(stat="identity", fill="#41b6c4") +
  geom_text(aes(label=ratio), vjust=-0.3, size=3, angle=0) +
  facet_wrap(~month) +
  labs(title="Participacion en la Venta Mensual\n Por Dia del Mes", 
       x="Dia del Mes", y="% de Participacion") +
  theme_bw()
gr

#labels = c("ShellCard","ENZO VALVERDE","VICTOR CATALAN","GIOVANNI VALVERDE","NN1","MANUEL MARIN", "NN2","JUAN CRUZ",
#"XXXXXXXXXXXXXXXX","JORGE PINO","ALEJANDRO FIGUEROA","ARIEL CARO","EDUARDO MARDONES","ALFREDO LEON","GONZALO RAQUELICH"))
