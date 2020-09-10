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
str(f416)

fes_2019 <- c('01/01/2019','19/04/2019','20/04/2019','01/05/2019','21/05/2019',
              '07/06/2019','29/06/2019','16/07/2019','15/08/2019','20/08/2019',  
              '18/09/2019','19/09/2019','20/09/2019','29/09/2019','12/10/2019',  
              '31/10/2019','01/11/2019','08/12/2019','25/12/2019','31/12/2019')

fes_2019 <- as.Date(fes_2019, format = '%d/%m/%Y')

sand_2019 <- for (i in fes_2019[i]) {
                if(wday(fes_2019[i])) 
}
# esta es la clasificacion que debo hacer
# si fes_2019 es martes dia anterior sandwish
# si fes_2019 es jueves dia posterior sandwish
  
  or(i in 1:nrow(f416)) {
    #  f416$pay[i] <- trimws(x = f416$pay[i])
    #  f416$id_cashier[i] <- trimws(x = f416$id_cashier[i])
    #  f416$product[i] <- trimws(x = f416$product[i])
    #}
# Include a number 1 (festivo), in event
f416[f416$date %in% as.Date(fes_2019, format = '%d/%m/%Y') && wday(fes_2019) ==, 'event'] <- 1
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



######################## FUNCION FOR PARA ELIMINAR ESPACIOS EN BLANCOS ############################


### Bucle para eliminar espacion en blancos al inicio y final de una cadena string
for(i in 1:nrow(f416)) {
  f416$pay[i] <- trimws(x = f416$pay[i])
  f416$id_cashier[i] <- trimws(x = f416$id_cashier[i])
  f416$product[i] <- trimws(x = f416$product[i])
}
###  reemplazado por -> strip.white = TRUE en las seccion de carga de BD
# Aporte de IsraAgus
