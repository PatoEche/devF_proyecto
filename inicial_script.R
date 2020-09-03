rm(list=ls())

### IMPORTAR DATOS - LIBRARY -----------------------------------------------

setwd('~')
dir1 <- paste(getwd(), "/DEVF/Curso Data Science/Proyecto/devF_proyecto/Assets", sep="")
WD <- normalizePath(dir1, winslash = "\\", mustWork = NA)
file416 <- read.csv(paste(WD, "Abr2019-Jun2019wet.csv", sep="\\"), stringsAsFactors = FALSE)

library(tidyverse)
library(dplyr) # More efficient DATA FRAME working
library(reshape) # Rename column
library(data.table) # Modify NA more efficient

### First view
str(file416)

### Create a DataFrame just with necessary columns and modify name
f416 <- select(file416, ï..Date2, Textbox70, Product1,Textbox42, Textbox44, Pump1, Cashier1, Name1, MOP11)

str(f416)

### SECCION DE MANIPULACION DE DATOS ELIMINAR SUMAR BORRAR AGREGAR CAMBIAR ########
#-----------------------------------------------------------------------------------#

### Modificacion de nombres de columnas
name_head <- colnames(f416)
new_name_head <- c('date', 'hour', 'product', 'qty', 'value', 'pump', 'id_cashier','name_cashier', 'pay')
names(f416)[names(f416) == name_head] <- new_name_head


### Modificar los valores eliminando o agregando caracteres
# f416$name_cashier = gsub('"', '', f416$name_cashier)
# f416$name_cashier = gsub(' ', '', f416$name_cashier)


### Bucle para eliminar espacion en blancos al inicio y final de una cadena string
for(i in 1:nrow(f416)) {
  f416$pay[i] <- trimws(x = f416$pay[i])
  f416$id_cashier[i] <- trimws(x = f416$id_cashier[i])
  f416$product[i] <- trimws(x = f416$product[i])
}

#-------------------
str(f416)
backupF416 <- data.frame(f416)
f416 <- data.frame(backupF416)
#---------------------


### Modify value
f416$value = gsub(',00', '', f416$value)
f416$value = gsub('\\.', '', f416$value)
f416$qty = gsub(',', '\\.', f416$qty)

### Modify char
f416 <- transform(f416, value = as.numeric(value), 
          id_cashier = as.numeric(id_cashier),
          qty = as.numeric(qty))

### Verify & Replace NA to 0
sapply(f416, function(x) sum(is.na(x)))
f416 <- replace(f416, is.na(f416), 0)

### Modify date and hour
library(lubridate)
f416$date <- as.Date(f416$date, format = '%d/%m/%Y')


### AGRUPACION Y ANALISIS ESTADISTICO -----------------------------------
#------------------------------------------------------------------------#

#### Creamos un bucle que recorrera la dimension completa (n columnas) de nuestra dataframe
#### imprimiendo el nombre de la columna y una tabal con la distribucion absoluta de c/column
for(i in 1:dim(fi416)[2]) {
  print(colnames(fi416)[i])
  print(table(fi416[,i]))
}

f416 %>% 
  group_by(cod_cashier) %>%
  summarise(n_transacciones=n(),
            ) %>%
  filter(n_transacciones > 0,
         )

fi416 %>% 
  group_by(date) %>%
  summarise(n_transacciones=n(),
            mean = mean(value)
  ) %>%
  filter(date < '05/04/2019',
  )

f416 %>% 
  group_by(qty) %>%
  summarise(n_transacciones=n(),
            ) %>%
  filter(n_transacciones > 0,
        )

value_day_hour <- f416 %>%
  group_by(date) %>%
#  group_by(hour) %>%
  summarise(sum(value))
  
  