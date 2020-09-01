rm(list=ls())

### IMPORTAR DATOS - LIBRARY -----------------------------------------------

setwd('~')
dir1 <- paste(getwd(), "/DEVF/Curso Data Science/Proyecto/devF_proyecto/Assets", sep="")
WD <- normalizePath(dir1, winslash = "\\", mustWork = NA)
file416 <- read.csv(paste(WD, "Abr2019-Jun2019wet.csv", sep="\\"), stringsAsFactors = FALSE)

library('tidyverse')
library(dplyr) # More efficient DATA FRAME working
library(reshape) # Rename column
library(data.table) # Modify de NA more efficient

### First view
str(file416)

### Create a DataFrame just with necessary columns
f416 <- select(file416, ï..Date2, Textbox70, Product1, Textbox44, Pump1, Cashier1, Name1, MOP11)

### Verificacion de Nulos
sapply(f416, function(x) sum(is.na(x)))


### SECCION DE MANIPULACION DE DATOS ELIMINAR SUMAR BORRAR AGREGAR CAMBIAR ########
#-----------------------------------------------------------------------------------#

### Delete unnecessary column
#f416 <- f416 %>%
#  select(-MOP21) %>%
# select(-MOP31) %>%

### Modificacion de nombres de columnas
f416 <- rename(f416, c(date=ï..Date2))
f416 <- rename(f416, c(hour=Textbox70))
f416 <- rename(f416, c(product=Product1))
f416 <- rename(f416, c(value=Textbox44))
f416 <- rename(f416, c(pump=Pump1))
f416 <- rename(f416, c(cod_cashier=Cashier1))
f416 <- rename(f416, c(name_cashier=Name1))
f416 <- rename(f416, c(media_pay=MOP11))


### Sumar cantidad de datos de una columna
sum(f416$name_cashier=="MANUEL MARIN")

### Modificar los valores eliminando o agregando caracteres
# f416$name_cashier = gsub('"', '', f416$name_cashier)
# f416$name_cashier = gsub(' ', '', f416$name_cashier)


### Bucle para eliminar espacion en blancos al inicio y final de una cadena string
for(i in 1:nrow(f416)) {
  f416$media_pay[i] <- trimws(x = f416$media_pay[i])
  f416$cod_cashier[i] <- trimws(x = f416$cod_cashier[i])
  f416$product[i] <- trimws(x = f416$product[i])
}
str(f416)
backupF416 <- data.frame(f416)

### Modify value float
f416$value = gsub(',00', '', f416$value)
f416$value = gsub('\\.', '', f416$value)

### Modify char in number
fil416 <- transform(f416, value = as.numeric(value), 
          cod_cashier = as.numeric(cod_cashier))

### Replace NA to 0
fi416 <- replace(fil416, is.na(fil416), 0)



str(fi416)
print(fi416$value[1])
### Data and hour must be checked at future

#------------------------------------------------------------------------#
#------------------------------------------------------------------------#


sapply(f416, function(x) sum(is.na(x)))

colnames(f416)
rowname(f416)
f416[,2]
class(f416[,2])
is.numeric(f416[,2])
table(f416[,2])
table(f416[,3])
table(f416[,3])


#### Creamos un bucle que recorrera la dimension completa (n columnas) de nuestra dataframe
#### imprimiendo el nombre de la columna y una tabal con la distribucion absoluta de c/column
for(i in 1:dim(f416)[2]) {
  print(colnames(f416)[i])
  print(table(f416[,i]))
}


