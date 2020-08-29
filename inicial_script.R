rm(list=ls())

### Import file csv
setwd('~')
dir1 <- paste(getwd(), "/DEVF/Curso Data Science/Proyecto/devF_proyecto/Assets", sep="")
WD <- normalizePath(dir1, winslash = "\\", mustWork = NA)
f416 <- read.csv(paste(WD, "Abr2019-Jun2019wet.csv", sep="\\"), stringsAsFactors = FALSE)

library('tidyverse')
library(dplyr) #manejo mas eficaz de dataframe


str(f416)
sapply(f416, function(x) sum(is.na(x)))



### SECCION DE MANIPULACION DE DATOS ELIMINAR SUMAR BORRAR AGREGAR CAMBIAR ########
#-----------------------------------------------------------------------------------#

### ELIMINACION DE COLUMNAS
f416 <- f416 %>%
  select(-MOP21) %>%
  select(-MOP31)

### Sumar cantidad de datos de una columna
f416$Name1=="MANUELMARIN"
sum(f416$Name1=="MANUELMARIN")

### Modificar los valores eliminando o agregando caracteres
f416$Name1 = gsub('"', '', f416$Name1)
f416$Name1 = gsub(' ', '', f416$Name1)

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

#### Tratamientos de NA
#--------------------------------------------------------------------------

