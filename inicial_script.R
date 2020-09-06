rm(list=ls())

###
# IMPORTAR LIBRERIAS ----
###
library(tidyverse)
library(dplyr) # More efficient DATA FRAME working
library(reshape) # Rename column
library(data.table) # Modify NA more efficient
library(corrplot)

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
### Verify & Replace NA to 9999
f416 <- replace(f416, is.na(f416), 9999) # 9999 representa asignacion automatica


###
# Estadísticas del dataframe
###
stads <- summary(f416)
stads

###
# Correlacion entre variables qty
###
correlacion <- select(f416, qty, value, date) %>%
  cor(method = c("pearson"))

print(correlacion)
corrplot(correlacion,bg="gray", addgrid.col = "black")

# la correlacion entre 

correlacion <- select(f416, qty, value, id_cashier) %>%
  cor(method = c("pearson"))

print(correlacion)
corrplot(correlacion,bg="gray", addgrid.col = "black")






### Modify date and hour
# library 
library(lubridate)
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

str(f416)

### AGRUPACION Y ANALISIS ESTADISTICO -----------------------------------
#------------------------------------------------------------------------#

### Info about qty per day / abril
qty_day <- f416 %>%
  mutate(month = format(date, "%m"), year = format(date, "%Y"), day = format(date, "%d")) %>%
  group_by(month, day) %>%
  filter(month == '05',) %>%
  summarise(total = sum(qty)
  )

library(ggplot2)
q <- data.frame(qty_day)
p <- ggplot(q)
p <- p + aes(x = day, y = total, fill = total)
p <- p + geom_bar(position = 'dodge',stat = 'identity')
p


### Info about sale, qty or ticket per day
cashier_day <- f416 %>%
  mutate(month = format(date, "%m"), year = format(date, "%Y"), day = format(date, "%d")) %>%
  filter(month == '04' & id_cashier != 0) %>%
  group_by(id_cashier) %>%
  summarise(total_cashier=n(), # sum(qty)=la cantidad de litros por cashier / o tambien comunt=n() el numero de ticket
  )

library(ggplot2)
cd <- data.frame(cashier_day)
cd <- ggplot(cd, aes(x = id_cashier, y = total_cashier, fill = total_cashier)) +
  geom_bar(stat = 'identity')
cd

### Grafico ticket cashier per day

tempo <- f416 %>%
  mutate(month = format(date, "%m"), year = format(date, "%Y"), day = format(date, "%d")) %>%
  filter(month == '04', id_cashier != 0) %>%
  group_by(day, id_cashier) %>%
  summarise(ratio = n()) %>% ### numero de transacciones por cajeros x dia
  mutate(ratio = round(ratio, 0)     
  )

gr <- ggplot(tempo, aes(x=id_cashier, y=ratio)) +
  geom_bar(stat="identity", fill="#41b6c4") +
  geom_text(aes(label=ratio), vjust=-0.3, size=3, angle=0) +
  facet_wrap(~day) +
  labs(title="Promedio ticket cashier per day \n Mes de Abril", 
       x="Cashier", y="Promedio Ticket") +
  theme_bw()
gr
