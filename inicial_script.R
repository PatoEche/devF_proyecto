rm(list=ls())

################################## IMPORTAR LIBRERIAS #######################################

library(tidyverse)
library(dplyr) # More efficient DATA FRAME working
library(reshape) # Rename column
library(data.table) # Modify NA more efficient
library(corrplot)
library(lubridate)
library(ggplot2)

################################# IMPORTAR DATOS ############################################

setwd('~')
dir1 <- paste(getwd(), "/DEVF/Curso Data Science/Proyecto/devF_proyecto/Assets", sep="")
dir2 <- paste(getwd(), "/DEVF/Curso Data Science/Proyecto/devF_proyecto/Graph", sep="")
WD <- normalizePath(dir1, winslash = "\\", mustWork = NA)
WD2 <- normalizePath(dir2, winslash = "\\", mustWork = NA)
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


################## SECCION DE MANIPULACION DE DATOS Y ANALISIS EXPLORATORIO ##########################3

###
# Descripción del dataset
###
head(f416)
str(f416)
print(paste('Número de filas en el dataframe', nrow(f416), sep=": "))
print(paste('Número de columnas en el dataframe', ncol(f416), sep=": "))

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


### Modificacion Formato fecha, Transformamos fecha en formato R
f416$date <- as.Date(f416$date, format = '%d/%m/%Y')
# Incluir evento (laboral =0, festivo =1, findesemana =2)
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
# Estadísticas del dataframe
###
stads <- summary(f416)
stads

############################ AGRUPACION Y ANALISIS DESCRIPTIVO ##############################


####
# PREGUNTA: Hay dias del mes con mayor frecuencia?
# PREGUNTA: Son estos dias frecuentes a lo largo del anio?
# Hipotesis: Hay periodos, como las quincenas y fin de mes que la venta aumenta
###

# Distribucion de la participacion por dia en cada mes
litros <- f416 %>%
  filter(month != 7) %>%
  group_by(month,day, date) %>%
  summarise(litro_dia = sum(qty)) %>%
  ungroup() %>%
  group_by(month) %>%
  mutate(litros_mes= sum(litro_dia),
         promedio = round((litro_dia/litros_mes)*100, 1)
  )
litros$day <- factor(litros$day,
                     levels = litros$day,
                     #labels = as.Date.character(litros$date)
                     labels = lubridate::days(litros$day)
                )
litros$month <- factor(litros$month,
                       levels = c(4,5,6,7),
                       labels = c("Abril","Mayo","Junio","Julio")
                )

gr <- ggplot(litros, aes(x=day, y=promedio)) +
  geom_bar(stat="identity", fill="#41b6c4") +
  geom_text(aes(label=promedio), vjust=-0.3, size=2, angle=0) +
  facet_wrap(~month) +
  labs(title="Distribucion de venta diaria \n Trimestre Abril - Junio", 
       x="Dias", y="% Participacion en el Mes") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90))

gr
ggsave(paste(WD2, "distribucionventaspordia.png", sep="\\"), plot=gr, width=12, height=12)
summary(litros)
### O                                                                                                                                        mana, Festivos y fin de semanas largo, baja participacion
###       Promedio diario: 17000 ltr / equivalente al 3,3% mensual, es decir, bajo esto son dias de baja venta
      

###
# PREGUNTA: Cual es la distribucion de ventas de cada cajero a lo largo del tiempo?
# Hipotesis: Hay distintos tipos de cajeros (Lentos y Rapidos)
###

# Distribucion de la cantidad de ticket de cada cajero
tempo <- f416 %>%
  filter(month != 7) %>%
  group_by(month, id_cashier) %>%
  summarise(total_cashier=n(),
  )

tempo$id_cashier <- factor(tempo$id_cashier,
                           levels = c(1000, 1001, 1002, 1003, 1005,  1006, 1008, 1009,
                                      1011, 1012, 1014, 1016, 1018, 1019, 1020),
                           labels = c("ShCa","ENVA","VICA","GIVA","NN1","MAMA", "NN2","JUCR",
                                      "NN3","JOPI","ALFI","ARCA","EDMA","ALLE","GORA")
)

tempo$month <- factor(tempo$month,
                           levels = c(4,5,6,7),
                           labels = c("Abril","Mayo","Junio","Julio")
)

line <- ggplot(tempo, aes(x=month,y=total_cashier,group=id_cashier,color=id_cashier)) +
        geom_line() +
        labs(title="Cantidad de Ticket",subtitle='Trimestre Abr-Jun',
        x="Mes",y="N Ticket", group='Cajero') +
        theme_bw()

line
ggsave(paste(WD2, "distribucionticketporcajero.png", sep="\\"), plot=line, width=12, height=12)

###
# Visualizacion Mensual de cantidad de Ticket por cajero
###

tempo <- f416 %>%
  filter(month == 4) %>%
  group_by(id_cashier) %>%
  summarise(total_cashier=n(),
  )

tempo$id_cashier <- factor(tempo$id_cashier,
                    levels = c(1000, 1001, 1002, 1003, 1005,  1006, 1008, 1009,
                             1011, 1012, 1014, 1016, 1018, 1019, 1020),
                    labels = c("ShCa","ENVA","VICA","GIVA","NN1","MAMA", "NN2","JUCR",
                             "NN3","JOPI","ALFI","ARCA","EDMA","ALLE","GORA")
                    )

bar <- ggplot(tempo, aes(x = id_cashier, y = total_cashier, fill = total_cashier)) +
      geom_bar(stat = 'identity') +
      labs(title="Número de ventas registradas por Cajero",subtitle='Abril',
           x="Identificador Cajero",y="Transacciones")
bar
summary(tempo)
ggsave(paste(WD2, "ventasporcajeroMES.png", sep="\\"), plot=bar, width=12, height=12)

### OBS: El promedio de venta por cajero es de 1335 litros en el mes de Abril. Sobre esto es mas eficiente, bajo menors eficiente
###       Tambien se debe considerar los dias trabajados de cada cajero (considera Mes de Abril)


###
# PREGUNTA: Cual es la distribucion semanal en las ventas de cada cajero?
# Hipotesis: Cajeros con mayor numero de ticket durante los dias de semana
###

# Matriz de color, Venta cajero por dia de semana

tempo <- f416 %>%
  filter(month != 7) %>%
  group_by(day_week, id_cashier) %>%
  summarise(ticket = n(), litros_cajero = sum(qty)
  )

tempo$id_cashier <- factor(tempo$id_cashier,
                           levels = c(1000, 1001, 1002, 1003, 1005,  1006, 1008, 1009,
                                      1011, 1012, 1014, 1016, 1018, 1019, 1020),
                           labels = c("ShCa","ENVA","VICA","GIVA","NN1","MAMA", "NN2","JUCR",
                                      "NN3","JOPI","ALFI","ARCA","EDMA","ALLE","GORA")
                    )

mt <- ggplot(tempo, aes(id_cashier,day_week)) +
      geom_tile(aes(fill = ticket)) + 
      scale_fill_gradient(low = "green", high = "red") +
      theme_classic()
mt
ml <- ggplot(tempo, aes(id_cashier,day_week)) +
  geom_tile(aes(fill = litros_cajero)) + 
  scale_fill_gradient(low = "green", high = "red") +
  theme_classic()
ml
summary(tempo)
ggsave(paste(WD2, "ticketpordiasemana.png", sep="\\"), plot=mt, width=12, height=12)
ggsave(paste(WD2, "litrospordiasemana.png", sep="\\"), plot=ml, width=12, height=12)

### OBS: El domingo es el dia con menos ticket y litros vendidos, las semana registra el area de sobrecompra
###       El promedio de ticket es de 564 diarios

###
# PREGUNTA: Cual es la participacion de cada cajero en cada dia de la semana?
# Hipotesis: Los cajeros mantienen una participacion mayor durante la semana.
###

# Grafico litros por cajero por dia en un Mes
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

gr <- ggplot(tempo, aes(x=id_cashier, y=ratio)) +
  geom_bar(stat="identity", fill="#41b6c4") +
  geom_text(aes(label=ratio), vjust=-0.3, size=2, angle=0) +
  facet_wrap(~day) +
  labs(title="Promedio por Cajero \n Venta Diaria  Mes de Abril", 
       x="Cajero", y="% por Cajero") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90))
gr

summary(tempo)
ggsave(paste(WD2, "ratioventascajerospormes.png", sep="\\"), plot=gr, width=12, height=12)

## OBS: La participacion promedio diaria de cada cajero debe ser superior al 9,5 de la venta diaria, para ser eficiente
###     O los litros diarios de cada cajero deben superar los 1630 litros
###     Destacar que un ticket equivale a una venta, esto quiere decir que vender 10 ticket de 10 litros equivale a vender
###     solo un ticket de 100 litros. Se debe considerar al calcular la eficiencia de los cajeros.


############## DEFINICION Y CALCULO DE VARIABLE OBJETIVO ############################

###
# PREGUNTA: Existe diferencia entre los cajeros y su participacion en la venta total de cada mes?
# PREGUNTA: Es esta diferencia constante en el tiempo?
# Hipotesis: Hay cajeros que aportan mas a las ventas y son consistentes en el tiempo, es decir,
# son mas eficientes (Mayor cantidad de Ventas o litros, nos permite catregorizar por eficiencia)
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
  theme_bw() +
  theme(axis.text.x = element_text(angle=90))
gr

summary(tempo)
ggsave(paste(WD2, "participacionMensualporCajero.png", sep="\\"), plot=gr, width=12, height=12)

### OBS: La participacion promedio podria servir como rango de categorizacion de una variable objetivo EFICIENCIA
###       El porcentaje de participacion sobre el promedio 7,3 podrias er considerado mas eficiente


###
# PREGUNTA: Cantidad de turnos trabajados en el mes de cada cajero?
# Hipotesis: Aun cuando los cajeros trabajan una cantidad diferente de turnos, hay cajeros 
#             con mejor eficiencia
###

# Ticket diarios por cajero
tempo <- f416 %>%
        filter(month != 7) %>%
        group_by(month, day, id_cashier) %>%
        summarise(ticket_diarios = n(), litros_diarios=sum(qty))

tempo$id_cashier <- factor(tempo$id_cashier,
                           levels = c(1000, 1001, 1002, 1003, 1005,  1006, 1008, 1009,
                                      1011, 1012, 1014, 1016, 1018, 1019, 1020),
                           labels = c("ShCa","ENVA","VICA","GIVA","NN1","MAMA", "NN2","JUCR",
                                      "NN3","JOPI","ALFI","ARCA","EDMA","ALLE","GORA")
)

gt <- ggplot(tempo, aes(x=id_cashier, y=ticket_diarios)) +
  geom_point()
gt
gl <- ggplot(tempo, aes(x=id_cashier, y=litros_diarios)) +
  geom_point()
gl

summary(tempo)
ggsave(paste(WD2, "ticketdiariosCajero.png", sep="\\"), plot=gt, width=12, height=12)
ggsave(paste(WD2, "litrosdiariosCajero.png", sep="\\"), plot=gl, width=12, height=12)

### OBS: El promedio de ticket diarios es de 61,73 por cajero (considera 3 meses)
###       El promedio de litros diarios es de 1679 por casjero (considera 3 meses)

# Ticket Mensuales por cajero

tempo2 <- f416 %>%
  filter(month != 7) %>%
  group_by(month, day, id_cashier) %>%
  summarise(ticket_diarios = n(), litros_diarios=sum(qty)) %>%
  ungroup() %>%
  group_by(month, id_cashier) %>%
  summarise(ticket_periodo = sum(ticket_diarios), litros_periodo= sum(litros_diarios),
            litros_ticket=round((litros_periodo/litros_diarios),1))


tempo2$id_cashier <- factor(tempo2$id_cashier,
                           levels = c(1000, 1001, 1002, 1003, 1005,  1006, 1008, 1009,
                                      1011, 1012, 1014, 1016, 1018, 1019, 1020),
                           labels = c("ShCa","ENVA","VICA","GIVA","NN1","MAMA", "NN2","JUCR",
                                      "NN3","JOPI","ALFI","ARCA","EDMA","ALLE","GORA")
)
gt <- ggplot(tempo2, aes(x=id_cashier, y=ticket_periodo)) +
  geom_point()
gt
gl <- ggplot(tempo2, aes(x=id_cashier, y=litros_periodo)) +
  geom_point()
gl

summary(tempo2)
ggsave(paste(WD2, "ticketPeriodoCajero.png", sep="\\"), plot=gt, width=12, height=12)
ggsave(paste(WD2, "litrosPeriodoCajero.png", sep="\\"), plot=gl, width=12, height=12)

### OBS: Algunos cajeros tienen todos los meses mas de 1500, el promedio es 1390 tickey y 37810 litros mensuales
###     Se puede extraer que en el periodo Abril - Junio -> a venta promedio por ticket es de 27,2 litros


# Dias trabajados por cajero
# Porcentaje dias trabajados del cajero en el periodo

tempo3 <- f416 %>%
  filter(month != 7) %>%
  group_by(id_cashier, month, day) %>%
  summarise(ticket_diarios = n(), t = sum(qty))%>%
  group_by(id_cashier) %>%
  summarise(dias_trabajados=n(), total=sum(t)) %>%
  mutate(promedio_trabajados= round((dias_trabajados/(length(unique(f416$date))-1))*100, 1),
         eficiencia_litros = round((total/dias_trabajados),1))
  
# Total de turnos realizados por todos los cajeros en 3 meses >>> 923
# sum(tempo3$dias_trabajados)

tempo3$id_cashier <- factor(tempo3$id_cashier,
                           levels = c(1000, 1001, 1002, 1003, 1005,  1006, 1008, 1009,
                                      1011, 1012, 1014, 1016, 1018, 1019, 1020),
                           labels = c("ShCa","ENVA","VICA","GIVA","NN1","MAMA", "NN2","JUCR",
                                      "NN3","JOPI","ALFI","ARCA","EDMA","ALLE","GORA")
)

gp <- ggplot(tempo3, aes(x=id_cashier, y=promedio_trabajados)) +
  geom_point()
gp
gt <- ggplot(tempo3, aes(x=id_cashier, y=dias_trabajados)) +
  geom_point()
gt

summary(tempo3)

ggsave(paste(WD2, "promedioTrabajadoPeriodo.png", sep="\\"), plot=gt, width=12, height=12)
ggsave(paste(WD2, "diasTrabajadosCajero.png", sep="\\"), plot=gl, width=12, height=12)

### OBS: Podemos observar que el promedio de dias trabajados ed del 67,59% del todal de dias
###       O equivalente a 61 dias de los 91 dias (3 meses Abril-Junio)


###
############################ CREACION DATA FRAME PARA EL ALGORITMO #######################
###

temporal <- f416
temporal <- filter(temporal,month!=7)
temporal$id_cashier <- factor(temporal$id_cashier,
                            levels = c(1000, 1001, 1002, 1003, 1005,  1006, 1008, 1009,
                                       1011, 1012, 1014, 1016, 1018, 1019, 1020),
                            labels = c("ShCa","ENVA","VICA","GIVA","NN1","MAMA", "NN2","JUCR",
                                       "NN3","JOPI","ALFI","ARCA","EDMA","ALLE","GORA")
)
tempo3 <-select(tempo3, -dias_trabajados, -total, -promedio_trabajados)
temporal <- merge(temporal, tempo3, by="id_cashier")


# Transformar columna event y eficiencia en dummy para utilizar en Kmean

temporal <- temporal %>%
            mutate(valor_eficiencia=1,
                  valor_event=1) %>%
            spread(key= eficiencia_litros, value=valor_eficiencia) %>%
            spread(key=event, value=valor_event)
    