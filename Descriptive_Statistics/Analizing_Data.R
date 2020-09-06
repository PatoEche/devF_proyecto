# ------------------------------- #
# Limpiamos el entorno de trabajo #
# ------------------------------- #
rm(list=ls())

# --------- #
# Librerias #
# --------- #
library(tidyverse)
library(dmetar)

# ----------------- #
# Conjunto de datos #
# ----------------- #
dir <- "/Users/israelvargas/Documents/Diplomados-Cursos/DevF/Data\ Science/Poyecto/devF_proyecto/Assets"
dataset <- read.csv(
    paste(
        dir, 
        "Abr2019-Jun2019wet.csv", 
        sep = "/"
      ), 
    stringsAsFactors = TRUE
  )
dataset

str(dataset) 

# -- #
# Eliminando espacios en blanco
# -- #
# Ejmplo: "GAS97RM                "
dataset$Product1 <- trimws(
    dataset$Product1, 
    which = c(
        "both", 
        "left", 
        "right"
      ), 
    whitespace = "[ \t\r\n]"
  )

# ------------------------------- #
# Preparando el conjunto de datos #
# ------------------------------- #
# Filtramos los valores que solo se refieran a productos de gasolina
filter_dataset <- filter(
    dataset, 
    Product1 %in% c(
        "GAS93RM",
        "GAS95RM",
        "GAS97RM",
        "DIXTRA"
      )
  )
filter_dataset

statistics <- summary(filter_dataset)
statistics

# Dado que las fechas son 
