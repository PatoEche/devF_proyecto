rm(list=ls())

### Import file csv
setwd('~')
dir1 <- paste(getwd(), "/DEVF/Curso Data Science/Proyecto/devF_proyecto/Assets", sep="")
WD <- normalizePath(dir1, winslash = "\\", mustWork = NA)
f416 <- read.csv(paste(WD, "Abr2019-Jun2019wet.csv", sep="\\"), stringsAsFactors = FALSE)

str(f416)
