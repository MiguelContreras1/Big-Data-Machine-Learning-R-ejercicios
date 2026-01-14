# LIMPIEZA DE DATOS Y CREACIÓN DE VARIABLES
rm(list = ls())

# instalar y cargar paquetes

install.packages("pacman") 
library(pacman)
library(here)

# cargar bases 

train <- readRDS(here("C:/Users/MIGUEL  CONTRERAS/Desktop/Taller-3-BDML/stores/train.Rds"))
test <- readRDS(here("C:/Users/MIGUEL  CONTRERAS/Desktop/Taller-3-BDML/stores/test.Rds"))

# inspeccionar las bases

View(train)
View(test)
head(train)
head(test)
colnames(train)
colnames(test)

# ver los missing values 

skim(train)
skim(test)



# tratamiento de datos de la descripción para sacar las variables. DESCRIPTION







# IMPUTACIÓN DE DATOS


## Base train

# surface1

# barthrooms1

# property_type

# estrato

# piso

# ascensor

# parqueadero


## Base test

# surface1

# barthrooms1

# property_type

# estrato

# piso

# ascensor

# parqueadero



## Crear variables con DATOS ESPACIALES

# dista_park

# dista_park - train

# dista_park - test



# dista_bus

# dista_bus - train

# dista_bus - test



# dista_salud

# dista_salud - train

# dista_salud - test



# dista_policia

# dista_policia - train

# dista_policia - test



# dista_banco

# dista_banco - train

# dista_banco - test




# dista_bar

# dista_bar - train

# dista_bar - test



# partir la base train en training y testiing, luego se evalúa dentro de muestra (sobre la de testing) y fuera de muestra (test)



