library(readr)
library(tidyverse)
library(gdata)
library(stargazer)
library(haven)
library(caTools)
library(ggplot2)
library(randomForest)


#Cargar datos
df_Ocu <- read_dta("C:/Users/juanc/OneDrive/Documents/Universidad/Maestria/Big Data Machine Learning/Trabajo final/DTA/Ocupados.dta")
df_1 <- read_dta("C:/Users/juanc/OneDrive/Documents/Universidad/Maestria/Big Data Machine Learning/Trabajo final/DTA/Características generales, seguridad social en salud y educación.dta")


#Quitar pensionados, lo que se quiere es esudir el ingreso del sistema pensional
df_Ocu <- df_Ocu[df_Ocu$P6920 != 3,]


#Crear identificaciones unicas para cada observaciony juntar las bases de datos
df_Ocu$ID <- paste(df_Ocu$DIRECTORIO, df_Ocu$HOGAR, sep="_")
df_Ocu$ID <- paste(df_Ocu$ID, df_Ocu$ORDEN, sep="_")
df_1$ID <- paste(df_1$DIRECTORIO, df_1$HOGAR, sep="_")
df_1$ID <- paste(df_1$ID, df_1$ORDEN, sep="_")

df <- merge(df_Ocu, df_1, by="ID")


#Elegir varibles explicativas
df <- dplyr::select(df, c("P3271", "P6040", "P2057", "P3042", "INGLABO",
                   "P6585S2", "P6585S3", "P6920"))
df <- mutate(df, cotiza = P6920, subsidio_familiar = P6585S3, subsidio_transporte = P6585S2,
             ingreso = INGLABO, educacion = P3042, campesino = P2057, edad = P6040, hombre = P3271)
df <- dplyr::select(df, c("cotiza", "subsidio_familiar", "subsidio_transporte",
                          "ingreso", "educacion", "campesino", "edad", "hombre"))


#Eliminar missing values
df <- na.omit(df)


#Cambiar 2 por 0 para las variables factor y cambiar la clase
df$hombre[df$hombre == 2] <- 0
df$cotiza[df$cotiza == 2] <- 0
df$subsidio_familiar[df$subsidio_familiar == 2] <- 0
df$subsidio_transporte[df$subsidio_transporte == 2] <- 0
df$campesino[df$campesino == 2] <- 0

df$hombre <- as.factor(df$hombre)
df$cotiza <- as.factor(df$cotiza)
df$subsidio_familiar <- as.factor(df$subsidio_familiar)
df$subsidio_transporte <- as.factor(df$subsidio_transporte)
df$campesino <- as.factor(df$campesino)
df$educacion <- as.factor(df$educacion)


#Quitar aquellos que no saben, no informan
df <- df[df$educacion != 99,]
df <- df[df$subsidio_familiar != 9,]
df <- df[df$subsidio_transporte!=9,]
df <- df[df$campesino != 9,]


#Solo mayores de edad
df <- df %>% dplyr::filter(edad >= 18)


#Base de entrenaminto y testeo
#siembro semilla y divido
set.seed(12345)
split = sample.split(df$cotiza, SplitRatio = 0.8)
train = subset(df, split == TRUE)
test = subset(df, split == FALSE)


#Formula del modelo
modelo <- as.formula("cotiza ~ subsidio_familiar + subsidio_transporte +
                   ingreso + educacion + campesino + edad + hombre")