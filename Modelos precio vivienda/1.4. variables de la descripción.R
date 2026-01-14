# script de la clase
require(pacman) 
p_load(tidyverse,rio,skimr,viridis,
       gstat, ## variogram
       sf, ## leer/escribir/manipular datos espaciales
       leaflet, ## Visualizaciones dinámicas
       nngeo, ## st_nn function
       spdep, ## Construct neighbours list from polygon list 
       osmdata) ## Get OSM's data

p_load(dplyr)

test <- readRDS("C:/Users/juanc/OneDrive/Documents/Universidad/Maestria/Big Data Machine Learning/Taller 3/test.Rds")
train <- readRDS("C:/Users/juanc/OneDrive/Documents/Universidad/Maestria/Big Data Machine Learning/Taller 3/train.Rds")


# pasar todo a minúsculas en la variable description
train$description <- tolower(train$description)
test$description <- tolower(test$description)

#######

x1 <- "[:space:]+[:digit:]+[:punct:]+[:digit:]+[:space:]+metros" 
x2 <- "[:space:]+[:digit:]+[:punct:]+[:digit:]+[:space:]+mt2"
x3 <- "[:space:]+[:digit:]+[:punct:]+[:digit:]+[:space:]+mts2"
x4 <- "[:space:]+[:digit:]+[:punct:]+[:digit:]+[:space:]+m2" 
x5 <- "[:space:]+[:digit:]+[:punct:]+[:digit:]+[:space:]+mt"
x6 <- "[:space:]+[:digit:]+[:punct:]+[:digit:]+[:space:]+mts"

y1 <- "[:space:]+[:digit:]+[:space:]+metros" 
y2 <- "[:space:]+[:digit:]+[:space:]+mt2"
y3 <- "[:space:]+[:digit:]+[:space:]+mts2"
y4 <- "[:space:]+[:digit:]+[:space:]+m2" 
y5 <- "[:space:]+[:digit:]+[:space:]+mt"
y6 <- "[:space:]+[:digit:]+[:space:]+mts"

z1 <- "[:space:]+[:digit:]+metros" 
z2 <- "[:space:]+[:digit:]+mt2"
z3 <- "[:space:]+[:digit:]+mts2"
z4 <- "[:space:]+[:digit:]+m2" 
z5 <- "[:space:]+[:digit:]+mt"
z6 <- "[:space:]+[:digit:]+mts"

###############


train = train %>% mutate(new_surface = str_extract(string = train$description,
                                                pattern =  paste0(x1,"|",x2,"|",x3,"|",x4,"|",x5,"|",x6,"|",
                                                                  y1,"|",y2,"|",y3,"|",y4,"|",y5,"|",y6,"|",
                                                                  z1,"|",z2,"|",z3,"|",z4,"|",z5,"|",z6)))

sum(table(train$new_surface)) 




test = test %>% mutate(new_surface = str_extract(string = test$description,
                                                pattern =  paste0(x1,"|",x2,"|",x3,"|",x4,"|",x5,"|",x6,"|",
                                                                  y1,"|",y2,"|",y3,"|",y4,"|",y5,"|",y6,"|",
                                                                  z1,"|",z2,"|",z3,"|",z4,"|",z5,"|",z6)))

sum(table(test$new_surface)) 


## borrar todas las m2, metros, metros2, etc en base test

test$ns<-test$new_surface
test$st<-test$surface_total

test$new_surface_2<-str_remove_all(test$new_surface,"metros")
test$new_surface_2<-str_remove_all(test$new_surface_2,"mt2")
test$new_surface_2<-str_remove_all(test$new_surface_2,"mts2")
test$new_surface_2<-str_remove_all(test$new_surface_2,"m2")
test$new_surface_2<-str_remove_all(test$new_surface_2,"mt")
test$new_surface_2<-str_remove_all(test$new_surface_2,"mts")

test$new_surface_2<-str_remove_all(test$new_surface_2, "[\n]")
test$new_surface_2<-str_remove_all(test$new_surface_2, "[ ]")
test$new_surface_2<-str_replace_all(test$new_surface_2, ",", ".")


test$new_surface_2<-str_replace_all(test$new_surface_2, "[:digit:]+[.]+[:digit:]+[:digit:]+[:digit:]",
                                               str_remove_all(test$new_surface_2, "[.]"))

test$new_surface_2<-as.numeric(test$new_surface_2)

summary(test$new_surface_2)


## borrar todas las m2, metros, metros2, etc en base train

train$ns<-train$new_surface
train$st<-train$surface_total

#Elimino todos los patrones de letras para poder extraer el numero de metros unicamente

train$new_surface_2<-str_remove_all(train$new_surface,"metros")
train$new_surface_2<-str_remove_all(train$new_surface_2,"mt2")
train$new_surface_2<-str_remove_all(train$new_surface_2,"mts2")
train$new_surface_2<-str_remove_all(train$new_surface_2,"m2")
train$new_surface_2<-str_remove_all(train$new_surface_2,"mt")
train$new_surface_2<-str_remove_all(train$new_surface_2,"mts")

#Elimina espacio y caracteres especiales
train$new_surface_2<-str_remove_all(train$new_surface_2, "[\n]")
train$new_surface_2<-str_remove_all(train$new_surface_2, "[ ]")
train$new_surface_2<-str_replace_all(train$new_surface_2, ",", ".")



#######


test = test %>% mutate(surface_total2 = ifelse(is.na(new_surface_2),
                                               yes = surface_covered,
                                               no = new_surface_2))


train = train %>% mutate(surface_total2 = ifelse(is.na(new_surface_2),
                                               yes = surface_covered,
                                               no = new_surface_2))




##################


# extraer baños (bathrooms1) usando expresiones regulares

bt <- "[:space:]+[:digit:]+[:space:]+baños"
bt1 <- "[:space:]+[:digit:]+[:space:]+banos"
bt2 <- "[:space:]+[:digit:]+baños"
bt3 <- "[:space:]+[:digit:]+banos"
bt4 <- "[:space:]+[:digit:]+[:space:]+baño"
bt5 <- "[:space:]+[:digit:]+[:space:]+bano"

train = train %>% mutate(bathrooms1 = str_extract(string = train$description,
                                                   pattern =  paste0(bt,"|",bt1,"|",bt2,"|",bt3,"|",bt4,"|",bt5)))

test = test %>% mutate(bathrooms1 = str_extract(string = test$description,
                                                 pattern =  paste0(bt,"|",bt1,"|",bt2,"|",bt3,"|",bt4,"|",bt5)))



### 
test$bathrooms2 <- str_locate(string = test$bathrooms1 , pattern = "2") ##  Locate the positions of pattern matches in a string
test$bathrooms2 <- str_locate(string = test$bathrooms1 , pattern = "3")


# reemplazar área total por área cubierta cuando no tenga info - en train

table(is.na(train$surface_total))
train$surface_total <- ifelse(is.na(train$surface_total),train$surface_covered,train$surface_total)
table(is.na(train$surface_total))
train$surface_total <- ifelse(is.na(train$surface_total),train$new_surface,train$surface_total)
table(is.na(train$surface_total))

# reemplazar área total por área cubierta cuando no tenga info - en test

table(is.na(test$surface_total))
test$surface_total <- ifelse(is.na(test$surface_total),test$surface_covered,test$surface_total)
table(is.na(test$surface_total))
test$surface_total <- ifelse(is.na(test$surface_total),test$new_surface,test$surface_total)
table(is.na(test$surface_total))


# sacar las variables: estrato, piso, ascensor, parqueadero

## estrato
es <- "[:space:]+estrato+[:space:]+[:digit:]"
es1 <- "[:space:]+estrato+[:digit:]"
es2 <- "estrato+[:space:]+[:digit:]"

train = train %>% mutate(estrato = str_extract(string = train$description,
                                                  pattern =  paste0(es,"|",es1,"|",es2)))

test = test %>% mutate(estrato = str_extract(string = test$description,
                                                pattern =  paste0(es,"|",es1,"|",es2)))


## piso
pi <- "[:space:]+piso+[:space:]+[:digit:]"
pi1 <- "[:space:]+piso+[:digit:]"
pi2 <- "piso+[:space:]+[:digit:]"

train = train %>% mutate(piso = str_extract(string = train$description,
                                               pattern =  paste0(pi,"|",pi1,"|",pi2)))

test = test %>% mutate(piso = str_extract(string = test$description,
                                             pattern =  paste0(pi,"|",pi1,"|",pi2)))

## ascensor 
as <- "[:space:]+ascensor"
as1 <- "ascensor+[:space:]"


train = train %>% mutate(ascensor = str_extract(string = train$description,
                                                  pattern =  paste0(as,"|",as1)))

test = test %>% mutate(ascensor = str_extract(string = test$description,
                                                pattern =  paste0(as,"|",as1)))

 # parqueadero
train$description <- str_replace_all(train$description, pattern = "parqueadero" , 
replacement = "parqueaderos")

test$description <- str_replace_all(test$description, pattern = "parqueadero" , 
                                    replacement = "parqueaderos")


pa <- "[:space:]+[:digit:]+[:space:]+parqueadero"
pa1 <- "[:digit:]+[:space:]+parqueadero"


train = train %>% mutate(parqueadero = str_extract(string = train$description,
                                            pattern =  paste0(pa,"|",pa1)))

test = test %>% mutate(parqueadero = str_extract(string = test$description,
                                          pattern =  paste0(pa,"|",pa1)))


##Eliminar missing values
train <- train[!(is.na(train$bathrooms) | is.na(train$surface_total2)),]
test <- test[!(is.na(test$bathrooms) | is.na(test$surface_total2)),]

