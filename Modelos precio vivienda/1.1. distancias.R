require(pacman)

## llamar y/o instalar librerias
p_load(tidyverse,rio,skimr,
       sf, ## datos espaciales
       leaflet, ## visualizaciones
       tmaptools, ## geocodificar
       ggsn, ## map scale bar 
       osmdata) ## packages with census data

#Dividir por ciudad
train_bg <- train[train$city == "Bogotá D.C",]
train_md <- train[train$city == "Medellín",]


#Crear sf de hogares train
hogares_bg_train <- st_as_sf(x=train_bg, coords = c("lon", "lat"), crs=4326)
hogares_md_train <- st_as_sf(x=train_md, coords = c("lon", "lat"), crs=4326)


#Crear sf de hogares test
hogares_test <- st_as_sf(x=test, coords = c("lon", "lat"), crs=4326)


#Crear sf puntos de interes Bogotá
parques_bg <- opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key = "leisure", value = "park") %>%
  osmdata_sf() %>% .$osm_polygons %>% dplyr::select(osm_id,name)

estaciones_bus_bg <- opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key = "amenity", value = "bus_station") %>%
  osmdata_sf() %>% .$osm_polygons %>% dplyr::select(osm_id,name)

hospitales_bg <- opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key = "amenity", value = "hospital") %>%
  osmdata_sf() %>% .$osm_polygons %>% dplyr::select(osm_id,name)

estaciones_policia_bg <- opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key = "amenity", value = "police") %>%
  osmdata_sf() %>% .$osm_polygons %>% dplyr::select(osm_id,name)

bancos_bg <- opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key = "amenity", value = "bank") %>%
  osmdata_sf() %>% .$osm_polygons %>% dplyr::select(osm_id,name)


#Crear sf puntos de interes Medellín
parques_md <- opq(bbox = getbb("Medellin Colombia")) %>%
  add_osm_feature(key = "leisure", value = "park") %>%
  osmdata_sf() %>% .$osm_polygons %>% dplyr::select(osm_id,name)

estaciones_bus_md <- opq(bbox = getbb("Medellin Colombia")) %>%
  add_osm_feature(key = "amenity", value = "bus_station") %>%
  osmdata_sf() %>% .$osm_polygons %>% dplyr::select(osm_id,name)

hospitales_md <- opq(bbox = getbb("Medellin Colombia")) %>%
  add_osm_feature(key = "amenity", value = "hospital") %>%
  osmdata_sf() %>% .$osm_polygons %>% dplyr::select(osm_id,name)

estaciones_policia_md <- opq(bbox = getbb("Medellin Colombia")) %>%
  add_osm_feature(key = "amenity", value = "police") %>%
  osmdata_sf() %>% .$osm_polygons %>% dplyr::select(osm_id,name)

bancos_md <- opq(bbox = getbb("Medellin Colombia")) %>%
  add_osm_feature(key = "amenity", value = "bank") %>%
  osmdata_sf() %>% .$osm_polygons %>% dplyr::select(osm_id,name)


#Crear sf puntos de interes Cali
parques_cl <- opq(bbox = getbb("Cali Colombia")) %>%
  add_osm_feature(key = "leisure", value = "park") %>%
  osmdata_sf() %>% .$osm_polygons %>% dplyr::select(osm_id,name)

estaciones_bus_cl <- opq(bbox = getbb("Cali Colombia")) %>%
  add_osm_feature(key = "amenity", value = "bus_station") %>%
  osmdata_sf() %>% .$osm_polygons %>% dplyr::select(osm_id,name)

hospitales_cl <- opq(bbox = getbb("Cali Colombia")) %>%
  add_osm_feature(key = "amenity", value = "hospital") %>%
  osmdata_sf() %>% .$osm_polygons %>% dplyr::select(osm_id,name)

estaciones_policia_cl <- opq(bbox = getbb("Cali Colombia")) %>%
  add_osm_feature(key = "amenity", value = "police") %>%
  osmdata_sf() %>% .$osm_polygons %>% dplyr::select(osm_id,name)

bancos_cl <- opq(bbox = getbb("Cali Colombia")) %>%
  add_osm_feature(key = "amenity", value = "bank") %>%
  osmdata_sf() %>% .$osm_polygons %>% dplyr::select(osm_id,name)


#Distancias medias con puntos de interes bogota
matrix_dist_parque_bg <- st_distance(x=hogares_bg_train , y=parques_bg)
min_dist_parque_bg <- apply(matrix_dist_parque_bg , 1 , min)
hogares_bg_train$distancia_parque <- min_dist_parque_bg
train_bg$distancia_parque <- min_dist_parque_bg

matrix_dist_estaciones_bus_bg <- st_distance(x=hogares_bg_train , y=estaciones_bus_bg)
min_dist_estaciones_bus_bg <- apply(matrix_dist_estaciones_bus_bg , 1 , min)
hogares_bg_train$distancia_estacion_bus <- min_dist_estaciones_bus_bg
train_bg$distancia_estacion_bus <- min_dist_estaciones_bus_bg

matrix_dist_hospitales_bg <- st_distance(x=hogares_bg_train , y=hospitales_bg)
min_dist_hospitales_bg <- apply(matrix_dist_hospitales_bg , 1 , min)
hogares_bg_train$distancia_hospital <- min_dist_hospitales_bg
train_bg$distancia_hospital <- min_dist_hospitales_bg

matrix_dist_estaciones_policia_bg <- st_distance(x=hogares_bg_train , y=estaciones_policia_bg)
min_dist_estaciones_policia_bg <- apply(matrix_dist_estaciones_policia_bg , 1 , min)
hogares_bg_train$distancia_estacion_policia <- min_dist_estaciones_policia_bg
train_bg$distancia_estacion_policia <- min_dist_estaciones_policia_bg

matrix_dist_bancos_bg <- st_distance(x=hogares_bg_train , y=bancos_bg)
min_dist_bancos_bg <- apply(matrix_dist_bancos_bg , 1 , min)
hogares_bg_train$distancia_banco <- min_dist_bancos_bg
train_bg$distancia_banco <- min_dist_bancos_bg


#Distancias medias con puntos de interes medellin
matrix_dist_parque_md <- st_distance(x=hogares_md_train , y=parques_md)
min_dist_parque_md <- apply(matrix_dist_parque_md , 1 , min)
hogares_md_train$distancia_parque <- min_dist_parque_md
train_md$distancia_parque <- min_dist_parque_md

matrix_dist_estaciones_bus_md <- st_distance(x=hogares_md_train , y=estaciones_bus_md)
min_dist_estaciones_bus_md <- apply(matrix_dist_estaciones_bus_md , 1 , min)
hogares_md_train$distancia_estacion_bus <- min_dist_estaciones_bus_md
train_md$distancia_estacion_bus <- min_dist_estaciones_bus_md

matrix_dist_hospitales_md <- st_distance(x=hogares_md_train , y=hospitales_md)
min_dist_hospitales_md <- apply(matrix_dist_hospitales_md , 1 , min)
hogares_md_train$distancia_hospital <- min_dist_hospitales_md
train_md$distancia_hospital <- min_dist_hospitales_md

matrix_dist_estaciones_policia_md <- st_distance(x=hogares_md_train , y=estaciones_policia_md)
min_dist_estaciones_policia_md <- apply(matrix_dist_estaciones_policia_md , 1 , min)
hogares_md_train$distancia_estacion_policia <- min_dist_estaciones_policia_md
train_md$distancia_estacion_policia <- min_dist_estaciones_policia_md

matrix_dist_bancos_md <- st_distance(x=hogares_md_train , y=bancos_md)
min_dist_bancos_md <- apply(matrix_dist_bancos_md , 1 , min)
hogares_md_train$distancia_banco <- min_dist_bancos_md
train_md$distancia_banco <- min_dist_bancos_md


#Unir las bases nuevamente
hogares_train <- rbind(hogares_bg_train, hogares_md_train)
train <- rbind(train_bg, train_md)

#Distancias medias con puntos de interes cali
matrix_dist_parque_cl <- st_distance(x=hogares_test , y=parques_cl)
min_dist_parque_cl <- apply(matrix_dist_parque_cl , 1 , min)
hogares_test$distancia_parque <- min_dist_parque_cl
test$distancia_parque <- min_dist_parque_cl

matrix_dist_estaciones_bus_cl <- st_distance(x=hogares_test , y=estaciones_bus_cl)
min_dist_estaciones_bus_cl <- apply(matrix_dist_estaciones_bus_cl , 1 , min)
hogares_test$distancia_estacion_bus <- min_dist_estaciones_bus_cl
test$distancia_estacion_bus <- min_dist_estaciones_bus_cl

matrix_dist_hospitales_cl <- st_distance(x=hogares_test , y=hospitales_cl)
min_dist_hospitales_cl <- apply(matrix_dist_hospitales_cl , 1 , min)
hogares_test$distancia_hospital <- min_dist_hospitales_cl
test$distancia_hospital <- min_dist_hospitales_cl

matrix_dist_estaciones_policia_cl <- st_distance(x=hogares_test , y=estaciones_policia_cl)
min_dist_estaciones_policia_cl <- apply(matrix_dist_estaciones_policia_cl , 1 , min)
hogares_test$distancia_estacion_policia <- min_dist_estaciones_policia_cl
test$distancia_estacion_policia <- min_dist_estaciones_policia_cl

matrix_dist_bancos_cl <- st_distance(x=hogares_test , y=bancos_cl)
min_dist_bancos_cl <- apply(matrix_dist_bancos_cl , 1 , min)
hogares_test$distancia_banco <- min_dist_bancos_cl
test$distancia_banco <- min_dist_bancos_cl

#Data frames finales
head(hogares_train)
head(hogares_test)
