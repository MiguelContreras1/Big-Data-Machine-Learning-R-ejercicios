## MODELO 1 VARIABLES

require("tidyverse")
require("dplyr")
require("caret")
require("stargazer")


modelo1 <- as.formula ("price ~ surface_total2 + bedrooms + bathrooms + distancia_parque + distancia_estacion_bus + distancia_hospital + distancia_estacion_policia")


modelo2 <- as.formula ("price ~ surface_total2 + bedrooms + bathrooms + property_type + 
                       distancia_parque + distancia_estacion_bus + distancia_hospital + distancia_estacion_policia + 
                       estrato + piso + ascensor + parqueadero")



