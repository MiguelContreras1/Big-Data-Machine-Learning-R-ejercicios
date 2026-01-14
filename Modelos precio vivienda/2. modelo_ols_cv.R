## MODELO OLS_CV

library("tidyverse")
library("dplyr")
library("caret")
library("stargazer")

set.seed(123)
ols <- lm("price" ~ "bedrooms" + "bathrooms" + "distancia_parque" + "distancia_estacion_bus" + 
            "distancia_hospital" + "distancia_estacion_policia",data=training1)

training <- as.data.frame(training)
training1 <- training[,c("price", "surface_total2", "bedrooms" ,"bathrooms" ,"distancia_parque" ,
                       "distancia_estacion_bus","distancia_hospital", 
                      "distancia_estacion_policia")]
training1$surface_total2 <- training1$surface_total2 %>% as.numeric()

as.data.frame(training1)

# variables que están como 
# property_type          0         1       4   11     0        2          0
# surface_total2     19313         0.531   1   20     0     2359          0

# convertir en numérica la variable surface_total2 (está como character)

surface_total2

# otra forma
set.seed(123)
ols <- lm(
  price ~ surface_total2 + bedrooms + bathrooms + distancia_parque + distancia_estacion_bus + distancia_hospital + 
    distancia_estacion_policia,
  data = training1,
  method = "lm",
  trControl = ctrl,
  preProcess = c("center", "scale")
)
summary(ols)

## 
testing <- as.data.frame(testing)
testing1 <- testing[,c("price", "surface_total2", "bedrooms" ,"bathrooms" ,"distancia_parque" ,
                         "distancia_estacion_bus","distancia_hospital", 
                         "distancia_estacion_policia")]

testing1$surface_total2 <- testing1$surface_total2 %>% as.numeric()

as.data.frame(testing1)


# predicción dentro de muestra (testing)
pred_ols_in <- predict(ols , testing1)

# predicción fuera de muestra (en test)
pred_ols_out <- predict(ols , test)

