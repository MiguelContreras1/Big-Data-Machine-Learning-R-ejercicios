## MODELO ELASTIC NET

library(pacman)
p_load(tidyverse, fastDummies, caret, glmnet, MLmetrics)

library("dplyr") 
library("caret")

set.seed(123)
lambda <- 10^seq(-2, 3, length = 100)

elasticnet <- train(modelo1, 
               data = train, 
               method = "glmnet", 
               trControl = trainControl("cv", number = 5), 
               tuneGrid = expand.grid(alpha = 0.7,lambda=lambda), 
               preProcess = c("center", "scale")
)

## o así, no se
elasticnet <- train(modelo1, 
                    data = train, 
                    method = "glmnet", 
                    trControl = trainControl("cv", number = 5), 
                    preProcess = c("center", "scale")
)



set.seed(123)
lambda <- 10^seq(-2, 3, length = 100)

elasticnet <- train(price ~ surface_total2 + bedrooms + bathrooms + distancia_parque + distancia_estacion_bus + distancia_hospital + distancia_estacion_policia, 
               data = training11, 
               method = "glmnet",
               trControl = trainControl("cv", number = 5),
               tuneGrid = expand.grid(alpha = 0.7, lambda=lambda), 
               preProcess = c("center", "scale")
)

elasticnet

summary(elasticnet)








elasticnet

# predicciones

testing$elasticnet <- predict(elasticnet,newdata = testing)
with(testing,mean((price-elasticnet)^2))

test$elasticnet <- predict(elasticnet,newdata = test)
with(test,mean((price-elasticnet)^2))

# predicción dentro de muestra (testing)
pred_elasticnet_in <- predict(elasticnet , testing)

# predicción fuera de muestra (en test)
pred_elasticnet_out <- predict(elasticnet , test)


# regularización elasticnet

regula_elasticnet <- elasticnet$beta %>% 
  as.matrix() %>%
  t() %>% 
  as_tibble() %>%
  mutate(lambda = elasticnet$lambda)

regula_elasticnet <- regula_elasticnet %>%
  pivot_longer(
    cols = !lambda, 
    names_to = "predictor",
    values_to = "coeficientes"
  )

regula_elasticnet %>%
  ggplot(aes(x = lambda, y = coeficientes, color = predictor)) +
  geom_line() +
  scale_x_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10",
                                  scales::math_format(10^.x))
  ) +
  labs(title = "Coeficientes del modelo en función de la regularización (Elastic Net)", x = "Lambda", y = "Coeficientes") +
  theme_bw() +
  theme(legend.position="bottom")



