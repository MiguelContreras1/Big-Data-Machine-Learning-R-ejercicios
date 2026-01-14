## MODELO RIDGE

library(pacman)
p_load(tidyverse, fastDummies, caret, glmnet, MLmetrics)

library("dplyr") 
library("caret")

set.seed(123)
lambda <- 10^seq(-2, 3, length = 100)

ridge <- train(modelo1, 
               data = training, 
               method = "glmnet", 
               trControl = trainControl("cv", number = 5), 
               tuneGrid = expand.grid(alpha = 0,lambda=lambda), 
               preProcess = c("center", "scale")
)



library(pacman)
p_load(tidyverse, fastDummies, caret, glmnet, MLmetrics)

library("dplyr") 
library("caret")

training11 <- na.omit(training1)

set.seed(123)
lambda <- 10^seq(-2, 3, length = 100)

ridge <- train(price ~ surface_total2 + bedrooms + bathrooms + distancia_parque + distancia_estacion_bus + distancia_hospital + distancia_estacion_policia, 
               data = training11, 
               method = "glmnet",
               trControl = trainControl("cv", number = 5),
               tuneGrid = expand.grid(alpha = 0, lambda=lambda), 
               preProcess = c("center", "scale")
)


summary(ridge)





ridge

# predicciones 

testing$ridge <- predict(ridge,newdata = testing)
with(testing,mean((price-ridge)^2))

test$ridge <- predict(ridge,newdata = test)
with(test,mean((price-ridge)^2))


# predicción dentro de muestra (testing)
pred_ridge_in <- predict(ridge , testing)

# predicción fuera de muestra (en test)
pred_ridge_out <- predict(ridge , test)


# regularización ridge

regula_ridge <- ridge$beta %>% 
  as.matrix() %>%
  t() %>% 
  as_tibble() %>%
  mutate(lambda = ridge$lambda)

regula_ridge <- regula_ridge %>%
  pivot_longer(
    cols = !lambda, 
    names_to = "predictor",
    values_to = "coeficientes"
  )

regula_ridge %>%
  ggplot(aes(x = lambda, y = coeficientes, color = predictor)) +
  geom_line() +
  scale_x_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10",
                                  scales::math_format(10^.x))
  ) +
  labs(title = "Coeficientes del modelo en función de la regularización (Ridge)", x = "Lambda", y = "Coeficientes") +
  theme_bw() +
  theme(legend.position="bottom")


