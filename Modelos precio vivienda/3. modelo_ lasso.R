## MODELO LASSO

library(pacman)
p_load(tidyverse, fastDummies, caret, glmnet, MLmetrics)

library("dplyr") 
library("caret")

training11 <- na.omit(training1)

set.seed(123)
lambda <- 10^seq(-2, 3, length = 100)

lasso <- train(price ~ surface_total2 + bedrooms + bathrooms + distancia_parque + distancia_estacion_bus + distancia_hospital + distancia_estacion_policia, 
               data = training11, 
               method = "glmnet",
               trControl = trainControl("cv", number = 5),
               tuneGrid = expand.grid(alpha = 1, lambda=lambda), 
               preProcess = c("center", "scale")
)

summary(lasso)


lasso


###

lasso


# predicción dentro de muestra (testing)
pred_lasso_in <- predict(lasso , testing1)
pred_lasso_in
write.csv(pred_lasso_in)


# predicción fuera de muestra (en test)
pred_lasso_out <- predict(lasso , test)



# regularización lasso

regula_lasso <- lasso$beta %>% 
  as.matrix() %>%
  t() %>% 
  as_tibble() %>%
  mutate(lambda = lasso$lambda)

regula_lasso <- regula_lasso %>%
  pivot_longer(
    cols = !lambda, 
    names_to = "predictor",
    values_to = "coeficientes"
  )

regula_lasso %>%
  ggplot(aes(x = lambda, y = coeficientes, color = predictor)) +
  geom_line() +
  scale_x_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10",
                                  scales::math_format(10^.x))
  ) +
  labs(title = "Coeficientes del modelo en función de la regularización (Lasso)", x = "Lambda", y = "Coeficientes") +
  theme_bw() +
  theme(legend.position="bottom")







