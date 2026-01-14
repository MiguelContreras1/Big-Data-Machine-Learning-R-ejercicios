require("xgboost")
require("randomForest")
require("class")
require("ROCR")
require("kableExtra")


##XGBoost
xgbtrain <- xgb.DMatrix(label = train$price, data=data.matrix(train[,c("surface_total2", "bedrooms", "bathrooms", "distancia_parque", "distancia_estacion_bus", "distancia_hospital", "distancia_estacion_policia")]))

xgb = xgb.train(data = xgbtrain, max.depth = 3, nrounds = 100)

pred_xgb_in <- predict(xgb, data.matrix(train[,c("surface_total2", "bedrooms", "bathrooms", "distancia_parque", "distancia_estacion_bus", "distancia_hospital", "distancia_estacion_policia")]) , type="response")

rmse_xgb <- RMSE(pred_xgb_in, train$price)
mae_xgb <- MAE(pred_xgb_in, train$price)



##Random Forest
rf <- randomForest::randomForest(modelo1, data = train[,c("surface_total2", "price", "bedrooms", "bathrooms", "distancia_parque", "distancia_estacion_bus", "distancia_hospital", "distancia_estacion_policia")], ntree = 100,
                   importance=TRUE)

pred_rf_in <- predict(rf, train[,c("surface_total2", "bedrooms", "bathrooms", "distancia_parque", "distancia_estacion_bus", "distancia_hospital", "distancia_estacion_policia")])

rmse_rf <- RMSE(pred_rf_in, train$price)
mae_rf <- MAE(pred_rf_in, train$price)



##Lasso
set.seed(123)
lambda <- 10^seq(-2, 3, length = 100)

lasso <- train(modelo1, 
               data = train[,c("price", "surface_total2", "bedrooms", "bathrooms", "distancia_parque", "distancia_estacion_bus", "distancia_hospital", "distancia_estacion_policia")], 
               method = "glmnet",
               trControl = trainControl("cv", number = 5),
               tuneGrid = expand.grid(alpha = 1, lambda=lambda), 
               preProcess = c("center", "scale"))

pred_lasso_in <- predict(lasso, train[,c("surface_total2", "bedrooms", "bathrooms", "distancia_parque", "distancia_estacion_bus", "distancia_hospital", "distancia_estacion_policia")])

rmse_lasso <- RMSE(pred_lasso_in, train$price)
mae_lasso <- MAE(pred_lasso_in, train$price)



##Ridge
set.seed(123)
lambda <- 10^seq(-2, 3, length = 100)

ridge <- train(modelo1, 
               data = train[,c("price", "surface_total2", "bedrooms", "bathrooms", "distancia_parque", "distancia_estacion_bus", "distancia_hospital", "distancia_estacion_policia")], 
               method = "glmnet", 
               trControl = trainControl("cv", number = 5), 
               tuneGrid = expand.grid(alpha = 0,lambda=lambda), 
               preProcess = c("center", "scale")
)

pred_ridge_in <- predict(ridge, train[,c("surface_total2", "bedrooms", "bathrooms", "distancia_parque", "distancia_estacion_bus", "distancia_hospital", "distancia_estacion_policia")])

rmse_ridge <- RMSE(pred_ridge_in, train$price)
mae_ridge <- MAE(pred_ridge_in, train$price)



##Elastic net
## o así, no se
elasticnet <- train(modelo1, 
                    data = train[,c("price", "surface_total2", "bedrooms", "bathrooms", "distancia_parque", "distancia_estacion_bus", "distancia_hospital", "distancia_estacion_policia")], 
                    method = "glmnet", 
                    trControl = trainControl("cv", number = 5), 
                    preProcess = c("center", "scale")
)

pred_en_in <- predict(elasticnet, train[,c("surface_total2", "bedrooms", "bathrooms", "distancia_parque", "distancia_estacion_bus", "distancia_hospital", "distancia_estacion_policia")])

rmse_en <- RMSE(pred_en_in, train$price)
mae_en <- MAE(pred_en_in, train$price)




#Desempeño in sample
metricas_xgb <- data.frame(modelo = "XGBoost",
                           "RMSE" = rmse_xgb,
                           "MAE" = mae_xgb)

metricas_rf <- data.frame(modelo = "Random Forest",
                          "RMSE" = rmse_rf,
                          "MAE" = mae_rf)

metricas_lasso <- data.frame(modelo = "Lasso",
                             "RMSE" = rmse_lasso,
                             "MAE" = mae_lasso)

metricas_ridge <- data.frame(modelo = "Ridge",
                             "RMSE" = rmse_ridge,
                             "MAE" = mae_ridge)

metricas_en <- data.frame(modelo = "Elastic Net",
                             "RMSE" = rmse_en,
                             "MAE" = mae_en)


metricas <- bind_rows(metricas_rf, metricas_xgb, metricas_lasso, metricas_ridge, metricas_en)

metricas %>%
  kbl(digits = 2)  %>%
  kable_styling(full_width = T)




##Resultados
pred_rf <- predict(rf, data.matrix(test[,c("surface_total2", "bedrooms", "bathrooms", "distancia_parque", "distancia_estacion_bus", "distancia_hospital", "distancia_estacion_policia")]))

test$price <- pred_rf

write.csv(test, "C:/Users/juanc/OneDrive/Documents/Universidad/Maestria/Big Data Machine Learning/Taller 3/resultados_casas_cali.csv")
