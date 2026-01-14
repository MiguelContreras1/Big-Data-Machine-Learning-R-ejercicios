require("xgboost")

xgbtrain <- xgb.DMatrix(label = train$price, data=data.matrix(train[,c("surface_total2", "bedrooms", "bathrooms", "distancia_parque", "distancia_estacion_bus", "distancia_hospital", "distancia_estacion_policia")]))

xgb = xgb.train(data = xgbtrain, max.depth = 3, nrounds = 100)

pred_xgb <- predict(xgb, data.matrix(test[,c("surface_total2", "bedrooms", "bathrooms", "distancia_parque", "distancia_estacion_bus", "distancia_hospital", "distancia_estacion_policia")]))

test$price <- pred_xgb