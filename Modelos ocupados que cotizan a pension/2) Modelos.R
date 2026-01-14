require("caret")
require("class")
require("ROCR")
require("MLmetrics")
require("kableExtra")
require("gamlr")
require("randomForest")
require("ada")
require("xgboost")

### KNN ##############################

k1 <- knn(train = train,
          test = test,
          cl = train$cotiza,
          k=1,
          prob = T)
tibble(test$cotiza, k1)
cm_knn<-confusionMatrix(data=k1 , reference=test$cotiza , mode="sens_spec" , positive="1")

cm_knn

################################################################


### LOGIT ##############################

# Estimación y predicción
set.seed(300)
require("gamlr")
logit<-glm(modelo, data=train, family = binomial())
pred_logit <- predict(logit , newdata=test , type="response")

#grafica de predicciones
ggplot(data=test , mapping=aes(cotiza,pred_logit)) + 
  geom_boxplot(aes(fill=as.factor(cotiza))) + theme_test()

#clasificación
rule=0.5
clas_logit<-factor(ifelse(pred_logit>rule,1, 0))
tibble(test$cotiza, clas_logit)
cm_logit = confusionMatrix(data=clas_logit , 
                           reference=test$cotiza , 
                           mode="sens_spec" , positive="1")
cm_logit

################################################################


### PROBIT ##############################

# Estimación y predicción
set.seed(200)
probit<-glm(modelo, data=train, family = binomial(link = "probit"))
pred_probit <- predict(probit , newdata=test , type="response")

#grafica de predicciones
ggplot(data=test , mapping=aes(cotiza,pred_probit)) + 
  geom_boxplot(aes(fill=as.factor(cotiza))) + theme_test()

#clasificación
rule=0.5
clas_probit<-factor(ifelse(pred_probit>rule, 1, 0))
tibble(test$cotiza, clas_probit)
cm_probit <- confusionMatrix(data=clas_probit , 
                             reference=test$cotiza , 
                             mode="sens_spec" , positive="1")
cm_probit

################################################################


### Random Forest ##############################

#Estimación y predicción
set.seed(20982)
ctrl <- trainControl(number = 3, method = "cv")
forest <- randomForest(modelo, data = train)
pred_rf <- predict(forest, test)

#Clasificación
tibble(test$cotiza, pred_rf)
cm_rf <- confusionMatrix(data=pred_rf, 
                         reference=test$cotiza , 
                         mode="sens_spec" , positive="1")
cm_rf

################################################################


### AdaBoost ##############################

#Estimación y predicción
set.seed(3243)
ctrl <- trainControl(number = 3, method = "cv")
adaboost <- train(modelo,
                  data = train,
                  method = "ada",
                  trControl = ctrl)
pred_ada <- predict(adaboost, test)

#Clasificación
tibble(test$cotiza, pred_ada)
cm_ada <- confusionMatrix(data=pred_ada, 
                          reference=test$cotiza , 
                          mode="sens_spec" , positive="1")
cm_ada

################################################################


### XGBoost ##############################

#Estimación y predicción
set.seed(5643)
ctrl <- trainControl(number = 3, method = "cv")
xgboost <- train(modelo,
                  data = train,
                  method = "xgbTree",
                  trControl = ctrl)
pred_xgb <- predict(xgboost, test)

#Clasificación
tibble(test$cotiza, pred_xgb)
cm_xgb <- confusionMatrix(data=pred_xgb, 
                          reference=test$cotiza , 
                          mode="sens_spec" , positive="1")
cm_xgb



################################################################