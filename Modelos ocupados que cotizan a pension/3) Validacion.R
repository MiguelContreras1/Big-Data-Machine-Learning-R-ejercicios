
################################################################################
##                          Metricas de desempeño
################################################################################


### Desempeño KNN ############################

acc_knn <- Accuracy(y_pred = k1, y_true = test$cotiza)

pre_knn <- Precision(y_pred = k1, y_true = test$cotiza, positive = "1")

rec_knn <- Recall(y_pred = k1, y_true = test$cotiza, positive = "1")

f1_knn <- F1_Score(y_pred = k1, y_true = test$cotiza, positive = "1")

metricas_knn <- data.frame(Modelo = "KNN", 
                           "Evaluación" = NA,
                           "Accuracy" = acc_knn,
                           "Precision" = pre_knn,
                           "Recall" = rec_knn,
                           "F1" = f1_knn)
#########################################################################


### Desempeño logit ##############################

pred_logit_in <- predict(logit , newdata=train , type="response")
pred_logit_out <- predict(logit , newdata=test , type="response")
clas_logit_in <- factor(ifelse(pred_logit_in>rule,1, 0))
clas_logit_out <- factor(ifelse(pred_logit_out>rule,1, 0))


acc_logit_in <- Accuracy(y_pred = clas_logit_in, y_true = train$cotiza)
acc_logit_out <- Accuracy(y_pred = clas_logit_out, y_true = test$cotiza)

pre_logit_in <- Precision(y_pred = clas_logit_in, y_true = train$cotiza, positive = "1")
pre_logit_out <- Precision(y_pred = clas_logit_out, y_true = test$cotiza, positive = "1")

rec_logit_in <- Recall(y_pred = clas_logit_in, y_true = train$cotiza, positive = "1")
rec_logit_out <- Recall(y_pred = clas_logit_out, y_true = test$cotiza, positive = "1")

f1_logit_in <- F1_Score(y_pred = clas_logit_in, y_true = train$cotiza, positive = "1")
f1_logit_out <- F1_Score(y_pred = clas_logit_out, y_true = test$cotiza, positive = "1")

metricas_logit_in <- data.frame(Modelo = "LOGIT", 
                                "Evaluación" = "Dentro de muestra",
                                "Accuracy" = acc_logit_in,
                                "Precision" = pre_logit_in,
                                "Recall" = rec_logit_in,
                                "F1" = f1_logit_in)

metricas_logit_out <- data.frame(Modelo = "LOGIT", 
                                 "Evaluación" = "Fuera de muestra",
                                 "Accuracy" = acc_logit_out,
                                 "Precision" = pre_logit_out,
                                 "Recall" = rec_logit_out,
                                 "F1" = f1_logit_out)

metricas_logit<-bind_rows(metricas_logit_in, metricas_logit_out)
#########################################################################


### Desempeño probit ##############################

pred_probit_in <- predict(probit , newdata=train , type="response")
pred_probit_out <- predict(probit , newdata=test , type="response")
clas_probit_in <- factor(ifelse(pred_probit_in>rule,1, 0))
clas_probit_out <- factor(ifelse(pred_probit_out>rule,1, 0))


acc_probit_in <- Accuracy(y_pred = clas_probit_in, y_true = train$cotiza)
acc_probit_out <- Accuracy(y_pred = clas_probit_out, y_true = test$cotiza)

pre_probit_in <- Precision(y_pred = clas_probit_in, y_true = train$cotiza, positive = "1")
pre_probit_out <- Precision(y_pred = clas_probit_out, y_true = test$cotiza, positive = "1")

rec_probit_in <- Recall(y_pred = clas_probit_in, y_true = train$cotiza, positive = "1")
rec_probit_out <- Recall(y_pred = clas_probit_out, y_true = test$cotiza, positive = "1")

f1_probit_in <- F1_Score(y_pred = clas_probit_in, y_true = train$cotiza, positive = "1")
f1_probit_out <- F1_Score(y_pred = clas_probit_out, y_true = test$cotiza, positive = "1")

metricas_probit_in <- data.frame(Modelo = "PROBIT", 
                                 "Evaluación" = "Dentro de muestra",
                                 "Accuracy" = acc_probit_in,
                                 "Precision" = pre_probit_in,
                                 "Recall" = rec_probit_in,
                                 "F1" = f1_probit_in)

metricas_probit_out <- data.frame(Modelo = "PROBIT", 
                                  "Evaluación" = "Fuera de muestra",
                                  "Accuracy" = acc_probit_out,
                                  "Precision" = pre_probit_out,
                                  "Recall" = rec_probit_out,
                                  "F1" = f1_probit_out)

metricas_probit<-bind_rows(metricas_probit_in, metricas_probit_out)

#########################################################################


### Desempeño Random Forest ##############################

clas_rf_in <- predict(forest, train)
clas_rf_out <- predict(forest, test)


acc_rf_in <- Accuracy(y_pred = clas_rf_in, y_true = train$cotiza)
acc_rf_out <- Accuracy(y_pred = clas_rf_out, y_true = test$cotiza)

pre_rf_in <- Precision(y_pred = clas_rf_in, y_true = train$cotiza, positive = "1")
pre_rf_out <- Precision(y_pred = clas_rf_out, y_true = test$cotiza, positive = "1")

rec_rf_in <- Recall(y_pred = clas_rf_in, y_true = train$cotiza, positive = "1")
rec_rf_out <- Recall(y_pred = clas_rf_out, y_true = test$cotiza, positive = "1")

f1_rf_in <- F1_Score(y_pred = clas_rf_in, y_true = train$cotiza, positive = "1")
f1_rf_out <- F1_Score(y_pred = clas_rf_out, y_true = test$cotiza, positive = "1")

metricas_rf_in <- data.frame(Modelo = "Random Forest", 
                             "Evaluación" = "Dentro de muestra",
                             "Accuracy" = acc_rf_in,
                             "Precision" = pre_rf_in,
                             "Recall" = rec_rf_in,
                             "F1" = f1_rf_in)

metricas_rf_out <- data.frame(Modelo = "Random Forest", 
                              "Evaluación" = "Fuera de muestra",
                              "Accuracy" = acc_rf_out,
                              "Precision" = pre_rf_out,
                              "Recall" = rec_rf_out,
                              "F1" = f1_rf_out)

metricas_rf<-bind_rows(metricas_rf_in, metricas_rf_out)

#########################################################################


### Desempeño Adaboost ##############################

clas_ada_in <- predict(adaboost, train)
clas_ada_out <- predict(adaboost, test)


acc_ada_in <- Accuracy(y_pred = clas_ada_in, y_true = train$cotiza)
acc_ada_out <- Accuracy(y_pred = clas_ada_out, y_true = test$cotiza)

pre_ada_in <- Precision(y_pred = clas_ada_in, y_true = train$cotiza, positive = "1")
pre_ada_out <- Precision(y_pred = clas_ada_out, y_true = test$cotiza, positive = "1")

rec_ada_in <- Recall(y_pred = clas_ada_in, y_true = train$cotiza, positive = "1")
rec_ada_out <- Recall(y_pred = clas_ada_out, y_true = test$cotiza, positive = "1")

f1_ada_in <- F1_Score(y_pred = clas_ada_in, y_true = train$cotiza, positive = "1")
f1_ada_out <- F1_Score(y_pred = clas_ada_out, y_true = test$cotiza, positive = "1")

metricas_ada_in <- data.frame(Modelo = "Adaboost", 
                              "Evaluación" = "Dentro de muestra",
                              "Accuracy" = acc_ada_in,
                              "Precision" = pre_ada_in,
                              "Recall" = rec_ada_in,
                              "F1" = f1_ada_in)

metricas_ada_out <- data.frame(Modelo = "Adaboost", 
                               "Evaluación" = "Fuera de muestra",
                               "Accuracy" = acc_ada_out,
                               "Precision" = pre_ada_out,
                               "Recall" = rec_ada_out,
                               "F1" = f1_ada_out)

metricas_ada<-bind_rows(metricas_ada_in, metricas_ada_out)

#########################################################################


### Desempeño XGBoost ##############################

clas_xgb_in <- predict(xgboost, train)
clas_xgb_out <- predict(xgboost, test)


acc_xgb_in <- Accuracy(y_pred = clas_xgb_in, y_true = train$cotiza)
acc_xgb_out <- Accuracy(y_pred = clas_xgb_out, y_true = test$cotiza)

pre_xgb_in <- Precision(y_pred = clas_xgb_in, y_true = train$cotiza, positive = "1")
pre_xgb_out <- Precision(y_pred = clas_xgb_out, y_true = test$cotiza, positive = "1")

rec_xgb_in <- Recall(y_pred = clas_xgb_in, y_true = train$cotiza, positive = "1")
rec_xgb_out <- Recall(y_pred = clas_xgb_out, y_true = test$cotiza, positive = "1")

f1_xgb_in <- F1_Score(y_pred = clas_xgb_in, y_true = train$cotiza, positive = "1")
f1_xgb_out <- F1_Score(y_pred = clas_xgb_out, y_true = test$cotiza, positive = "1")

metricas_xgb_in <- data.frame(Modelo = "XGboost", 
                              "Evaluación" = "Dentro de muestra",
                              "Accuracy" = acc_xgb_in,
                              "Precision" = pre_xgb_in,
                              "Recall" = rec_xgb_in,
                              "F1" = f1_xgb_in)

metricas_xgb_out <- data.frame(Modelo = "XGboost", 
                               "Evaluación" = "Fuera de muestra",
                               "Accuracy" = acc_xgb_out,
                               "Precision" = pre_xgb_out,
                               "Recall" = rec_xgb_out,
                               "F1" = f1_xgb_out)

metricas_xgb<-bind_rows(metricas_xgb_in, metricas_xgb_out)


#########################################################################


### Tabla de metricas ##############################

metricas<-bind_rows(metricas_xgb, metricas_rf, metricas_knn, metricas_ada, metricas_logit, metricas_probit)

validacion <- metricas %>%
  kbl(digits = 2)  %>%
  kable_styling(full_width = T)

save_kable(validacion, "C:/Users/juanc/OneDrive/Documents/Universidad/Maestria/Big Data Machine Learning/Trabajo final/Validacion_modelos.png")

#########################################################################



################################################################################
##                          ROC Curves
################################################################################


### Curva ROC KNN ##############################

prob<-attr(k1, "prob")
pred_knn <- prediction(prob, test$cotiza)
ROC_KNN <- performance(pred_knn, "tpr", "fpr")


########################################################################


### Curva ROC logit ##############################

prediction_logit <- prediction(pred_logit, test$cotiza)
ROC_logit <- performance(prediction_logit, "tpr", "fpr")

########################################################################


### curva ROC probit ##############################

prediction_probit <- prediction(pred_probit, test$cotiza)
ROC_probit <- performance(prediction_probit, "tpr", "fpr")
########################################################################


### Curva ROC Random Forest ##############################

prob_rf <- predict(forest, test, "prob")
prediction_rf <- prediction(prob_rf[,"1"], test$cotiza)
ROC_rf <- performance(prediction_rf, "tpr", "fpr")



########################################################################


### Curva ROC Adaboost ##############################

prob_ada <- predict(adaboost, test, "prob")
prediction_ada <- prediction(prob_ada[,"1"], test$cotiza)
ROC_ada <- performance(prediction_ada, "tpr", "fpr")

########################################################################


### Curva ROC XGBoost ##############################

prob_xgb <- predict(xgboost, test, "prob")
prediction_xgb <- prediction(prob_xgb[,"1"], test$cotiza)
ROC_xgb <- performance(prediction_xgb, "tpr", "fpr")

########################################################################


### Curvas de ROC por separado ##############################

png("C:/Users/juanc/OneDrive/Documents/Universidad/Maestria/Big Data Machine Learning/Trabajo final/Curvas ROC Separadas.png")
par(mfrow=c(3, 2))
plot(ROC_KNN, main = "ROC Curve KNN", colorize = T)
abline(a = 0, b = 1)

plot(ROC_rf, main = "ROC Curve Random Forest", colorize = T)
abline(a = 0, b = 1)

plot(ROC_logit, main = "ROC Curve Logit", colorize = T)
abline(a = 0, b = 1)

plot(ROC_probit, main = "ROC Curve probit", colorize = T)
abline(a = 0, b = 1)

plot(ROC_ada, main = "ROC Curve Adaboost", colorize = T)
abline(a = 0, b = 1)

plot(ROC_xgb, main = "ROC Curve XGBoost", colorize = T)
abline(a = 0, b = 1)
dev.off()

########################################################################


### Curvas de ROC juntas ##############################

png("C:/Users/juanc/OneDrive/Documents/Universidad/Maestria/Big Data Machine Learning/Trabajo final/Curvas ROC Juntas.png")
plot(ROC_KNN, main = "ROC Curve", col="deeppink", lwd = 2)
plot(ROC_rf, col = "purple", add = T, lwd = 2)
plot(ROC_logit, col = "blue", add = T, lwd = 2)
plot(ROC_probit, col = "darkgreen", add = T, lwd = 2)
plot(ROC_ada, col = "chartreuse", add = T, lwd = 2)
plot(ROC_xgb, col = "brown", add = T, lwd = 2)
abline(a = 0, b = 1, lwd = 2)
legend(0.6, 0.4, legend = c("XGBoost", "Adaboost", "Random Forest", "KNN", "Logit", "Probit"), 
       fill = c("brown", "chartreuse", "orange", "deeppink", "blue", "green3"))
dev.off()
########################################################################