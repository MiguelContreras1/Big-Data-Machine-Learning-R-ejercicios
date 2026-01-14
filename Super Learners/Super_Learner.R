# SuperLearner

rm(list = ls()) 

# Cargar e instalar paquetes
library(pacman)
p_load(tidyverse, SuperLearner, caret, glmnet, randomForest, RhpcBLASctl, MLmetrics)

# La instalación de xgboost es diferente
# install.packages("xgboost", 
#                  repos = c("http://dmlc.ml/drat/", getOption("repos")), 
#                  type = "source")
library(xgboost)

# Cargar datos
data(Boston, package = "MASS")
glimpse(Boston)
?MASS::Boston

# Cuántos NAs tenemos
colSums(is.na(Boston))

# La base está ordenada entonces la vamos a mezclar para que la partición sea
# aleatoria
Boston <- Boston[sample(nrow(Boston)),]

# Vamos a predecir la variable medv que es el valor de las casas
outcome = Boston$medv

# Creamos nuestros predictores
data = subset(Boston, select = -medv)

# Hacemos nuestra división en train vs test
# Train es el 80% de nuestra base
set.seed(666)
train_obs = sample(floor(nrow(data)*0.8))

# Creamos X train
X_train = data[train_obs, ]

# Vamos a partir train en train y dev
set.seed(666)
dev_obs = sample(floor(nrow(X_train)*0.4))

X_dev = X_train[dev_obs,]
X_train = X_train[-dev_obs,]

# Creamos nuestra base de testeo
X_test = data[-train_obs, ]

# Vamos a convertir nuestro problema en clasificación.
# La idea es predecir si el valor de la casa es mayor o menor e igual a $22,000 
outcome_bin = as.numeric(outcome > 22)

# Creamos y_train y y_test
y_train = outcome_bin[train_obs]
y_dev = y_train[dev_obs]
y_train = y_train[-dev_obs]
y_test = outcome_bin[-train_obs]

# Veamos la distribución de nuestra variable objetivo
prop.table(table(outcome_bin, useNA = "ifany"))
prop.table(table(y_train, useNA = "ifany"))

# Revisemos que modelos tenemos para jugar 
listWrappers()["All"]

# Escojamos algunos modelos y encontremos los mejores hiperparametros

# Random Forest. Podemos tunear, num.trees, mtry, min.node.size
SL.ranger
# Elastic Net. Podemos tunear, alpha, nfolds, nlambda
SL.glmnet

# Empecemos con el RF
tunegrid_rf <- data.frame(min.node.size = c(1, 3, 5, 7, 9, 15),
                          mtry = ceiling(sqrt(ncol(X_dev))),
                          splitrule = "gini")
# Ponemos solo 3 folds por temas computacionales pero deberían ser entre 5-10
dev <- data.frame(y_dev, X_dev)
dev$y_dev <- as.factor(dev$y_dev)
set.seed(666)
cv3 <- trainControl(number = 3, method = "cv")
modelo_rf <- train(y_dev ~ .,
                   data = dev, 
                   method = "ranger", 
                   trControl = cv3,
                   metric = 'Accuracy', 
                   tuneGrid = tunegrid_rf) 
modelo_rf$bestTune

# Creamos una función que nos cambia el min.node.size
# "..." significa "todos los otros argumentos de la función"
SL.rf.better = function(...) {
  SL.randomForest(..., min.node.size = 9, mtry = 4)
}

# Seguimos con ElasticNet
tunegrid_glmnet <- data.frame(alpha = c(0, 0.3, 0.5, 0.7, 1),
                              lambda = seq(0, 1, length.out = 100))

set.seed(666)
cv3 <- trainControl(number = 3, method = "cv")
modelo_glmnet <- train(y_dev ~ .,
                       data = dev, 
                       method = "glmnet",
                       preProcess = c("center", "scale"),
                       trControl = cv3,
                       metric = 'Accuracy', 
                       tuneGrid = tunegrid_glmnet) 
modelo_glmnet$bestTune

# Creamos una función que nos cambie los parametros del ElasticNet
# "..." significa "todos los otros argumentos de la función"
SL.glmnet.better = function(...) {
  SL.glmnet(..., alpha = 0.7, lambda = 0.03)
}

# Ajustamos el CV del SuperLearner
# Vamos a usar 3 folds por temas computacionales
# pero deberíamos usar algo alrededor de 20.
X_train2 <- data.frame(scale(X_train))
set.seed(666)
cv_sl = CV.SuperLearner(Y = y_train, X = X_train2, 
                        family = binomial(), 
                        cvControl = list(V = 10),
                        SL.library = c("SL.mean", "SL.glmnet", "SL.rf.better", 
                                       "SL.lm"),
                        control = list(saveFitLibrary = T))
summary(cv_sl)

y_hat_insample_sl <- as.numeric(cv_sl$SL.predict > 0.5)
y_hat_insample <- data.frame(SuperLearner::predict.SuperLearner(cv_sl)$library.predict)
y_hat_insample_mean <- as.numeric(y_hat_insample$SL.mean_All > 0.5)
y_hat_insample_glment <- as.numeric(y_hat_insample$SL.glmnet_All > 0.5)
y_hat_insample_rf <- as.numeric(y_hat_insample$SL.rf.better_All > 0.5)
y_hat_insample_ols <- as.numeric(y_hat_insample$SL.lm_All > 0.5)

Accuracy(y_hat_insample_sl, y_train)
Accuracy(y_hat_insample_mean, y_train)
Accuracy(y_hat_insample_glment, y_train)
Accuracy(y_hat_insample_rf, y_train)
Accuracy(y_hat_insample_ols, y_train)

X_test2 <- scale(X_test)
X_test2 <- as.data.frame(X_test2)
X_test2$chas <- 0
y_hat_outsample <- predict(cv_sl$AllSL, newdata = X_test2)

y_hat_outsample2 <- list()
for (i in 1:10) {
  y_hat_outsample2[[i]] <- cbind(y_hat_outsample[[i]]$pred, 
                                 y_hat_outsample[[i]]$library.predict) %>%
    as.data.frame() %>%
    mutate(obs = 1:nrow(.))
}

y_hat_outsample2 <- bind_rows(y_hat_outsample2) %>%
  group_by(obs) %>%
  summarise_all(mean)

y_hat_outsample_sl <- as.numeric(y_hat_outsample2$V1 > 0.5)
y_hat_outsample_mean <- as.numeric(y_hat_outsample2$SL.mean_All > 0.5)
y_hat_outsample_glment <- as.numeric(y_hat_outsample2$SL.glmnet_All > 0.5)
y_hat_outsample_rf <- as.numeric(y_hat_outsample2$SL.rf.better_All > 0.5)
y_hat_outsample_ols <- as.numeric(y_hat_outsample2$SL.lm_All > 0.5)

Accuracy(y_hat_outsample_sl, y_test)
Accuracy(y_hat_outsample_mean, y_test)
Accuracy(y_hat_outsample_glment, y_test)
Accuracy(y_hat_outsample_rf, y_test)
Accuracy(y_hat_outsample_ols, y_test)
