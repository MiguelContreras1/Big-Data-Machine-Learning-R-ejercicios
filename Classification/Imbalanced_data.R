
## Cargamos la librería AER donde está alojada la data
rm(list=ls())
library(pacman)
p_load(AER, tidyverse, caret, MLmetrics, tidymodels, themis)

## Importamos los datos
data("Affairs")

## Codificamos la variable de affairs según el diccionario
diccionario_affairs = c("Nunca", "Una vez", "Dos veces", "Tres veces", "4 a 10 veces", "Más de 10 veces")
Affairs$affairs <- factor(Affairs$affairs, 
                          levels = c(0, 1, 2, 3, 7, 12), 
                          labels = diccionario_affairs)

ggplot(Affairs, aes(x = affairs)) + geom_bar(fill = "darkblue") +
labs(title = "¿Con qué frecuencia tuvo relaciones sexuales extramatrimoniales \n durante el último año?", 
     x = "",y = "Frecuencia") + coord_flip() + theme_bw()

# Creamos variable infiel
Affairs$infiel <- Affairs$affairs != "Nunca"
prop.table(table(Affairs$infiel))

## function
LogLikelihood <- function (y_pred, y_true) {
                 # Número cercano a cercano para evitar división por cero
                 eps <- 1e-15 
                 # Si la probabilidad predicha es 0, agregale eps
                 # Si la probabilidad predicha es 1, restele eps
                 y_pred <- pmax(pmin(y_pred, 1 - eps), eps)
                 # Pasamos de booleano a numerico
                 y_true <- y_true + 0
                 LogLoss <- sum(y_true*log(y_pred) + (1 - y_true) * log(1 - y_pred))
return(LogLoss)
}

y_hat <- seq(0.001, 0.999, length.out = 100)

l <- c()
for (i in 1:100) {
     li <- LogLikelihood(y_pred = y_hat[i] , y_true = 1)
     l <- c(l, li)
}

plot_f <- data.frame(y_hat = y_hat, log_likelihood = l)
ggplot(plot_f, aes(x = y_hat, y = log_likelihood)) +
geom_point() + geom_line() +
geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
theme_bw() + labs(x = "Valor predicho", y = "Log-verosimilitud")

# Log-verosimilitud cuando se dice que todas las personas son fieles
n <- nrow(Affairs)
y_hat_fieles <- rep(0, n)
l_fieles <- LogLikelihood(y_pred = y_hat_fieles, y_true = Affairs$infiel)

# Log-verosimilitud cuando todos son infieles
y_hat_infieles <- rep(1, n)
l_infieles <- LogLikelihood(y_pred = y_hat_infieles, y_true = Affairs$infiel)

# Log-verosimilitud cuando el 75% de las personas es fiel seleccionado al azar
y_hat_fieles75 <- c(rep(0, round(n*0.75, 0)), rep(1, round(n*0.25, 0)))

# Corremos 10000 simulaciones
set.seed(666)
l_fieles75 <- c()
for (i in 1:10000) {
     y_hat_fieles75_i <- sample(y_hat_fieles75, n)
     l_fieles75_i <- LogLikelihood(y_pred = y_hat_fieles75_i, 
     y_true = Affairs$infiel)
     l_fieles75 <- c(l_fieles75, l_fieles75_i)
}

# Log-verosimilitud cuando el 100% de las predicciones es correcta
l_maximo <- LogLikelihood(y_pred = Affairs$infiel, y_true = Affairs$infiel)

ggplot() +
geom_histogram(aes(x = l_fieles75), fill = "darkblue") + theme_bw() +
geom_vline(aes(xintercept = l_fieles, color = "100% Fieles"), linetype = "dashed") +
geom_vline(aes(xintercept = l_infieles, color = "100% Infieles"), linetype = "dashed") +
geom_vline(aes(xintercept = l_maximo, color = "Máxima Log-verosimilitud"), linetype = "dashed") +
labs(x = "Log-verosimilitud", y = "Frecuencia") + 
scale_color_manual(name = "Escenario", 
                   values = c("100% Infieles" = "red", "100% Fieles" = "blue","Máxima Log-verosimilitud" = "green"))


glimpse(Affairs)
Affairs$occupation <- factor(Affairs$occupation)
Affairs$education <- factor(Affairs$education)

# Dummyficamos ANTES de partir la base en train/test
# Creamos las variables edad y años de casados al cuadrado
df <- model.matrix(~ . + I(age^2) + I(yearsmarried^2) - affairs - 1, Affairs)

# Dividimos train/test (70/30)
smp_size <- floor(0.7*n)
set.seed(666)
train_ind <- sample(1:n, size = smp_size)
train <- df[train_ind, ]
test <- df[-train_ind, ]

# Estandarizamos DESPUÉS de partir la base en train/test
variables_numericas <- c("age", "yearsmarried", "religiousness",
"rating", "I(age^2)", "I(yearsmarried^2)")
escalador <- preProcess(train[, variables_numericas])
train_s <- train
test_s <- test
train_s[,variables_numericas] <- predict(escalador, train[,variables_numericas])
test_s[,variables_numericas] <- predict(escalador, test[,variables_numericas])


train_s <- data.frame(train_s)
test_s <- data.frame(test_s)
train <- data.frame(train)
test <- data.frame(test)

train_s$infielTRUE <- as.numeric(train_s$infielTRUE)
modelo1 <- lm(formula = infielTRUE ~ ., data = train_s)
probs_insample1 <- predict(modelo1, train_s)
probs_insample1[probs_insample1 < 0] <- 0
probs_insample1[probs_insample1 > 1] <- 1
probs_outsample1 <- predict(modelo1, test_s)
probs_outsample1[probs_outsample1 < 0] <- 0
probs_outsample1[probs_outsample1 > 1] <- 1

# Convertimos la probabilidad en una predicción
y_hat_insample1 <- as.numeric(probs_insample1 > 0.5)
y_hat_outsample1 <- as.numeric(probs_outsample1 > 0.5)

acc_insample1 <- Accuracy(y_pred = y_hat_insample1, y_true = train$infielTRUE)
acc_outsample1 <- Accuracy(y_pred = y_hat_outsample1, y_true = test$infielTRUE)

pre_insample1 <- Precision(y_pred = y_hat_insample1, y_true = train$infielTRUE, positive = 1)
pre_outsample1 <- Precision(y_pred = y_hat_outsample1, y_true = test$infielTRUE, positive = 1)

rec_insample1 <- Recall(y_pred = y_hat_insample1, y_true = train$infielTRUE, positive = 1)
rec_outsample1 <- Recall(y_pred = y_hat_outsample1, y_true = test$infielTRUE, positive = 1)

f1_insample1 <- F1_Score(y_pred = y_hat_insample1, y_true = train$infielTRUE, positive = 1)
f1_outsample1 <- F1_Score(y_pred = y_hat_outsample1, y_true = test$infielTRUE, positive = 1)

metricas_insample1 <- data.frame(Modelo = "Regresión lineal", 
"Muestreo" = NA, 
"Evaluación" = "Dentro de muestra",
"Accuracy" = acc_insample1,
"Precision" = pre_insample1,
"Recall" = rec_insample1,
"F1" = f1_insample1)

metricas_outsample1 <- data.frame(Modelo = "Regresión lineal", 
"Muestreo" = NA, 
"Evaluación" = "Fuera de muestra",
"Accuracy" = acc_outsample1,
"Precision" = pre_outsample1,
"Recall" = rec_outsample1,
"F1" = f1_outsample1)

metricas1 <- bind_rows(metricas_insample1, metricas_outsample1)


# Implementamos oversampling
train_s$infielTRUE <- factor(train_s$infielTRUE)
train_s2 <- recipe(infielTRUE ~ ., data = train_s) %>%
themis::step_smote(infielTRUE, over_ratio = 1) %>%
prep() %>%
bake(new_data = NULL)

prop.table(table(train_s$infielTRUE))
nrow(train_s)
prop.table(table(train_s2$infielTRUE))
nrow(train_s2)

train_s2$infielTRUE <- as.numeric(train_s2$infielTRUE) - 1
modelo2 <- lm(formula = "infielTRUE ~ .", data = train_s2)
probs_insample2 <- predict(modelo2, train_s2)
probs_insample2[probs_insample2 < 0] <- 0
probs_insample2[probs_insample2 > 1] <- 1
probs_outsample2 <- predict(modelo2, test_s)
probs_outsample2[probs_outsample2 < 0] <- 0
probs_outsample2[probs_outsample2 > 1] <- 1

# Convertimos la probabilidad en una predicción
y_hat_insample2 <- as.numeric(probs_insample2 > 0.5)
y_hat_outsample2 <- as.numeric(probs_outsample2 > 0.5)

acc_insample2 <- Accuracy(y_pred = y_hat_insample2, y_true = train_s2$infielTRUE)
acc_outsample2 <- Accuracy(y_pred = y_hat_outsample2, y_true = test$infielTRUE)

pre_insample2 <- Precision(y_pred = y_hat_insample2, y_true = train_s2$infielTRUE, positive = 1)
pre_outsample2 <- Precision(y_pred = y_hat_outsample2, y_true = test$infielTRUE, positive = 1)

rec_insample2 <- Recall(y_pred = y_hat_insample2, y_true = train_s2$infielTRUE, positive = 1)
rec_outsample2 <- Recall(y_pred = y_hat_outsample2, y_true = test$infielTRUE, positive = 1)

f1_insample2 <- F1_Score(y_pred = y_hat_insample2, y_true = train_s2$infielTRUE, positive = 1)
f1_outsample2 <- F1_Score(y_pred = y_hat_outsample2, y_true = test$infielTRUE, positive = 1)

metricas_insample2 <- data.frame(Modelo = "Regresión lineal", 
"Muestreo" = "SMOTE - Oversampling", 
"Evaluación" = "Dentro de muestra",
"Accuracy" = acc_insample2,
"Precision" = pre_insample2,
"Recall" = rec_insample2,
"F1" = f1_insample2)

metricas_outsample2 <- data.frame(Modelo = "Regresión lineal", 
"Muestreo" = "SMOTE - Oversampling", 
"Evaluación" = "Fuera de muestra",
"Accuracy" = acc_outsample2,
"Precision" = pre_outsample2,
"Recall" = rec_outsample2,
"F1" = f1_outsample2)

metricas2 <- bind_rows(metricas_insample2, metricas_outsample2)
metricas <- bind_rows(metricas1, metricas2)

# Implementamos oversampling
train_s$infielTRUE <- factor(train_s$infielTRUE)
train_s3 <- recipe(infielTRUE ~ ., data = train_s) %>%
themis::step_downsample(infielTRUE) %>%
prep() %>%
bake(new_data = NULL)

prop.table(table(train_s$infielTRUE))
nrow(train_s)
prop.table(table(train_s3$infielTRUE))
nrow(train_s3)

train_s3$infielTRUE <- as.numeric(train_s3$infielTRUE) - 1
modelo3 <- lm(formula = "infielTRUE ~ .", data = train_s3)
probs_insample3 <- predict(modelo3, train_s3)
probs_insample3[probs_insample3 < 0] <- 0
probs_insample3[probs_insample3 > 1] <- 1
probs_outsample3 <- predict(modelo3, test_s)
probs_outsample3[probs_outsample3 < 0] <- 0
probs_outsample3[probs_outsample3 > 1] <- 1

# Convertimos la probabilidad en una predicción
y_hat_insample3 <- as.numeric(probs_insample3 > 0.5)
y_hat_outsample3 <- as.numeric(probs_outsample3 > 0.5)

acc_insample3 <- Accuracy(y_pred = y_hat_insample3, y_true = train_s3$infielTRUE)
acc_outsample3 <- Accuracy(y_pred = y_hat_outsample3, y_true = test$infielTRUE)

pre_insample3 <- Precision(y_pred = y_hat_insample3, y_true = train_s3$infielTRUE, positive = 1)
pre_outsample3 <- Precision(y_pred = y_hat_outsample3, y_true = test$infielTRUE, positive = 1)

rec_insample3 <- Recall(y_pred = y_hat_insample3, y_true = train_s3$infielTRUE, positive = 1)
rec_outsample3 <- Recall(y_pred = y_hat_outsample3, y_true = test$infielTRUE, positive = 1)

f1_insample3 <- F1_Score(y_pred = y_hat_insample3, y_true = train_s3$infielTRUE, positive = 1)
f1_outsample3 <- F1_Score(y_pred = y_hat_outsample3, y_true = test$infielTRUE, positive = 1)

metricas_insample3 <- data.frame(Modelo = "Regresión lineal", 
"Muestreo" = "Undersampling", 
"Evaluación" = "Dentro de muestra",
"Accuracy" = acc_insample3,
"Precision" = pre_insample3,
"Recall" = rec_insample3,
"F1" = f1_insample3)

metricas_outsample3 <- data.frame(Modelo = "Regresión lineal", 
"Muestreo" = "Undersampling", 
"Evaluación" = "Fuera de muestra",
"Accuracy" = acc_outsample3,
"Precision" = pre_outsample3,
"Recall" = rec_outsample3,
"F1" = f1_outsample3)

metricas3 <- bind_rows(metricas_insample3, metricas_outsample3)
metricas <- bind_rows(metricas, metricas3)

# Esto no se debería hacer sobre la base de testeo pero se hace solo a modo ilustrativo
thresholds <- seq(0.1, 0.9, length.out = 100)
opt_t <- data.frame()
for (t in thresholds) {
y_pred_t <- as.numeric(probs_outsample1 > t)
f1_t <- F1_Score(y_true = test$infielTRUE, y_pred = y_pred_t,
positive = 1)
fila <- data.frame(t = t, F1 = f1_t)
opt_t <- bind_rows(opt_t, fila)
}

mejor_t <-  opt_t$t[which(opt_t$F1 == max(opt_t$F1, na.rm = T))]

ggplot(opt_t, aes(x = t, y = F1)) + geom_point(size = 0.7) + geom_line() + theme_bw() +
geom_vline(xintercept = mejor_t, linetype = "dashed", 
color = "red") +
labs(x = "Threshold")

# Convertimos la probabilidad en una predicción
y_hat_insample4 <- as.numeric(probs_insample1 > mejor_t)
y_hat_outsample4 <- as.numeric(probs_outsample1 > mejor_t)

acc_insample4 <- Accuracy(y_pred = y_hat_insample4, y_true = train_s$infielTRUE)
acc_outsample4 <- Accuracy(y_pred = y_hat_outsample4, y_true = test$infielTRUE)

pre_insample4 <- Precision(y_pred = y_hat_insample4, y_true = train_s$infielTRUE, positive = 1)
pre_outsample4 <- Precision(y_pred = y_hat_outsample4, y_true = test$infielTRUE, positive = 1)

rec_insample4 <- Recall(y_pred = y_hat_insample4, y_true = train_s$infielTRUE, positive = 1)
rec_outsample4 <- Recall(y_pred = y_hat_outsample4, y_true = test$infielTRUE, positive = 1)

f1_insample4 <- F1_Score(y_pred = y_hat_insample4, y_true = train_s$infielTRUE, positive = 1)
f1_outsample4 <- F1_Score(y_pred = y_hat_outsample4, y_true = test$infielTRUE, positive = 1)

metricas_insample4 <- data.frame(Modelo = "Regresión lineal - Threshold óptimo", 
"Muestreo" = NA, 
"Evaluación" = "Dentro de muestra",
"Accuracy" = acc_insample4,
"Precision" = pre_insample4,
"Recall" = rec_insample4,
"F1" = f1_insample4)

metricas_outsample4 <- data.frame(Modelo = "Regresión lineal - Threshold óptimo", 
"Muestreo" = NA, 
"Evaluación" = "Fuera de muestra",
"Accuracy" = acc_outsample4,
"Precision" = pre_outsample4,
"Recall" = rec_outsample4,
"F1" = f1_outsample4)

metricas4 <- bind_rows(metricas_insample4, metricas_outsample4)
metricas <- bind_rows(metricas, metricas4)



