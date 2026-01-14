## clean environment
rm(list=ls())

## Llamar/instalar las librerías para esta sesión:
require(pacman)
p_load(tidyverse , rio , caret ,  modelsummary , gamlr,
       ROCR, # ROC
       pROC) # optimal cutoff

## solver packages conflicts
predict <- stats::predict
  
##=== 1. data acquisition ===##

## source
browseURL("https://www1.nyc.gov/site/tlc/about/tlc-trip-record-data.page")
browseURL("https://www1.nyc.gov/assets/tlc/downloads/pdf/data_dictionary_trip_records_green.pdf")

## about data
# id_row: ID row
# payment_ccard: A numeric code signifying how the passenger paid for the trip. 1= credit card ; 0=otherways 
# trip_type: 1= Street-hail ; 2= Dispatch
# pickup_hour: The hour when the meter was engaged
# trip_distance: The elapsed trip distance in miles reported by the taximeter.
# passenger_count: The number of passengers in the vehicle. 
# fare_amount: The time-and-distance fare calculated by the meter.
# tip_amount: Does not include cash tips.
# tolls_amount: Total amount of all tolls paid in trip.
# total_amount: The total amount charged to passengers. Does not include cash tips.
# pu_location_id: TLC Taxi Zone in which the taximeter was engaged
# do_location_id: TLC Taxi Zone in which the taximeter was disengaged

## load data
data <- import("https://eduard-martinez.github.io/teaching/meca-4107/5_train.rds") %>%
        mutate(payment_ccard=factor(payment_ccard,levels=c(1,0),labels=c("Si","No")) ,  p_card = ifelse(payment_ccard=="Si",1,0))

##=== 2. prepare data ===##

## proporciones
prop.table(table(data$payment_ccard))

## now create the evaluation and test sets
set.seed(2104)

## set training dataset
split1 <- createDataPartition(data$payment_ccard , p = 0.7)[[1]]

training = data[split1,]

## set evaluation and testing datasets
other <- data[-split1,]

split2 <- createDataPartition(y = other$payment_ccard , p = 1/3)[[1]]

evaluation <- other[split2,]

testing <- other[-split2,]

## check balance
prop.table(table(training$payment_ccard))

prop.table(table(testing$payment_ccard))

prop.table(table(evaluation$payment_ccard))

##=== 3. predictions: logit ===##

## modelo a ajustar
model <- as.formula("payment_ccard ~ trip_distance + pickup_hour + pu_location_id:do_location_id + passenger_count + factor(trip_type)")

## estimations
glm_logit <- glm(model , family=binomial(link="logit") , data=training)

## predict payment_ccard
testing$predict_logit <- predict(glm_logit , testing , type="response") %>% as.numeric()

## definir la regla
ggplot(data=testing , mapping = aes(payment_ccard , predict_logit)) + 
geom_boxplot(aes(fill=payment_ccard)) + theme_test()

testing <- testing %>% 
           mutate(p_logit=ifelse(predict_logit>0.3,1,0) %>% 
                                 factor(.,levels=c(1,0),labels=c("Si","No")))

testing %>% group_by(trip_type,p_logit) %>% mutate(count=1) %>% summarize(n=sum(count)) %>%
ggplot(., aes(fill=as.factor(p_logit), y=n, x=as.factor(trip_type))) + 
geom_bar(position="fill", stat="identity")


testing %>% group_by(trip_type,p_card) %>% mutate(count=1) %>% summarize(n=sum(count)) %>%
ggplot(., aes(fill=as.factor(p_card), y=n, x=as.factor(trip_type))) + 
geom_bar(position="fill", stat="identity")

## confusion mnatrix
confusionMatrix(data=testing$p_logit, 
                reference=testing$payment_ccard , 
                mode="sens_spec" , positive="Si")

##=== 4. predictions: CV ===##

## define control
fiveStats <- function(...) c(twoClassSummary(...), defaultSummary(...))
control <- trainControl(method = "cv", number = 5,
                        summaryFunction = fiveStats, 
                        classProbs = TRUE,
                        verbose=FALSE,
                        savePredictions = T)

## train
caret_logit = train(model,
                    data=training,
                    method="glm",
                    trControl = control,
                    family = "binomial",
                    preProcess = c("center", "scale"))
caret_logit

## predict
## ROC
testing$predict_caret <- predict(caret_logit , testing , type="prob")[1] %>% unlist()

testing <- testing %>% mutate(p_caret=ifelse(predict_caret>0.6,1,0))

pred <- prediction(testing$p_caret , testing$p_card)

roc_ROCR <- performance(pred,"tpr","fpr")

plot(roc_ROCR, main = "ROC curve", col = "red")
abline(a = 0, b = 1)

auc_roc = performance(pred, measure = "auc")
auc_roc@y.values[[1]]

##=== 5. optimal cutoff (parte 1) ===##
cuttof <- seq(min(testing$predict_caret), max(testing$predict_caret) , length.out = 100) 
auc = list()
count = 1
for ( i in cuttof){
      testing$p <- ifelse(testing$predict_caret>i,1,0)
      pred_i <- prediction(testing$p , testing$p_card)
      auc_roc_i = performance(pred_i, measure = "auc")
      auc[[count]] = auc_roc_i@y.values[[1]]
      count = count + 1
}
auc = auc %>% unlist()
auc

## make dataframe
dt_auc = data.frame("auc" = auc , "cuttof" = cuttof)

## plot cuttof vs AUC
ggplot(data=dt_auc , aes(x=cuttof,y=auc)) + geom_point() + geom_line() + theme_test() +
geom_vline(xintercept = dt_auc$cuttof[dt_auc$auc==max(dt_auc$auc)] , color="red")

## cuttof que maximiza
dt_auc$cuttof[dt_auc$auc==max(dt_auc$auc)]

##=== 6. optimal cutoff (parte 2) ===##

evalResults <- data.frame(payment_ccard = evaluation$payment_ccard)

evalResults$Roc <- predict(caret_logit, newdata = evaluation , type = "prob")[,1]

rfROC <- roc(evalResults$payment_ccard, evalResults$Roc, levels = rev(levels(evalResults$payment_ccard)))

rfROC

## cuttof que mazimiza
rfThresh <- coords(rfROC, x = "best", best.method = "closest.topleft")

rfThresh
rfThresh$threshold ## cuttof que mazimiza

##=== 7. submit ===##

## data test
data_submit <- import("https://eduard-martinez.github.io/teaching/meca-4107/5_test_class.rds")

## prediction
data_submit$prediction <- predict(caret_logit , data_submit , type="prob")[,1]

data_submit = data_submit %>% mutate(p=ifelse(prediction>=rfThresh$threshold,1,0))

submit = data_submit %>% select(id_row,p)

## export results
export(submit,"~/Downloads/201725842.rds")


