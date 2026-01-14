rm(list = ls()) 

# Load relevant packages
require(pacman)
p_load(tidyverse,knitr,kableExtra,here,jtools,ggstance,broom,broom.mixed,skimr)

# Set working directory
path <- here()
setwd("C:/Users/MIGUEL  CONTRERAS/Documents/UNIANDES/semestre 3/big data & machine learning for applied economics/graciones y presentaciones de clases/clase 2/complementaria 2")

# Import dataset
load("bike.RData")
bike.features.of.interest = c('season','holiday', 'workingday', 'weathersit', 'temp', 'hum', 'windspeed', 'days_since_2011')
X = bike[bike.features.of.interest]
y = bike[,'cnt']
dat = cbind(X, y)

# Before run the linear model, check all the variables classes
skim(dat) 

mod = lm(y ~ ., data = dat, x = TRUE)
lm_summary = summary(mod)$coefficients
lm_summary_print = lm_summary

lm_summary_print[,'t value'] = abs(lm_summary_print[,'t value'])

pretty_rownames = function(rnames){
  rnames = gsub('^`', '', rnames)
  rnames = gsub('`$', '', rnames)
  rnames = gsub('`', ':', rnames)
  rnames
}

kable(lm_summary_print[,c('Estimate', 'Std. Error', 't value')], digits = 1, col.names = c('Weight', 'SE', "|t|"), booktabs = TRUE, center = TRUE) %>% kable_styling(position = "center")

# R base output
summary(mod)

# Output as a dataframe
tidy(mod)

# jtools output
summ(mod)

# Report robust standard errors (bota error)
summ(mod, robust = "HC1")

# Standardized/scaled coefficients
summ(mod, scale = TRUE)

summ(mod, scale = TRUE, transform.response = TRUE)

# interpretación visual - Scatter plot
glimpse(dat)

# Fit the model at the mean 
Modes <- function(x) {
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  ux[tab == max(tab)]
}

mean_dat <- apply(select_if(dat, is.numeric), 2, mean)
mean_dat <- data.frame(t(mean_dat))
mode_dat <- apply(select_if(dat, is.factor), 2, Modes)
mode_dat <- data.frame(t(mode_dat))
mean_obs <- cbind(mode_dat, mean_dat)
mean_obs2 <- mean_obs[rep(1, 40),]
mean_obs2$temp <- seq(-5, 35, length = 40)
mean_obs2$y_hat <- predict(mod, mean_obs2)

ggplot(dat, aes(y = y, x = temp)) +
  geom_point() +
  geom_line(data = mean_obs2, aes(x = temp, y = y_hat, 
                                  color = "with controls"), size = 1) +
  stat_smooth(formula = 'y ~ x', method = lm, se = FALSE, 
              aes(color = "without controls"), 
              size = 1) +
  theme_bw() +
  labs(x = "Temperature in Celsius", 
       y = "Number of bicycles rented",
       title = "Predicted values with changes in temperature") +
  scale_color_manual(name = "Model", values = c("red", "blue"))


# Using jtools function (bota error)
plot_summs(mod, colors = "black", robust = TRUE)

# (bota error)
plot_summs(mod, colors = "black", rescale.distributions = TRUE,
           plot.distributions = TRUE, robust = TRUE)

# (bota error)
plot_summs(mod, colors = "black", scale = TRUE, robust = TRUE)

# Building the plot by ourselves
weights = data.frame(
  Features = rownames(lm_summary),
  Estimate = lm_summary[,'Estimate'],
  std_error = lm_summary[,'Std. Error']
)

# The function qnorm() find the boundary value that determines the area 
# under the normal density curve before alpha/2.
alpha = 0.05 # 95% Confidence Interval
weights$lower = weights$Estimate - qnorm(alpha/2) * weights$std_error
weights$upper = weights$Estimate + qnorm(alpha/2) * weights$std_error
weights = weights[!(weights$Features == '(Intercept)'),]

ggplot(weights) +
  geom_vline(xintercept = 0, linetype = 4) +
  geom_point(aes(x = Estimate, y = Features)) +
  geom_segment(aes(y = Features, yend = Features, x = lower, xend = upper),
               arrow = arrow(angle = 90, ends = 'both', 
                             length = unit(0.1, 'cm'))) +
  labs(x = 'Weight estimate') +
  theme_bw() 

# Get weights for each observation (by ourselves)

# Create dummys
X <- model.matrix(y ~ ., data = dat)

# Column means
avx <- colMeans(X)
# X1 is centred by dropping column means
X1 <- sweep(X, 2L, avx)
# Get coefficients
beta <- mod$coef

head(t(beta*t(X1)))

# Get weights for each observation (using a function)
weights <- data.frame(predict(mod, type = 'terms'))

# Create reference dataset
get_reference_dataset = function(dat){
  df = lapply(dat, function(feature){
    if(class(feature) == 'factor'){
      factor(levels(feature)[1], levels = levels(feature))
    } else {
      0
    }
  })
  data.frame(df)
}
reference_dataset <- get_reference_dataset(dat)

reference_X = predict(mod, newdata = reference_dataset, 
                      type = 'terms')

effect = data.frame(t(apply(weights, 1, function(x){ x -
    reference_X[1,names(weights)]})))
effect_long <- tidyr::gather(effect)

ggplot(effect_long) +
  geom_hline(yintercept = 0, linetype = 4) +
  geom_boxplot(aes(x = key, y = value, group = key)) +
  coord_flip() +
  scale_y_continuous('Feature effect') +
  scale_x_discrete('') +
  labs(title = "Distribution of effects across the data per feature.") + 
  theme_bw()

# explicar predicción individual
i = 6
get_effects <- function(mod, dat) {
  
  X = data.frame(predict(mod, type = 'terms'))
  
  # Nicer colnames
  colnames(X) = gsub('^X\\.', '', colnames(X))
  colnames(X) = gsub('\\.', ' ', colnames(X))
  
  # predict with type='terms' centers the results, so we have to add the mean again
  reference_X = predict(mod, newdata=get_reference_dataset(dat), type='terms')
  X_star = data.frame(t(apply(X, 1, function(x){ x - reference_X[1,names(X)]})))
  X_star
}

effects = get_effects(mod, dat)
predictions = predict(mod)

effects_i = tidyr::gather(effects[i, ])
predictions_mean = mean(predictions)

# For proper indexing, names have to be removed
names(predictions) = NULL
pred_i = predictions[i]


df = data.frame(feature = colnames(bike), value = t(bike[i,]))
colnames(df) = c("feature", "value")
kable(df, col.names = c("Feature", "Value"), row.names = FALSE, booktabs = TRUE, caption = sprintf("Feature values for instance %i", i)) %>% kable_styling(position = "center")


ggplot(effect_long) +
  geom_hline(yintercept = 0, linetype = 4) +
  geom_boxplot(aes(x = key, y = value, group = key)) +
  coord_flip() +
  scale_y_continuous('Feature effect') +
  scale_x_discrete('') +
  theme_bw() +
  geom_point(data= effects_i, aes(x = key, y = value), color = 'red', 
             shape = 4, size=4) +
  ggtitle(sprintf('Predicted value for instance: %.0f\nAverage predicted value: %.0f\nActual value: %.0f', pred_i, predictions_mean, y[i]))










