## ESTADÍSTICAS DESCRIPTIVAS

# Tablas de algunas de las variables

install.packages("gtsummary")
require ("gtsummary") 
install.packages("haven")
require("haven")
train <- zap_labels(train)

train %>%
  select(city, price) %>%
  tbl_summary(by=city) %>%
  add_overall() %>%
  add_n()

train %>%
  select(bedrooms, price) %>%
  tbl_summary(by=bedrooms) %>%
  add_overall() %>%
  add_n()

train %>%
  select(bathrooms, price) %>%
  tbl_summary(by=bathrooms) %>%
  add_overall() %>%
  add_n()

train %>%
  select(property_type, city) %>%
  tbl_summary(by=city) %>%
  add_overall() %>%
  add_n()

train %>%
  select(bedrooms, city) %>%
  tbl_summary(by=city) %>%
  add_overall() %>%
  add_n()

train %>%
  select(bathrooms, city) %>%
  tbl_summary(by=city) %>%
  add_overall() %>%
  add_n()

test %>%
  select(property_type, city) %>%
  tbl_summary(by=city) %>%
  add_overall() %>%
  add_n()

test %>%
  select(bedrooms, city) %>%
  tbl_summary(by=city) %>%
  add_overall() %>%
  add_n()

test %>%
  select(bathrooms, city) %>%
  tbl_summary(by=city) %>%
  add_overall() %>%
  add_n()


## gráficas

box_plot <- ggplot(data=train , mapping = aes(as.factor(city) , price)) + 
  geom_boxplot()

box_plot <- box_plot +
  scale_fill_grey() + theme_classic()+
  labs(x= "ciudad", y ="precio") 

box_plot


box_plot2 <- ggplot(data=train , mapping = aes(as.factor(city) , bedrooms)) + 
  geom_boxplot()

box_plot2 <- box_plot2 +
  scale_fill_grey() + theme_classic()+
  labs(x= "ciudad", y ="habitaciones") 

box_plot2


box_plot3 <- ggplot(data=train , mapping = aes(as.factor(city) , bathrooms)) + 
  geom_boxplot()

box_plot3 <- box_plot3 +
  scale_fill_grey() + theme_classic()+
  labs(x= "ciudad", y ="baños") 

box_plot3


# Estadísticas de las variables depuradas















