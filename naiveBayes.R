setwd("C:\\UW\\Second Semester\\Social Networks in Economics Geo\\Project - OLS vs ML")
wd = getwd()
install.packages(c("sf", "spdep", "sp", "varycoef", "ggplot2", "haven", "tidyverse"))
library(sf)		# for spatial analysis
library(spdep)		# for spatial analysis
library(sp)		# for spatial analysis
library(varycoef)	# original dataset house
library(ggplot2)		# for (spatial) ggplot
library(haven)		# to read STATA file
library(tidyverse)
# optional packages: to assess the quality of predictions
install.packages(c("metrica", "dplyr", "purrr", "tidyr"))
library(metrica) # https://adriancorrendo.github.io/metrica/ 
library(dplyr)
library(purrr)
library(tidyr)
install.packages("oce")
library(oce) 
install.packages("Metrics")
library(Metrics)
library(lmtest)
install.packages("nortest") 
library(nortest)

# reading Lucas country census track
# https://koordinates.com/layer/99834-lucas-county-ohio-census-tracts/ 
map<-st_read("#housing data/lucas-county-ohio-census-tracts.shp") # NAD83
map<-st_transform(map, crs=4269) # to NAD83

# reading data from STATA
data.file<-read_dta(file="#housing data/LucasCountytemp2-NN50.dta") # class tibble
data.file<-as.data.frame(data.file)

# reading original ‘house’ data from package {varycoef}
data.pcg<-house		# data from package
head(data.pcg)
data.pcg$id<-1:dim(data.pcg)[1]

# small area fixed effects - SAFE
# we get the ID of the region for each observation
data.sf<-st_as_sf(data.pcg, coords=c("long", "lat"), crs=2834)
data.sf<-st_transform(data.sf, crs=4269) # to NAD83
SAFE<-st_join(data.sf, map, join=st_intersects)
data.pcg$SAFE<-as.factor(SAFE$TRACTCE10)

# merge of datasets – raw and transformed
data<-merge(data.pcg, data.file, by.x="id", by.y="id")
data.sf<-st_as_sf(data, coords=c("long", "lat"), crs=2834)# sf class
data.sf<-st_transform(data.sf, crs=4269) # convert to NAD83

# split into train (up to year 1997) and test (year 1998) data
data.train<-data[data$syear!="1998",]
data.test<-data[data$syear=="1998",]
data.train.sf<-data.sf[data.sf$syear!="1998",]
data.test.sf<-data.sf[data.sf$syear=="1998",]

# Rimuoviamo la geometria per lavorare con un data frame
data.train <- st_drop_geometry(data.train)
data.test <- st_drop_geometry(data.test)

# Selezioniamo le variabili di interesse per il training
train_data <- data.train %>%
  select(price_class, log_age, log_lotsize, log_livearea, Story2more, wall, beds, baths, 
         dGarage, dToledo, MeanPrice, XWX2M, XWX3M, XWX4M, XWX6M, XWX7M, XWX8M, SAFE)

# Creiamo il modello Naive Bayes
model.nb <- naiveBayes(price_class ~ ., data = train_data)

# Sommario del modello
summary(model.nb)

# Previsioni sul set di test
test_data <- data.test %>%
  select(log_age, log_lotsize, log_livearea, Story2more, wall, beds, baths, 
         dGarage, dToledo, MeanPrice, XWX2M, XWX3M, XWX4M, XWX6M, XWX7M, XWX8M, SAFE)

# Previsioni
prediction <- predict(model.nb, newdata = test_data)

# Creazione di un dataframe con le osservazioni e le previsioni
data.pred.nb <- data.frame(obs = data.test$price_class, pred = prediction)

# Calcolo della matrice di confusione
conf_matrix <- table(Predicted = data.pred.nb$pred, Actual = data.pred.nb$obs)
print(conf_matrix)

# Visualizzazione della matrice di confusione
confusionMatrix(conf_matrix)

# Plot delle previsioni rispetto alle osservazioni reali
ggplot(data.pred.nb, aes(x = obs, fill = pred)) +
  geom_bar(position = "dodge") +
  labs(title = "Confronto tra osservazioni e previsioni", x = "Osservazioni", y = "Conteggio") +
  theme_minimal()

# Fine del codice

