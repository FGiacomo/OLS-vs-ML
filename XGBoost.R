# ----------- Caricamento librerie e dati -----------------------
setwd("C:\\UW\\Second Semester\\Social Networks in Economics Geo\\Project - OLS vs ML")

library(xgboost)    # Per XGBoost
library(pROC)       # Per ROC e AUC
library(caret)      # Per Confusion Matrix
library(sf)         # Per analisi spaziali
library(spdep)      # Per analisi spaziali
library(sp)         # Per analisi spaziali
library(ggplot2)    # Per grafici
library(haven)      # Per leggere file STATA
library(tidyverse)  # Pacchetto generale

# Caricamento della mappa (geodata)
map <- st_read("#housing data/lucas-county-ohio-census-tracts.shp") # NAD83
map <- st_transform(map, crs=4269) # Trasforma in NAD83

# Lettura dei dati da STATA
data.file <- read_dta(file="#housing data/LucasCountytemp2-NN50.dta") # Tipo tibble
data.file <- as.data.frame(data.file)

# Lettura dei dati originali 'house' dal pacchetto varycoef
data.pcg <- house   # Dati dal pacchetto varycoef
head(data.pcg)
data.pcg$id <- 1:dim(data.pcg)[1]

# Small area fixed effects (SAFE)
data.sf <- st_as_sf(data.pcg, coords=c("long", "lat"), crs=2834)
data.sf <- st_transform(data.sf, crs=4269) # Trasforma in NAD83
SAFE <- st_join(data.sf, map, join=st_intersects)
data.pcg$SAFE <- as.factor(SAFE$TRACTCE10)

# Unione dei dataset
data <- merge(data.pcg, data.file, by.x="id", by.y="id")
data.sf <- st_as_sf(data, coords=c("long", "lat"), crs=2834)  # Classe sf
data.sf <- st_transform(data.sf, crs=4269) # Converte a NAD83

# Suddivisione in train e test
data.train <- data[data$syear != "1998", ]
data.test <- data[data$syear == "1998", ]

# Creazione della variabile binaria
data.train$price_binary <- ifelse(data.train$log_price > median(data.train$log_price, na.rm = TRUE), 1, 0)
data.test$price_binary  <- ifelse(data.test$log_price > median(data.test$log_price, na.rm = TRUE), 1, 0)
data.train <- na.omit(data.train)

# Selezione dei predittori
predictors <- c("log_age", "log_lotsize", "log_livearea", "Story2more", "wall", 
                "beds", "baths", "dGarage", "dToledo", "MeanPrice", 
                "XWX2M", "XWX3M", "XWX4M", "XWX6M", "XWX7M", "XWX8M", "SAFE")

# Creazione delle matrici dei predittori per train e test
train_matrix <- model.matrix(~ . - 1, data = data.train[, predictors])
test_matrix  <- model.matrix(~ . - 1, data = data.test[, predictors])

# Creazione degli oggetti DMatrix per XGBoost
dtrain <- xgb.DMatrix(data = train_matrix, label = data.train$price_binary)
dtest  <- xgb.DMatrix(data = test_matrix,  label = data.test$price_binary)

# Impostazione dei parametri per XGBoost
params <- list(
  objective = "binary:logistic",
  eval_metric = "auc",
  eta = 0.1,
  max_depth = 6
)

# Train del modello XGBoost
set.seed(123)
xgb_model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 100,
  watchlist = list(train = dtrain, test = dtest),
  early_stopping_rounds = 10,
  verbose = 1
)

# Previsioni delle probabilità per la classe positiva (1)
pred_probs_xgb <- predict(xgb_model, newdata = test_matrix)

# Calcolo della curva ROC e dell'AUC
roc_curve_xgb <- roc(data.test$price_binary, pred_probs_xgb)
auc_value_xgb <- roc_curve_xgb$auc
cat("AUC:", auc_value_xgb, "\n")

# Plot della curva ROC
plot(roc_curve_xgb, 
     col = "blue", 
     lwd = 2,
     main = paste("ROC Curve - XGBoost (AUC =", round(auc_value_xgb, 3), ")"),
     xlab = "False Positive Rate (FPR)",
     ylab = "True Positive Rate (TPR)")
abline(a = 0, b = 1, lty = 2, col = "red")
grid()

# Predizioni binarie con soglia 0.5
pred_class_xgb <- ifelse(pred_probs_xgb > 0.5, 1, 0)

# Calcolo della confusion matrix
conf_matrix_xgb <- confusionMatrix(factor(pred_class_xgb, levels = c(0, 1)), 
                                   factor(data.test$price_binary, levels = c(0, 1)))
print(conf_matrix_xgb)
