# ----------- Caricamento librerie e dati -----------------------
library(rpart)      # per il Decision Tree
library(rpart.plot)  # per visualizzare l'albero
library(sf)          # per analisi spaziali
library(spdep)       # per analisi spaziali
library(sp)          # per analisi spaziali
library(ggplot2)     # per ggplot
library(haven)       # per leggere file STATA
library(tidyverse)   # pacchetto generale

# Caricamento della mappa (geodata)
map <- st_read("#housing data/lucas-county-ohio-census-tracts.shp") # NAD83
map <- st_transform(map, crs=4269) # trasforma in NAD83

# Lettura dei dati da STATA
data.file <- read_dta(file="#housing data/LucasCountytemp2-NN50.dta") # tipo tibble
data.file <- as.data.frame(data.file)

# Lettura dei dati originali 'house' dal pacchetto varycoef
data.pcg <- house   # dati dal pacchetto varycoef
head(data.pcg)
data.pcg$id <- 1:dim(data.pcg)[1]

# Small area fixed effects (SAFE)
data.sf <- st_as_sf(data.pcg, coords=c("long", "lat"), crs=2834)
data.sf <- st_transform(data.sf, crs=4269) # trasforma in NAD83
SAFE <- st_join(data.sf, map, join=st_intersects)
data.pcg$SAFE <- as.factor(SAFE$TRACTCE10)

# Unione dei dataset (grezzi e trasformati)
data <- merge(data.pcg, data.file, by.x="id", by.y="id")
data.sf <- st_as_sf(data, coords=c("long", "lat"), crs=2834)  # classe sf
data.sf <- st_transform(data.sf, crs=4269) # converte a NAD83

# Suddivisione dei dati in train (fino al 1997) e test (anno 1998)
data.train <- data[data$syear != "1998", ]
data.test <- data[data$syear == "1998", ]
data.train.sf <- data.sf[data.sf$syear != "1998", ]
data.test.sf <- data.sf[data.sf$syear == "1998", ]

# Plot dei dati e della mappa
plot(st_geometry(map), mar=c(1,1,1,1))
plot(st_geometry(data.sf), add=TRUE, bg="red", pch=21)
degAxis(1)
degAxis(2)

plot(st_geometry(map), mar=c(1,1,1,1))
plot(st_geometry(data.train.sf), add=TRUE, bg="red", pch=21)
degAxis(1)
degAxis(2)

plot(st_geometry(map), mar=c(1,1,1,1))
plot(st_geometry(data.test.sf), add=TRUE, bg="red", pch=21)
degAxis(1)
degAxis(2)

# Modello per il Decision Tree
eq <- log_price ~ log_age + log_lotsize + log_livearea + Story2more + wall + beds + baths + dGarage + dToledo + MeanPrice + XWX2M + XWX3M + XWX4M + XWX6M + XWX7M + XWX8M + SAFE

# Costruzione del modello Decision Tree
model.tree <- rpart(eq, data = data.train, method = "anova")  # Metodo anova per regressione continua

# Visualizzazione dell'albero di decisione
rpart.plot(model.tree, type=2, extra=101, fallen.leaves=TRUE, cex=0.7)

# Previsioni sul dataset di test
prediction <- predict(model.tree, data.test)
data.pred.tree <- data.frame(obs=data.test$log_price, pred=prediction)

# Rimozione dei NA dalle previsioni
missing.tree <- which(is.na(data.pred.tree$pred) == TRUE)
data.pred.tree <- data.pred.tree[-missing.tree, ]  # elimina i NA

# Visualizzazione dei risultati
summary(data.pred.tree)
