# memoire
### library ----
### 


packages <- c(
 "readxl", "randomForest", "olsrr", "ggplot2", "gets", "dynlm",
 "leaps", "PerformanceAnalytics", "ggcorrplot", "corrplot", "zoo",
 "viridis", "tseries", "dplyr", "tidyr", "EnvStats", "moments",
 "tibble", "seasonal", "RJDemetra", "forecast", "tsoutliers",
 "smooth", "gridExtra", "scales", "TSA", "seastests", "fBasics"
)

install.packages(packages)

library(readxl)
library(randomForest)
library(olsrr)
library(ggplot2)
library(gets)
library(dynlm)
library(leaps)
library(PerformanceAnalytics)
library(ggcorrplot)
library(corrplot)
library(zoo)
library(viridis)
library(tseries)
library(dplyr)
library(tidyr)
library(EnvStats)
library(moments)
library(tibble)
library(seasonal)
library(RJDemetra)
library(forecast)
library(tsoutliers)
library(smooth)
library(gridExtra)
library(scales)
library(TSA)
library(seastests)
library(fBasics)

### Ici on teste le meilleur modele sur chaque serie

### Importation de la base -----
### https://www.insee.fr/fr/statistiques/serie/001763867#Telechargement
df_IPC01 <- read_excel("/Users/sabiron/Documents/Mémoire/Données/serie_001763867_05052025.xlsx")
df_IPC01 <- df_IPC01[-c(1:3),] # Supp le premières colonne inutile
df_IPC01 <- df_IPC01[nrow(df_IPC01):1, ] # Pour inverser l'ordre


names(df_IPC01) <- c("Date", "IPC")
df_IPC01$Date <- as.Date(paste0(df_IPC01$Date, "-01"))
df_IPC01$IPC <- as.numeric(df_IPC01$IPC)
str(df_IPC01)
df_IPC01 <- df_IPC01[-c(1:240),]
df_IPC01 <- df_IPC01[-c(169:183),]


tsIPC01 <- ts(df_IPC01$IPC, start = c(2010, 1), frequency = 12)


summary(df_IPC01)

# I - Serie agrée -----
# A) Traitement de la serie ----
### 1 Graphique -----
ggplot(df_IPC01, aes(x = Date, y = IPC, color = IPC)) +
 geom_line(size = 1) +
 scale_color_gradient(low = "darkgreen", high = "red") +
 scale_x_date(date_breaks = "3 years", date_labels = "%Y") +
 scale_y_continuous(breaks = seq(60, 140, by = 10)) +
 labs(title = "Évolution de l’IPC alimentaire (2010–2023)",
      x = "Temps", y = "IPC_01", color = "IPC") +
 theme_minimal(base_size = 14) +
 theme(legend.position = "right") +
 
 # Événements majeurs
 geom_vline(xintercept = as.Date("2020-03-01"), linetype = "dashed", color = "darkblue") +
 annotate("text", x = as.Date("2020-03-01"), y = max(df_IPC01$IPC, na.rm = TRUE), label = "Covid-19", angle = 90, vjust = -0.5, color = "darkblue") +
 
 geom_vline(xintercept = as.Date("2022-02-01"), linetype = "dashed", color = "darkblue") +
 annotate("text", x = as.Date("2022-02-01"), y = max(df_IPC01$IPC, na.rm = TRUE), label = "Ukraine", angle = 90, vjust = -0.5, color = "darkblue") +
 
 # Source
 annotate("text", x = min(df_IPC01$Date), y = min(df_IPC01$IPC, na.rm = TRUE),
          label = "Source : INSEE", hjust = 0, size = 3.5, color = "black")



### 2 tests (CVS stationnaire) ----

combined_test(tsIPC01)
seasdum(tsIPC01) #Seasonal Dummies

adf.test(tsIPC01)
kpss.test(tsIPC01)

xss <-diff(tsIPC01)
adf.test(xss)
kpss.test(xss) # La serie est stationnaire ap diff :)

myregx13 <- regarima_x13(tsIPC01, spec ="RG5c")
s_transform(myregx13)  # test log/level
# Pas de transformation en log.

# Notre serie n'est paz CVS ni stationnaire.


### 3 CVS -----

# Fais avec le package seasonal
x13_result <- seas(tsIPC01)
final(x13_result)  # serie CVS
plot(x13_result) # du package seasonal
summary(x13_result)

# Avec RJDmetra
s_transform(myregx13)
myspec <- x13_spec("RSA5c")
mysax13 <- x13(tsIPC01, myspec) # SCV de RJGDmetra
#plot(mysax13$final) # JTM

# Extraire la série désaisonnalisée (CVS)
ts1IPC01_CVS_RJDmetra <- ts(mysax13$final$series[, "sa"],
                            start = start(tsIPC01), frequency = frequency(tsIPC01))

ts2IPC01_CVS_seasonal <- ts(final(x13_result), start = start(tsIPC01), frequency = frequency(tsIPC01))

# RJD metra
library(zoo)
dates <- time(tsIPC01)

df2 <- data.frame(
 Date = as.Date(as.yearmon(time(tsIPC01)), frac = 1),
 Originale = as.numeric(tsIPC01),
 CVS_RJD = as.numeric(ts1IPC01_CVS_RJDmetra),
 CVS_Seasonal = as.numeric(ts2IPC01_CVS_seasonal)
)


###  4 Test sur la CVS ----
# RJDmetra
combined_test(ts1IPC01_CVS_RJDmetra)
seasdum(ts1IPC01_CVS_RJDmetra) #Seasonal Dummies

adf.test(ts1IPC01_CVS_RJDmetra)
kpss.test(ts1IPC01_CVS_RJDmetra)
xss <-diff(ts1IPC01_CVS_RJDmetra)
adf.test(xss)
kpss.test(xss)

# Seasonal
combined_test(ts2IPC01_CVS_seasonal)
seasdum(ts2IPC01_CVS_seasonal) #Seasonal Dummies

adf.test(ts2IPC01_CVS_seasonal)
kpss.test(ts2IPC01_CVS_seasonal)
xss <-diff(ts2IPC01_CVS_seasonal)
adf.test(xss)
kpss.test(xss)


### 5 Outliers ----
# ts1
outliers01 <- tso(ts1IPC01_CVS_RJDmetra)
print(outliers01)

plot(outliers01)
show(outliers01)

ts1IPC01_CVS_RJDmetra_corr <- outliers01$yadj
df2$CVS_RJD_2 <- as.numeric(ts1IPC01_CVS_RJDmetra_corr)
# On va donc utiliser ts_corr1 pour la suite, il reste plus qu'a la différencier.
#ts2
outliers01 <- tso(ts2IPC01_CVS_seasonal)
print(outliers01)

plot(outliers01)
show(outliers01)

ts2IPC01_CVS_seasonal_corr <- outliers01$yadj
df2$CVS_Seasonal_2 <- as.numeric(ts2IPC01_CVS_seasonal_corr)


combined_test(ts2IPC01_CVS_seasonal_corr)
seasdum(ts2IPC01_CVS_seasonal_corr) #Seasonal Dummies
# Toujours CVS logique

adf.test(ts2IPC01_CVS_seasonal_corr)
kpss.test(ts2IPC01_CVS_seasonal_corr)

ts2IPC01_CVS_seasonal_corr_diff <- diff(ts2IPC01_CVS_seasonal_corr)
adf.test(ts2IPC01_CVS_seasonal_corr_diff)
kpss.test(ts2IPC01_CVS_seasonal_corr_diff)

### 6 Virification de la stationnarité ----
ts1IPC01_CVS_RJDmetra_corr_diff <- diff(ts1IPC01_CVS_RJDmetra_corr)

combined_test(ts1IPC01_CVS_RJDmetra_corr_diff)
seasdum(ts1IPC01_CVS_RJDmetra_corr_diff) #Seasonal Dummies
# Toujours CVS logique

ts1IPC01_CVS_RJDmetra_corr_diff <- diff(ts1IPC01_CVS_RJDmetra_corr)
adf.test(ts1IPC01_CVS_RJDmetra_corr_diff)
kpss.test(ts1IPC01_CVS_RJDmetra_corr_diff)

combined_test(ts1IPC01_CVS_RJDmetra_corr_diff)
seasdum(ts1IPC01_CVS_RJDmetra_corr_diff) #Seasonal Dummies
adf.test(ts1IPC01_CVS_RJDmetra_corr_diff)
kpss.test(ts1IPC01_CVS_RJDmetra_corr_diff)

autoplot(ts1IPC01_CVS_RJDmetra_corr_diff) +
 ggtitle("Indice des prix à la consommation (IPC 01)- série différenciée") +
 xlab("Date") + ylab("IPC")+
 theme_minimal()+
 geom_line(color = "blue")

autoplot(tsIPC01) +
 ggtitle("Indice des prix à la consommation (IPC 01)- série brute") +
 xlab("Date") + ylab("IPC")+
 theme_minimal()+
 geom_line(color = "blue")


df2 <- df2[-1, ]  # on retire la première ligne
df2$CVS_RJD_3_corr_diff <- as.numeric(ts1IPC01_CVS_RJDmetra_corr_diff)


### 7 Stat descriptives ----
# On va donc utililser la serie diff ts_corr1_diff pour la suite de notre analyse

stat1 = basicStats(tsIPC01)
show(stat1)

# Normalité
shapiro.test(tsIPC01)

# Boîte à moustache

boxplot(
 tsIPC01,
 main = "Boîte à moustaches de la série",
 ylab = "Valeurs",
 col = "lightgreen"
)


ts1IPC01_CVS_RJDmetra_corr_diff |>
 as.numeric() |>
 tibble(valeurs = _) |>
 ggplot(aes(x = valeurs, y = "")) +
 geom_violin(fill = "white", color = "blue") +
 geom_boxplot(
  width = 0.5, fill = "grey", outlier.color = "lightgreen",
  outlier.size = 2, staplewidth = 0.4
 ) +
 labs(
  title = "Visualisation de la distribution des valeurs de l'IPC 01",
  x = "Valeurs",
  y = "Distribution"
 ) +
 theme_bw() +
 theme(
  plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
  axis.title.y = element_text(size = 14),
  axis.title.x = element_text(size = 14),
  axis.text.x = element_text(size = 12)
 )

# B) Estimation des modèles sur la serie agrée ----

# a.  Estimer et commenter les paramètres des modèles AR(1), AR(p) et ARIMA(p,d,q) et de la méthode LED Holt-Winters, ADAM ETS, ADAM ETS ARIMA, SSARIMA et CES
# b.  Paramètres : présenter sous forme de tableau les paramètres des modèles précédents. Déterminer et commenter le meilleur modèle d’après les critères AIC et AICc

# On entraine nos modele jusqu'a 2022 pour faire la prevision en 2023.

ts_train01 <- window(ts1IPC01_CVS_RJDmetra_corr_diff, start = c(2010, 3), end = c(2022, 12))
ts_test01 <- window(ts1IPC01_CVS_RJDmetra_corr_diff, start = c(2023, 1), end = c(2023, 12))



autoplot(ts1IPC01_CVS_RJDmetra_corr_diff) +
 ggtitle("alors - (2010-2024)") +
 xlab("Date") + ylab("alors")+
 theme_minimal()+
 geom_line(color = "blue")


# 1 Modele univariée  -----

## 1.1 ARMA ----

modele_arima <- auto.arima(ts_train01, stepwise = FALSE, approximation = FALSE)

summary(modele_arima)

j <- ncol(modele_arima$var.coef)
tstat <- matrix(nrow=j, ncol=1)
for(i in 1:j)
{
 tstat[i,1] <- modele_arima$coef[i]/sqrt(modele_arima$var.coef[i,i])
}
tstat

# Graphique des valeurs ajustées
plot(modele_arima$fitted)


prev_arima <- forecast(modele_arima, h=12)
prev_arima
plot(prev_arima)

aic_arma <- AIC(modele_arima)
cat("AIC du modèle SSARIMA :", round(aic_arma, 2), "\n")

aicc_arma <- AICc(modele_arima)
cat("AICc du modèle SSARIMA :", round(aicc_arma, 2), "\n")

## 1.2 AR(1)-----

modele_ar1 <- Arima(ts_train01, order = c(1, 0, 0), include.mean = TRUE)


summary(modele_ar1)

j <- ncol(modele_ar1$var.coef)
tstat <- matrix(nrow=j, ncol=1)
for(i in 1:j)
{
 tstat[i,1] <- modele_ar1$coef[i]/sqrt(modele_ar1$var.coef[i,i])
}
tstat

fitted_ar1 <- ts_train01 - residuals(modele_ar1)
plot(fitted_ar1, type = "l", col = "blue", main = "Valeurs ajustées - AR(1)", ylab = "Prévisions")

prev_ar1 <- forecast(modele_ar1, h = 12)
prev_ar1
plot(prev_ar1)

aic_ar1 <- AIC(modele_ar1)
cat("AIC du modèle AR(1) :", round(aic_ar1, 2), "\n")

aicc_ar1 <- AICc(modele_ar1)
cat("AICc du modèle AR(1) :", round(aicc_ar1, 2), "\n")

### 1.3 AR(P) -------
modele_arp <- auto.arima(ts_train01,
                         d = 0,
                         max.p = 10,        # tu peux ajuster ce maximum
                         max.q = 0,         # pas de MA
                         stationary = TRUE,
                         seasonal = FALSE,
                         stepwise = FALSE,
                         approximation = FALSE)


# Résumé du modèle
summary(modele_arp)

# t-stat des coefficients
# j <- length(modele_arp$coef)
# tstat_arp <- numeric(j)
# for(i in 1:j) {
#  tstat_arp[i] <- modele_arp$coef[i] / sqrt(modele_arp$var.coef[i, i])
# }
# tstat_arp

fitted_arP <- ts_train01 - residuals(modele_arp)
plot(fitted_arP, type = "l", col = "blue", main = "Valeurs ajustées - AR(1)", ylab = "Prévisions")

prev_arP <- forecast(modele_arp, h = 12)
prev_arP
plot(prev_arP)
aic_arp <- AIC(modele_arp)
cat("AIC du modèle SSARIMA :", round(aic_arp, 2), "\n")

aicc_arp <- AICc(modele_arp)
cat("AICc du modèle SSARIMA :", round(aicc_arp, 2), "\n")

### 1.4 X13 ----

myregx13 <- regarima_x13(ts_train01, spec ="RG5c")
summary(myregx13)
s_transform(myregx13)
#plot(myregx13)
myregx13$forecast
forex13_01 <- matrix(myregx13$forecast[1:12])
forex13_01

plot(ts_train01,
     xlim = c(time(ts_train01)[1], tail(time(ts_train01), 1) + 1),
     ylim = range(ts_train01, forex13_01),
     main = "Prévisions issues de X13-ARIMA-SEATS",
     ylab = "Valeurs", xlab = "Temps")
lines(ts(forex13_01,
         start = c(2023, 1),  # à adapter si nécessaire
         frequency = frequency(ts_train01)),
      col = "blue", lwd = 2, lty = 2)
legend("topleft", legend = c("Série observée", "Prévision X13"),
       col = c("black", "blue"), lty = c(1,2), lwd = 2)



### 1.4 stl ----

decomp = stlm(ts_train01, s.window="periodic")
show(decomp)

fitstl = stlm(ts_train01)
prevstl <- forecast(fitstl,12) #période d'une année

plot(prevstl)
summary(prevstl)

### 1.5 HoltWinters -----

WH_add <- HoltWinters(ts_train01, gamma = FALSE)
WH_add
show(WH_add)
WH_add$coefficients
plot(WH_add)
forecast_hw <- forecast(WH_add, h = 12)
plot(forecast_hw)
forecast_hw

# Nombre d'observations
res <- residuals(WH_add)
n <- length(res)

# Nombre de paramètres estimés :
# alpha + beta (+ initial level + initial trend sont souvent comptés aussi)
k <- 2  # tu peux mettre 4 si tu veux inclure les états initiaux

# Somme des carrés des résidus
RSS <- sum(res^2)

# Calcul manuel de l'AIC
aic_hw <- n * log(RSS / n) + 2 * k

# Calcul de l'AICc
aicc_hw <- aic_hw + (2 * k * (k + 1)) / (n - k - 1)

# Affichage
cat("AIC :", round(aic_hw, 2), "\n")
cat("AICc :", round(aicc_hw, 2), "\n")


## 1.6 ADAM ETS ----

fit_ADAM_ETS <- auto.adam(ts_train01,
                          model = "ZZZ",
                          lags = c(1, 12),
                          distribution = "dnorm",  # imposer une distribution adaptée
                          silent = TRUE)
# ZZZ car ne spécifie rien (tendance, saisonnalité, erreur)
fit_ADAM_ETS
summary(fit_ADAM_ETS)

par(mfcol=c(2,2))
plot(fit_ADAM_ETS)
par(mfcol=c(1,1))


plot(fit_ADAM_ETS$states)
plot(fit_ADAM_ETS$residuals)

prev_ADAM_ETS <- forecast(fit_ADAM_ETS, h=12)
show(prev_ADAM_ETS)
plot(prev_ADAM_ETS)


## 1.7 ADAM ETS + SARIMA ----

fitadam3 <- auto.adam(ts_train01,
                      model = "ZZZ",
                      lags = c(1, 12),
                      orders = list(ar = c(3,3), i = 1, ma = c(3,3)),
                      select = TRUE,
                      bootstrap = TRUE)
fitadam3

summary(fitadam3)

par(mfcol=c(2,2))
plot(fitadam3)

par(mfcol=c(1,1))
plot(fitadam3$residuals)

prev_AES <- forecast(fitadam3,12)
show(prev_AES)
plot(prev_AES)

### 1.8 SSARIMA ----

fit_SSARIMA <- auto.ssarima(ts_train01, lags=c(1,12), orders=list(ar=c(3,3), i=(2), ma=c(3,3), select=TRUE))
summary(fit_SSARIMA)

par(mfcol=c(2,2))
plot(fit_SSARIMA)

par(mfcol=c(1,1))

plot(fit_SSARIMA$residuals)

prev_SSARIMA <- forecast(fit_SSARIMA, h=12)
prev_SSARIMA
plot(prev_SSARIMA)

aic_ssarima <- AIC(fit_SSARIMA)
cat("AIC du modèle SSARIMA :", round(aic_ssarima, 2), "\n")

aicc_ssarima <- AICc(fit_SSARIMA)
cat("AICc du modèle SSARIMA :", round(aicc_ssarima, 2), "\n")


## 1.9 naïves ----

pred_naive <- naive(ts_train01, h=12)
show(pred_naive)
plot(pred_naive)

# 2 Modele multivariée  -----

## 2.1 ARX ----

## 2.1.1 traiement de la base ----
#df_ARX <- read_excel("~/Documents/Mémoire/Données/dfGB.xlsx")
df_ARX <- read_excel("~/Documents/Mémoire/Données/df_ARX")
df_ARX$Date <- as.Date(df_ARX$Date)
str(df_ARX)


# On prend les varaibles suivantes pour eviter les problemes d’endogénités
ts_Climat <- ts(df_ARX$Climat, start = c(2010, 1), frequency = 12)
ts_Geopolitical_Risk <- ts(df_ARX$Geopolitical_Risk, start = c(2010, 1), frequency = 12)
ts_Inflation_US <- ts(df_ARX$Inflation_US, start = c(2010, 1), frequency = 12)
ts_FAO_Food_Index <- ts(df_ARX$FAO_Food_Index, start = c(2010, 1), frequency = 12)
ts_PrixMondialGaz <- ts(df_ARX$PrixMondialGaz, start = c(2010, 1), frequency = 12)
ts_Imporation_combustibles <- ts(df_ARX$Imporation_combustibles, start = c(2010, 1), frequency = 12)
ts_Tx_change_EUR_USD <- ts(df_ARX$Tx_change_EUR_USD, start = c(2010, 1), frequency = 12)
ts_Prix_Petrole <- ts(df_ARX$Prix_Petrole, start = c(2010, 1), frequency = 12)


# stat + log

df_ARX_log <- df_ARX
df_ARX$Inflation_US_log <- log(df_ARX$Inflation_US)
df_ARX$FAO_Food_Index <- log(df_ARX$FAO_Food_Index)

ts_Inflation_US_log <- ts(df_ARX$Inflation_US_log, start = c(2010, 1), frequency = 12)
ts_FAO_Food_Index_log <- ts(df_ARX$FAO_Food_Index, start = c(2010, 1), frequency = 12)


### 2.1.1 a) ts_Climat ----

# 1- test global
autoplot(ts_Climat) +
 ggtitle("Anomalies de température - (2010-2023)") +
 xlab("Date") + ylab("alors")+
 theme_minimal()+
 geom_line(color = "blue")


combined_test(ts_Climat)
seasdum(ts_Climat) #Seasonal Dummies
# La serie est saisonnière donc ou doit la diff

adf.test(ts_Climat) #stationnaire
kpss.test(ts_Climat) # Non stationnaire

xss <-diff(ts_Climat)
adf.test(xss)
kpss.test(xss) # La serie est stationnaire ap diff :)


myregx13 <- regarima_x13(ts_Climat, spec ="RG5c")
s_transform(myregx13)  # test log/level
# Pas de transformation en log.

# 2 - On CVS la serie

# Avec RJDmetra
myregx13 <- regarima_x13(ts_Climat, spec ="RG5c")
summary(myregx13)
s_transform(myregx13)

myspec <- x13_spec("RSA5c")
mysax13 <- x13(ts_Climat, myspec) # SCV de RJGDmetra
#plot(mysax13$final) # JTM

# Extraire la série désaisonnalisée (CVS)
ts_Climat_CVS <- ts(mysax13$final$series[, "sa"],
                    start = start(ts_Climat), frequency = frequency(ts_Climat))

# 3 - On refait les test sur la serie CVS
autoplot(ts_Climat_CVS) +
 ggtitle("alors - (2010-2024)") +
 xlab("Date") + ylab("alors")+
 theme_minimal()+
 geom_line(color = "blue")


combined_test(ts_Climat_CVS)
seasdum(ts_Climat_CVS) #Seasonal Dummies
# La serie est saisonnière donc ou doit la diff

adf.test(ts_Climat_CVS) #stationnaire
kpss.test(ts_Climat_CVS) # Non stationnaire

ts_Climat_CVS_diff <-diff(ts_Climat_CVS)
adf.test(ts_Climat_CVS_diff)
kpss.test(ts_Climat_CVS_diff) # La serie est stationnaire ap diff :)

myregx13 <- regarima_x13(ts_Climat_CVS, spec ="RG5c")
s_transform(myregx13)  # test log/level
# Pas de transformation en log.

autoplot(ts_Climat_CVS_diff) +
 ggtitle("Anomalies de température - série CVS et différenciée - (2010-2024)") +
 xlab("Date") + ylab("alors")+
 theme_minimal()+
 geom_line(color = "blue")



### 2.1.1 b) ts_Geopolitical_Risk  ----
# On la garde comme ca car tous les feux sont aux vert
autoplot(ts_Geopolitical_Risk) +
 ggtitle("Indice de risque géopolitique (GPR) - (2010-2023)") +
 xlab("Date") + ylab("alors")+
 theme_minimal()+
 geom_line(color = "blue")

combined_test(ts_Geopolitical_Risk)
seasdum(ts_Geopolitical_Risk) #Seasonal Dummies
# La serie n'est pas  saisonnière

adf.test(ts_Geopolitical_Risk)
kpss.test(ts_Geopolitical_Risk)
# La serie est stationnaire en niv

myregx13 <- regarima_x13(ts_Geopolitical_Risk, spec ="RG5c")
s_transform(myregx13)  # test log/level
# Pas de transformation en log.



### 2.1.1 c) ts_Inflation_US  ----
# Pourquoi pas la passer en Log pour la rendre stationnaire ?
autoplot(ts_Inflation_US) +
 ggtitle("alors - (2010-2024)") +
 xlab("Date") + ylab("alors")+
 theme_minimal()+
 geom_line(color = "blue")


combined_test(ts_Inflation_US)
seasdum(ts_Inflation_US) #Seasonal Dummies
# La serie est saisonnière donc

adf.test(ts_Inflation_US) # Non stationnaire
kpss.test(ts_Inflation_US) # Non stationnaire

xss <-diff(ts_Inflation_US)
adf.test(xss) # stationnaire
kpss.test(xss) # non stationnaire de peux.

# 2 - On CVS la serie
# Avec RJDmetra
myregx13 <- regarima_x13(ts_Inflation_US, spec ="RG5c")
summary(myregx13)
s_transform(myregx13)

myspec <- x13_spec("RSA5c")
mysax13 <- x13(ts_Inflation_US, myspec) # SCV de RJGDmetra
#plot(mysax13$final) # JTM

# Extraire la série désaisonnalisée (CVS)
ts_Inflation_US_CVS <- ts(mysax13$final$series[, "sa"],
                          start = start(ts_Inflation_US), frequency = frequency(ts_Inflation_US))

# 3 - On refait les test sur la serie CVS
autoplot(ts_Inflation_US_CVS) +
 ggtitle("alors - (2010-2024)") +
 xlab("Date") + ylab("alors")+
 theme_minimal()+
 geom_line(color = "blue")


combined_test(ts_Inflation_US_CVS)
seasdum(ts_Inflation_US_CVS) #Seasonal Dummies
# La serie est saisonnière donc ou doit la diff

adf.test(ts_Inflation_US_CVS) #stationnaire
kpss.test(ts_Inflation_US_CVS) # Non stationnaire

xss <-diff(ts_Inflation_US_CVS)
adf.test(xss)
kpss.test(xss) # La serie est non stationnaire mm apres diff :/

myregx13 <- regarima_x13(ts_Inflation_US_CVS, spec ="RG5c")
s_transform(myregx13)  # test log/level
# Pas de transformation en log.

# 4- avec un log
ts_Inflation_US_log


autoplot(ts_Inflation_US_log) +
 ggtitle("alors - (2010-2024)") +
 xlab("Date") + ylab("alors")+
 theme_minimal()+
 geom_line(color = "blue")


combined_test(ts_Inflation_US_log)
seasdum(ts_Inflation_US_log) #Seasonal Dummies
# La serie est saisonnière donc ou doit la diff

adf.test(ts_Inflation_US_log) #stationnaire
kpss.test(ts_Inflation_US_log) # Non stationnaire

xss <-diff(ts_Inflation_US_log)
adf.test(xss)
kpss.test(xss) # La serie est non stationnaire mm apres diff :/

# 5 - CVS
# Avec RJDmetra
myregx13 <- regarima_x13(ts_Inflation_US_log, spec ="RG5c")
summary(myregx13)
s_transform(myregx13)

myspec <- x13_spec("RSA5c")
mysax13 <- x13(ts_Inflation_US_log, myspec) # SCV de RJGDmetra
#plot(mysax13$final) # JTM

# Extraire la série désaisonnalisée (CVS)
ts_Inflation_US_CVS_log <- ts(mysax13$final$series[, "sa"],
                              start = start(ts_Inflation_US_log), frequency = frequency(ts_Inflation_US))

combined_test(ts_Inflation_US_CVS_log)
seasdum(ts_Inflation_US_CVS_log) #Seasonal Dummies
# La serie est saisonnière donc ou doit la diff

adf.test(ts_Inflation_US_CVS_log) #stationnaire
kpss.test(ts_Inflation_US_CVS_log) # Non stationnaire

ts_Inflation_US_CVS_log_diff <-diff(ts_Inflation_US_CVS_log)
adf.test(ts_Inflation_US_CVS_log_diff)
kpss.test(ts_Inflation_US_CVS_log_diff) # La serie est non stationnaire mm apres diff :/
# le log n'a rien fait

ts_Inflation_US_CVS_log_Double_diff <-diff(ts_Inflation_US_CVS_log_diff)
adf.test(ts_Inflation_US_CVS_log_Double_diff)
kpss.test(ts_Inflation_US_CVS_log_Double_diff)


autoplot(ts_Inflation_US) +
 ggtitle("alors - (2010-2024)") +
 xlab("Date") + ylab("alors")+
 theme_minimal()+
 geom_line(color = "blue")


autoplot(ts_Inflation_US_CVS_log_Double_diff) +
 ggtitle("alors - (2010-2024)") +
 xlab("Date") + ylab("alors")+
 theme_minimal()+
 geom_line(color = "blue")



### 2.1.1 d) ts_FAO_Food_Index  ----
# Pourquoi pas mettre un log

autoplot(ts_FAO_Food_Index) +
 ggtitle("alors - (2010-2024)") +
 xlab("Date") + ylab("alors")+
 theme_minimal()+
 geom_line(color = "blue")


combined_test(ts_FAO_Food_Index)
seasdum(ts_FAO_Food_Index) #Seasonal Dummies
# La serie est non saisonnière

adf.test(ts_FAO_Food_Index) # Non stationnaire
kpss.test(ts_FAO_Food_Index) # Non stationnaire

ts_FAO_Food_Index_diff <-diff(ts_FAO_Food_Index)
adf.test(ts_FAO_Food_Index_diff)
kpss.test(ts_FAO_Food_Index_diff) # La serie est stationnaire ap diff :)

autoplot(ts_FAO_Food_Index_diff) +
 ggtitle("Indice FAO des prix des produits alimentaires - (2010-2023)") +
 xlab("Date") + ylab("FAO_Food_Index ")+
 theme_minimal()+
 geom_line(color = "blue")

autoplot(ts_FAO_Food_Index) +
 ggtitle("Indice FAO des prix des produits alimentaires - série CVS et différenciée- (2010-2023)") +
 xlab("Date") + ylab("FAO_Food_Index ")+
 theme_minimal()+
 geom_line(color = "blue")

### 2.1.1 e) ts_PrixMondialGaz  ----
# Parfait !

autoplot(ts_PrixMondialGaz) +
 ggtitle("alors - (2010-2024)") +
 xlab("Date") + ylab("alors")+
 theme_minimal()+
 geom_line(color = "blue")

combined_test(ts_PrixMondialGaz)
seasdum(ts_PrixMondialGaz) #Seasonal Dummies
# La serie est non saisonnière


adf.test(ts_PrixMondialGaz) #stationnaire
kpss.test(ts_PrixMondialGaz) # Non stationnaire

ts_PrixMondialGaz_diff <-diff(ts_PrixMondialGaz)
adf.test(ts_PrixMondialGaz_diff)
kpss.test(ts_PrixMondialGaz_diff) # La serie est stationnaire ap diff :)


myregx13 <- regarima_x13(ts_PrixMondialGaz, spec ="RG5c")
s_transform(myregx13)  # test log/level
# Pas de transformation en log.

autoplot(ts_PrixMondialGaz) +
 ggtitle("Prix mondial du gaz naturel (UE) - (2010-2023)") +
 xlab("Date") + ylab("PrixMondialGaz")+
 theme_minimal()+
 geom_line(color = "blue")

autoplot(ts_PrixMondialGaz_diff) +
 ggtitle("Prix mondial du gaz naturel (UE) - série CVS et différenciée - (2010-2023)") +
 xlab("Date") + ylab("PrixMondialGaz")+
 theme_minimal()+
 geom_line(color = "blue")


### 2.1.1 f) ts_Imporation_combustibles  ----


autoplot(ts_Imporation_combustibles) +
 ggtitle("alors - (2010-2024)") +
 xlab("Date") + ylab("alors")+
 theme_minimal()+
 geom_line(color = "blue")


combined_test(ts_Imporation_combustibles)
seasdum(ts_Imporation_combustibles) #Seasonal Dummies
# La serie est saisonnière donc ou doit la diff

adf.test(ts_Imporation_combustibles) #stationnaire
kpss.test(ts_Imporation_combustibles) # Non stationnaire

xss <-diff(ts_Imporation_combustibles)
adf.test(xss)
kpss.test(xss) # La serie est stationnaire ap diff :)


myregx13 <- regarima_x13(ts_Imporation_combustibles, spec ="RG5c")
s_transform(myregx13)  # test log/level
# Pas de transformation en log.

# Avec RJDmetra
myregx13 <- regarima_x13(ts_Imporation_combustibles, spec ="RG5c")
summary(myregx13)
s_transform(myregx13)

myspec <- x13_spec("RSA5c")
mysax13 <- x13(ts_Imporation_combustibles, myspec) # SCV de RJGDmetra
#plot(mysax13$final) # JTM

# Extraire la série désaisonnalisée (CVS)
ts_Imporation_combustibles_CVS <- ts(mysax13$final$series[, "sa"],
                                     start = start(ts_Imporation_combustibles), frequency = frequency(ts_Inflation_US))

# 3 - On refait les test sur la serie CVS
autoplot(ts_Imporation_combustibles_CVS) +
 ggtitle("alors - (2010-2024)") +
 xlab("Date") + ylab("alors")+
 theme_minimal()+
 geom_line(color = "blue")


combined_test(ts_Imporation_combustibles_CVS)
seasdum(ts_Imporation_combustibles_CVS) #Seasonal Dummies
# La serie est saisonnière donc ou doit la diff

adf.test(ts_Imporation_combustibles_CVS) #stationnaire
kpss.test(ts_Imporation_combustibles_CVS) # stationnaire

ts_Imporation_combustibles_CVS_diff <-diff(ts_Imporation_combustibles_CVS)
adf.test(ts_Imporation_combustibles_CVS_diff)
kpss.test(ts_Imporation_combustibles_CVS_diff) # La serie est stationnaire ap diff :)



autoplot(ts_Imporation_combustibles) +
 ggtitle("Importations de combustibles - (2010-2023)") +
 xlab("Date") + ylab("Importation_combustibles ")+
 theme_minimal()+
 geom_line(color = "blue")

autoplot(ts_Imporation_combustibles_CVS_diff) +
 ggtitle("Importations de combustibles - série CVS et différenciée - (2010-2023)") +
 xlab("Date") + ylab("Importation_combustibles ")+
 theme_minimal()+
 geom_line(color = "blue")


### 2.1.1 i) ts_Tx_change_EUR_USD  ----

autoplot(ts_Tx_change_EUR_USD) +
 ggtitle("alors - (2010-2024)") +
 xlab("Date") + ylab("alors")+
 theme_minimal()+
 geom_line(color = "blue")


combined_test(ts_Tx_change_EUR_USD)
seasdum(ts_Tx_change_EUR_USD) #Seasonal Dummies
# La serie est non  saisonnière

adf.test(ts_Tx_change_EUR_USD) # non stationnaire
kpss.test(ts_Tx_change_EUR_USD) # Non stationnaire

ts_Tx_change_EUR_USD_diff <-diff(ts_Tx_change_EUR_USD)
adf.test(ts_Tx_change_EUR_USD_diff)
kpss.test(ts_Tx_change_EUR_USD_diff) # La serie est stationnaire ap diff :)


myregx13 <- regarima_x13(ts_Tx_change_EUR_USD, spec ="RG5c")
s_transform(myregx13)  # test log/level
# Pas de transformation en log.

autoplot(ts_Tx_change_EUR_USD) +
 ggtitle("Taux de change EUR/USD - (2010-2023)") +
 xlab("Date") + ylab("Tx_change_EUR_USD ")+
 theme_minimal()+
 geom_line(color = "blue")


autoplot(ts_Tx_change_EUR_USD_diff) +
 ggtitle("Taux de change EUR/USD - série CVS et différenciée - (2010-2023)") +
 xlab("Date") + ylab("Tx_change_EUR_USD ")+
 theme_minimal()+
 geom_line(color = "blue")


### 2.1.1 j) ts_Prix_Petrole ----


autoplot(ts_Prix_Petrole) +
 ggtitle("alors - (2010-2024)") +
 xlab("Date") + ylab("alors")+
 theme_minimal()+
 geom_line(color = "blue")


combined_test(ts_Prix_Petrole)
seasdum(ts_Prix_Petrole) #Seasonal Dummies
# La serie est non  saisonnière

adf.test(ts_Prix_Petrole) # non stationnaire
kpss.test(ts_Prix_Petrole) # Non stationnaire

ts_Prix_Petrole_diff <-diff(ts_Prix_Petrole)
adf.test(ts_Prix_Petrole_diff)
kpss.test(ts_Prix_Petrole_diff) # La serie est stationnaire ap diff :)


myregx13 <- regarima_x13(ts_Prix_Petrole, spec ="RG5c")
s_transform(myregx13)  # test log/level

autoplot(ts_Prix_Petrole) +
 ggtitle("Prix du pétrole brut (USD/baril) - (2010-2023)") +
 xlab("Date") + ylab("Prix_Petrole ")+
 theme_minimal()+
 geom_line(color = "blue")

autoplot(ts_Prix_Petrole_diff) +
 ggtitle("Prix du pétrole brut (USD/baril) - (2010-2023)") +
 xlab("Date") + ylab("Prix_Petrole ")+
 theme_minimal()+
 geom_line(color = "blue")


# Pas de transformation en log.

# ts_Climat_CVS_diff
# ts_Geopolitical_Risk
# ts_Inflation_US_CVS_log_Double_diff
# ts_FAO_Food_Index_diff
# ts_PrixMondialGaz_diff
# ts_Imporation_combustibles_CVS_diff
# ts_Tx_change_EUR_USD_diff
# ts_Prix_Petrole_diff
# ts_train01



start_common <- c(2010, 3)
end_common   <- c(2022, 12)

ts2_train01 <- window(ts_train01, start = start_common, end = end_common)
ts2_Climat_CVS_diff <- window(ts_Climat_CVS_diff, start = start_common, end = end_common)
ts2_Geopolitical_Risk <- window(ts_Geopolitical_Risk, start = start_common, end = end_common)
ts2_FAO_Food_Index_diff <- window(ts_FAO_Food_Index_diff, start = start_common, end = end_common)
ts2_PrixMondialGaz_diff <- window(ts_PrixMondialGaz_diff, start = start_common, end = end_common)
ts2_Imporation_combustibles_CVS_diff <- window(ts_Imporation_combustibles_CVS_diff, start = start_common, end = end_common)
ts2_Tx_change_EUR_USD_diff <- window(ts_Tx_change_EUR_USD_diff, start = start_common, end = end_common)
ts2_Prix_Petrole_diff <- window(ts_Prix_Petrole_diff, start = start_common, end = end_common)

dates_ts <- seq(from = as.Date("2010-03-01"), to = as.Date("2022-12-01"), by = "month")

df_model <- data.frame(
 Date = dates_ts,
 IPC_ALIM = as.numeric(ts2_train01),
 Climat_CVS_diff = as.numeric(ts2_Climat_CVS_diff),
 Geopolitical_Risk = as.numeric(ts2_Geopolitical_Risk),
 FAO_Food_Index_diff = as.numeric(ts2_FAO_Food_Index_diff),
 PrixMondialGaz_diff = as.numeric(ts2_PrixMondialGaz_diff),
 Imporation_combustibles_CVS_diff = as.numeric(ts2_Imporation_combustibles_CVS_diff),
 Tx_change_EUR_USD_diff = as.numeric(ts2_Tx_change_EUR_USD_diff),
 Prix_Petrole_diff = as.numeric(ts2_Prix_Petrole_diff)
)






### 2.1.1 /- trainbase ----
ts2_Climat_CVS_diff <- window(ts_Climat_CVS_diff, start = c(2010, 3), end = c(2022, 12))
ts2_Geopolitical_Risk <- window(ts_Geopolitical_Risk, start = c(2010, 3), end = c(2022, 12))
ts2_FAO_Food_Index_diff <- window(ts_FAO_Food_Index_diff, start = c(2010, 3), end = c(2022, 12))
ts2_PrixMondialGaz_diff <- window(ts_PrixMondialGaz_diff, start = c(2010, 3), end = c(2022, 12))
ts2_Imporation_combustibles_CVS_diff <- window(ts_Imporation_combustibles_CVS_diff, start = c(2010, 3), end = c(2022, 12))
ts2_Tx_change_EUR_USD_diff <- window(ts_Tx_change_EUR_USD_diff, start = c(2010, 3), end = c(2022, 12))
ts2_Prix_Petrole_diff <- window(ts_Prix_Petrole_diff, start = c(2010, 3), end = c(2022, 12))

dates_ts <- seq(from = as.Date("2010-03-01"), to = as.Date("2022-12-01"), by = "month")

df_model <- data.frame(
 Date = dates_ts,
 IPC_ALIM = as.numeric(ts_train01),
 Climat_CVS_diff = as.numeric(ts2_Climat_CVS_diff),
 Geopolitical_Risk = as.numeric(ts2_Geopolitical_Risk),
 FAO_Food_Index_diff = as.numeric(ts2_FAO_Food_Index_diff),
 PrixMondialGaz_diff = as.numeric(ts2_PrixMondialGaz_diff),
 Imporation_combustibles_CVS_diff = as.numeric(ts2_Imporation_combustibles_CVS_diff),
 Tx_change_EUR_USD_diff = as.numeric(ts2_Tx_change_EUR_USD_diff),
 Prix_Petrole_diff = as.numeric(ts2_Prix_Petrole_diff)
)
trainingbase <- df_model

### 2.1.1 /- testingbase ----

# Extraction de la période 2023-01 à 2024-12
ts3_Climat_CVS_diff <- window(ts_Climat_CVS_diff, start = c(2023, 1), end = c(2023, 12))
ts3_Geopolitical_Risk <- window(ts_Geopolitical_Risk, start = c(2023, 1), end = c(2023, 12))
ts3_Inflation_US_CVS_log_Double_diff <- window(ts_Inflation_US_CVS_log_Double_diff, start = c(2023, 1), end = c(2023, 12))
ts3_FAO_Food_Index_diff <- window(ts_FAO_Food_Index_diff, start = c(2023, 1), end = c(2023, 12))
ts3_PrixMondialGaz_diff <- window(ts_PrixMondialGaz_diff, start = c(2023, 1), end = c(2023, 12))
ts3_Imporation_combustibles_CVS_diff <- window(ts_Imporation_combustibles_CVS_diff, start = c(2023, 1), end = c(2023, 12))
ts3_Tx_change_EUR_USD_diff <- window(ts_Tx_change_EUR_USD_diff, start = c(2023, 1), end = c(2023, 12))
ts3_Prix_Petrole_diff <- window(ts_Prix_Petrole_diff, start = c(2023, 1), end = c(2023, 12))

dates_ts3 <- seq(from = as.Date("2023-01-01"), to = as.Date("2023-12-01"), by = "month")

testingbase <- data.frame(
 Date = dates_ts3,
 IPC_ALIM = as.numeric(ts_test01),
 Climat_CVS_diff = as.numeric(ts3_Climat_CVS_diff),
 Geopolitical_Risk = as.numeric(ts3_Geopolitical_Risk),
 Inflation_US_CVS_log_Double_diff = as.numeric(ts3_Inflation_US_CVS_log_Double_diff),
 FAO_Food_Index_diff = as.numeric(ts3_FAO_Food_Index_diff),
 PrixMondialGaz_diff = as.numeric(ts3_PrixMondialGaz_diff),
 Imporation_combustibles_CVS_diff = as.numeric(ts3_Imporation_combustibles_CVS_diff),
 Tx_change_EUR_USD_diff = as.numeric(ts3_Tx_change_EUR_USD_diff),
 Prix_Petrole_diff = as.numeric(ts3_Prix_Petrole_diff)
)


#View(df_model)
str(df_model)

df_model2 <- df_model[,-1]
cor_matrix <- cor(df_model2)
ggcorrplot(cor_matrix, lab = TRUE)
corrplot(cor_matrix, method = "circle")
cor(df_model2)
chart.Correlation(df_model2, histogram = TRUE, pch = 19)

## 2.1.2 Sélection de variables ----

### 2.1.2 a) ----  Best subset ----

full_model <- lm(IPC_ALIM ~ Climat_CVS_diff  + Geopolitical_Risk +
                  FAO_Food_Index_diff + PrixMondialGaz_diff +
                  Imporation_combustibles_CVS_diff +
                  Tx_change_EUR_USD_diff +
                  Prix_Petrole_diff, data = trainingbase)

# Sélection du meilleur sous-ensemble
best_subset <- ols_step_best_subset(full_model)
#plot(best_subset)  # Pour visualiser les meilleurs modèles selon AIC/BIC
summary(best_subset)

leaps <- regsubsets(
 IPC_ALIM ~ Climat_CVS_diff + Geopolitical_Risk + FAO_Food_Index_diff +
  PrixMondialGaz_diff + Imporation_combustibles_CVS_diff +
  Tx_change_EUR_USD_diff + Prix_Petrole_diff,
 data = df_model,
 nbest = 1,
 method = "exhaustive"
)
res.sum <- summary(leaps)
summary(leaps)
data.frame(
 Adj.R2 = which.max(res.sum$adjr2),
 CP     = which.min(res.sum$cp),
 BIC    = which.min(res.sum$bic)
)

plot(leaps, scale = "adjr2", main = "Adjusted R²")

# 3 = PrixMondialGaz_diff Imporation_combustibles_CVS_diff Prix_Petrole_diff
# 2 = PrixMondialGaz_diff Imporation_combustibles_CVS_diff

### 2.1.2  b) -----  Best subset --------

mX <- data.matrix(trainingbase[, 3:9])
Model01 <- arx(trainingbase$IPC_ALIM,
               mc = TRUE,
               ar = 1:3,
               mxreg = mX,
               vcov.type = "ordinary")
getsm <- getsm(Model01, t.pval = 0.10)
getsm

## 2.1.3 ARX modele ----

# Série cible (y) : vecteur numérique
y <- as.numeric(ts_train01)

# Variables exogènes
xreg_train <- as.matrix(sapply(trainingbase[, c("PrixMondialGaz_diff",
                                                "Imporation_combustibles_CVS_diff")], as.numeric))

xreg_test <- as.matrix(sapply(testingbase[, c("PrixMondialGaz_diff",
                                              "Imporation_combustibles_CVS_diff")], as.numeric))

# Estimation ARX avec constante et AR(1)
model_arx <- arx(y, mc = TRUE, ar = 1, mxreg = xreg_train)
summary(model_arx)

coefs <- model_arx$coefficients
vcov <- model_arx$vcov.mean

# Calculer les t-statistiques
tstat <- coefs / sqrt(diag(vcov))
round(tstat, 3)  # pour affichage lisible

forecast_arx <- predict(model_arx, n.ahead = 12, newmxreg = xreg_test)

ts.plot(ts_test01, forecast_arx, col = c("black", "blue"), lty = c(1, 2), lwd = 2,
        main = "Prévision ARX (2023)", ylab = "Y")

error_arx <- ts_test01 - as.numeric(forecast_arx)
c(
 RMSE = sqrt(mean(error_arx^2)),
 MAE = mean(abs(error_arx)),
 MAPE = mean(abs(error_arx / ts_test01)) * 100
)


library(forecast)

# Y doit être une série ts
y_ts <- ts(ts_train01, start = c(2010, 3), frequency = 12)

# Estimation ARMAX(1,0,1) avec 2 variables explicatives
model_armax <- Arima(y_ts, order = c(1, 0, 1), xreg = xreg_train, include.mean = TRUE)

# Résumé
summary(model_armax)

coefs <- model_armax$coef  # ou coefficients
vcov <- model_armax$var.coef  # matrice de variance-covariance
tstat <- coefs / sqrt(diag(vcov))
round(tstat, 3)

# Prévision
forecast_armax <- forecast(model_armax, xreg = xreg_test, h = 12)

# Visualisation
ts.plot(ts_test01, forecast_armax$mean, col = c("black", "red"), lty = c(1,2), lwd = 2,
        main = "Prévision ARMAX (2023)", ylab = "Y")

# Évaluation des performances
error_armax <- ts_test01 - as.numeric(forecast_armax$mean)
c(
 RMSE = sqrt(mean(error_armax^2)),
 MAE = mean(abs(error_armax)),
 MAPE = mean(abs(error_armax / ts_test01)) * 100
)


ts.plot(ts_test01, forecast_arx, forecast_armax$mean,
        col = c("black", "blue", "red"),
        lty = c(1, 2, 2), lwd = 2,
        main = "Prévisions ARX vs ARMAX (2023)", ylab = "Y")



# # Afficher les valeurs prévues vs. observées
# data.frame(
#  Observé = observedbase,
#  Prévision_ARIMAX_auto = as.numeric(forecastx32$mean),
#  Prévision_ARIMAX102 = as.numeric(forecastx33$mean),
#  Prévision_ARX = as.numeric(forecastx41$mean)
# )
#
### 3-----  RLM


full_model <- lm(IPC_ALIM ~ Climat_CVS_diff  + Geopolitical_Risk +
                  FAO_Food_Index_diff + PrixMondialGaz_diff +
                  Imporation_combustibles_CVS_diff + Tx_change_EUR_USD_diff +
                  Prix_Petrole_diff, data = trainingbase)
summary(full_model)

full_model <- lm(IPC_ALIM ~
                  PrixMondialGaz_diff +
                  Imporation_combustibles_CVS_diff  +
                  Prix_Petrole_diff, data = trainingbase)
summary(full_model)

confint(full_model)
anova(full_model)

#plot(full_model)

# AIC corrigé (AICc)
AICc(full_model, return.K = FALSE, second.ord = TRUE)

# AIC simple (non corrigé)
AICc(full_model, return.K = FALSE, second.ord = FALSE)

forecast <- NULL
library(forecast)
forecast <- forecast(full_model, newdata = testingbase)

# Affichage complet
forecast

# Prévision à 22 pas avec les X futurs
forecast <- predict(full_model, newdata = testingbase)

# Affichage sous forme matricielle (utile pour comparaison)
as.matrix(forecast)

ts_forecast_LM <- ts(as.numeric(forecast), start = c(2023, 1), frequency = 12)

plot(ts_train01,
     xlim = c(time(ts_train01)[1], 2024),
     ylim = range(ts_train01, ts_forecast_LM),
     main = "Prévisions du modèle (2023)",
     ylab = "Valeurs", xlab = "Temps",
     col = "black", lwd = 2)

lines(ts_forecast_LM,
      col = "blue", lwd = 2, lty = 2)

legend("topleft", legend = c("Série observée", "Prévisions"),
       col = c("black", "blue"), lty = c(1, 2), lwd = 2)



## 2.2  Désagrégée pondérée ----

dfcomp <- read_excel("/Users/sabiron/Documents/S2_ecap/soutenance/Site/df_COICOP.xlsx")

dfcomp$Pain_cereales <- as.numeric(dfcomp$Pain_cereales)
dfcomp$Viande <- as.numeric(dfcomp$Viande)
dfcomp$Poissons_fruitsdemer <- as.numeric(dfcomp$Poissons_fruitsdemer)
dfcomp$Lait_fromage_oeufs <- as.numeric(dfcomp$Lait_fromage_oeufs)
dfcomp$Huiles_graisses <- as.numeric(dfcomp$Huiles_graisses)
dfcomp$Fruits <- as.numeric(dfcomp$Fruits)
dfcomp$Legumes <- as.numeric(dfcomp$Legumes)
dfcomp$Sucre_confiture_confiserie <- as.numeric(dfcomp$Sucre_confiture_confiserie)
dfcomp$n_c_a <- as.numeric(dfcomp$n_c_a)
dfcomp$Cafe_the_cacao <- as.numeric(dfcomp$Cafe_the_cacao)
dfcomp$Eauxminerales_boissonsrafraîchissantes <- as.numeric(dfcomp$Eauxminerales_boissonsrafraîchissantes)

str(dfcomp)

### 2.2.1 *liste des meilleurs modele de chaque composantes *----

#-------------- 01.1.1 - Pain et céréales ---------

ts_Pain_cereales <- ts(dfcomp$Pain_cereales, start = c(2010, 1), frequency = 12)
# Avec RJDmetra
myregx13_01 <- regarima_x13(ts_Pain_cereales, spec ="RG5c")
s_transform(myregx13_01)
myspec <- x13_spec("RSA5c")
mysax13 <- x13(ts_Pain_cereales, myspec) # SCV de RJGDmetra
#plot(mysax13$final) # JTM

# Extraire la série désaisonnalisée (CVS)
tsIPC01_1_1_CVS_RJDmetra <- ts(mysax13$final$series[, "sa"],
                               start = start(ts_Pain_cereales), frequency = frequency(ts_Pain_cereales))

outliers <- tso(tsIPC01_1_1_CVS_RJDmetra)
tsIPC01_1_1_CVS_RJDmetra_corr <- outliers$yadj
tsIPC01_1_1_CVS_RJDmetra_corr_diff <- diff(tsIPC01_1_1_CVS_RJDmetra_corr)
ts_train01_1_1 <- window(tsIPC01_1_1_CVS_RJDmetra_corr_diff, start = c(2010, 2), end = c(2022, 12))
ts_test01_1_1 <- window(tsIPC01_1_1_CVS_RJDmetra_corr_diff, start = c(2023, 1), end = c(2023, 12))


modele_ar1_01_1_1 <- Arima(ts_train01_1_1, order = c(1, 0, 0), include.mean = TRUE)


summary(modele_ar1_01_1_1)

j <- ncol(modele_ar1_01_1_1$var.coef)
tstat <- matrix(nrow=j, ncol=1)
for(i in 1:j)
{
 tstat[i,1] <- modele_ar1_01_1_1$coef[i]/sqrt(modele_ar1_01_1_1$var.coef[i,i])
}
tstat

fitted_ar1 <- ts_train01_1_1 - residuals(modele_ar1_01_1_1)
plot(fitted_ar1, type = "l", col = "blue", main = "Valeurs ajustées - AR(1)", ylab = "Prévisions")

prev_ar1_01_1_1 <- forecast(modele_ar1_01_1_1, h = 12)
prev_ar1_01_1_1
plot(prev_ar1_01_1_1)

aic_ar1 <- AIC(modele_ar1_01_1_1)
cat("AIC du modèle AR(1) :", round(aic_ar1, 2), "\n")

aicc_ar1 <- AICc(modele_ar1_01_1_1)
cat("AICc du modèle AR(1) :", round(aicc_ar1, 2), "\n")


#--------------  01.1.2 - Viande ---------

ts_Viande <- ts(dfcomp$Viande, start = c(2010, 1), frequency = 12)
myregx13_01 <- regarima_x13(ts_Viande, spec ="RG5c")
s_transform(myregx13_01)
myspec <- x13_spec("RSA5c")
mysax13 <- x13(ts_Viande, myspec) # SCV de RJGDmetra
#plot(mysax13$final) # JTM

# Extraire la série désaisonnalisée (CVS)
tsIPC01_1_2_CVS_RJDmetra <- ts(mysax13$final$series[, "sa"],
                               start = start(ts_Viande), frequency = frequency(ts_Viande))

outliers <- tso(tsIPC01_1_2_CVS_RJDmetra)
tsIPC01_1_2_CVS_RJDmetra_corr <- outliers$yadj
tsIPC01_1_2_CVS_RJDmetra_corr_diff <- diff(tsIPC01_1_2_CVS_RJDmetra_corr)

ts_train01_1_2 <- window(tsIPC01_1_2_CVS_RJDmetra_corr_diff, start = c(2010, 2), end = c(2022, 12))
ts_test01_1_2 <- window(tsIPC01_1_2_CVS_RJDmetra_corr_diff, start = c(2023, 1), end = c(2023, 12))


fit_SSARIMA_01_1_2 <- auto.ssarima(ts_train01_1_2, lags=c(1,12), orders=list(ar=c(3,3), i=(2), ma=c(3,3), select=TRUE))
summary(fit_SSARIMA_01_1_2)

par(mfcol=c(2,2))
plot(fit_SSARIMA_01_1_2)

par(mfcol=c(1,1))

plot(fit_SSARIMA_01_1_2$residuals)

prev_SSARIMA_01_1_2 <- forecast(fit_SSARIMA_01_1_2, h=12)
prev_SSARIMA_01_1_2
plot(prev_SSARIMA_01_1_2)

aic_ssarima <- AIC(fit_SSARIMA_01_1_2)
cat("AIC du modèle SSARIMA :", round(aic_ssarima, 2), "\n")

aicc_ssarima <- AICc(fit_SSARIMA_01_1_2)
cat("AICc du modèle SSARIMA :", round(aicc_ssarima, 2), "\n")


#-------------- 01.1.3 - Poissons et fruits de mer ---------

ts_Poissons_fruitsdemer <- ts(dfcomp$Poissons_fruitsdemer, start = c(2010, 1), frequency = 12)
myregx13_01_1_3 <- regarima_x13(ts_Poissons_fruitsdemer, spec ="RG5c")
s_transform(myregx13_01_1_3)
myspec <- x13_spec("RSA5c")
mysax13 <- x13(ts_Poissons_fruitsdemer, myspec) # SCV de RJGDmetra
tsIPC01_1_3_CVS_RJDmetra_corr <- outliers$yadj
tsIPC01_1_3_CVS_RJDmetra_corr_diff <- diff(tsIPC01_1_3_CVS_RJDmetra_corr)

# Extraire la série désaisonnalisée (CVS)
tsIPC01_1_3_CVS_RJDmetra <- ts(mysax13$final$series[, "sa"],
                               start = start(ts_Poissons_fruitsdemer), frequency = frequency(ts_Poissons_fruitsdemer))
ts_train01_1_3 <- window(tsIPC01_1_3_CVS_RJDmetra_corr_diff, start = c(2010, 2), end = c(2022, 12))
ts_test01_1_3 <- window(tsIPC01_1_3_CVS_RJDmetra_corr_diff, start = c(2023, 1), end = c(2023, 12))

fit_SSARIMA_01_1_3 <- auto.ssarima(tsIPC01_1_3_CVS_RJDmetra_corr_diff, lags=c(1,12), orders=list(ar=c(3,3), i=(2), ma=c(3,3), select=TRUE))
summary(fit_SSARIMA_01_1_3)

par(mfcol=c(2,2))
plot(fit_SSARIMA_01_1_3)

par(mfcol=c(1,1))

plot(fit_SSARIMA_01_1_3$residuals)

prev_SSARIMA_01_1_3 <- forecast(fit_SSARIMA_01_1_3, h=12)
prev_SSARIMA_01_1_3
plot(prev_SSARIMA_01_1_3)

aic_ssarima <- AIC(fit_SSARIMA_01_1_3)
cat("AIC du modèle SSARIMA :", round(aic_ssarima, 2), "\n")

aicc_ssarima <- AICc(fit_SSARIMA_01_1_3)
cat("AICc du modèle SSARIMA :", round(aicc_ssarima, 2), "\n")

#-------------- 01.1.4 - Lait, fromage et oeufs ---------
ts_Lait_fromage_oeufs <- ts(dfcomp$Lait_fromage_oeufs, start = c(2010, 1), frequency = 12)
myregx13_01_1_4 <- regarima_x13(ts_Lait_fromage_oeufs, spec ="RG5c")
s_transform(myregx13_01_1_4)  # test log/level
# Pas de transformation en log.

# Avec RJDmetra
s_transform(myregx13_01_1_4)
myspec <- x13_spec("RSA5c")
mysax13 <- x13(ts_Lait_fromage_oeufs, myspec) # SCV de RJGDmetra
#plot(mysax13$final) # JTM

# Extraire la série désaisonnalisée (CVS)
tsIPC01_1_4_CVS_RJDmetra <- ts(mysax13$final$series[, "sa"],
                               start = start(ts_Lait_fromage_oeufs), frequency = frequency(ts_Lait_fromage_oeufs))


outliers <- tso(tsIPC01_1_4_CVS_RJDmetra)
tsIPC01_1_4_CVS_RJDmetra_corr <- outliers$yadj
tsIPC01_1_4_CVS_RJDmetra_corr_diff <- diff(tsIPC01_1_4_CVS_RJDmetra_corr)

ts_train01_1_4 <- window(tsIPC01_1_4_CVS_RJDmetra_corr_diff, start = c(2010, 2), end = c(2022, 12))
ts_test01_1_4 <- window(tsIPC01_1_4_CVS_RJDmetra_corr_diff, start = c(2023, 1), end = c(2023, 12))

WH_add_01_1_4 <- HoltWinters(ts_train01_1_4, gamma = FALSE)
WH_add_01_1_4
show(WH_add_01_1_4)
WH_add_01_1_4$coefficients
plot(WH_add_01_1_4)
forecast_hw01 <- forecast(WH_add_01_1_4, h = 12)
plot(forecast_hw01)
forecast_hw01

#-------------- 01.1.5 - Huiles et graisses ---------

ts_Huiles_graisses <- ts(dfcomp$Huiles_graisses, start = c(2010, 1), frequency = 12)
myregx13_01_1_5 <- regarima_x13(ts_Huiles_graisses, spec ="RG5c")
s_transform(myregx13_01_1_5)  # test log/level

s_transform(myregx13_01_1_5)
myspec <- x13_spec("RSA5c")
mysax13 <- x13(ts_Huiles_graisses, myspec) # SCV de RJGDmetra
#plot(mysax13$final) # JTM

# Extraire la série désaisonnalisée (CVS)
tsIPC01_1_5_CVS_RJDmetra <- ts(mysax13$final$series[, "sa"],
                               start = start(ts_Huiles_graisses), frequency = frequency(ts_Huiles_graisses))

outliers <- tso(tsIPC01_1_5_CVS_RJDmetra)

tsIPC01_1_5_CVS_RJDmetra_corr <- outliers$yadj
tsIPC01_1_5_CVS_RJDmetra_corr_diff <- diff(tsIPC01_1_5_CVS_RJDmetra_corr)

ts_train01_1_5 <- window(tsIPC01_1_5_CVS_RJDmetra_corr_diff, start = c(2010, 2), end = c(2022, 12))
ts_test01_1_5 <- window(tsIPC01_1_5_CVS_RJDmetra_corr_diff, start = c(2023, 1), end = c(2023, 12))


fit_ADAM_ETS_01_1_5 <- auto.adam(ts_train01_1_5,
                                 model = "ZZZ",
                                 lags = c(1, 12),
                                 silent = TRUE)
# ZZZ car ne spécifie rien (tendance, saisonnalité, erreur)
fit_ADAM_ETS_01_1_5
summary(fit_ADAM_ETS_01_1_5)

par(mfcol=c(2,2))
plot(fit_ADAM_ETS_01_1_5)
par(mfcol=c(1,1))


plot(fit_ADAM_ETS_01_1_5$states)
plot(fit_ADAM_ETS_01_1_5$residuals)

prev_ADAM_ETS_01_1_5 <- forecast(fit_ADAM_ETS_01_1_5, h=12)
show(prev_ADAM_ETS_01_1_5)
plot(prev_ADAM_ETS_01_1_5)

#-------------- 01.1.6 - Fruits ---------
ts_Fruits <- ts(dfcomp$Fruits, start = c(2010, 1), frequency = 12)
myregx13_01_1_6 <- regarima_x13(ts_Fruits, spec ="RG5c")
s_transform(myregx13_01_1_6)  # test log/level

s_transform(myregx13_01_1_6)
myspec <- x13_spec("RSA5c")
mysax13 <- x13(ts_Fruits, myspec) # SCV de RJGDmetra
#plot(mysax13$final) # JTM

# Extraire la série désaisonnalisée (CVS)
tsIPC01_1_6_CVS_RJDmetra <- ts(mysax13$final$series[, "sa"],
                               start = start(ts_Fruits), frequency = frequency(ts_Fruits))

outliers <- tso(tsIPC01_1_6_CVS_RJDmetra)

tsIPC01_1_6_CVS_RJDmetra_corr <- outliers$yadj
tsIPC01_1_6_CVS_RJDmetra_corr_diff <- diff(tsIPC01_1_6_CVS_RJDmetra_corr)

ts_train01_1_6 <- window(tsIPC01_1_6_CVS_RJDmetra_corr_diff, start = c(2010, 2), end = c(2022, 12))
ts_test01_1_6 <- window(tsIPC01_1_6_CVS_RJDmetra_corr_diff, start = c(2023, 1), end = c(2023, 12))

fit_SSARIMA_01_1_6 <- auto.ssarima(ts_train01_1_6, lags=c(1,12), orders=list(ar=c(3,3), i=(2), ma=c(3,3), select=TRUE))
summary(fit_SSARIMA_01_1_6)

par(mfcol=c(2,2))
plot(fit_SSARIMA_01_1_6)

par(mfcol=c(1,1))

plot(fit_SSARIMA_01_1_6$residuals)

prev_SSARIMA_01_1_6 <- forecast(fit_SSARIMA_01_1_6, h=12)
prev_SSARIMA_01_1_6
plot(prev_SSARIMA_01_1_6)

aic_ssarima <- AIC(fit_SSARIMA_01_1_6)
cat("AIC du modèle SSARIMA :", round(aic_ssarima, 2), "\n")

aicc_ssarima <- AICc(fit_SSARIMA_01_1_6)
cat("AICc du modèle SSARIMA :", round(aicc_ssarima, 2), "\n")



#-------------- 01.1.7 - Légumes ---------

ts_Legumes <- ts(dfcomp$Legumes, start = c(2010, 1), frequency = 12)
myregx13_01_1_7 <- regarima_x13(ts_Legumes, spec ="RG5c")
s_transform(myregx13_01_1_7)  # test log/level

s_transform(myregx13_01_1_7)
myspec <- x13_spec("RSA5c")
mysax13 <- x13(ts_Legumes, myspec) # SCV de RJGDmetra
#plot(mysax13$final) # JTM

# Extraire la série désaisonnalisée (CVS)
tsIPC01_1_7_CVS_RJDmetra <- ts(mysax13$final$series[, "sa"],
                               start = start(ts_Legumes), frequency = frequency(ts_Legumes))

outliers <- tso(tsIPC01_1_7_CVS_RJDmetra)
tsIPC01_1_7_CVS_RJDmetra_corr <- outliers$yadj

tsIPC01_1_7_CVS_RJDmetra_corr_diff <- diff(tsIPC01_1_7_CVS_RJDmetra_corr)

ts_train01_1_7 <- window(tsIPC01_1_7_CVS_RJDmetra_corr_diff, start = c(2010, 2), end = c(2022, 12))
ts_test01_1_7 <- window(tsIPC01_1_7_CVS_RJDmetra_corr_diff, start = c(2023, 1), end = c(2023, 12))

fit_SSARIMA_01_1_7 <- auto.ssarima(ts_train01_1_7, lags=c(1,12), orders=list(ar=c(3,3), i=(2), ma=c(3,3), select=TRUE))
summary(fit_SSARIMA_01_1_7)

par(mfcol=c(2,2))
plot(fit_SSARIMA_01_1_7)

par(mfcol=c(1,1))

plot(fit_SSARIMA_01_1_7$residuals)

prev_SSARIMA_01_1_7 <- forecast(fit_SSARIMA_01_1_7, h=12)
prev_SSARIMA_01_1_7
plot(prev_SSARIMA_01_1_7)

aic_ssarima <- AIC(fit_SSARIMA_01_1_7)
cat("AIC du modèle SSARIMA :", round(aic_ssarima, 2), "\n")

aicc_ssarima <- AICc(fit_SSARIMA_01_1_7)
cat("AICc du modèle SSARIMA :", round(aicc_ssarima, 2), "\n")


#-------------- 01.1.8 - Sucre, confiture, miel, chocolat et confiserie ---------

ts_Sucre_confiture_confiserie <- ts(dfcomp$Sucre_confiture_confiserie, start = c(2010, 1), frequency = 12)

myregx13_01_1_8 <- regarima_x13(ts_Sucre_confiture_confiserie, spec ="RG5c")
s_transform(myregx13_01_1_8)  # test log/level

s_transform(myregx13_01_1_8)
myspec <- x13_spec("RSA5c")
mysax13 <- x13(ts_Sucre_confiture_confiserie, myspec) # SCV de RJGDmetra
#plot(mysax13$final) # JTM

# Extraire la série désaisonnalisée (CVS)
tsIPC01_1_8_CVS_RJDmetra <- ts(mysax13$final$series[, "sa"],
                               start = start(ts_Sucre_confiture_confiserie), frequency = frequency(ts_Sucre_confiture_confiserie))

outliers <- tso(tsIPC01_1_8_CVS_RJDmetra)

tsIPC01_1_8_CVS_RJDmetra_corr <- outliers$yadj

tsIPC01_1_8_CVS_RJDmetra_corr_diff <- diff(tsIPC01_1_8_CVS_RJDmetra_corr)

ts_train01_1_8 <- window(tsIPC01_1_8_CVS_RJDmetra_corr_diff, start = c(2010, 2), end = c(2022, 12))
ts_test01_1_8 <- window(tsIPC01_1_8_CVS_RJDmetra_corr_diff, start = c(2023, 1), end = c(2023, 12))

modele_ar1_01_1_8 <- Arima(ts_train01_1_8, order = c(1, 0, 0), include.mean = TRUE)

summary(modele_ar1_01_1_8)

j <- ncol(modele_ar1_01_1_8$var.coef)
tstat <- matrix(nrow=j, ncol=1)
for(i in 1:j)
{
 tstat[i,1] <- modele_ar1_01_1_8$coef[i]/sqrt(modele_ar1_01_1_8$var.coef[i,i])
}
tstat

fitted_ar1 <- ts_train01_1_8 - residuals(modele_ar1_01_1_8)
plot(fitted_ar1, type = "l", col = "blue", main = "Valeurs ajustées - AR(1)", ylab = "Prévisions")

prev_ar1_01_1_8 <- forecast(modele_ar1_01_1_8, h = 12)
prev_ar1_01_1_8
plot(prev_ar1_01_1_8)

#--------------  01.1.9 - Produits alimentaires n.c.a. ---------

ts_n_c_a <- ts(dfcomp$n_c_a, start = c(2010, 1), frequency = 12)
outliers <- tso(ts_n_c_a)
tsIPC01_1_9_corr <- outliers$yadj
tsIPC01_1_9_corr_diff <- diff(tsIPC01_1_9_corr)

ts_train01_1_9 <- window(tsIPC01_1_9_corr_diff, start = c(2010, 2), end = c(2022, 12))
ts_test01_1_9 <- window(tsIPC01_1_9_corr_diff, start = c(2023, 1), end = c(2023, 12))


modele_arp_01_1_9 <- auto.arima(ts_train01_1_9,
                                d = 0,
                                max.p = 10,        # tu peux ajuster ce maximum
                                max.q = 0,         # pas de MA
                                stationary = TRUE,
                                seasonal = FALSE,
                                stepwise = FALSE,
                                approximation = FALSE)


# Résumé du modèle
summary(modele_arp_01_1_9)

# t-stat des coefficients
j <- length(modele_arp_01_1_9$coef)
tstat_arp <- numeric(j)
for(i in 1:j) {
 tstat_arp[i] <- modele_arp_01_1_9$coef[i] / sqrt(modele_arp_01_1_9$var.coef[i, i])
}
tstat_arp

fitted_arP <- ts_train01_1_9 - residuals(modele_arp_01_1_9)
plot(fitted_arP, type = "l", col = "blue", main = "Valeurs ajustées - AR(1)", ylab = "Prévisions")

prev_arP_01_1_9 <- forecast(modele_arp_01_1_9, h = 12)
prev_arP_01_1_9
plot(prev_arP_01_1_9)


#-------------- 01.2.1 - Café, thé et cacao ---------

ts_Cafe_the_cacao <- ts(dfcomp$Cafe_the_cacao, start = c(2010, 1), frequency = 12)
myregx13_01_2_1 <- regarima_x13(ts_Cafe_the_cacao, spec ="RG5c")
s_transform(myregx13_01_2_1)  # test log/level

s_transform(myregx13_01_2_1)
myspec <- x13_spec("RSA5c")
mysax13 <- x13(ts_Cafe_the_cacao, myspec) # SCV de RJGDmetra
#plot(mysax13$final) # JTM

# Extraire la série désaisonnalisée (CVS)
tsIPC01_2_1_CVS_RJDmetra <- ts(mysax13$final$series[, "sa"],
                               start = start(ts_Cafe_the_cacao), frequency = frequency(ts_Cafe_the_cacao))

outliers <- tso(tsIPC01_2_1_CVS_RJDmetra)
tsIPC01_2_1_CVS_RJDmetra_corr <- outliers$yadj

tsIPC01_2_1_CVS_RJDmetra_corr_diff <- diff(tsIPC01_2_1_CVS_RJDmetra_corr)

ts_train01_2_1 <- window(tsIPC01_2_1_CVS_RJDmetra_corr_diff, start = c(2010, 2), end = c(2022, 12))
ts_test01_2_1 <- window(tsIPC01_2_1_CVS_RJDmetra_corr_diff, start = c(2023, 1), end = c(2023, 12))


modele_ar1_01_2_1 <- Arima(ts_train01_2_1, order = c(1, 0, 0), include.mean = TRUE)

summary(modele_ar1_01_2_1)

j <- ncol(modele_ar1_01_2_1$var.coef)
tstat <- matrix(nrow=j, ncol=1)
for(i in 1:j)
{
 tstat[i,1] <- modele_ar1_01_2_1$coef[i]/sqrt(modele_ar1_01_2_1$var.coef[i,i])
}
tstat

fitted_ar1 <- ts_train01_2_1 - residuals(modele_ar1_01_2_1)
plot(fitted_ar1, type = "l", col = "blue", main = "Valeurs ajustées - AR(1)", ylab = "Prévisions")

prev_ar1_01_2_1 <- forecast(modele_ar1_01_2_1, h = 12)
prev_ar1_01_2_1
plot(prev_ar1_01_2_1)


#-------------- 01.2.2 - Eaux minérales, boissons rafraîchissantes, jus de fruits et de légumes ---------

ts_Eauxminerales_boissonsrafraîchissantes <- ts(dfcomp$Eauxminerales_boissonsrafraîchissantes, start = c(2010, 1), frequency = 12)

myregx13_01_2_2 <- regarima_x13(ts_Eauxminerales_boissonsrafraîchissantes, spec ="RG5c")
s_transform(myregx13_01_2_2)
myspec <- x13_spec("RSA5c")
mysax13 <- x13(ts_Eauxminerales_boissonsrafraîchissantes, myspec) # SCV de RJGDmetra
#plot(mysax13$final) # JTM

# Extraire la série désaisonnalisée (CVS)
tsIPC01_2_2_CVS_RJDmetra <- ts(mysax13$final$series[, "sa"],
                               start = start(ts_Eauxminerales_boissonsrafraîchissantes), frequency = frequency(ts_Eauxminerales_boissonsrafraîchissantes))

outliers <- tso(tsIPC01_2_2_CVS_RJDmetra)

tsIPC01_2_2_CVS_RJDmetra_corr <- outliers$yadj
tsIPC01_2_2_CVS_RJDmetra_corr_diff <- diff(tsIPC01_2_2_CVS_RJDmetra_corr)

ts_train01_2_2 <- window(tsIPC01_2_2_CVS_RJDmetra_corr_diff, start = c(2010, 2), end = c(2022, 12))
ts_test01_2_2 <- window(tsIPC01_2_2_CVS_RJDmetra_corr_diff, start = c(2023, 1), end = c(2023, 12))


myregx13_01_2_2 <- regarima_x13(ts_train01_2_2, spec ="RG5c")
summary(myregx13_01_2_2)
s_transform(myregx13_01_2_2)
#plot(myregx13_01_2_2)
myregx13_01_2_2$forecast
forex13_01_2_2 <- matrix(myregx13_01_2_2$forecast[1:12])
forex13_01_2_2

plot(ts_train01_2_2,
     xlim = c(time(ts_train01_2_2)[1], tail(time(ts_train01_2_2), 1) + 1),
     ylim = range(ts_train01_2_2, forex13_01_2_2),
     main = "Prévisions issues de X13-ARIMA-SEATS",
     ylab = "Valeurs", xlab = "Temps")
lines(ts(forex13_01_2_2,
         start = c(2023, 1),  # à adapter si nécessaire
         frequency = frequency(ts_train01_2_2)),
      col = "blue", lwd = 2, lty = 2)
legend("topleft", legend = c("Série observée", "Prévision X13"),
       col = c("black", "blue"), lty = c(1,2), lwd = 2)


### 2.2.1 add des prev désagrée ----

# Extraire les vecteurs de prévision
prevs_list <- list(
 as.numeric(prev_ar1_01_1_1$mean),       # Pain et céréales
 as.numeric(prev_SSARIMA_01_1_2$mean),   # Viande
 as.numeric(prev_SSARIMA_01_1_3$mean),   # Poissons
 as.numeric(forecast_hw01$mean),         # Lait, fromage et œufs
 as.numeric(prev_ADAM_ETS_01_1_5$mean),  # Huiles et graisses
 as.numeric(prev_SSARIMA_01_1_6$mean),   # Fruits
 as.numeric(prev_SSARIMA_01_1_7$mean),   # Légumes
 as.numeric(prev_ar1_01_1_8$mean),       # Sucre et confiserie
 as.numeric(prev_arP_01_1_9$mean),       # Prod. alimentaires n.c.a.
 as.numeric(prev_ar1_01_2_1$mean),       # Café, thé, cacao
 as.numeric(forex13_01_2_2)              # Boissons non alcoolisées
)

weights <- c(21.82, 18.92, 4.5, 13.04, 2.36, 6.56, 10.83, 7.78, 4.96, 3.51, 5.72) / 100

# Initialiser le vecteur de l'IPC alimentaire
prev_ipc_alimentaire <- numeric(12)

for (i in 1:12) {
 prev_ipc_alimentaire[i] <- sum(sapply(1:length(weights), function(j) weights[j] * prevs_list[[j]][i]))
}

prev_ipc_ts <- ts(prev_ipc_alimentaire, start = c(2023, 1), frequency = 12)


# Affichage complet de la série réelle (sans tronquer la période)
plot(ts_train01,  # série IPC alimentaire observée (2010 → ...)
     ylim = range(ts_train01, prev_ipc_ts),
     main = "Prévision désagrégée IPC alimentaire",
     ylab = "Valeur", xlab = "Temps")

# Ajouter les prévisions désagrégées pour 2023
lines(prev_ipc_ts, col = "blue", lwd = 2, lty = 2)

# Légende
legend("topleft",
       legend = c("Observé", "Prévision désagrégée"),
       col = c("black", "blue"),
       lty = c(1, 2), lwd = 2)

pred_ipc_disagg <- structure(list(
 method = "Prévision désagrégée pondérée",
 mean = prev_ipc_ts,
 lower = NULL,
 upper = NULL,
 level = NULL,
 x = ts_train01,
 fitted = NULL,
 residuals = NULL
), class = "forecast")

# Visualiser comme un vrai forecast
plot(pred_ipc_disagg)


## 2.2 Random Forest ----

df_GB <- read_excel("/Users/sabiron/Documents/Mémoire/Données/dfGB.xlsx")
df_GB <- df_GB[-1,] # On supp la 1er ligne pour mettre la serie diff apres
#View(df_GB)
df_GB$Y <- df2$CVS_RJD_3_corr_diff
str(df_GB)

# 1. Entraînement = lignes 1 à 156 (2010-2022)
train_data <- df_GB[1:155, ]
test_data  <- df_GB[156:167, ]

# 2. Séparer X et Y
X_train <- train_data %>% select(-Date, -Y)
Y_train <- train_data$Y

X_test <- test_data %>% select(-Date, -Y)
Y_test <- test_data$Y

# 3. Entraîner le modèle
set.seed(123)
modele_rf <- randomForest(x = X_train, y = Y_train, ntree = 5000)

# 4. Prédictions sur 2023
predictions <- predict(modele_rf, newdata = X_test)


# Afficher l'importance des variables
importance(modele_rf)

# Visualiser l'importance des variables
varImpPlot(modele_rf,
           main = "Importance des variables - Random Forest",
           type = 2)  # type = 1 pour l'erreur de permutation, type = 2 pour l'impureté Gini

# 5. Évaluation
mae <- mean(abs(predictions - Y_test))
rmse <- sqrt(mean((predictions - Y_test)^2))

cat("MAE (2023) :", mae, "\n")
cat("RMSE (2023) :", rmse, "\n")

# 6. Résumé des prévisions
resultats <- data.frame(
 Date = test_data$Date,
 Y_reel = Y_test,
 Y_prevu = predictions,
 Erreur = Y_test - predictions
)

print(resultats)

library(ggplot2)

ggplot(resultats, aes(x = Date)) +
 geom_line(aes(y = Y_reel, color = "Réel")) +
 geom_line(aes(y = Y_prevu, color = "Prévu")) +
 labs(title = "Prévision de l'IPC - Année 2023", y = "IPC", color = "") +
 theme_minimal()


# Transformer les prédictions en série temporelle (12 mois à partir de janvier 2023)
pred_rf_ts <- ts(predictions, start = c(2023, 1), frequency = 12)

# Recréer correctement l'objet forecast
prev_rf <- structure(list(
 mean = pred_rf_ts,
 lower = matrix(NA, nrow = 12, ncol = 2, dimnames = list(NULL, c("80%", "95%"))),
 upper = matrix(NA, nrow = 12, ncol = 2, dimnames = list(NULL, c("80%", "95%"))),
 level = c(80, 95),
 x = Y_train,
 fitted = fitted(modele_rf),
 method = "Random Forest",
 model = modele_rf
), class = "forecast")

# Donner une classe 'forecast' pour compatibilité avec d'autres fonctions
class(prev_rf) <- "forecast"

# Tu peux maintenant utiliser:
print(prev_rf)



# C) évolution des prévisions----

ts_test01 <- window(ts1IPC01_CVS_RJDmetra_corr_diff, start = c(2023, 1), end = c(2023, 12))

# Fonction mise à jour : transforme la date en format mois
extract_forecast_df <- function(fcast_obj, name) {
 df <- data.frame(
  date = as.Date(as.yearmon(time(fcast_obj$mean))),  # conversion en date
  value = as.numeric(fcast_obj$mean),
  model = name
 )
 df %>% slice_head(n = 12)
}

ts_forex13_01 <- ts(as.numeric(forex13_01), start = c(2023, 1), frequency = 12)

# 2. Construire un objet forecast manuellement (sans recalculer)
forecast_x13_01 <- structure(list(
 mean = ts_forex13_01,  # ici, un vrai ts avec start/end
 lower = matrix(NA, nrow = 12, ncol = 2, dimnames = list(NULL, c("80%", "95%"))),
 upper = matrix(NA, nrow = 12, ncol = 2, dimnames = list(NULL, c("80%", "95%"))),
 level = c(80, 95),
 x = NULL,
 method = "X13",
 fitted = NULL,
 residuals = NULL
), class = "forecast")

prev_rf$mean <- ts(prev_rf$mean, start = c(2023, 1), frequency = 12)

dfs_01 <- list(
 extract_forecast_df(prev_arima, "ARMA"),
 extract_forecast_df(prev_ar1, "AR(1)"),
 extract_forecast_df(prev_arP, "AR(P)"),
 extract_forecast_df(forecast_x13_01, "X13"),
 extract_forecast_df(prevstl, "STL"),
 extract_forecast_df(forecast_hw, "HoltWinters"),
 extract_forecast_df(prev_ADAM_ETS, "ADAM_ETS"),
 extract_forecast_df(prev_AES, "ADAM_ETS+SARIMA"),
 extract_forecast_df(prev_SSARIMA, "SSARIMA"),
 extract_forecast_df(pred_ipc_disagg, "Désagrégée"),
 extract_forecast_df(prev_rf, "Random forest"),
 extract_forecast_df(pred_naive, "Naïve"),
 extract_forecast_df(forecast_armax, "ARX"))

#Je vais comparer aveclesvaleur reel grace a ts_test01
observed_df_01 <- data.frame(
 date = as.Date(as.yearmon(time(ts_test01))),
 value = as.numeric(ts_test01),
 model = "Observée")

# Combine toutes les prévisions et l'observée
df_all_01 <- bind_rows(dfs_01, observed_df_01)
df_all_01$date <- as.Date(df_all_01$date, format = "%Y-%m-%d")
library(ggplot2)

# 1. Graphe simple avec axe en numérique
ggplot(df_all_01, aes(x = as.numeric(date), y = value, color = model)) +
 geom_line(size = 1.2) +
 scale_x_continuous(
  breaks = as.numeric(df_all_01$date[1:12]),
  labels = format(df_all_01$date[1:12], "%b %Y")
 ) +
 labs(
  title = "Prévisions sur 12 mois (2023) par modèle",
  x = "Mois", y = "Valeur prévue", color = "Modèle"
 ) +
 theme_minimal(base_size = 13) +
 theme(
  axis.text.x = element_text(angle = 45, hjust = 1),
  legend.position = "bottom",
  legend.title = element_text(face = "bold"),
  plot.title = element_text(face = "bold")
 )

# 2. Graphe avec gestion des linetypes + couleurs manuelles
ggplot(df_all_01, aes(x = date, y = value, color = model, linetype = model)) +
 geom_line(size = 1.2) +
 labs(
  title = "Prévisions - Sur 12 mois (2023) par modèle",
  x = "Mois", y = "Valeur prévue", color = "Modèle", linetype = "Modèle"
 ) +
 scale_color_manual(values = c(
  "ARMA" = "forestgreen",
  "AR(1)" = "brown",
  "AR(P)" = "darkcyan",
  "X13" = "darkblue",
  "STL" = "yellow",
  "HoltWinters" = "orange",
  "ADAM_ETS" = "grey20",
  "ADAM_ETS+SARIMA" = "red",
  "SSARIMA" = "purple",
  "Désagrégée" = "darkred",
  "Random forest" = "deeppink",
  "ARX" ="deeppink",
  "Naïve" = "black",
  "ARX" ="deeppink",
  "Observée" = "blue"
  
 )) +
 scale_linetype_manual(values = c(
  "ARMA" = "solid",
  "AR(1)" = "dotted",
  "AR(P)" = "dotted",
  "X13" = "dashed",
  "STL" = "solid",
  "HoltWinters" = "solid",
  "ADAM_ETS" = "solid",
  "ADAM_ETS+SARIMA" = "solid",
  "SSARIMA" = "solid",
  "Désagrégée" = "solid",
  "Random forest" = "dotdash",
  "ARX" ="solid",
  "Naïve" = "solid",
  "ARX" = "solid",
  "Observée" = "solid"
  
 )) +
 scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
 theme_minimal(base_size = 13) +
 theme(
  axis.text.x = element_text(angle = 45, hjust = 1),
  legend.position = "bottom",
  legend.title = element_text(face = "bold"),
  plot.title = element_text(face = "bold")
 )

pW <-ggplot(df_all_01, aes(x = date, y = value, color = model, linetype = model)) +
 geom_line(size = 1.2) +
 labs(
  title = "Prévisions - Sur 12 mois (2023) par modèle",
  x = "Mois", y = "Valeur prévue", color = "Modèle", linetype = "Modèle"
 ) +
 scale_color_manual(values = c(
  "ARMA" = "forestgreen",
  "AR(1)" = "brown",
  "AR(P)" = "darkcyan",
  "X13" = "darkblue",
  "STL" = "yellow",
  "HoltWinters" = "orange",
  "ADAM_ETS" = "grey20",
  "ADAM_ETS+SARIMA" = "red",
  "Naïve" = "black",
  "SSARIMA" = "purple",
  "Observée" = "blue"
  
 )) +
 scale_linetype_manual(values = c(
  "ARMA" = "solid",
  "AR(1)" = "dotted",
  "AR(P)" = "dotted",
  "X13" = "dashed",
  "Naïve" = "solid",
  "STL" = "solid",
  "HoltWinters" = "solid",
  "ADAM_ETS" = "solid",
  "ADAM_ETS+SARIMA" = "solid",
  "SSARIMA" = "solid",
  "Observée" = "solid"
  
 )) +
 scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
 theme_minimal(base_size = 13) +
 theme(
  axis.text.x = element_text(angle = 45, hjust = 1),
  legend.position = "bottom",
  legend.title = element_text(face = "bold"),
  plot.title = element_text(face = "bold")
 )


pZ <- ggplot(df_all_01, aes(x = date, y = value, color = model, linetype = model)) +
 geom_line(size = 1.2) +
 labs(
  title = "Prévisions - Sur 12 mois (2023) par modèle",
  x = "Mois", y = "Valeur prévue", color = "Modèle", linetype = "Modèle"
 ) +
 scale_color_manual(values = c(
  "Désagrégée" = "darkred",
  "Random forest" = "deeppink",
  "Naïve" = "black",
  "ARX" ="deeppink",
  "Observée" = "blue"
  
 )) +
 scale_linetype_manual(values = c(
  "Désagrégée" = "solid",
  "Random forest" = "dotdash",
  "Naïve" = "solid",
  "ARX" = "solid",
  "Observée" = "solid"
  
 )) +
 scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
 theme_minimal(base_size = 13) +
 theme(
  axis.text.x = element_text(angle = 45, hjust = 1),
  legend.position = "bottom",
  legend.title = element_text(face = "bold"),
  plot.title = element_text(face = "bold")
 )


pW <-ggplot(df_all_01, aes(x = date, y = value, color = model, linetype = model)) +
 geom_line(size = 1.2) +
 labs(
  title = "Prévisions - Sur 12 mois (2023) par modèle",
  x = "Mois", y = "Valeur prévue", color = "Modèle", linetype = "Modèle"
 ) +
 scale_color_manual(values = c(
  "ARMA" = "forestgreen",
  "AR(1)" = "brown",
  "AR(P)" = "darkcyan",
  "X13" = "darkblue",
  "STL" = "yellow",
  "HoltWinters" = "orange",
  "ADAM_ETS" = "grey20",
  "ADAM_ETS+SARIMA" = "red",
  "Naïve" = "black",
  "SSARIMA" = "purple",
  "Observée" = "blue"
  
 )) +
 scale_linetype_manual(values = c(
  "ARMA" = "solid",
  "AR(1)" = "dotted",
  "AR(P)" = "dotted",
  "X13" = "dashed",
  "Naïve" = "solid",
  "STL" = "solid",
  "HoltWinters" = "solid",
  "ADAM_ETS" = "solid",
  "ADAM_ETS+SARIMA" = "solid",
  "SSARIMA" = "solid",
  "Observée" = "solid"
  
 )) +
 scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
 theme_minimal(base_size = 13) +
 theme(
  axis.text.x = element_text(angle = 45, hjust = 1),
  legend.position = "bottom",
  legend.title = element_text(face = "bold"),
  plot.title = element_text(face = "bold")
 )


pA <- ggplot(df_all_01, aes(x = date, y = value, color = model, linetype = model)) +
 geom_line(size = 1.2) +
 labs(
  title = "Prévisions - Sur 12 mois (2023)- methdode désagrégée",
  x = "Mois", y = "Valeur prévue", color = "Modèle", linetype = "Modèle"
 ) +
 scale_color_manual(values = c(
  "Désagrégée" = "darkred",
  "Observée" = "blue"
  
 )) +
 scale_linetype_manual(values = c(
  "Désagrégée" = "solid",
  "Observée" = "solid"
  
 )) +
 scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
 theme_minimal(base_size = 13) +
 theme(
  axis.text.x = element_text(angle = 45, hjust = 1),
  legend.position = "bottom",
  legend.title = element_text(face = "bold"),
  plot.title = element_text(face = "bold")
 )

pA
# D) qualité de prevision -----

## MSE & R²OOS
## MSE & R²OOS avec le modèle Naïf comme référence

# Observations réelles
actual_values01 <- ts_test01

# Liste des prévisions pour chaque modèle
forecasts_01 <- list(
 prev_arima$mean,
 prev_ar1$mean,
 prev_arP$mean,
 forecast_x13_01$mean,
 prevstl$mean,
 forecast_hw$mean,
 prev_ADAM_ETS$mean,
 prev_AES$mean,
 prev_SSARIMA$mean,
 pred_ipc_disagg$mean,
 prev_rf$mean,
 forecast_armax$mean,
 pred_naive$mean
 
)


# Fonction MSE + R²OOS avec benchmark explicite
calculate_metrics_01 <- function(actual, forecast, benchmark_forecast) {
 mse <- mean((actual - forecast)^2)
 sst <- sum((actual - benchmark_forecast)^2)
 sse <- sum((actual - forecast)^2)
 r2oos <- 1 - (sse / sst)
 return(list(mse = mse, r2oos = r2oos))
}

# Naïve utilisé comme benchmark
benchmark_forecast <- as.numeric(pred_naive$mean)

# Calcul des métriques
metrics <- lapply(forecasts_01, function(fcast) {
 calculate_metrics_01(as.numeric(actual_values01), as.numeric(fcast), benchmark_forecast)
})

# Data frame de résultats
metrics_df_01 <- as.data.frame(do.call(rbind, metrics))
rownames(metrics_df_01) <- c(
 "ARMA", "AR(1)", "AR(P)", "X13", "STL", "HoltWinters",
 "ADAM_ETS", "ADAM_ETS+SARIMA", "SSARIMA", "Désagrégée",
 "Random forest", "ARX", "Naïve"
)

print(metrics_df_01)

## CSPE

# Fonction CSPE
calculate_cspe <- function(actual, forecast) {
 errors <- (actual - forecast)^2
 cspe <- cumsum(errors)
 return(cspe)
}

# CSPE pour chaque modèle
cspe_list <- lapply(forecasts_01, function(fcast) {
 calculate_cspe(as.numeric(actual_values01), as.numeric(fcast))
})

# Création du data.frame CSPE
cspe_df <- do.call(cbind, cspe_list)
colnames(cspe_df) <- rownames(metrics_df_01)
cspe_df <- cbind(Date = as.Date(time(actual_values01)), cspe_df)
cspe_df <- as.data.frame(cspe_df)

# Reshape long format
cspe_df_long <- pivot_longer(cspe_df, cols = -Date, names_to = "Model", values_to = "CSPE")
cspe_df_long$Date <- as.Date(cspe_df_long$Date)

# Affichage CSPE
ggplot(cspe_df_long, aes(x = Date, y = CSPE, color = Model, linetype = Model)) +
 geom_line(size = 0.8) +
 labs(title = "Cumulative Squared Prediction Errors (CSPE)",
      x = "Date", y = "CSPE", color = "Modèle", linetype = "Modèle") +
 scale_color_manual(values = c(
  "ARMA" = "forestgreen",
  "AR(1)" = "brown",
  "AR(P)" = "darkcyan",
  "X13" = "darkblue",
  "STL" = "gold",
  "HoltWinters" = "orange",
  "ADAM_ETS" = "grey20",
  "ADAM_ETS+SARIMA" = "red",
  "SSARIMA" = "purple",
  "Désagrégée" = "darkred",
  "Random forest" = "deeppink",
  "Naïve" = "black",
  "ARX" = "pink"  # dark pink
 )) +
 scale_linetype_manual(values = c(
  "ARMA" = "solid", "AR(1)" = "dotted", "AR(P)" = "dotted",
  "X13" = "dashed", "STL" = "solid", "HoltWinters" = "solid",
  "ADAM_ETS" = "solid", "ADAM_ETS+SARIMA" = "solid",
  "SSARIMA" = "solid", "Désagrégée" = "solid",
  "Random forest" = "dotdash", "Naïve" = "solid",
  "ARX" = "solid"
 )) +
 
 scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
 theme_minimal(base_size = 13) +
 theme(axis.text.x = element_text(angle = 45, hjust = 1),
       legend.position = "bottom",
       legend.title = element_text(face = "bold"),
       plot.title = element_text(face = "bold"))



# E) Test de précision-----

# Définir la période de temps
start_date <- c(2023, 1)
end_date <- c(2023, 12)

# Fonction pour ajuster les séries temporelles
adjust_time_series <- function(ts_data) {
 return(window(ts_data, start = start_date, end = end_date))
}

# Ajuster les séries temporelles pour chaque modèle
for_observed     <- adjust_time_series(ts_test01)
for_ARMA         <- adjust_time_series(prev_arima$mean)
for_AR1          <- adjust_time_series(prev_ar1$mean)
for_ARP          <- adjust_time_series(prev_arP$mean)
for_X13          <- adjust_time_series(forecast_x13_01$mean)
for_STL          <- adjust_time_series(prevstl$mean)
for_HW           <- adjust_time_series(forecast_hw$mean)
for_ADAM_ETS     <- adjust_time_series(prev_ADAM_ETS$mean)
for_AES          <- adjust_time_series(prev_AES$mean)
for_SSARIMA      <- adjust_time_series(prev_SSARIMA$mean)
for_DESAGREGEE   <- adjust_time_series(pred_ipc_disagg$mean)
for_RF           <- adjust_time_series(prev_rf$mean)
for_ARMAX        <- adjust_time_series(forecast_armax$mean)
for_NAIVE        <- adjust_time_series(pred_naive$mean)


# Vérifiez que toutes les séries temporelles ont la bonne longueur

# ts_test01 <- window(tsIPC01_2_2_CVS_RJDmetra_corr_diff, start = c(2023, 1), end = c(2023, 12))
# ts_test01_2_2 <- window(tsIPC01_2_2_CVS_RJDmetra_corr_diff, start = c(2023, 1), end = c(2023, 12))
# print(length(ts_test01))

print(length(for_observed))
print(length(for_ARMA))
print(length(for_AR1))
print(length(for_ARP))
print(length(for_X13))
print(length(for_STL))
print(length(for_HW))
print(length(for_ADAM_ETS))
print(length(for_AES))
print(length(for_SSARIMA))
print(length(for_DESAGREGEE))
print(length(for_RF))
print(length(for_ARMAX))
print(length(for_NAIVE))


# Calculer les erreurs de prévision
error_ARMA <- for_ARMA - for_observed
error_AR1 <- for_AR1 - for_observed
error_ARP <- for_ARP - for_observed
error_X13 <- for_X13 - for_observed
error_HW <- for_HW - for_observed
error_ADAM_ETS <- for_ADAM_ETS - for_observed
error_AES <- for_AES - for_observed
error_SSARIMA <- for_SSARIMA - for_observed
error_NAIVE <- for_NAIVE - for_observed
error_STL <- for_STL - for_observed
error_DESAGREGEE <- for_DESAGREGEE - for_observed
error_RF <- for_RF - for_observed
error_ARMAX <- for_ARMAX - for_observed


# Calculer les MSE
mse_ARMA <- mean(for_ARMA^2)
mse_AR1 <- mean(for_AR1^2)
mse_ARP <- mean(for_ARP^2)
mse_X13 <- mean(for_X13^2)
mse_HW <- mean(for_HW^2)
mse_ADAM <- mean(for_ADAM_ETS^2)
mse_AES <- mean(for_AES^2)
mse_SSARIMA <- mean(for_SSARIMA^2)
mse_NAIVE <- mean(for_NAIVE^2)
mse_STL <- mean(for_STL^2)
mse_DESAGREGEE <- mean(for_DESAGREGEE^2)
mse_RF <- mean(for_RF^2)
mse_ARMAX <- mean(for_ARMAX^2)


# Calculer d'autres mesures d'erreur
accuracy(for_ARMA, for_observed, h = 12)
accuracy(for_AR1, for_observed, h = 12)
accuracy(for_ARP, for_observed, h = 12)
accuracy(for_X13, for_observed, h = 12)
accuracy(for_HW, for_observed, h = 12)
accuracy(for_ADAM_ETS, for_observed, h = 12)
accuracy(for_AES, for_observed, h = 12)
accuracy(for_SSARIMA, for_observed, h = 12)
accuracy(for_NAIVE, for_observed, h = 12)
accuracy(for_STL, for_observed, h = 12)
accuracy(for_DESAGREGEE, for_observed, h = 12)
accuracy(for_RF, for_observed, h = 12)
accuracy(for_ARMAX, for_observed, h = 12)

# Calculer le test DM

dm.test(error_NAIVE, error_ARMA, h = length(for_observed))
dm.test(error_NAIVE, error_AR1, h = length(for_observed))
dm.test(error_NAIVE, error_ARP, h = length(for_observed))
dm.test(error_NAIVE, error_X13, h = length(for_observed))
dm.test(error_NAIVE, error_HW, h = length(for_observed))
dm.test(error_NAIVE, error_ADAM_ETS, h = length(for_observed))
dm.test(error_NAIVE, error_AES, h = length(for_observed))
dm.test(error_NAIVE, error_SSARIMA, h = length(for_observed))
dm.test(error_NAIVE, error_STL, h = length(for_observed))
dm.test(error_NAIVE, error_DESAGREGEE, h = length(for_observed))
dm.test(error_NAIVE, error_RF, h = length(for_observed))
dm.test(error_NAIVE, error_ARMAX, h = length(for_observed))


# Test de Diebold-Mariano avec h = 1
dm.test(error_NAIVE, error_ARMA, h = 1)
dm.test(error_NAIVE, error_AR1, h = 1)
dm.test(error_NAIVE, error_ARP, h = 1)
dm.test(error_NAIVE, error_X13, h = 1)
dm.test(error_NAIVE, error_HW, h = 1)
dm.test(error_NAIVE, error_ADAM_ETS, h = 1)
dm.test(error_NAIVE, error_AES, h = 1)
dm.test(error_NAIVE, error_SSARIMA, h = 1)
dm.test(error_NAIVE, error_STL, h = 1)
dm.test(error_NAIVE, error_DESAGREGEE, h = 1)
dm.test(error_NAIVE, error_RF, h = 1)
dm.test(error_NAIVE, error_ARMAX, h = 1)




# F) Rowling désagrée -----


#-------------- F) 01.1.1 - Pain et céréales ---------

ts_train01_1_1 <- window(tsIPC01_1_1_CVS_RJDmetra_corr_diff, start = c(2010, 2), end = c(2022, 12))
ts_test01_1_1 <- window(tsIPC01_1_1_CVS_RJDmetra_corr_diff, start = c(2023, 1), end = c(2023, 12))

adj_total <- tsIPC01_1_1_CVS_RJDmetra_corr_diff

estim <- 155  # taille de l'échantillon d'apprentissage
h <- 12       # horizon de prévision

format_ARIMA_01_1_1 <- matrix(nrow = h, ncol = 1)

for (i in 1:h) {
 # Définir la fenêtre glissante
 rolling_series <- adj_total[i:(estim - 1 + i)]
 
 # Recréer l'objet ts avec bonne date de début
 start_year <- 2010 + floor((i - 1) / 12)
 start_month <- 2 + ((i - 1) %% 12)
 if (start_month > 12) {
  start_year <- start_year + 1
  start_month <- start_month - 12
 }
 
 rolling_ts <- ts(rolling_series, start = c(start_year, start_month), frequency = 12)
 
 # ARIMA(1,0,0)
 model_ARIMA <- Arima(rolling_ts, order = c(1, 0, 0), include.mean = TRUE)
 forecast_ARIMA <- forecast(model_ARIMA, h = 1)
 
 format_ARIMA_01_1_1[i, 1] <- forecast_ARIMA$mean[1]
}

print(format_ARIMA_01_1_1)

# Créer un data frame comparatif
compare_01_1_1 <- data.frame(
 Mois = time(ts_test01_1_1),
 Valeur_réelle = as.numeric(ts_test01_1_1),
 Prévision_ARIMA = as.numeric(format_ARIMA_01_1_1)
)

# Afficher les résultats
print(compare_01_1_1)


# Tracer les valeurs réelles et prédites
plot(1:12, compare_01_1_1$Valeur_réelle, type = "l", col = "black", lwd = 2,
     xlab = "Mois", ylab = "Valeur différenciée",
     main = "Prévision rolling vs Réel - 01.1.1 Pain et céréales",
     xaxt = "n",
     ylim = range(c(compare_01_1_1$Valeur_réelle, compare_01_1_1$Prévision_ARIMA)) * c(0.95, 1.05),
     xlim = c(0.5, 12.5))
axis(1, at = 1:12, labels = month.abb, las = 2)

# Ajouter la courbe de prévision
lines(1:12, compare_01_1_1$Prévision_ARIMA, col = "blue", lwd = 2, lty = 2)



#--------------  test ----

# 1. Ajuster le modèle AR(1) sur le train uniquement
model_ARIMA_static <- Arima(ts_train01_1_1, order = c(1, 0, 0), include.mean = TRUE)

# 2. Faire une prévision sur 12 mois
forecast_ARIMA_static <- forecast(model_ARIMA_static, h = 12)
forecast_ARIMA_static

# 3. Créer un data frame comparatif avec les prévisions
compare_static_01_1_1 <- data.frame(
 Mois = time(ts_test01_1_1),
 Réel = as.numeric(ts_test01_1_1),
 Prévision = as.numeric(forecast_ARIMA_static$mean)
)

# Tracer les valeurs réelles
plot(1:12, compare_static_01_1_1$Réel, type = "l", col = "black", lwd = 2,
     ylim = range(c(compare_static_01_1_1$Réel, compare_static_01_1_1$Prévision)) * c(0.9, 1.1),
     xlab = "Mois", ylab = "Valeur différenciée",
     main = "Prévision statique vs Réel - 01.1.1 Pain et céréales",
     xaxt = "n", xlim = c(0.5, 12.5))

# Ajouter les mois comme étiquettes
axis(1, at = 1:12, labels = month.abb, las = 2)

# Ajouter la série de prévisions
lines(1:12, compare_static_01_1_1$Prévision, col = "blue", lwd = 2, lty = 2)


#--------------  F)  01.1.2 - Viande ---------

library(smooth)  # pour auto.ssarima()
library(forecast)

# Série corrigée (déjà différenciée si nécessaire)
adj_total <- tsIPC01_1_2_CVS_RJDmetra_corr_diff

# Paramètres de la fenêtre
estim <- 155  # taille du jeu d'entraînement
h <- 12       # nombre de mois de test

# Stockage des prévisions
format_SSARIMA_01_1_2 <- matrix(nrow = h, ncol = 1)

for (i in 1:h) {
 # Fenêtre glissante : observations de i à estim - 1 + i
 rolling_series <- adj_total[i:(estim - 1 + i)]
 
 # Reconstruire la série ts avec date correcte
 start_year <- 2010 + floor((i - 1) / 12)
 start_month <- 2 + ((i - 1) %% 12)
 if (start_month > 12) {
  start_year <- start_year + 1
  start_month <- start_month - 12
 }
 
 rolling_ts <- ts(rolling_series, start = c(start_year, start_month), frequency = 12)
 
 # Modèle SSARIMA
 model_SSARIMA <- auto.ssarima(rolling_ts, lags = c(1, 12),
                               orders = list(ar = c(3, 3), i = 2, ma = c(3, 3), select = TRUE))
 
 # Prévision à 1 pas
 forecast_SSARIMA <- forecast(model_SSARIMA, h = 1)
 
 # Enregistrer la prévision
 format_SSARIMA_01_1_2[i, 1] <- forecast_SSARIMA$mean[1]
}

# Comparaison avec la vraie série de test
compare_01_1_2 <- data.frame(
 Mois = time(ts_test01_1_2),
 Valeur_réelle = as.numeric(ts_test01_1_2),
 Prévision_SSARIMA = as.numeric(format_SSARIMA_01_1_2)
)

# Afficher la comparaison
print(compare_01_1_2)

# Tracer les valeurs réelles et prédites
plot(1:12, compare_01_1_2$Valeur_réelle, type = "l", col = "black", lwd = 2,
     xlab = "Mois", ylab = "Valeur différenciée",
     main = "Prévision rolling SSARIMA vs Réel - 01.1.2 Viande",
     xaxt = "n",
     ylim = range(c(compare_01_1_2$Valeur_réelle, compare_01_1_2$Prévision_SSARIMA)) * c(0.95, 1.05),
     xlim = c(0.5, 12.5))

axis(1, at = 1:12, labels = month.abb, las = 2)
lines(1:12, compare_01_1_2$Prévision_SSARIMA, col = "blue", lwd = 2, lty = 2)



#--------------  test ----

# 1. Ajuster le modèle AR(1) sur le train uniquement
model_SSARIMA_test_0112 <- auto.ssarima(ts_train01_1_2, lags=c(1,12), orders=list(ar=c(3,3), i=(2), ma=c(3,3), select=TRUE))

# 2. Faire une prévision sur 12 mois
forecast_ARIMA_static <- forecast(model_SSARIMA_test_0112, h = 12)
forecast_ARIMA_static

# 3. Créer un data frame comparatif avec les prévisions
compare_static_01_1_1 <- data.frame(
 Mois = time(ts_test01_1_2),
 Réel = as.numeric(ts_test01_1_2),
 Prévision = as.numeric(forecast_ARIMA_static$mean)
)

# Tracer les valeurs réelles
plot(1:12, compare_static_01_1_1$Réel, type = "l", col = "black", lwd = 2,
     ylim = range(c(compare_static_01_1_1$Réel, compare_static_01_1_1$Prévision)) * c(0.9, 1.1),
     xlab = "Mois", ylab = "Valeur différenciée",
     main = "Prévision statique vs Réel - 01.1.1 Pain et céréales",
     xaxt = "n", xlim = c(0.5, 12.5))

# Ajouter les mois comme étiquettes
axis(1, at = 1:12, labels = month.abb, las = 2)

# Ajouter la série de prévisions
lines(1:12, compare_static_01_1_1$Prévision, col = "blue", lwd = 2, lty = 2)









#-------------- F)  01.1.3 - Poissons et fruits de mer ---------

# Série corrigée
adj_total <- tsIPC01_1_3_CVS_RJDmetra_corr_diff

# Fenêtre de prévision
estim <- 155
h <- 12

# Initialiser la matrice de prévisions
format_SSARIMA_01_1_3 <- matrix(nrow = h, ncol = 1)

for (i in 1:h) {
 # Fenêtre glissante
 rolling_series <- adj_total[i:(estim - 1 + i)]
 
 # Reconstruire l'objet ts
 start_year <- 2010 + floor((i - 1) / 12)
 start_month <- 2 + ((i - 1) %% 12)
 if (start_month > 12) {
  start_year <- start_year + 1
  start_month <- start_month - 12
 }
 
 rolling_ts <- ts(rolling_series, start = c(start_year, start_month), frequency = 12)
 
 # Modèle SSARIMA
 model_SSARIMA <- auto.ssarima(
  rolling_ts,
  lags = c(1, 12),
  orders = list(ar = c(3, 3), i = 2, ma = c(3, 3), select = TRUE)
 )
 
 # Prévision à 1 mois
 forecast_SSARIMA <- forecast(model_SSARIMA, h = 1)
 
 # Enregistrement
 format_SSARIMA_01_1_3[i, 1] <- forecast_SSARIMA$mean[1]
}

# Comparaison avec valeurs réelles
compare_01_1_3 <- data.frame(
 Mois = time(ts_test01_1_3),
 Valeur_réelle = as.numeric(ts_test01_1_3),
 Prévision_SSARIMA = as.numeric(format_SSARIMA_01_1_3)
)

# Affichage des résultats
print(compare_01_1_3)

# Graphe
plot(1:12, compare_01_1_3$Valeur_réelle, type = "l", col = "black", lwd = 2,
     xlab = "Mois", ylab = "Valeur différenciée",
     main = "Prévision rolling SSARIMA vs Réel - 01.1.3 Poissons & fruits de mer",
     xaxt = "n",
     ylim = range(c(compare_01_1_3$Valeur_réelle, compare_01_1_3$Prévision_SSARIMA)) * c(0.95, 1.05),
     xlim = c(0.5, 12.5))

axis(1, at = 1:12, labels = month.abb, las = 2)
lines(1:12, compare_01_1_3$Prévision_SSARIMA, col = "blue", lwd = 2, lty = 2)



#--------------  test ----

# 1. Ajuster le modèle AR(1) sur le train uniquement
model_SSARIMA_test_0113 <- auto.ssarima(ts_train01_1_3, lags=c(1,12), orders=list(ar=c(3,3), i=(2), ma=c(3,3), select=TRUE))

# 2. Faire une prévision sur 12 mois
forecast_ARIMA_static <- forecast(model_SSARIMA_test_0113, h = 12)
forecast_ARIMA_static

# 3. Créer un data frame comparatif avec les prévisions
compare_static_01_1_1 <- data.frame(
 Mois = time(ts_test01_1_3),
 Réel = as.numeric(ts_test01_1_3),
 Prévision = as.numeric(forecast_ARIMA_static$mean)
)

# Tracer les valeurs réelles
plot(1:12, compare_static_01_1_1$Réel, type = "l", col = "black", lwd = 2,
     ylim = range(c(compare_static_01_1_1$Réel, compare_static_01_1_1$Prévision)) * c(0.9, 1.1),
     xlab = "Mois", ylab = "Valeur différenciée",
     main = "Prévision statique vs Réel - 01.1.1 Pain et céréales",
     xaxt = "n", xlim = c(0.5, 12.5))

# Ajouter les mois comme étiquettes
axis(1, at = 1:12, labels = month.abb, las = 2)

# Ajouter la série de prévisions
lines(1:12, compare_static_01_1_1$Prévision, col = "blue", lwd = 2, lty = 2)



#--------------   F)  01.1.4 - Lait, fromage et oeufs ---------

# Série corrigée
adj_total <- tsIPC01_1_4_CVS_RJDmetra_corr_diff

# Paramètres de fenêtre
estim <- 155
h <- 12

# Stockage des prévisions
format_HW_01_1_4 <- matrix(nrow = h, ncol = 1)

for (i in 1:h) {
 # Fenêtre glissante
 rolling_series <- adj_total[i:(estim - 1 + i)]
 
 # Créer un ts avec fréquence correcte
 start_year <- 2010 + floor((i - 1) / 12)
 start_month <- 2 + ((i - 1) %% 12)
 if (start_month > 12) {
  start_year <- start_year + 1
  start_month <- start_month - 12
 }
 
 rolling_ts <- ts(rolling_series, start = c(start_year, start_month), frequency = 12)
 
 # Modèle Holt sans saisonnalité (gamma = FALSE)
 model_HW <- HoltWinters(rolling_ts, gamma = FALSE)
 
 # Prévision à 1 pas
 forecast_HW <- forecast(model_HW, h = 1)
 
 # Enregistrement
 format_HW_01_1_4[i, 1] <- forecast_HW$mean[1]
}

# Comparer aux données réelles
compare_01_1_4 <- data.frame(
 Mois = time(ts_test01_1_4),
 Valeur_réelle = as.numeric(ts_test01_1_4),
 Prévision_HW = as.numeric(format_HW_01_1_4)
)

# Afficher
print(compare_01_1_4)

# Tracer les valeurs réelles et prédites avec HoltWinters
plot(1:12, compare_01_1_4$Valeur_réelle, type = "l", col = "black", lwd = 2,
     xlab = "Mois", ylab = "Valeur différenciée",
     main = "Prévision rolling HoltWinters vs Réel - 01.1.4 Lait, fromage, oeufs",
     xaxt = "n",
     ylim = range(c(compare_01_1_4$Valeur_réelle, compare_01_1_4$Prévision_HW)) * c(0.95, 1.05),
     xlim = c(0.5, 12.5))

# Ajouter les mois comme étiquettes
axis(1, at = 1:12, labels = month.abb, las = 2)

# Ajouter la courbe des prévisions
lines(1:12, compare_01_1_4$Prévision_HW, col = "blue", lwd = 2, lty = 2)



#--------------  F) 01.1.5 - Huiles et graisses ---------

# Série corrigée
adj_total <- tsIPC01_1_5_CVS_RJDmetra_corr_diff

# Paramètres
estim <- 155
h <- 12

# Stockage des prévisions
format_ADAM_01_1_5 <- matrix(nrow = h, ncol = 1)

for (i in 1:h) {
 # Fenêtre glissante
 rolling_series <- adj_total[i:(estim - 1 + i)]
 
 # Recréer l'objet ts
 start_year <- 2010 + floor((i - 1) / 12)
 start_month <- 2 + ((i - 1) %% 12)
 if (start_month > 12) {
  start_year <- start_year + 1
  start_month <- start_month - 12
 }
 
 rolling_ts <- ts(rolling_series, start = c(start_year, start_month), frequency = 12)
 
 # Modèle ADAM ETS automatique
 model_ADAM <- auto.adam(rolling_ts, model = "ZZZ", lags = c(1, 12), silent = TRUE)
 
 # Prévision à 1 pas
 forecast_ADAM <- forecast(model_ADAM, h = 1)
 
 # Enregistrer
 format_ADAM_01_1_5[i, 1] <- forecast_ADAM$mean[1]
}

# Comparaison avec la série réelle de test
compare_01_1_5 <- data.frame(
 Mois = time(ts_test01_1_5),
 Valeur_réelle = as.numeric(ts_test01_1_5),
 Prévision_ADAM = as.numeric(format_ADAM_01_1_5)
)

# Afficher les résultats
print(compare_01_1_5)

# Graphe comparatif
plot(1:12, compare_01_1_5$Valeur_réelle, type = "l", col = "black", lwd = 2,
     xlab = "Mois", ylab = "Valeur différenciée",
     main = "Prévision rolling ADAM ETS vs Réel - 01.1.5 Huiles et graisses",
     xaxt = "n",
     ylim = range(c(compare_01_1_5$Valeur_réelle, compare_01_1_5$Prévision_ADAM)) * c(0.95, 1.05),
     xlim = c(0.5, 12.5))

axis(1, at = 1:12, labels = month.abb, las = 2)
lines(1:12, compare_01_1_5$Prévision_ADAM, col = "blue", lwd = 2, lty = 2)


#--------------  F)  01.1.6 - Fruits ---------

# Série corrigée
adj_total <- tsIPC01_1_6_CVS_RJDmetra_corr_diff

# Paramètres
estim <- 155
h <- 12

# Stockage des prévisions
format_SSARIMA_01_1_6 <- matrix(nrow = h, ncol = 1)

for (i in 1:h) {
 # Fenêtre glissante
 rolling_series <- adj_total[i:(estim - 1 + i)]
 
 # Recréer l'objet ts
 start_year <- 2010 + floor((i - 1) / 12)
 start_month <- 2 + ((i - 1) %% 12)
 if (start_month > 12) {
  start_year <- start_year + 1
  start_month <- start_month - 12
 }
 
 rolling_ts <- ts(rolling_series, start = c(start_year, start_month), frequency = 12)
 
 # Modèle SSARIMA
 model_SSARIMA <- auto.ssarima(
  rolling_ts,
  lags = c(1, 12),
  orders = list(ar = c(3, 3), i = 2, ma = c(3, 3), select = TRUE)
 )
 
 # Prévision à 1 pas
 forecast_SSARIMA <- forecast(model_SSARIMA, h = 1)
 
 # Enregistrer
 format_SSARIMA_01_1_6[i, 1] <- forecast_SSARIMA$mean[1]
}

# Comparaison avec les données réelles
compare_01_1_6 <- data.frame(
 Mois = time(ts_test01_1_6),
 Valeur_réelle = as.numeric(ts_test01_1_6),
 Prévision_SSARIMA = as.numeric(format_SSARIMA_01_1_6)
)

# Afficher
print(compare_01_1_6)

# Graphique
plot(1:12, compare_01_1_6$Valeur_réelle, type = "l", col = "black", lwd = 2,
     xlab = "Mois", ylab = "Valeur différenciée",
     main = "Prévision rolling SSARIMA vs Réel - 01.1.6 Fruits",
     xaxt = "n",
     ylim = range(c(compare_01_1_6$Valeur_réelle, compare_01_1_6$Prévision_SSARIMA)) * c(0.95, 1.05),
     xlim = c(0.5, 12.5))

axis(1, at = 1:12, labels = month.abb, las = 2)
lines(1:12, compare_01_1_6$Prévision_SSARIMA, col = "blue", lwd = 2, lty = 2)


#--------------  F)  01.1.7 - Légumes ---------

# Série corrigée
adj_total <- tsIPC01_1_7_CVS_RJDmetra_corr_diff

# Paramètres
estim <- 155
h <- 12

# Stockage des prévisions
format_SSARIMA_01_1_7 <- matrix(nrow = h, ncol = 1)

for (i in 1:h) {
 # Fenêtre glissante
 rolling_series <- adj_total[i:(estim - 1 + i)]
 
 # Recréer l'objet ts
 start_year <- 2010 + floor((i - 1) / 12)
 start_month <- 2 + ((i - 1) %% 12)
 if (start_month > 12) {
  start_year <- start_year + 1
  start_month <- start_month - 12
 }
 
 rolling_ts <- ts(rolling_series, start = c(start_year, start_month), frequency = 12)
 
 # Modèle SSARIMA
 model_SSARIMA <- auto.ssarima(
  rolling_ts,
  lags = c(1, 12),
  orders = list(ar = c(3, 3), i = 2, ma = c(3, 3), select = TRUE)
 )
 
 # Prévision à 1 pas
 forecast_SSARIMA <- forecast(model_SSARIMA, h = 1)
 
 # Enregistrement
 format_SSARIMA_01_1_7[i, 1] <- forecast_SSARIMA$mean[1]
}

# Comparaison avec données réelles
compare_01_1_7 <- data.frame(
 Mois = time(ts_test01_1_7),
 Valeur_réelle = as.numeric(ts_test01_1_7),
 Prévision_SSARIMA = as.numeric(format_SSARIMA_01_1_7)
)

# Affichage
print(compare_01_1_7)

# Graphique
plot(1:12, compare_01_1_7$Valeur_réelle, type = "l", col = "black", lwd = 2,
     xlab = "Mois", ylab = "Valeur différenciée",
     main = "Prévision rolling SSARIMA vs Réel - 01.1.7 Légumes",
     xaxt = "n",
     ylim = range(c(compare_01_1_7$Valeur_réelle, compare_01_1_7$Prévision_SSARIMA)) * c(0.95, 1.05),
     xlim = c(0.5, 12.5))

axis(1, at = 1:12, labels = month.abb, las = 2)
lines(1:12, compare_01_1_7$Prévision_SSARIMA, col = "blue", lwd = 2, lty = 2)


#--------------  F)  01.1.8 - Sucre, confiture, miel, chocolat et confiserie ---------

# Série corrigée
adj_total <- tsIPC01_1_8_CVS_RJDmetra_corr_diff

# Paramètres
estim <- 155
h <- 12

# Stockage des prévisions
format_ARIMA_01_1_8 <- matrix(nrow = h, ncol = 1)

for (i in 1:h) {
 # Fenêtre glissante
 rolling_series <- adj_total[i:(estim - 1 + i)]
 
 # Recréer l'objet ts avec bonne fréquence
 start_year <- 2010 + floor((i - 1) / 12)
 start_month <- 2 + ((i - 1) %% 12)
 if (start_month > 12) {
  start_year <- start_year + 1
  start_month <- start_month - 12
 }
 
 rolling_ts <- ts(rolling_series, start = c(start_year, start_month), frequency = 12)
 
 # Modèle ARIMA(1,0,0)
 model_ARIMA <- Arima(rolling_ts, order = c(1, 0, 0), include.mean = TRUE)
 
 # Prévision à 1 pas
 forecast_ARIMA <- forecast(model_ARIMA, h = 1)
 
 # Enregistrement
 format_ARIMA_01_1_8[i, 1] <- forecast_ARIMA$mean[1]
}

# Comparaison avec les vraies valeurs de 2023
compare_01_1_8 <- data.frame(
 Mois = time(ts_test01_1_8),
 Valeur_réelle = as.numeric(ts_test01_1_8),
 Prévision_ARIMA = as.numeric(format_ARIMA_01_1_8)
)

# Affichage
print(compare_01_1_8)

# Graphique
plot(1:12, compare_01_1_8$Valeur_réelle, type = "l", col = "black", lwd = 2,
     xlab = "Mois", ylab = "Valeur différenciée",
     main = "Prévision rolling AR(1) vs Réel - 01.1.8 Sucre, chocolat, confiserie",
     xaxt = "n",
     ylim = range(c(compare_01_1_8$Valeur_réelle, compare_01_1_8$Prévision_ARIMA)) * c(0.95, 1.05),
     xlim = c(0.5, 12.5))

axis(1, at = 1:12, labels = month.abb, las = 2)
lines(1:12, compare_01_1_8$Prévision_ARIMA, col = "blue", lwd = 2, lty = 2)


#--------------  F)  01.1.9 - Produits alimentaires n.c.a. ---------

# Série corrigée
adj_total <- tsIPC01_1_9_corr_diff

# Paramètres
estim <- 155
h <- 12

# Stockage des prévisions
format_ARp_01_1_9 <- matrix(nrow = h, ncol = 1)

for (i in 1:h) {
 # Fenêtre glissante
 rolling_series <- adj_total[i:(estim - 1 + i)]
 
 # Recréer objet ts
 start_year <- 2010 + floor((i - 1) / 12)
 start_month <- 2 + ((i - 1) %% 12)
 if (start_month > 12) {
  start_year <- start_year + 1
  start_month <- start_month - 12
 }
 
 rolling_ts <- ts(rolling_series, start = c(start_year, start_month), frequency = 12)
 
 # Modèle AR(p) via auto.arima (sans MA, pas saisonnier)
 model_ARp <- auto.arima(
  rolling_ts,
  d = 0,
  max.p = 10,
  max.q = 0,
  stationary = TRUE,
  seasonal = FALSE,
  stepwise = FALSE,
  approximation = FALSE
 )
 
 # Prévision à 1 pas
 forecast_ARp <- forecast(model_ARp, h = 1)
 
 # Enregistrer
 format_ARp_01_1_9[i, 1] <- forecast_ARp$mean[1]
}

# Comparaison avec valeurs réelles
compare_01_1_9 <- data.frame(
 Mois = time(ts_test01_1_9),
 Valeur_réelle = as.numeric(ts_test01_1_9),
 Prévision_ARp = as.numeric(format_ARp_01_1_9)
)

# Affichage
print(compare_01_1_9)

# Graphique
plot(1:12, compare_01_1_9$Valeur_réelle, type = "l", col = "black", lwd = 2,
     xlab = "Mois", ylab = "Valeur différenciée",
     main = "Prévision rolling AR(p) vs Réel - 01.1.9 Produits n.c.a.",
     xaxt = "n",
     ylim = range(c(compare_01_1_9$Valeur_réelle, compare_01_1_9$Prévision_ARp)) * c(0.95, 1.05),
     xlim = c(0.5, 12.5))

axis(1, at = 1:12, labels = month.abb, las = 2)
lines(1:12, compare_01_1_9$Prévision_ARp, col = "blue", lwd = 2, lty = 2)



#--------------  F)  01.2.1 - Café, thé et cacao ---------

library(forecast)

# Série corrigée
adj_total <- tsIPC01_2_1_CVS_RJDmetra_corr_diff

# Paramètres
estim <- 155
h <- 12

# Stockage des prévisions
format_ARIMA_01_2_1 <- matrix(nrow = h, ncol = 1)

for (i in 1:h) {
 # Fenêtre glissante
 rolling_series <- adj_total[i:(estim - 1 + i)]
 
 # Recréer ts
 start_year <- 2010 + floor((i - 1) / 12)
 start_month <- 2 + ((i - 1) %% 12)
 if (start_month > 12) {
  start_year <- start_year + 1
  start_month <- start_month - 12
 }
 
 rolling_ts <- ts(rolling_series, start = c(start_year, start_month), frequency = 12)
 
 # ARIMA(1,0,0)
 model_ARIMA <- Arima(rolling_ts, order = c(1, 0, 0), include.mean = TRUE)
 
 # Prévision à 1 mois
 forecast_ARIMA <- forecast(model_ARIMA, h = 1)
 
 # Enregistrement
 format_ARIMA_01_2_1[i, 1] <- forecast_ARIMA$mean[1]
}

# Comparaison avec les vraies valeurs
compare_01_2_1 <- data.frame(
 Mois = time(ts_test01_2_1),
 Valeur_réelle = as.numeric(ts_test01_2_1),
 Prévision_ARIMA = as.numeric(format_ARIMA_01_2_1)
)

# Affichage
print(compare_01_2_1)

# Graphique
plot(1:12, compare_01_2_1$Valeur_réelle, type = "l", col = "black", lwd = 2,
     xlab = "Mois", ylab = "Valeur différenciée",
     main = "Prévision rolling AR(1) vs Réel - 01.2.1 Café, thé, cacao",
     xaxt = "n",
     ylim = range(c(compare_01_2_1$Valeur_réelle, compare_01_2_1$Prévision_ARIMA)) * c(0.95, 1.05),
     xlim = c(0.5, 12.5))

axis(1, at = 1:12, labels = month.abb, las = 2)
lines(1:12, compare_01_2_1$Prévision_ARIMA, col = "blue", lwd = 2, lty = 2)


#--------------   F) 01.2.2 - Eaux minérales, boissons rafraîchissantes, jus de fruits et de légumes ---------

# Série corrigée
adj_total <- tsIPC01_2_2_CVS_RJDmetra_corr_diff

# Paramètres
estim <- 155
h <- 12

# Prévisions rolling
format_X13_01_2_2 <- matrix(nrow = h, ncol = 1)

for (i in 1:h) {
 # Fenêtre glissante
 rolling_series <- adj_total[i:(estim - 1 + i)]
 
 # Recréer ts avec bonne date
 start_year <- 2010 + floor((i - 1) / 12)
 start_month <- 2 + ((i - 1) %% 12)
 if (start_month > 12) {
  start_year <- start_year + 1
  start_month <- start_month - 12
 }
 
 rolling_ts <- ts(rolling_series, start = c(start_year, start_month), frequency = 12)
 
 # Modèle regarima_x13 RG5c
 model_X13 <- regarima_x13(rolling_ts, spec = "RG5c")
 
 # Extraire prévision à 1 mois
 forecast_1 <- model_X13$forecast[1]
 format_X13_01_2_2[i, 1] <- forecast_1
}

# Comparaison avec les vraies valeurs
compare_01_2_2_rolling <- data.frame(
 Mois = time(ts_test01_2_2),
 Valeur_réelle = as.numeric(ts_test01_2_2),
 Prévision_X13 = as.numeric(format_X13_01_2_2)
)

# Afficher les prévisions
print(compare_01_2_2_rolling)

# Tracer le graphique
plot(1:12, compare_01_2_2_rolling$Valeur_réelle, type = "l", col = "black", lwd = 2,
     main = "Prévision rolling X13 vs Réel - 01.2.2 Eaux, jus, boissons",
     xlab = "Mois", ylab = "Valeur différenciée",
     xaxt = "n",
     ylim = range(c(compare_01_2_2_rolling$Valeur_réelle, compare_01_2_2_rolling$Prévision_X13)) * c(0.95, 1.05),
     xlim = c(0.5, 12.5))

axis(1, at = 1:12, labels = month.abb, las = 2)
lines(1:12, compare_01_2_2_rolling$Prévision_X13, col = "blue", lwd = 2, lty = 2)

# Graphique
plot(1:12, compare_01_2_1$Valeur_réelle, type = "l", col = "black", lwd = 2,
     xlab = "Mois", ylab = "Valeur différenciée",
     main = "Prévision rolling AR(1) vs Réel - 01.2.1 Café, thé, cacao",
     xaxt = "n",
     ylim = range(c(compare_01_2_1$Valeur_réelle, compare_01_2_1$Prévision_ARIMA)) * c(0.95, 1.05),
     xlim = c(0.5, 12.5))

axis(1, at = 1:12, labels = month.abb, las = 2)
lines(1:12, compare_01_2_1$Prévision_ARIMA, col = "blue", lwd = 2, lty = 2)

plot(1:12, compare_01_1_9$Valeur_réelle, type = "l", col = "black", lwd = 2,
     xlab = "Mois", ylab = "Valeur différenciée",
     main = "Prévision rolling AR(p) vs Réel - 01.1.9 Produits n.c.a.",
     xaxt = "n",
     ylim = range(c(compare_01_1_9$Valeur_réelle, compare_01_1_9$Prévision_ARp)) * c(0.95, 1.05),
     xlim = c(0.5, 12.5))

axis(1, at = 1:12, labels = month.abb, las = 2)
lines(1:12, compare_01_1_9$Prévision_ARp, col = "blue", lwd = 2, lty = 2)

plot(1:12, compare_01_1_8$Valeur_réelle, type = "l", col = "black", lwd = 2,
     xlab = "Mois", ylab = "Valeur différenciée",
     main = "Prévision rolling AR(1) vs Réel - 01.1.8 Sucre, chocolat, confiserie",
     xaxt = "n",
     ylim = range(c(compare_01_1_8$Valeur_réelle, compare_01_1_8$Prévision_ARIMA)) * c(0.95, 1.05),
     xlim = c(0.5, 12.5))

axis(1, at = 1:12, labels = month.abb, las = 2)
lines(1:12, compare_01_1_8$Prévision_ARIMA, col = "blue", lwd = 2, lty = 2)

# Graphique
plot(1:12, compare_01_1_7$Valeur_réelle, type = "l", col = "black", lwd = 2,
     xlab = "Mois", ylab = "Valeur différenciée",
     main = "Prévision rolling SSARIMA vs Réel - 01.1.7 Légumes",
     xaxt = "n",
     ylim = range(c(compare_01_1_7$Valeur_réelle, compare_01_1_7$Prévision_SSARIMA)) * c(0.95, 1.05),
     xlim = c(0.5, 12.5))

axis(1, at = 1:12, labels = month.abb, las = 2)
lines(1:12, compare_01_1_7$Prévision_SSARIMA, col = "blue", lwd = 2, lty = 2)

# Graphique
plot(1:12, compare_01_1_6$Valeur_réelle, type = "l", col = "black", lwd = 2,
     xlab = "Mois", ylab = "Valeur différenciée",
     main = "Prévision rolling SSARIMA vs Réel - 01.1.6 Fruits",
     xaxt = "n",
     ylim = range(c(compare_01_1_6$Valeur_réelle, compare_01_1_6$Prévision_SSARIMA)) * c(0.95, 1.05),
     xlim = c(0.5, 12.5))

axis(1, at = 1:12, labels = month.abb, las = 2)
lines(1:12, compare_01_1_6$Prévision_SSARIMA, col = "blue", lwd = 2, lty = 2)
# Graphe comparatif
plot(1:12, compare_01_1_5$Valeur_réelle, type = "l", col = "black", lwd = 2,
     xlab = "Mois", ylab = "Valeur différenciée",
     main = "Prévision rolling ADAM ETS vs Réel - 01.1.5 Huiles et graisses",
     xaxt = "n",
     ylim = range(c(compare_01_1_5$Valeur_réelle, compare_01_1_5$Prévision_ADAM)) * c(0.95, 1.05),
     xlim = c(0.5, 12.5))

axis(1, at = 1:12, labels = month.abb, las = 2)
lines(1:12, compare_01_1_5$Prévision_ADAM, col = "blue", lwd = 2, lty = 2)
# Tracer les valeurs réelles et prédites avec HoltWinters
plot(1:12, compare_01_1_4$Valeur_réelle, type = "l", col = "black", lwd = 2,
     xlab = "Mois", ylab = "Valeur différenciée",
     main = "Prévision rolling HoltWinters vs Réel - 01.1.4 Lait, fromage, oeufs",
     xaxt = "n",
     ylim = range(c(compare_01_1_4$Valeur_réelle, compare_01_1_4$Prévision_HW)) * c(0.95, 1.05),
     xlim = c(0.5, 12.5))

# Ajouter les mois comme étiquettes
axis(1, at = 1:12, labels = month.abb, las = 2)

# Ajouter la courbe des prévisions
lines(1:12, compare_01_1_4$Prévision_HW, col = "blue", lwd = 2, lty = 2)

# Tracer les valeurs réelles et prédites
plot(1:12, compare_01_1_2$Valeur_réelle, type = "l", col = "black", lwd = 2,
     xlab = "Mois", ylab = "Valeur différenciée",
     main = "Prévision rolling SSARIMA vs Réel - 01.1.2 Viande",
     xaxt = "n",
     ylim = range(c(compare_01_1_2$Valeur_réelle, compare_01_1_2$Prévision_SSARIMA)) * c(0.95, 1.05),
     xlim = c(0.5, 12.5))

axis(1, at = 1:12, labels = month.abb, las = 2)
lines(1:12, compare_01_1_2$Prévision_SSARIMA, col = "blue", lwd = 2, lty = 2)

plot(1:12, compare_01_1_1$Valeur_réelle, type = "l", col = "black", lwd = 2,
     xlab = "Mois", ylab = "Valeur différenciée",
     main = "Prévision rolling vs Réel - 01.1.1 Pain et céréales",
     xaxt = "n",
     ylim = range(c(compare_01_1_1$Valeur_réelle, compare_01_1_1$Prévision_ARIMA)) * c(0.95, 1.05),
     xlim = c(0.5, 12.5))
axis(1, at = 1:12, labels = month.abb, las = 2)

# Ajouter la courbe de prévision
lines(1:12, compare_01_1_1$Prévision_ARIMA, col = "blue", lwd = 2, lty = 2)

# ----- F.2 Agréée ----

formats_list <- list(
 format_ARIMA_01_1_1,     # 01.1.1 Pain et céréales
 format_SSARIMA_01_1_2,   # 01.1.2 Viande
 format_SSARIMA_01_1_3,   # 01.1.3 Poissons
 format_HW_01_1_4,        # 01.1.4 Lait, fromage, œufs
 format_ADAM_01_1_5,      # 01.1.5 Huiles
 format_SSARIMA_01_1_6,   # 01.1.6 Fruits
 format_SSARIMA_01_1_7,   # 01.1.7 Légumes
 format_ARIMA_01_1_8,     # 01.1.8 Sucre, confiture
 format_ARp_01_1_9,       # 01.1.9 Produits n.c.a
 format_ARIMA_01_2_1,     # 01.2.1 Café, thé, cacao
 format_X13_01_2_2        # 01.2.2 Eaux, jus, boissons
)


weights <- c(21.82, 18.92, 4.5, 13.04, 2.36, 6.56, 10.83, 7.78, 4.96, 3.51, 5.72) / 100

h <- 12  # 12 mois
rolling_aggregated_forecast <- numeric(h)

for (i in 1:h) {
 rolling_aggregated_forecast[i] <- sum(sapply(1:length(weights), function(j) {
  formats_list[[j]][i, 1] * weights[j]
 }))
}

# --- Création du data.frame avec la série réelle agrégée si dispo ---
aggregate_compare <- data.frame(
 Mois = time(ts_test01_1_1),  # ou toute autre série de référence 2023
 Prévision_agrégée = rolling_aggregated_forecast
)

# --- Tracer le résultat ---
# --- Prévisions déjà agrégées (poids * prévisions rolling) ---
# (assure-toi d’avoir exécuté la partie précédente avec rolling_aggregated_forecast)

# --- Valeurs réelles de l’IPC agrégé sur 12 mois ---
ts_test01 <- window(ts1IPC01_CVS_RJDmetra_corr_diff, start = c(2023, 1), end = c(2023, 12))

# --- DataFrame comparatif ---
compare_agrégé <- data.frame(
 Mois = time(ts_test01),
 Valeur_réelle = as.numeric(ts_test01),
 Prévision_agrégée = rolling_aggregated_forecast
)

# --- Affichage du tableau ---
print(compare_agrégé)

# --- Graphique comparatif ---
plot(1:12, compare_agrégé$Valeur_réelle, type = "l", col = "black", lwd = 2,
     xlab = "Mois", ylab = "IPC (valeur corrigée)",
     main = "Prévision rolling agrégée vs Réel - IPC Produits alimentaires",
     xaxt = "n",
     ylim = range(c(compare_agrégé$Valeur_réelle, compare_agrégé$Prévision_agrégée)) * c(0.95, 1.05),
     xlim = c(0.5, 12.5))

axis(1, at = 1:12, labels = month.abb, las = 2)
lines(1:12, compare_agrégé$Prévision_agrégée, col = "blue", lwd = 2, lty = 2)
legend("topright", legend = c("Réel", "Prévision agrégée"), col = c("black", "blue"),
       lty = c(1, 2), lwd = 2)


# Initialiser les noms des séries
series_names <- c("Pain et céréales", "Viande", "Poissons et fruits de mer",
                  "Lait, fromage et œufs", "Huiles et graisses", "Fruits",
                  "Légumes", "Sucre, confiture, chocolat", "Produits n.c.a.",
                  "Café, thé, cacao", "Eaux, jus, boissons")

# Créer le data.frame vide
df_forecasts <- data.frame(matrix(NA, nrow = 12, ncol = length(series_names)))
colnames(df_forecasts) <- series_names
rownames(df_forecasts) <- month.abb  # ou paste("Mois", 1:12)

# Remplissage avec les formats rolling (12 prévisions chacun)
df_forecasts[["Pain et céréales"]] <- format_ARIMA_01_1_1
df_forecasts[["Viande"]] <- format_SSARIMA_01_1_2
df_forecasts[["Poissons et fruits de mer"]] <- format_SSARIMA_01_1_3
df_forecasts[["Lait, fromage et œufs"]] <- format_HW_01_1_4
df_forecasts[["Huiles et graisses"]] <- format_ADAM_01_1_5
df_forecasts[["Fruits"]] <- format_SSARIMA_01_1_6
df_forecasts[["Légumes"]] <- format_SSARIMA_01_1_7
df_forecasts[["Sucre, confiture, chocolat"]] <- format_ARIMA_01_1_8
df_forecasts[["Produits n.c.a."]] <- format_ARp_01_1_9
df_forecasts[["Café, thé, cacao"]] <- format_ARIMA_01_2_1
df_forecasts[["Eaux, jus, boissons"]] <- format_X13_01_2_2

# Afficher le tableau
View(df_forecasts)



## F.3 Graphique indivuel des prev rowling -----
par(mfrow = c(1, 1))

par(mfrow = c(3, 2))

# 1. Pain et céréales
plot(1:12, compare_01_1_1$Valeur_réelle, type = "l", col = "black", lwd = 2,
     xlab = "Mois", ylab = "Valeur différenciée",
     main = "Prévision rolling vs Réel - 01.1.1 Pain et céréales",
     xaxt = "n", xlim = c(0.5, 12.5),
     ylim = range(c(compare_01_1_1$Valeur_réelle, compare_01_1_1$Prévision_ARIMA)) * c(0.95, 1.05))
axis(1, at = 1:12, labels = month.abb, las = 2)
lines(1:12, compare_01_1_1$Prévision_ARIMA, col = "blue", lwd = 2, lty = 2)

# 2. Viande
plot(1:12, compare_01_1_2$Valeur_réelle, type = "l", col = "black", lwd = 2,
     xlab = "Mois", ylab = "Valeur différenciée",
     main = "Prévision rolling SSARIMA vs Réel - 01.1.2 Viande",
     xaxt = "n", xlim = c(0.5, 12.5),
     ylim = range(c(compare_01_1_2$Valeur_réelle, compare_01_1_2$Prévision_SSARIMA)) * c(0.95, 1.05))
axis(1, at = 1:12, labels = month.abb, las = 2)
lines(1:12, compare_01_1_2$Prévision_SSARIMA, col = "blue", lwd = 2, lty = 2)

# 3.  Poissons & fruits de mer
plot(1:12, compare_01_1_3$Valeur_réelle, type = "l", col = "black", lwd = 2,
     xlab = "Mois", ylab = "Valeur différenciée",
     main = "Prévision rolling SSARIMA vs Réel - 01.1.3 Poissons & fruits de mer",
     xaxt = "n",
     ylim = range(c(compare_01_1_3$Valeur_réelle, compare_01_1_3$Prévision_SSARIMA)) * c(0.95, 1.05),
     xlim = c(0.5, 12.5))

axis(1, at = 1:12, labels = month.abb, las = 2)
lines(1:12, compare_01_1_3$Prévision_SSARIMA, col = "blue", lwd = 2, lty = 2)


# 3. Lait, fromage, œufs
plot(1:12, compare_01_1_4$Valeur_réelle, type = "l", col = "black", lwd = 2,
     xlab = "Mois", ylab = "Valeur différenciée",
     main = "Prévision rolling HoltWinters vs Réel - 01.1.4 Lait, fromage, oeufs",
     xaxt = "n", xlim = c(0.5, 12.5),
     ylim = range(c(compare_01_1_4$Valeur_réelle, compare_01_1_4$Prévision_HW)) * c(0.95, 1.05))
axis(1, at = 1:12, labels = month.abb, las = 2)
lines(1:12, compare_01_1_4$Prévision_HW, col = "blue", lwd = 2, lty = 2)

# 4. Huiles et graisses
plot(1:12, compare_01_1_5$Valeur_réelle, type = "l", col = "black", lwd = 2,
     xlab = "Mois", ylab = "Valeur différenciée",
     main = "Prévision rolling ADAM ETS vs Réel - 01.1.5 Huiles et graisses",
     xaxt = "n", xlim = c(0.5, 12.5),
     ylim = range(c(compare_01_1_5$Valeur_réelle, compare_01_1_5$Prévision_ADAM)) * c(0.95, 1.05))
axis(1, at = 1:12, labels = month.abb, las = 2)
lines(1:12, compare_01_1_5$Prévision_ADAM, col = "blue", lwd = 2, lty = 2)

# 5. Fruits
plot(1:12, compare_01_1_6$Valeur_réelle, type = "l", col = "black", lwd = 2,
     xlab = "Mois", ylab = "Valeur différenciée",
     main = "Prévision rolling SSARIMA vs Réel - 01.1.6 Fruits",
     xaxt = "n", xlim = c(0.5, 12.5),
     ylim = range(c(compare_01_1_6$Valeur_réelle, compare_01_1_6$Prévision_SSARIMA)) * c(0.95, 1.05))
axis(1, at = 1:12, labels = month.abb, las = 2)
lines(1:12, compare_01_1_6$Prévision_SSARIMA, col = "blue", lwd = 2, lty = 2)

# -- Deuxième série de 6 plots --
par(mfrow = c(3, 2))

# 6. Légumes
plot(1:12, compare_01_1_7$Valeur_réelle, type = "l", col = "black", lwd = 2,
     xlab = "Mois", ylab = "Valeur différenciée",
     main = "Prévision rolling SSARIMA vs Réel - 01.1.7 Légumes",
     xaxt = "n", xlim = c(0.5, 12.5),
     ylim = range(c(compare_01_1_7$Valeur_réelle, compare_01_1_7$Prévision_SSARIMA)) * c(0.95, 1.05))
axis(1, at = 1:12, labels = month.abb, las = 2)
lines(1:12, compare_01_1_7$Prévision_SSARIMA, col = "blue", lwd = 2, lty = 2)


# 7. Sucre, chocolat, confiserie
plot(1:12, compare_01_1_8$Valeur_réelle, type = "l", col = "black", lwd = 2,
     xlab = "Mois", ylab = "Valeur différenciée",
     main = "Prévision rolling AR(1) vs Réel - 01.1.8 Sucre, chocolat, confiserie",
     xaxt = "n", xlim = c(0.5, 12.5),
     ylim = range(c(compare_01_1_8$Valeur_réelle, compare_01_1_8$Prévision_ARIMA)) * c(0.95, 1.05))
axis(1, at = 1:12, labels = month.abb, las = 2)
lines(1:12, compare_01_1_8$Prévision_ARIMA, col = "blue", lwd = 2, lty = 2)

# 8. Produits n.c.a.
plot(1:12, compare_01_1_9$Valeur_réelle, type = "l", col = "black", lwd = 2,
     xlab = "Mois", ylab = "Valeur différenciée",
     main = "Prévision rolling AR(p) vs Réel - 01.1.9 Produits n.c.a.",
     xaxt = "n", xlim = c(0.5, 12.5),
     ylim = range(c(compare_01_1_9$Valeur_réelle, compare_01_1_9$Prévision_ARp)) * c(0.95, 1.05))
axis(1, at = 1:12, labels = month.abb, las = 2)
lines(1:12, compare_01_1_9$Prévision_ARp, col = "blue", lwd = 2, lty = 2)


# 9. Café, thé, cacao
plot(1:12, compare_01_2_1$Valeur_réelle, type = "l", col = "black", lwd = 2,
     xlab = "Mois", ylab = "Valeur différenciée",
     main = "Prévision rolling AR(1) vs Réel - 01.2.1 Café, thé, cacao",
     xaxt = "n", xlim = c(0.5, 12.5),
     ylim = range(c(compare_01_2_1$Valeur_réelle, compare_01_2_1$Prévision_ARIMA)) * c(0.95, 1.05))
axis(1, at = 1:12, labels = month.abb, las = 2)
lines(1:12, compare_01_2_1$Prévision_ARIMA, col = "blue", lwd = 2, lty = 2)

# 10. Eaux, jus, boissons
plot(1:12, compare_01_2_2_rolling$Valeur_réelle, type = "l", col = "black", lwd = 2,
     main = "Prévision rolling X13 vs Réel - 01.2.2 Eaux, jus, boissons",
     xlab = "Mois", ylab = "Valeur différenciée",
     xaxt = "n", xlim = c(0.5, 12.5),
     ylim = range(c(compare_01_2_2_rolling$Valeur_réelle, compare_01_2_2_rolling$Prévision_X13)) * c(0.95, 1.05))
axis(1, at = 1:12, labels = month.abb, las = 2)
lines(1:12, compare_01_2_2_rolling$Prévision_X13, col = "blue", lwd = 2, lty = 2)


plot(1:12, compare_01_2_2_rolling$Valeur_réelle, type = "l", col = "white", lwd = 2,
     main = "Légende",
     xlab = "", ylab = "",
     xaxt = "n", xlim = c(0.5, 12.5),
     ylim = range(c(compare_01_2_2_rolling$Valeur_réelle, compare_01_2_2_rolling$Prévision_X13)) * c(0.95, 1.05))

# Légende en plus gros
legend("topright",
       legend = c("Valeur réelle", "Prévision"),
       col = c("black", "blue"),
       lty = c(1, 2),
       lwd = 2,
       cex = 1.5,   # ⬅️ Ajuste la taille ici (1.5 = 150% de la taille normale)
       bty = "n")



df <- read_excel("~/Documents/Mémoire/Données/dfGB.xlsx")
#df <- read_excel("~/Documents/Mémoire/Données/df_ARX")
df$Date <- as.Date(df$Date)
str(df)


library(ggplot2)
library(patchwork)

# H) Graphique des variables ----

p1 <- ggplot(df, aes(x = Date, y = Prix_Petrole, color = Prix_Petrole)) +
 geom_line(size = 1) +
 scale_color_gradient(low = "blue", high = "darkgreen") +
 labs(title = "Cours du pétrole brut Brent, en dollars par baril",
      x = "Temps", y = "Cours du pétrole", color = "Prix_Petrole") +
 theme_minimal(base_size = 14) +
 theme(legend.position = "right") +
 
 # Événements majeurs
 geom_vline(xintercept = as.Date("2020-03-01"), linetype = "dashed", color = "red") +
 annotate("text", x = as.Date("2020-03-01"), y = max(df$Prix_Petrole, na.rm = TRUE), label = "Covid-19", angle = 90, vjust = -0.5, color = "red") +
 
 geom_vline(xintercept = as.Date("2022-02-01"), linetype = "dashed", color = "red") +
 annotate("text", x = as.Date("2022-02-01"), y = max(df$Prix_Petrole, na.rm = TRUE), label = "Ukraine", angle = 90, vjust = -0.5, color = "red") +
 
 # Source
 annotate("text", x = min(df$Date), y = min(df$Prix_Petrole, na.rm = TRUE),
          label = "Source : INSEE", hjust = 0, size = 3.5, color = "black")
p1

# 2/ Tx_change_EUR_USD

p2 <- ggplot(df, aes(x = Date, y = Tx_change_EUR_USD, color = Tx_change_EUR_USD)) +
 geom_line(size = 1) +
 scale_color_gradient(low = "blue", high = "darkgreen") +
 labs(title = "Évolution du taux de change USD/EUR",
      x = "Temps", y = "euros pour 1 USD", color = "Tx_change_EUR_USD") +
 theme_minimal(base_size = 14) +
 theme(legend.position = "right") +
 
 # Événements majeurs
 geom_vline(xintercept = as.Date("2020-03-01"), linetype = "dashed", color = "red") +
 annotate("text", x = as.Date("2020-03-01"), y = max(df$Tx_change_EUR_USD, na.rm = TRUE), label = "Covid-19", angle = 90, vjust = -0.5, color = "red") +
 
 geom_vline(xintercept = as.Date("2022-02-01"), linetype = "dashed", color = "red") +
 annotate("text", x = as.Date("2022-02-01"), y = max(df$Tx_change_EUR_USD, na.rm = TRUE), label = "Ukraine", angle = 90, vjust = -0.5, color = "red") +
 
 # Source
 annotate("text", x = min(df$Date), y = min(df$Tx_change_EUR_USD, na.rm = TRUE),
          label = "Source : INSEE", hjust = 0, size = 3.5, color = "black")
p2

# 3/ Taux_directeur

p3 <- ggplot(df, aes(x = Date, y = Taux_directeur, color = Taux_directeur)) +
 geom_line(size = 1) +
 scale_color_gradient(low = "blue", high = "darkgreen") +
 labs(title = "Évolution du taux directeur de la BCE",
      x = "Temps", y = "Taux d’intérêt directeur", color = "Taux_directeur") +
 theme_minimal(base_size = 14) +
 theme(legend.position = "right") +
 
 # Événements majeurs
 geom_vline(xintercept = as.Date("2020-03-01"), linetype = "dashed", color = "red") +
 annotate("text", x = as.Date("2020-03-01"), y = max(df$Taux_directeur, na.rm = TRUE), label = "Covid-19", angle = 90, vjust = -0.5, color = "red") +
 
 geom_vline(xintercept = as.Date("2022-02-01"), linetype = "dashed", color = "red") +
 annotate("text", x = as.Date("2022-02-01"), y = max(df$Taux_directeur, na.rm = TRUE), label = "Ukraine", angle = 90, vjust = -0.5, color = "red") +
 
 # Source
 annotate("text", x = min(df$Date), y = min(df$Taux_directeur, na.rm = TRUE),
          label = "Source : Banque de France, Webstat", hjust = 0, size = 3.5, color = "black")

p3

# 4/ IPP

p4 <- ggplot(df, aes(x = Date, y = IPP, color = IPP)) +
 geom_line(size = 1) +
 scale_color_gradient(low = "blue", high = "darkgreen") +
 labs(title = "Évolution Indices de prix de production et d’importation (IPPI)",
      x = "Temps", y = "Taux d’intérêt directeur", color = "IPP") +
 theme_minimal(base_size = 14) +
 theme(legend.position = "right") +
 
 # Événements majeurs
 geom_vline(xintercept = as.Date("2020-03-01"), linetype = "dashed", color = "red") +
 annotate("text", x = as.Date("2020-03-01"), y = max(df$IPP, na.rm = TRUE), label = "Covid-19", angle = 90, vjust = -0.5, color = "red") +
 
 geom_vline(xintercept = as.Date("2022-02-01"), linetype = "dashed", color = "red") +
 annotate("text", x = as.Date("2022-02-01"), y = max(df$IPP, na.rm = TRUE), label = "Ukraine", angle = 90, vjust = -0.5, color = "red") +
 
 # Source
 annotate("text", x = min(df$Date), y = min(df$IPP, na.rm = TRUE),
          label = "Source : Banque de France, Webstat", hjust = 0, size = 3.5, color = "black")

p4

# 5/ cout_horaire

p5 <- ggplot(df, aes(x = Date, y = cout_horaire, color = cout_horaire)) +
 geom_line(size = 1) +
 scale_color_gradient(low = "blue", high = "darkgreen") +
 labs(title = "Evolution du coût horaire du travail",
      x = "Temps", y = "coût du travail", color = "cout_horaire") +
 theme_minimal(base_size = 14) +
 theme(legend.position = "right") +
 
 # Événements majeurs
 geom_vline(xintercept = as.Date("2020-03-01"), linetype = "dashed", color = "red") +
 annotate("text", x = as.Date("2020-03-01"), y = max(df$cout_horaire, na.rm = TRUE), label = "Covid-19", angle = 90, vjust = -0.5, color = "red") +
 
 geom_vline(xintercept = as.Date("2022-02-01"), linetype = "dashed", color = "red") +
 annotate("text", x = as.Date("2022-02-01"), y = max(df$cout_horaire, na.rm = TRUE), label = "Ukraine", angle = 90, vjust = -0.5, color = "red") +
 
 # Source
 annotate("text", x = min(df$Date), y = min(df$cout_horaire, na.rm = TRUE),
          label = "Source : INSEE", hjust = 0, size = 3.5, color = "black")

p5

# 6/ consommation_ménages_alim

p6 <- ggplot(df, aes(x = Date, y = consommation_ménages_alim, color = consommation_ménages_alim)) +
 geom_line(size = 1) +
 scale_color_gradient(low = "blue", high = "darkgreen") +
 labs(title = "Evolution de la consommation des ménages en biens",
      x = "Temps", y = "coût du travail", color = "consommation_ménages_alim") +
 theme_minimal(base_size = 14) +
 theme(legend.position = "right") +
 
 # Événements majeurs
 geom_vline(xintercept = as.Date("2020-03-01"), linetype = "dashed", color = "red") +
 annotate("text", x = as.Date("2020-03-01"), y = max(df$consommation_ménages_alim, na.rm = TRUE), label = "Covid-19", angle = 90, vjust = -0.5, color = "red") +
 
 geom_vline(xintercept = as.Date("2022-02-01"), linetype = "dashed", color = "red") +
 annotate("text", x = as.Date("2022-02-01"), y = max(df$consommation_ménages_alim, na.rm = TRUE), label = "Ukraine", angle = 90, vjust = -0.5, color = "red") +
 
 # Source
 annotate("text", x = min(df$Date), y = min(df$consommation_ménages_alim, na.rm = TRUE),
          label = "Source : INSEE", hjust = 0, size = 3.5, color = "black")


# 7/ Imporation_combustibles

p7 <- ggplot(df, aes(x = Date, y = Imporation_combustibles, color = Imporation_combustibles)) +
 geom_line(size = 1) +
 scale_color_gradient(low = "blue", high = "darkgreen") +
 labs(title = "Evolution des importations de combustibles",
      x = "Temps", y = "importations de combustibles", color = "Imporation_combustibles") +
 theme_minimal(base_size = 14) +
 theme(legend.position = "right") +
 
 # Événements majeurs
 geom_vline(xintercept = as.Date("2020-03-01"), linetype = "dashed", color = "red") +
 annotate("text", x = as.Date("2020-03-01"), y = max(df$Imporation_combustibles, na.rm = TRUE), label = "Covid-19", angle = 90, vjust = -0.5, color = "red") +
 
 geom_vline(xintercept = as.Date("2022-02-01"), linetype = "dashed", color = "red") +
 annotate("text", x = as.Date("2022-02-01"), y = max(df$Imporation_combustibles, na.rm = TRUE), label = "Ukraine", angle = 90, vjust = -0.5, color = "red") +
 
 # Source
 annotate("text", x = min(df$Date), y = min(df$Imporation_combustibles, na.rm = TRUE),
          label = "Source : INSEE", hjust = 0, size = 3.5, color = "black")

p7

# 8/ confiance_menages

p8<- ggplot(df, aes(x = Date, y = confiance_menages, color = confiance_menages)) +
 geom_line(size = 1) +
 scale_color_gradient(low = "blue", high = "darkgreen") +
 labs(title = "Evolution de l'indice de confiance des ménages",
      x = "Temps", y = "importations de combustibles", color = "confiance_menages") +
 theme_minimal(base_size = 14) +
 theme(legend.position = "right") +
 
 # Événements majeurs
 geom_vline(xintercept = as.Date("2020-03-01"), linetype = "dashed", color = "red") +
 annotate("text", x = as.Date("2020-03-01"), y = max(df$confiance_menages, na.rm = TRUE), label = "Covid-19", angle = 90, vjust = -0.5, color = "red") +
 
 geom_vline(xintercept = as.Date("2022-02-01"), linetype = "dashed", color = "red") +
 annotate("text", x = as.Date("2022-02-01"), y = max(df$confiance_menages, na.rm = TRUE), label = "Ukraine", angle = 90, vjust = -0.5, color = "red") +
 
 # Source
 annotate("text", x = min(df$Date), y = min(df$confiance_menages, na.rm = TRUE),
          label = "Source : INSEE", hjust = 0, size = 3.5, color = "black")

p8

# 9/ PrixMondialGaz

p9 <- ggplot(df, aes(x = Date, y = PrixMondialGaz, color = PrixMondialGaz)) +
 geom_line(size = 1) +
 scale_color_gradient(low = "blue", high = "darkgreen") +
 labs(title = "Evolution du Prix mondial du gaz naturel, UE",
      x = "Temps", y = " Prix du gaz", color = "PrixMondialGaz") +
 theme_minimal(base_size = 14) +
 theme(legend.position = "right") +
 
 # Événements majeurs
 geom_vline(xintercept = as.Date("2020-03-01"), linetype = "dashed", color = "red") +
 annotate("text", x = as.Date("2020-03-01"), y = max(df$PrixMondialGaz, na.rm = TRUE), label = "Covid-19", angle = 90, vjust = -0.5, color = "red") +
 
 geom_vline(xintercept = as.Date("2022-02-01"), linetype = "dashed", color = "red") +
 annotate("text", x = as.Date("2022-02-01"), y = max(df$PrixMondialGaz, na.rm = TRUE), label = "Ukraine", angle = 90, vjust = -0.5, color = "red") +
 
 # Source
 annotate("text", x = min(df$Date), y = min(df$PrixMondialGaz, na.rm = TRUE),
          label = "Source : FRED", hjust = 0, size = 3.5, color = "black")

p9

# 10/ FAO_Food_Index

p10<- ggplot(df, aes(x = Date, y = Prix_Petrole, color = Prix_Petrole)) +
 geom_line(size = 1) +
 scale_color_gradient(low = "blue", high = "darkgreen") +
 labs(title = "Indice FAO des prix des produits alimentaires",
      x = "Temps", y = "Indice FAO", color = "Prix_Petrole") +
 theme_minimal(base_size = 14) +
 theme(legend.position = "right") +
 
 # Événements majeurs
 geom_vline(xintercept = as.Date("2020-03-01"), linetype = "dashed", color = "red") +
 annotate("text", x = as.Date("2020-03-01"), y = max(df$Prix_Petrole, na.rm = TRUE), label = "Covid-19", angle = 90, vjust = -0.5, color = "red") +
 
 geom_vline(xintercept = as.Date("2022-02-01"), linetype = "dashed", color = "red") +
 annotate("text", x = as.Date("2022-02-01"), y = max(df$Prix_Petrole, na.rm = TRUE), label = "Ukraine", angle = 90, vjust = -0.5, color = "red") +
 
 # Source
 annotate("text", x = min(df$Date), y = min(df$Prix_Petrole, na.rm = TRUE),
          label = "Source : FAO", hjust = 0, size = 3.5, color = "black")

p10

# 11/ Inflation_US

p11<- ggplot(df, aes(x = Date, y = Inflation_US, color = Inflation_US)) +
 geom_line(size = 1) +
 scale_color_gradient(low = "blue", high = "darkgreen") +
 labs(title = "Evolution de l'IPC urbains au US",
      x = "Temps", y = "IPC", color = "Inflation_US") +
 theme_minimal(base_size = 14) +
 theme(legend.position = "right") +
 
 # Événements majeurs
 geom_vline(xintercept = as.Date("2020-03-01"), linetype = "dashed", color = "red") +
 annotate("text", x = as.Date("2020-03-01"), y = max(df$Inflation_US, na.rm = TRUE), label = "Covid-19", angle = 90, vjust = -0.5, color = "red") +
 
 geom_vline(xintercept = as.Date("2022-02-01"), linetype = "dashed", color = "red") +
 annotate("text", x = as.Date("2022-02-01"), y = max(df$Inflation_US, na.rm = TRUE), label = "Ukraine", angle = 90, vjust = -0.5, color = "red") +
 
 # Source
 annotate("text", x = min(df$Date), y = min(df$Inflation_US, na.rm = TRUE),
          label = "Source : FRED", hjust = 0, size = 3.5, color = "black")

p11

# 12/ IPPAP

p12 <- ggplot(df, aes(x = Date, y = IPPAP, color = IPPAP)) +
 geom_line(size = 1) +
 scale_color_gradient(low = "blue", high = "darkgreen") +
 labs(title = "Evolution l'ndice des prix agricoles à la production",
      x = "Temps", y = "IPC", color = "IPPAP") +
 theme_minimal(base_size = 14) +
 theme(legend.position = "right") +
 
 # Événements majeurs
 geom_vline(xintercept = as.Date("2020-03-01"), linetype = "dashed", color = "red") +
 annotate("text", x = as.Date("2020-03-01"), y = max(df$IPPAP, na.rm = TRUE), label = "Covid-19", angle = 90, vjust = -0.5, color = "red") +
 
 geom_vline(xintercept = as.Date("2022-02-01"), linetype = "dashed", color = "red") +
 annotate("text", x = as.Date("2022-02-01"), y = max(df$IPPAP, na.rm = TRUE), label = "Ukraine", angle = 90, vjust = -0.5, color = "red") +
 
 # Source
 annotate("text", x = min(df$Date), y = min(df$IPPAP, na.rm = TRUE),
          label = "Source : FRED", hjust = 0, size = 3.5, color = "black")

p12

#  13/ Geopolitical_Risk

p13 <- ggplot(df, aes(x = Date, y = Geopolitical_Risk, color = Geopolitical_Risk)) +
 geom_line(size = 1) +
 scale_color_gradient(low = "blue", high = "darkgreen") +
 labs(title = "Évolution de l'indice de risque géopolitique GPR",
      x = "Temps", y = "Cours du pétrole", color = "Geopolitical_Risk") +
 theme_minimal(base_size = 14) +
 theme(legend.position = "right") +
 
 # Événements majeurs
 geom_vline(xintercept = as.Date("2020-03-01"), linetype = "dashed", color = "red") +
 annotate("text", x = as.Date("2020-03-01"), y = max(df$Geopolitical_Risk, na.rm = TRUE), label = "Covid-19", angle = 90, vjust = -0.5, color = "red") +
 
 geom_vline(xintercept = as.Date("2022-02-01"), linetype = "dashed", color = "red") +
 annotate("text", x = as.Date("2022-02-01"), y = max(df$Geopolitical_Risk, na.rm = TRUE), label = "Ukraine", angle = 90, vjust = -0.5, color = "red") +
 
 # Source
 annotate("text", x = min(df$Date), y = min(df$Geopolitical_Risk, na.rm = TRUE),
          label = "Source : Caldara, D., & Iacoviello. Geopolitical Risk Index.", hjust = 0, size = 3.5, color = "black")

p13

#  14/ Climat

p14 <- ggplot(df, aes(x = Date, y = Climat, color = Climat)) +
 geom_line(size = 1) +
 scale_color_gradient(low = "blue", high = "darkgreen") +
 labs(title = "Anomalies de Temperature",
      x = "Temps", y = "Cours du pétrole", color = "Climat") +
 theme_minimal(base_size = 14) +
 theme(legend.position = "right") +
 
 # Événements majeurs
 geom_vline(xintercept = as.Date("2020-03-01"), linetype = "dashed", color = "red") +
 annotate("text", x = as.Date("2020-03-01"), y = max(df$Climat, na.rm = TRUE), label = "Covid-19", angle = 90, vjust = -0.5, color = "red") +
 
 geom_vline(xintercept = as.Date("2022-02-01"), linetype = "dashed", color = "red") +
 annotate("text", x = as.Date("2022-02-01"), y = max(df$Climat, na.rm = TRUE), label = "Ukraine", angle = 90, vjust = -0.5, color = "red") +
 
 # Source
 annotate("text", x = min(df$Date), y = min(df$Climat, na.rm = TRUE),
          label = "Source : NOAA-NCEI", hjust = 0, size = 3.5, color = "black")

p14


# ON REGROUPE LES GRAPH
(p1 | p2) / (p3 | p4) / (p5 | p6) /(p7 | p8)

(p9 | p10)/ (p11 | p12) / (p13 | p14)

# I) Graphique des comp -----

ts_Climat <- ts(df$Climat, start = c(2010, 1), frequency = 12)
ts_Geopolitical_Risk <- ts(df$Geopolitical_Risk, start = c(2010, 1), frequency = 12)
ts_IPPAP <- ts(df$IPPAP, start = c(2010, 1), frequency = 12)
ts_Inflation_US <- ts(df$Inflation_US, start = c(2010, 1), frequency = 12)
ts_FAO_Food_Index <- ts(df$FAO_Food_Index, start = c(2010, 1), frequency = 12)
ts_PrixMondialGaz <- ts(df$PrixMondialGaz, start = c(2010, 1), frequency = 12)
ts_confiance_menages <- ts(df$confiance_menages, start = c(2010, 1), frequency = 12)
ts_Imporation_combustibles <- ts(df$Imporation_combustibles, start = c(2010, 1), frequency = 12)
ts_consommation_ménages_alim <- ts(df$consommation_ménages_alim, start = c(2010, 1), frequency = 12)
ts_cout_horaire <- ts(df$cout_horaire, start = c(2010, 1), frequency = 12)
ts_IPP <- ts(df$IPP, start = c(2010, 1), frequency = 12)
ts_Taux_directeur <- ts(df$Taux_directeur, start = c(2010, 1), frequency = 12)
ts_Tx_change_EUR_USD <- ts(df$Tx_change_EUR_USD, start = c(2010, 1), frequency = 12)
ts_Prix_Petrole <- ts(df$Prix_Petrole, start = c(2010, 1), frequency = 12)


# Graphique 01.1.1 - Pain et céréales
p1 <- autoplot(ts_Pain_cereales) +
 ggtitle("IPC - Coicop : 01.1.1 - Pain et céréales") +
 xlab("Date") + ylab("IPC - Pain et céréales") +
 theme_minimal() +
 geom_line(color = "blue")

# Graphique 01.1.2 - Viande
p2 <- autoplot(ts_Viande) +
 ggtitle("IPC - Coicop : 01.1.2 - Viande") +
 xlab("Date") + ylab("IPC - Viande") +
 theme_minimal() +
 geom_line(color = "blue")

# Graphique 01.1.3 - Poissons et fruits de mer
p3 <- autoplot(ts_Poissons_fruitsdemer) +
 ggtitle("IPC - Coicop : 01.1.3 - Poissons et fruits de mer") +
 xlab("Date") + ylab("IPC - 01.1.3") +
 theme_minimal() +
 geom_line(color = "blue")

# Graphique 01.1.4 - Lait, fromage et œufs
p4 <- autoplot(ts_Lait_fromage_oeufs) +
 ggtitle("IPC - Coicop : 01.1.4 - Lait, fromage et œufs") +
 xlab("Date") + ylab("IPC - 01.1.4") +
 theme_minimal() +
 geom_line(color = "blue")

# Graphique 01.1.5 - Huiles et graisses
p5 <- autoplot(ts_Huiles_graisses) +
 ggtitle("IPC - Coicop : 01.1.5 - Huiles et graisses") +
 xlab("Date") + ylab("IPC - 01.1.5") +
 theme_minimal() +
 geom_line(color = "blue")

# Graphique 01.1.6 - Fruits
p6 <- autoplot(ts_Fruits) +
 ggtitle("IPC - Coicop : 01.1.6 - Fruits") +
 xlab("Date") + ylab("IPC - 01.1.6") +
 theme_minimal() +
 geom_line(color = "blue")

# Graphique 01.1.7 - Légumes
p7 <- autoplot(ts_Legumes) +
 ggtitle("IPC - Coicop : 01.1.7 - Légumes") +
 xlab("Date") + ylab("IPC - 01.1.7") +
 theme_minimal() +
 geom_line(color = "blue")

# Graphique 01.1.8 - Sucre, confiture et confiserie
p8 <- autoplot(ts_Sucre_confiture_confiserie) +
 ggtitle("IPC - Coicop : 01.1.8 - Sucre, confiture et confiserie") +
 xlab("Date") + ylab("IPC - 01.1.8") +
 theme_minimal() +
 geom_line(color = "blue")

# Graphique 01.1.9 - Produits alimentaires n.c.a.
p9 <- autoplot(ts_n_c_a) +
 ggtitle("IPC - Coicop : 01.1.9 - Produits alimentaires n.c.a.") +
 xlab("Date") + ylab("IPC - 01.1.9") +
 theme_minimal() +
 geom_line(color = "blue")

# Graphique 01.2.1 - Café, thé et cacao
p10 <- autoplot(ts_Cafe_the_cacao) +
 ggtitle("IPC - Coicop : 01.2.1 - Café, thé et cacao") +
 xlab("Date") + ylab("IPC - 01.2.1") +
 theme_minimal() +
 geom_line(color = "blue")

# Graphique 01.2.2 - Eaux minérales, boissons et jus
p11 <- autoplot(ts_Eauxminerales_boissonsrafraîchissantes) +
 ggtitle("IPC - Coicop : 01.2.2 - Eaux minérales, boissons et jus") +
 xlab("Date") + ylab("IPC - 01.2.2") +
 theme_minimal() +
 geom_line(color = "blue")


(p1 | p2) / (p3 | p4) / (p5 | p6) / (p7 | p8) / (p9 | p10)/ (p11)


(pZ | pW)





