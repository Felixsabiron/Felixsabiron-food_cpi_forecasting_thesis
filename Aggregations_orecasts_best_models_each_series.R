### library----
library(readxl)
library(zoo)
library(ggplot2)
library(viridis)
library(tseries)
library(dplyr)
library(tidyr)
library(tseries)
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
set.seed(123)
library(rJava)


### Ici on agrège les prevision des meilleurs modèles de chaque series.

### ------- Creation de la base ----------
# https://www.insee.fr/fr/information/2402696
# https://www.insee.fr/fr/statistiques/8558558 -> pour les coef(que des 8)

#https://www.insee.fr/fr/statistiques/series/102342213?INDICATEUR=2238611&NATURE=2224016

variables  <- read_excel("~/Documents/Mémoire/Donneest2/famille_IPC-2015_15052025.xlsx")

variables [772,1] # 01.1.1 - Pain et céréales
variables [781,1] # 01.1.2 - Viande
variables [790,1] # 01.1.3 - Poissons et fruits de mer
variables [797,1] # 01.1.4 - Lait, fromage et oeufs
variables [805,1] # 01.1.5 - Huiles et graisses
variables [810,1] # 01.1.6 - Fruits
variables [815,1] # 01.1.7 - Légumes
variables [822,1] # 01.1.8 - Sucre, confiture, miel, chocolat et confiserie
variables [829,1] # 01.1.9 - Produits alimentaires n.c.a.
variables [836,1] # 01.2.1 - Café, thé et cacao
variables [840,1] # 01.2.2 - Eaux minérales, boissons rafraîchissantes, jus de fruits et de légumes

# Extraire les deux lignes d'intérêt
df_maisoui <- variables[c(772, 781,790,797,805,810,815,822,829,836,840), ]
df_maisoui <- df_maisoui[,-c(1:4)]
df_maisoui <- df_maisoui[,-c(1)]

library(tidyverse)

df_maisoui <- df_maisoui %>%
 mutate(Catégorie = c("Pain_cereales", "Viande", "Poissons_fruitsdemer","Lait_fromage_oeufs", 
                      "Huiles_graisses","Fruits","Legumes","Sucre_confiture_confiserie",
                      "n_c_a","Cafe_the_cacao","Eauxminerales_boissonsrafraîchissantes"))

# Réorganiser pour mettre les dates en ligne
df_COICOP <- df_maisoui %>%
 pivot_longer(
  cols = -Catégorie,
  names_to = "Date",
  values_to = "Valeur"
 ) %>%
 pivot_wider(
  names_from = Catégorie,
  values_from = Valeur
 )

# Affichage final
print(df_COICOP)
library(writexl)

# Export en fichier Excel

df_COICOP <- df_COICOP %>%
 mutate(Date = as.Date(paste0(Date, "-01")))
df_COICOP <- df_COICOP[-c(1:239),]
df_COICOP <- df_COICOP[-c(169:184),]

write_xlsx(df_COICOP, "df_COICOP.xlsx")


# Importation des ts ----
dfcomp <- read_xlsx("df_COICOP.xlsx")

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

# ts_Pain_cereales <- ts(dfcomp$Pain_cereales, start = c(2010, 1), frequency = 12)
# ts_Viande <- ts(dfcomp$Viande, start = c(2010, 1), frequency = 12)
# ts_Poissons_fruitsdemer <- ts(dfcomp$Poissons_fruitsdemer, start = c(2010, 1), frequency = 12)
# ts_Lait_fromage_oeufs <- ts(dfcomp$Lait_fromage_oeufs, start = c(2010, 1), frequency = 12)
# ts_Huiles_graisses <- ts(dfcomp$Huiles_graisses, start = c(2010, 1), frequency = 12)
# ts_Fruits <- ts(dfcomp$Fruits, start = c(2010, 1), frequency = 12)
# ts_Legumes <- ts(dfcomp$Legumes, start = c(2010, 1), frequency = 12)
# ts_Sucre_confiture_confiserie <- ts(dfcomp$Sucre_confiture_confiserie, start = c(2010, 1), frequency = 12)
# ts_n_c_a <- ts(dfcomp$n_c_a, start = c(2010, 1), frequency = 12)
# ts_Cafe_the_cacao <- ts(dfcomp$Cafe_the_cacao, start = c(2010, 1), frequency = 12)
# ts_Eauxminerales_boissonsrafraîchissantes <- ts(dfcomp$Eauxminerales_boissonsrafraîchissantes, start = c(2010, 1), frequency = 12)



#-------------- 01.1.1 - Pain et céréales ---------

#### 1. Serie -----
ts_Pain_cereales <- ts(dfcomp$Pain_cereales, start = c(2010, 1), frequency = 12)

autoplot(ts_Pain_cereales) +
 ggtitle("IPC - Coicop :  01.1.1 - Pain et céréales") +
 xlab("Date") + ylab("IPC - Pain et céréales")+
 theme_minimal()+
 geom_line(color = "blue")


### - tests (CVS stationnaire)

combined_test(ts_Pain_cereales)
seasdum(ts_Pain_cereales) #Seasonal Dummies

adf.test(ts_Pain_cereales)
kpss.test(ts_Pain_cereales)

xss <-diff(ts_Pain_cereales)
adf.test(xss)
kpss.test(xss) # La serie est stationnaire ap diff :)

myregx13_01 <- regarima_x13(ts_Pain_cereales, spec ="RG5c")
s_transform(myregx13_01)  # test log/level
# Pas de transformation en log.

### - CVS-

# Avec RJDmetra
s_transform(myregx13_01)
myspec <- x13_spec("RSA5c")
mysax13 <- x13(ts_Pain_cereales, myspec) # SCV de RJGDmetra
#plot(mysax13$final) # JTM

# Extraire la série désaisonnalisée (CVS)
tsIPC01_1_1_CVS_RJDmetra <- ts(mysax13$final$series[, "sa"],
                               start = start(ts_Pain_cereales), frequency = frequency(ts_Pain_cereales))

dfcomp_01_1_1 <- data.frame(
 Date = as.Date(as.yearmon(time(ts_Pain_cereales)), frac = 1),
 Originale = as.numeric(ts_Pain_cereales),
 CVS_RJD = as.numeric(tsIPC01_1_1_CVS_RJDmetra)
)

### Outliers-

outliers01_1_1 <- tso(tsIPC01_1_1_CVS_RJDmetra)
print(outliers01_1_1)

plot(outliers01_1_1)
show(outliers01_1_1)

tsIPC01_1_1_CVS_RJDmetra_corr <- outliers01_1_1$yadj
dfcomp_01_1_1$CVS_RJD_CORR <- as.numeric(tsIPC01_1_1_CVS_RJDmetra_corr)

### Virification de la stationnarité-

combined_test(tsIPC01_1_1_CVS_RJDmetra_corr)
seasdum(tsIPC01_1_1_CVS_RJDmetra_corr) #Seasonal Dummies
# Toujours CVS logique

adf.test(tsIPC01_1_1_CVS_RJDmetra_corr)
kpss.test(tsIPC01_1_1_CVS_RJDmetra_corr)

tsIPC01_1_1_CVS_RJDmetra_corr_diff <- diff(tsIPC01_1_1_CVS_RJDmetra_corr)

adf.test(tsIPC01_1_1_CVS_RJDmetra_corr_diff)
kpss.test(tsIPC01_1_1_CVS_RJDmetra_corr_diff)

autoplot(tsIPC01_1_1_CVS_RJDmetra_corr_diff) +
 ggtitle("IPC - Coicop :  01.1.1 - serie différenciée") +
 xlab("Date") + ylab("IPC 01.1.1")+
 theme_minimal()+
 geom_line(color = "blue")

autoplot(ts_Pain_cereales) +
 ggtitle("IPC - Coicop :  01.1.1 - Pain et céréales") +
 xlab("Date") + ylab("IPC - Pain et céréales")+
 theme_minimal()+
 geom_line(color = "blue")


dfcomp_01_1_1 <- dfcomp_01_1_1[-1, ]  # on retire la première ligne
dfcomp_01_1_1$CVS_RJD_CORR_DIFF <- as.numeric(tsIPC01_1_1_CVS_RJDmetra_corr_diff)

#### 2. Estimation des modèles -----

# a.  Estimer et commenter les paramètres des modèles AR(1), AR(p) et ARIMA(p,d,q) et de la méthode LED Holt-Winters, ADAM ETS, ADAM ETS ARIMA, SSARIMA et CES
# b.  Paramètres : présenter sous forme de tableau les paramètres des modèles précédents. Déterminer et commenter le meilleur modèle d’après les critères AIC et AICc

# On entraine nos modele jusqu'a 2022 pour faire la prevision en 2023.

ts_train01_1_1 <- window(tsIPC01_1_1_CVS_RJDmetra_corr_diff, start = c(2010, 2), end = c(2022, 12))
ts_test01_1_1 <- window(tsIPC01_1_1_CVS_RJDmetra_corr_diff, start = c(2023, 1), end = c(2023, 12))

### 2. Estimation du modèle

###. 2.1 AR/ARMA-

### 2.1.1 ARMA-

modele_arima_01_1_1 <- auto.arima(ts_train01_1_1, stepwise = FALSE, approximation = FALSE)

summary(modele_arima_01_1_1)

j <- ncol(modele_arima_01_1_1$var.coef)
tstat <- matrix(nrow=j, ncol=1)
for(i in 1:j)
{
 tstat[i,1] <- modele_arima_01_1_1$coef[i]/sqrt(modele_arima_01_1_1$var.coef[i,i])
}
tstat

# Graphique des valeurs ajustées
plot(modele_arima_01_1_1$fitted)


prev_arima_01_1_1 <- forecast(modele_arima_01_1_1, h=12)
prev_arima_01_1_1
plot(prev_arima_01_1_1)

aic_arma <- AIC(modele_arima_01_1_1)
cat("AIC du modèle SSARIMA :", round(aic_arma, 2), "\n")

aicc_arma <- AICc(modele_arima_01_1_1)
cat("AICc du modèle SSARIMA :", round(aicc_arma, 2), "\n")

## 2.1.2 AR(1)-

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

### 2.1.3 AR(P)-
modele_arp_01_1_1 <- auto.arima(ts_train01_1_1,
                                d = 0,
                                max.p = 10,        # tu peux ajuster ce maximum
                                max.q = 0,         # pas de MA
                                stationary = TRUE,
                                seasonal = FALSE,
                                stepwise = FALSE,
                                approximation = FALSE)


# Résumé du modèle
summary(modele_arp_01_1_1)

# t-stat des coefficients
j <- length(modele_arp_01_1_1$coef)
tstat_arp <- numeric(j)
for(i in 1:j) {
 tstat_arp[i] <- modele_arp_01_1_1$coef[i] / sqrt(modele_arp_01_1_1$var.coef[i, i])
}
tstat_arp

fitted_arP <- ts_train01_1_1 - residuals(modele_arp_01_1_1)
plot(fitted_arP, type = "l", col = "blue", main = "Valeurs ajustées - AR(1)", ylab = "Prévisions")

prev_arP_01_1_1 <- forecast(modele_arp_01_1_1, h = 12)
prev_arP_01_1_1
plot(prev_arP_01_1_1)

aic_arp <- AIC(modele_arp_01_1_1)
cat("AIC du modèle SSARIMA :", round(aic_arp, 2), "\n")

aicc_arp <- AICc(modele_arp_01_1_1)
cat("AICc du modèle SSARIMA :", round(aicc_arp, 2), "\n")

#### X13-

myregx13_01_1_1 <- regarima_x13(ts_train01_1_1, spec ="RG5c")
summary(myregx13_01_1_1)
s_transform(myregx13_01_1_1)
#plot(myregx13_01_1_1)
myregx13_01_1_1$forecast
forex13_01_1_1 <- matrix(myregx13_01_1_1$forecast[1:12])
forex13_01_1_1

plot(ts_train01_1_1,
     xlim = c(time(ts_train01_1_1)[1], tail(time(ts_train01_1_1), 1) + 1),
     ylim = range(ts_train01_1_1, forex13_01_1_1),
     main = "Prévisions issues de X13-ARIMA-SEATS",
     ylab = "Valeurs", xlab = "Temps")
lines(ts(forex13_01_1_1,
         start = c(2023, 1),  # à adapter si nécessaire
         frequency = frequency(ts_train01_1_1)),
      col = "blue", lwd = 2, lty = 2)
legend("topleft", legend = c("Série observée", "Prévision X13"),
       col = c("black", "blue"), lty = c(1,2), lwd = 2)


#### stl-

decomp = stlm(ts_train01_1_1, s.window="periodic")
show(decomp)
#

fitstl_01_1_1 = stlm(ts_train01_1_1)
prevstl_01_1_1 <- forecast(fitstl_01_1_1,12) #période d'une année

plot(prevstl_01_1_1)
summary(prevstl_01_1_1)


### .2.2 HoltWinters--

WH_add_01_1_1 <- HoltWinters(ts_train01_1_1, gamma = FALSE)
WH_add_01_1_1
show(WH_add_01_1_1)
WH_add_01_1_1$coefficients
plot(WH_add_01_1_1)
forecast_hw01 <- forecast(WH_add_01_1_1, h = 12)
plot(forecast_hw01)
forecast_hw01

# Nombre d'observations
res <- residuals(WH_add_01_1_1)
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


#### ADAM ETS-

fit_ADAM_ETS_01_1_1 <- auto.adam(ts_train01_1_1,
                                 model = "ZZZ",
                                 lags = c(1, 12),
                                 silent = TRUE)          
# ZZZ car ne spécifie rien (tendance, saisonnalité, erreur)
fit_ADAM_ETS_01_1_1
summary(fit_ADAM_ETS_01_1_1)

par(mfcol=c(2,2))
plot(fit_ADAM_ETS_01_1_1)
par(mfcol=c(1,1))


plot(fit_ADAM_ETS_01_1_1$states)
plot(fit_ADAM_ETS_01_1_1$residuals)

prev_ADAM_ETS_01_1_1 <- forecast(fit_ADAM_ETS_01_1_1, h=12)
show(prev_ADAM_ETS_01_1_1)
plot(prev_ADAM_ETS_01_1_1)

#### ADAM ETS + SARIMA-

fitadam3_01_1_1 <- auto.adam(ts_train01_1_1,
                             model = "ZZZ",
                             lags = c(1, 12),
                             orders = list(ar = c(3,3), i = 1, ma = c(3,3)),
                             select = TRUE,
                             bootstrap = TRUE)
fitadam3_01_1_1

summary(fitadam3_01_1_1)

par(mfcol=c(2,2))
plot(fitadam3_01_1_1)

par(mfcol=c(1,1))
plot(fitadam3_01_1_1$residuals)

prev_AES_01_1_1 <- forecast(fitadam3_01_1_1,12)
show(prev_AES_01_1_1)
plot(prev_AES_01_1_1)


### .2.5. SSARIMA-

fit_SSARIMA_01_1_1 <- auto.ssarima(ts_train01_1_1, lags=c(1,12), orders=list(ar=c(3,3), i=(2), ma=c(3,3), select=TRUE))
summary(fit_SSARIMA_01_1_1)

par(mfcol=c(2,2))
plot(fit_SSARIMA_01_1_1)

par(mfcol=c(1,1))

plot(fit_SSARIMA_01_1_1$residuals)

prev_SSARIMA_01_1_1 <- forecast(fit_SSARIMA_01_1_1, h=12)
prev_SSARIMA_01_1_1
plot(prev_SSARIMA_01_1_1)

aic_ssarima <- AIC(fit_SSARIMA_01_1_1)
cat("AIC du modèle SSARIMA :", round(aic_ssarima, 2), "\n")

aicc_ssarima <- AICc(fit_SSARIMA_01_1_1)
cat("AICc du modèle SSARIMA :", round(aicc_ssarima, 2), "\n")


#### naïves-

pred_naive_01_1_1 <- naive(ts_train01_1_1, h=12)
show(pred_naive_01_1_1)
plot(pred_naive_01_1_1)




#### 3. évolution des prévisions----

ts_2023_01_1_1 <- window(tsIPC01_1_1_CVS_RJDmetra_corr_diff, start = c(2023, 1), end = c(2023, 12))

# Fonction mise à jour : transforme la date en format mois
extract_forecast_df <- function(fcast_obj, name) {
 df <- data.frame(
  date = as.Date(as.yearmon(time(fcast_obj$mean))),  # conversion en date
  value = as.numeric(fcast_obj$mean),
  model = name
 )
 df %>% slice_head(n = 12)
}

ts_forex13_01_1_1 <- ts(as.numeric(forex13_01_1_1), start = c(2023, 1), frequency = 12)

# 2. Construire un objet forecast manuellement (sans recalculer)
forecast_x13_01_1_1 <- structure(list(
 mean = ts_forex13_01_1_1,  # ici, un vrai ts avec start/end
 lower = matrix(NA, nrow = 12, ncol = 2, dimnames = list(NULL, c("80%", "95%"))),
 upper = matrix(NA, nrow = 12, ncol = 2, dimnames = list(NULL, c("80%", "95%"))),
 level = c(80, 95),
 x = NULL,
 method = "X13",
 fitted = NULL,
 residuals = NULL
), class = "forecast")

dfs_01_1_1 <- list(
 extract_forecast_df(prev_arima_01_1_1, "ARMA"),
 extract_forecast_df(prev_ar1_01_1_1, "AR(1)"),
 extract_forecast_df(prev_arP_01_1_1, "AR(P)"),
 extract_forecast_df(forecast_x13_01_1_1, "X13"),
 extract_forecast_df(forecast_hw01, "HoltWinters"),
 extract_forecast_df(prev_ADAM_ETS_01_1_1, "ADAM_ETS"),
 extract_forecast_df(prev_AES_01_1_1, "ADAM_ETS+SARIMA"),
 extract_forecast_df(prev_SSARIMA_01_1_1, "SSARIMA"),
 extract_forecast_df(pred_naive_01_1_1, "Naïve")
)

#Je vais comparer aveclesvaleur reel grace a ts_test01_1_1
observed_df_01_1_1 <- data.frame(
 date = as.Date(as.yearmon(time(ts_test01_1_1))),
 value = as.numeric(ts_test01_1_1),
 model = "Observée")

# Combine toutes les prévisions et l'observée
df_all_01_1_1 <- bind_rows(dfs_01_1_1, observed_df_01_1_1)
df_all_01_1_1$date <- as.Date(df_all_01_1_1$date, format = "%Y-%m-%d")
library(ggplot2)

# 1. Graphe simple avec axe en numérique
ggplot(df_all_01_1_1, aes(x = as.numeric(date), y = value, color = model)) +
 geom_line(size = 1.2) +
 scale_x_continuous(
  breaks = as.numeric(df_all_01_1_1$date[1:12]),
  labels = format(df_all_01_1_1$date[1:12], "%b %Y")
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
ggplot(df_all_01_1_1, aes(x = date, y = value, color = model, linetype = model)) +
 geom_line(size = 1.2) +
 labs(
  title = "Prévisions - 01.1.1 : Pain et céréales - Sur 12 mois (2023) par modèle",
  x = "Mois", y = "Valeur prévue", color = "Modèle", linetype = "Modèle"
 ) +
 scale_color_manual(values = c(
  "ADAM_ETS" = "red",
  "ADAM_ETS+SARIMA" = "orange",
  "HoltWinters" = "yellow",
  "Naïve" = "black",
  "SSARIMA" = "purple",
  "X13" = "darkblue",
  "ARMA" = "forestgreen",
  "AR(1)" = "pink",
  "AR(P)" = "darkcyan",
  "Observée" = "black"
 )) +
 scale_linetype_manual(values = c(
  "ADAM_ETS" = "solid",
  "ADAM_ETS+SARIMA" = "solid",
  "HoltWinters" = "solid",
  "Naïve" = "solid",
  "SSARIMA" = "solid",
  "X13" = "dashed",
  "ARMA" = "dotted",
  "AR(1)" = "solid",
  "AR(P)" = "dotted",
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

`#### 4 qualité de prevision -----

## MSE & R²OOS
## MSE & R²OOS avec le modèle Naïf comme référence

# Observations réelles
actual_values01_1_1 <- ts_test01_1_1

# Liste des prévisions pour chaque modèle
forecasts_01_1_1 <- list(
 prev_arima_01_1_1$mean,
 prev_ar1_01_1_1$mean,
 prev_arP_01_1_1$mean,
 forecast_x13_01_1_1$mean,
 forecast_hw01$mean,
 prev_ADAM_ETS_01_1_1$mean,
 prev_AES_01_1_1$mean,
 prev_SSARIMA_01_1_1$mean,
 pred_naive_01_1_1$mean
)


# Fonction MSE + R²OOS avec benchmark explicite
calculate_metrics_01_1_1 <- function(actual, forecast, benchmark_forecast) {
 mse <- mean((actual - forecast)^2)
 sst <- sum((actual - benchmark_forecast)^2)
 sse <- sum((actual - forecast)^2)
 r2oos <- 1 - (sse / sst)
 return(list(mse = mse, r2oos = r2oos))
}

# Naïve utilisé comme benchmark
benchmark_forecast <- as.numeric(pred_naive_01_1_1$mean)

# Calcul des métriques
metrics <- lapply(forecasts_01_1_1, function(fcast) {
 calculate_metrics_01_1_1(as.numeric(actual_values01_1_1), as.numeric(fcast), benchmark_forecast)
})

# Data frame de résultats
metrics_df_01_1_1 <- as.data.frame(do.call(rbind, metrics))
rownames(metrics_df_01_1_1) <- c("ARMA", "AR(1)", "AR(P)","X13",  "HoltWinters","ADAM_ETS", "ADAM_ETS+SARIMA", "SSARIMA", 
                                 "Naïve")

print(metrics_df_01_1_1)



## CSPE

# Fonction CSPE
calculate_cspe <- function(actual, forecast) {
 errors <- (actual - forecast)^2
 cspe <- cumsum(errors)
 return(cspe)
}

# CSPE pour chaque modèle
cspe_list <- lapply(forecasts_01_1_1, function(fcast) {
 calculate_cspe(as.numeric(actual_values01_1_1), as.numeric(fcast))
})

# Création du data.frame CSPE
cspe_df <- do.call(cbind, cspe_list)
colnames(cspe_df) <- rownames(metrics_df_01_1_1)
cspe_df <- cbind(Date = as.Date(time(actual_values01_1_1)), cspe_df)
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
  "ARMA" = "forestgreen", "AR(1)" = "brown", "AR(P)" = "darkcyan","X13" = "darkblue",
  "HoltWinters" = "yellow", "ADAM_ETS" = "red", "ADAM_ETS+SARIMA" = "orange",
  "SSARIMA" = "purple", "Naïve" = "black"
 )) +
 scale_linetype_manual(values = c(
  "ARMA" = "dashed", "AR(1)" = "dotted", "AR(P)" = "solid","X13" = "dashed",
  "HoltWinters" = "solid", "ADAM_ETS" = "solid", "ADAM_ETS+SARIMA" = "solid",
  "SSARIMA" = "solid", "Naïve" = "dotdash"
 )) +
 
 
 scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
 theme_minimal(base_size = 13) +
 theme(axis.text.x = element_text(angle = 45, hjust = 1),
       legend.position = "bottom",
       legend.title = element_text(face = "bold"),
       plot.title = element_text(face = "bold"))



#### 5 Test de précision-----

# Définir la période de temps
start_date <- c(2023, 1)
end_date <- c(2023, 12)

# Fonction pour ajuster les séries temporelles
adjust_time_series <- function(ts_data) {
 return(window(ts_data, start = start_date, end = end_date))
}

# Ajuster les séries temporelles pour chaque modèle
for_observed <- adjust_time_series(ts_2023_01_1_1)
for_ARMA     <- adjust_time_series(prev_arima_01_1_1$mean)
for_AR1      <- adjust_time_series(prev_ar1_01_1_1$mean)
for_ARP      <- adjust_time_series(prev_arP_01_1_1$mean)
for_X13      <- adjust_time_series(forecast_x13_01_1_1$mean)
for_HW       <- adjust_time_series(forecast_hw01$mean)
for_ADAM_ETS     <- adjust_time_series(prev_ADAM_ETS_01_1_1$mean)
for_AES      <- adjust_time_series(prev_AES_01_1_1$mean)
for_SSARIMA  <- adjust_time_series(prev_SSARIMA_01_1_1$mean)
for_NAIVE    <- adjust_time_series(pred_naive_01_1_1$mean)



# Vérifiez que toutes les séries temporelles ont la bonne longueur

ts_2023_01_1_1 <- window(tsIPC01_1_1_CVS_RJDmetra_corr_diff, start = c(2023, 1), end = c(2023, 12))
ts_test01_1_1 <- window(tsIPC01_1_1_CVS_RJDmetra_corr_diff, start = c(2023, 1), end = c(2023, 12))
print(length(ts_2023_01_1_1))

print(length(for_observed))
print(length(for_ARMA))
print(length(for_AR1))
print(length(for_ARP))
print(length(for_X13))
print(length(for_HW))
print(length(for_ADAM_ETS))
print(length(for_AES))
print(length(for_SSARIMA))
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

# Calculer le test DM

dm.test(error_NAIVE, error_ARMA, h = length(for_observed))
dm.test(error_NAIVE, error_AR1, h = length(for_observed))
dm.test(error_NAIVE, error_ARP, h = length(for_observed))
dm.test(error_NAIVE, error_X13, h = length(for_observed))
dm.test(error_NAIVE, error_HW, h = length(for_observed))
dm.test(error_NAIVE, error_ADAM_ETS, h = length(for_observed))
dm.test(error_NAIVE, error_AES, h = length(for_observed))
dm.test(error_NAIVE, error_SSARIMA, h = length(for_observed))


# Test de Diebold-Mariano avec h = 1
dm.test(error_NAIVE, error_ARMA, h = 1)
dm.test(error_NAIVE, error_AR1, h = 1)
dm.test(error_NAIVE, error_ARP, h = 1)
dm.test(error_NAIVE, error_X13, h = 1)
dm.test(error_NAIVE, error_HW, h = 1)
dm.test(error_NAIVE, error_ADAM_ETS, h = 1)
dm.test(error_NAIVE, error_AES, h = 1)
dm.test(error_NAIVE, error_SSARIMA, h = 1)


## le meilleur modèle est AR(1) 	



#--------------  01.1.2 - Viande ---------

ts_Viande <- ts(dfcomp$Viande, start = c(2010, 1), frequency = 12)

autoplot(ts_Viande) +
 ggtitle("IPC - Coicop :  01.1.2 - Viande") +
 xlab("Date") + ylab("IPC - Viande")+
 theme_minimal()+
 geom_line(color = "blue")


### - tests (CVS stationnaire)

combined_test(ts_Viande)
seasdum(ts_Viande) #Seasonal Dummies

adf.test(ts_Viande)
kpss.test(ts_Viande)

xss <-diff(ts_Viande)
adf.test(xss)
kpss.test(xss) # La serie est stationnaire ap diff :)

myregx13_01 <- regarima_x13(ts_Viande, spec ="RG5c")
s_transform(myregx13_01)  # test log/level
# Pas de transformation en log.

### - CVS-

# Avec RJDmetra
s_transform(myregx13_01)
myspec <- x13_spec("RSA5c")
mysax13 <- x13(ts_Viande, myspec) # SCV de RJGDmetra
#plot(mysax13$final) # JTM

# Extraire la série désaisonnalisée (CVS)
tsIPC01_1_2_CVS_RJDmetra <- ts(mysax13$final$series[, "sa"],
                               start = start(ts_Viande), frequency = frequency(ts_Viande))

dfcomp_01_1_2 <- data.frame(
 Date = as.Date(as.yearmon(time(ts_Viande)), frac = 1),
 Originale = as.numeric(ts_Viande),
 CVS_RJD = as.numeric(tsIPC01_1_2_CVS_RJDmetra)
)

### Outliers-

outliers01_1_2 <- tso(tsIPC01_1_2_CVS_RJDmetra)
print(outliers01_1_2)

plot(outliers01_1_2)
show(outliers01_1_2)

tsIPC01_1_2_CVS_RJDmetra_corr <- outliers01_1_2$yadj
dfcomp_01_1_2$CVS_RJD_CORR <- as.numeric(tsIPC01_1_2_CVS_RJDmetra_corr)

### Virification de la stationnarité-

combined_test(tsIPC01_1_2_CVS_RJDmetra_corr)
seasdum(tsIPC01_1_2_CVS_RJDmetra_corr) #Seasonal Dummies
# Toujours CVS logique

adf.test(tsIPC01_1_2_CVS_RJDmetra_corr)
kpss.test(tsIPC01_1_2_CVS_RJDmetra_corr)

tsIPC01_1_2_CVS_RJDmetra_corr_diff <- diff(tsIPC01_1_2_CVS_RJDmetra_corr)
adf.test(tsIPC01_1_2_CVS_RJDmetra_corr_diff)
kpss.test(tsIPC01_1_2_CVS_RJDmetra_corr_diff)


autoplot(tsIPC01_1_2_CVS_RJDmetra_corr_diff) +
 ggtitle("IPC - Coicop :  01.1.2 - serie différenciée") +
 xlab("Date") + ylab("IPC 01.1.2")+
 theme_minimal()+
 geom_line(color = "blue")


dfcomp_01_1_2 <- dfcomp_01_1_2[-1, ]  # on retire la première ligne
dfcomp_01_1_2$CVS_RJD_CORR_DIFF <- as.numeric(tsIPC01_1_2_CVS_RJDmetra_corr_diff)

#### 2. Estimation des modèles-----

# a.  Estimer et commenter les paramètres des modèles AR(1), AR(p) et ARIMA(p,d,q) et de la méthode LED Holt-Winters, ADAM ETS, ADAM ETS ARIMA, SSARIMA et CES
# b.  Paramètres : présenter sous forme de tableau les paramètres des modèles précédents. Déterminer et commenter le meilleur modèle d’après les critères AIC et AICc

# On entraine nos modele jusqu'a 2022 pour faire la prevision en 2023.

ts_train01_1_2 <- window(tsIPC01_1_2_CVS_RJDmetra_corr_diff, start = c(2010, 2), end = c(2022, 12))
ts_test01_1_2 <- window(tsIPC01_1_2_CVS_RJDmetra_corr_diff, start = c(2023, 1), end = c(2023, 12))


combined_test(tsIPC01_1_1_CVS_RJDmetra_corr_diff)
seasdum(tsIPC01_1_1_CVS_RJDmetra_corr_diff) #Seasonal Dummies
adf.test(tsIPC01_1_1_CVS_RJDmetra_corr_diff)
kpss.test(tsIPC01_1_1_CVS_RJDmetra_corr_diff)


### 2. Estimation du modèle

###. 2.1 AR/ARMA-

### 2.1.1 ARMA-

modele_arima_01_1_2 <- auto.arima(ts_train01_1_2, stepwise = FALSE, approximation = FALSE)

summary(modele_arima_01_1_2)

j <- ncol(modele_arima_01_1_2$var.coef)
tstat <- matrix(nrow=j, ncol=1)
for(i in 1:j)
{
 tstat[i,1] <- modele_arima_01_1_2$coef[i]/sqrt(modele_arima_01_1_2$var.coef[i,i])
}
tstat

# Graphique des valeurs ajustées
plot(modele_arima_01_1_2$fitted)


prev_arima_01_1_2 <- forecast(modele_arima_01_1_2, h=12)
prev_arima_01_1_2
plot(prev_arima_01_1_2)

aic_arma <- AIC(modele_arima_01_1_2)
cat("AIC du modèle SSARIMA :", round(aic_arma, 2), "\n")

aicc_arma <- AICc(modele_arima_01_1_2)
cat("AICc du modèle SSARIMA :", round(aicc_arma, 2), "\n")

## 2.1.2 AR(1)-

modele_ar1_01_1_2 <- Arima(ts_train01_1_2, order = c(1, 0, 0), include.mean = TRUE)

summary(modele_ar1_01_1_2)

j <- ncol(modele_ar1_01_1_2$var.coef)
tstat <- matrix(nrow=j, ncol=1)
for(i in 1:j)
{
 tstat[i,1] <- modele_ar1_01_1_2$coef[i]/sqrt(modele_ar1_01_1_2$var.coef[i,i])
}
tstat

fitted_ar1 <- ts_train01_1_2 - residuals(modele_ar1_01_1_2)
plot(fitted_ar1, type = "l", col = "blue", main = "Valeurs ajustées - AR(1)", ylab = "Prévisions")

prev_ar1_01_1_2 <- forecast(modele_ar1_01_1_2, h = 12)
prev_ar1_01_1_2
plot(prev_ar1_01_1_2)

aic_ar1 <- AIC(modele_ar1_01_1_2)
cat("AIC du modèle AR(1) :", round(aic_ar1, 2), "\n")

aicc_ar1 <- AICc(modele_ar1_01_1_2)
cat("AICc du modèle AR(1) :", round(aicc_ar1, 2), "\n")

### 2.1.3 AR(P)-
modele_arp_01_1_2 <- auto.arima(ts_train01_1_2,
                                d = 0,
                                max.p = 10,        # tu peux ajuster ce maximum
                                max.q = 0,         # pas de MA
                                stationary = TRUE,
                                seasonal = FALSE,
                                stepwise = FALSE,
                                approximation = FALSE)


# Résumé du modèle
summary(modele_arp_01_1_2)

# t-stat des coefficients
j <- length(modele_arp_01_1_2$coef)
tstat_arp <- numeric(j)
for(i in 1:j) {
 tstat_arp[i] <- modele_arp_01_1_2$coef[i] / sqrt(modele_arp_01_1_2$var.coef[i, i])
}
tstat_arp

fitted_arP <- ts_train01_1_2 - residuals(modele_arp_01_1_2)
plot(fitted_arP, type = "l", col = "blue", main = "Valeurs ajustées - AR(1)", ylab = "Prévisions")

prev_arP_01_1_2 <- forecast(modele_arp_01_1_2, h = 12)
prev_arP_01_1_2
plot(prev_arP_01_1_2)

aic_arp <- AIC(modele_arp_01_1_2)
cat("AIC du modèle SSARIMA :", round(aic_arp, 2), "\n")

aicc_arp <- AICc(modele_arp_01_1_2)
cat("AICc du modèle SSARIMA :", round(aicc_arp, 2), "\n")

#### X13-

myregx13_01_1_2 <- regarima_x13(ts_train01_1_2, spec ="RG5c")
summary(myregx13_01_1_2)
s_transform(myregx13_01_1_2)
#plot(myregx13_01_1_2)
myregx13_01_1_2$forecast
forex13_01_1_2 <- matrix(myregx13_01_1_2$forecast[1:12])
forex13_01_1_2

plot(ts_train01_1_2,
     xlim = c(time(ts_train01_1_2)[1], tail(time(ts_train01_1_2), 1) + 1),
     ylim = range(ts_train01_1_2, forex13_01_1_2),
     main = "Prévisions issues de X13-ARIMA-SEATS",
     ylab = "Valeurs", xlab = "Temps")
lines(ts(forex13_01_1_2,
         start = c(2023, 1),  # à adapter si nécessaire
         frequency = frequency(ts_train01_1_2)),
      col = "blue", lwd = 2, lty = 2)
legend("topleft", legend = c("Série observée", "Prévision X13"),
       col = c("black", "blue"), lty = c(1,2), lwd = 2)


#### stl-

decomp = stlm(ts_train01_1_2, s.window="periodic")
show(decomp)
#

fitstl_01_1_2 = stlm(ts_train01_1_2)
prevstl_01_1_2 <- forecast(fitstl_01_1_2,12) #période d'une année

plot(prevstl_01_1_2)
summary(prevstl_01_1_2)


### .2.2 HoltWinters--

WH_add_01_1_2 <- HoltWinters(ts_train01_1_2, gamma = FALSE)
WH_add_01_1_2
show(WH_add_01_1_2)
WH_add_01_1_2$coefficients
plot(WH_add_01_1_2)
forecast_hw01 <- forecast(WH_add_01_1_2, h = 12)
plot(forecast_hw01)
forecast_hw01

# Nombre d'observations
res <- residuals(WH_add_01_1_2)
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


#### ADAM ETS-

fit_ADAM_ETS_01_1_2 <- auto.adam(ts_train01_1_2,
                                 model = "ZZZ",
                                 lags = c(1, 12),
                                 silent = TRUE)          
# ZZZ car ne spécifie rien (tendance, saisonnalité, erreur)
fit_ADAM_ETS_01_1_2
summary(fit_ADAM_ETS_01_1_2)

par(mfcol=c(2,2))
plot(fit_ADAM_ETS_01_1_2)
par(mfcol=c(1,1))


plot(fit_ADAM_ETS_01_1_2$states)
plot(fit_ADAM_ETS_01_1_2$residuals)

prev_ADAM_ETS_01_1_2 <- forecast(fit_ADAM_ETS_01_1_2, h=12)
show(prev_ADAM_ETS_01_1_2)
plot(prev_ADAM_ETS_01_1_2)

#### ADAM ETS + SARIMA-

fitadam3_01_1_2 <- auto.adam(ts_train01_1_2,
                             model = "ZZZ",
                             lags = c(1, 12),
                             orders = list(ar = c(3,3), i = 1, ma = c(3,3)),
                             select = TRUE,
                             bootstrap = TRUE)
fitadam3_01_1_2

summary(fitadam3_01_1_2)

par(mfcol=c(2,2))
plot(fitadam3_01_1_2)

par(mfcol=c(1,1))
plot(fitadam3_01_1_2$residuals)

prev_AES_01_1_2 <- forecast(fitadam3_01_1_2,12)
show(prev_AES_01_1_2)
plot(prev_AES_01_1_2)


### .2.5. SSARIMA-

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


#### naïves-

pred_naive_01_1_2 <- naive(ts_train01_1_2, h=12)
show(pred_naive_01_1_2)
plot(pred_naive_01_1_2)




#### 3. évolution des prévisions----

ts_2023_01_1_2 <- window(tsIPC01_1_2_CVS_RJDmetra_corr_diff, start = c(2023, 1), end = c(2023, 12))

# Fonction mise à jour : transforme la date en format mois
extract_forecast_df <- function(fcast_obj, name) {
 df <- data.frame(
  date = as.Date(as.yearmon(time(fcast_obj$mean))),  # conversion en date
  value = as.numeric(fcast_obj$mean),
  model = name
 )
 df %>% slice_head(n = 12)
}

ts_forex13_01_1_2 <- ts(as.numeric(forex13_01_1_2), start = c(2023, 1), frequency = 12)

# 2. Construire un objet forecast manuellement (sans recalculer)
forecast_x13_01_1_2 <- structure(list(
 mean = ts_forex13_01_1_2,  # ici, un vrai ts avec start/end
 lower = matrix(NA, nrow = 12, ncol = 2, dimnames = list(NULL, c("80%", "95%"))),
 upper = matrix(NA, nrow = 12, ncol = 2, dimnames = list(NULL, c("80%", "95%"))),
 level = c(80, 95),
 x = NULL,
 method = "X13",
 fitted = NULL,
 residuals = NULL
), class = "forecast")

dfs_01_1_2 <- list(
 extract_forecast_df(prev_arima_01_1_2, "ARMA"),
 extract_forecast_df(prev_ar1_01_1_2, "AR(1)"),
 extract_forecast_df(prev_arP_01_1_2, "AR(P)"),
 extract_forecast_df(forecast_x13_01_1_2, "X13"),
 extract_forecast_df(forecast_hw01, "HoltWinters"),
 extract_forecast_df(prev_ADAM_ETS_01_1_2, "ADAM_ETS"),
 extract_forecast_df(prev_AES_01_1_2, "ADAM_ETS+SARIMA"),
 extract_forecast_df(prev_SSARIMA_01_1_2, "SSARIMA"),
 extract_forecast_df(pred_naive_01_1_2, "Naïve")
)

#Je vais comparer aveclesvaleur reel grace a ts_test01_1_2
observed_df_01_1_2 <- data.frame(
 date = as.Date(as.yearmon(time(ts_test01_1_2))),
 value = as.numeric(ts_test01_1_2),
 model = "Observée")

# Combine toutes les prévisions et l'observée
df_all_01_1_2 <- bind_rows(dfs_01_1_2, observed_df_01_1_2)
df_all_01_1_2$date <- as.Date(df_all_01_1_2$date, format = "%Y-%m-%d")
library(ggplot2)

# 1. Graphe simple avec axe en numérique
ggplot(df_all_01_1_2, aes(x = as.numeric(date), y = value, color = model)) +
 geom_line(size = 1.2) +
 scale_x_continuous(
  breaks = as.numeric(df_all_01_1_2$date[1:12]),
  labels = format(df_all_01_1_2$date[1:12], "%b %Y")
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
ggplot(df_all_01_1_2, aes(x = date, y = value, color = model, linetype = model)) +
 geom_line(size = 1.2) +
 labs(
  title = "Prévisions - 01.1.2 : Viande - Sur 12 mois (2023) par modèle",
  x = "Mois", y = "Valeur prévue", color = "Modèle", linetype = "Modèle"
 ) +
 scale_color_manual(values = c(
  "ADAM_ETS" = "red",
  "ADAM_ETS+SARIMA" = "orange",
  "HoltWinters" = "yellow",
  "Naïve" = "black",
  "SSARIMA" = "purple",
  "X13" = "darkblue",
  "ARMA" = "forestgreen",
  "AR(1)" = "brown",
  "AR(P)" = "darkcyan",
  "Observée" = "black"
 )) +
 scale_linetype_manual(values = c(
  "ADAM_ETS" = "solid",
  "ADAM_ETS+SARIMA" = "solid",
  "HoltWinters" = "solid",
  "Naïve" = "solid",
  "SSARIMA" = "solid",
  "X13" = "dashed",
  "ARMA" = "dotted",
  "AR(1)" = "dotted",
  "AR(P)" = "dotted",
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

#### 4 qualité de prevision -----

## MSE & R²OOS
## MSE & R²OOS avec le modèle Naïf comme référence

# Observations réelles
actual_values01_1_2 <- ts_test01_1_2

# Liste des prévisions pour chaque modèle
forecasts_01_1_2 <- list(
 prev_arima_01_1_2$mean,
 prev_ar1_01_1_2$mean,
 prev_arP_01_1_2$mean,
 forecast_x13_01_1_2$mean,
 forecast_hw01$mean,
 prev_ADAM_ETS_01_1_2$mean,
 prev_AES_01_1_2$mean,
 prev_SSARIMA_01_1_2$mean,
 pred_naive_01_1_2$mean
)


# Fonction MSE + R²OOS avec benchmark explicite
calculate_metrics_01_1_2 <- function(actual, forecast, benchmark_forecast) {
 mse <- mean((actual - forecast)^2)
 sst <- sum((actual - benchmark_forecast)^2)
 sse <- sum((actual - forecast)^2)
 r2oos <- 1 - (sse / sst)
 return(list(mse = mse, r2oos = r2oos))
}

# Naïve utilisé comme benchmark
benchmark_forecast <- as.numeric(pred_naive_01_1_2$mean)

# Calcul des métriques
metrics <- lapply(forecasts_01_1_2, function(fcast) {
 calculate_metrics_01_1_2(as.numeric(actual_values01_1_2), as.numeric(fcast), benchmark_forecast)
})

# Data frame de résultats
metrics_df_01_1_2 <- as.data.frame(do.call(rbind, metrics))
rownames(metrics_df_01_1_2) <- c("ARMA", "AR(1)", "AR(P)","X13",  "HoltWinters","ADAM_ETS", "ADAM_ETS+SARIMA", "SSARIMA", 
                                 "Naïve")

print(metrics_df_01_1_2)

## CSPE

# Fonction CSPE
calculate_cspe <- function(actual, forecast) {
 errors <- (actual - forecast)^2
 cspe <- cumsum(errors)
 return(cspe)
}

# CSPE pour chaque modèle
cspe_list <- lapply(forecasts_01_1_2, function(fcast) {
 calculate_cspe(as.numeric(actual_values01_1_2), as.numeric(fcast))
})

# Création du data.frame CSPE
cspe_df <- do.call(cbind, cspe_list)
colnames(cspe_df) <- rownames(metrics_df_01_1_2)
cspe_df <- cbind(Date = as.Date(time(actual_values01_1_2)), cspe_df)
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
  "ARMA" = "forestgreen", "AR(1)" = "brown", "AR(P)" = "darkcyan","X13" = "darkblue",
  "HoltWinters" = "yellow", "ADAM_ETS" = "red", "ADAM_ETS+SARIMA" = "orange",
  "SSARIMA" = "purple", "Naïve" = "black"
 )) +
 scale_linetype_manual(values = c(
  "ARMA" = "dashed", "AR(1)" = "dotted", "AR(P)" = "solid","X13" = "dashed",
  "HoltWinters" = "solid", "ADAM_ETS" = "solid", "ADAM_ETS+SARIMA" = "solid",
  "SSARIMA" = "solid", "Naïve" = "dotdash"
 )) +
 
 
 scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
 theme_minimal(base_size = 13) +
 theme(axis.text.x = element_text(angle = 45, hjust = 1),
       legend.position = "bottom",
       legend.title = element_text(face = "bold"),
       plot.title = element_text(face = "bold"))



#### 5 Test de précision-----

# Définir la période de temps
start_date <- c(2023, 1)
end_date <- c(2023, 12)

# Fonction pour ajuster les séries temporelles
adjust_time_series <- function(ts_data) {
 return(window(ts_data, start = start_date, end = end_date))
}

# Ajuster les séries temporelles pour chaque modèle
for_observed <- adjust_time_series(ts_2023_01_1_2)
for_ARMA     <- adjust_time_series(prev_arima_01_1_2$mean)
for_AR1      <- adjust_time_series(prev_ar1_01_1_2$mean)
for_ARP      <- adjust_time_series(prev_arP_01_1_2$mean)
for_X13      <- adjust_time_series(forecast_x13_01_1_2$mean)
for_HW       <- adjust_time_series(forecast_hw01$mean)
for_ADAM_ETS     <- adjust_time_series(prev_ADAM_ETS_01_1_2$mean)
for_AES      <- adjust_time_series(prev_AES_01_1_2$mean)
for_SSARIMA  <- adjust_time_series(prev_SSARIMA_01_1_2$mean)
for_NAIVE    <- adjust_time_series(pred_naive_01_1_2$mean)



# Vérifiez que toutes les séries temporelles ont la bonne longueur

ts_2023_01_1_2 <- window(tsIPC01_1_2_CVS_RJDmetra_corr_diff, start = c(2023, 1), end = c(2023, 12))
ts_test01_1_2 <- window(tsIPC01_1_2_CVS_RJDmetra_corr_diff, start = c(2023, 1), end = c(2023, 12))
print(length(ts_2023_01_1_2))

print(length(for_observed))
print(length(for_ARMA))
print(length(for_AR1))
print(length(for_ARP))
print(length(for_X13))
print(length(for_HW))
print(length(for_ADAM_ETS))
print(length(for_AES))
print(length(for_SSARIMA))
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

# Calculer le test DM

dm.test(error_NAIVE, error_ARMA, h = length(for_observed))
dm.test(error_NAIVE, error_AR1, h = length(for_observed))
dm.test(error_NAIVE, error_ARP, h = length(for_observed))
dm.test(error_NAIVE, error_X13, h = length(for_observed))
dm.test(error_NAIVE, error_HW, h = length(for_observed))
dm.test(error_NAIVE, error_ADAM_ETS, h = length(for_observed))
dm.test(error_NAIVE, error_AES, h = length(for_observed))
dm.test(error_NAIVE, error_SSARIMA, h = length(for_observed))


# Test de Diebold-Mariano avec h = 1
dm.test(error_NAIVE, error_ARMA, h = 1)
dm.test(error_NAIVE, error_AR1, h = 1)
dm.test(error_NAIVE, error_ARP, h = 1)
dm.test(error_NAIVE, error_X13, h = 1)
dm.test(error_NAIVE, error_HW, h = 1)
dm.test(error_NAIVE, error_ADAM_ETS, h = 1)
dm.test(error_NAIVE, error_AES, h = 1)
dm.test(error_NAIVE, error_SSARIMA, h = 1)


## le meilleur modèle est SSARIMA 	


#-------------- 01.1.3 - Poissons et fruits de mer ---------


ts_Poissons_fruitsdemer <- ts(dfcomp$Poissons_fruitsdemer, start = c(2010, 1), frequency = 12)

autoplot(ts_Poissons_fruitsdemer) +
 ggtitle("IPC - Coicop :  01.1.3 - Poissons et fruits de mer") +
 xlab("Date") + ylab("IPC - 01.1.3")+
 theme_minimal()+
 geom_line(color = "blue")


### - tests (CVS stationnaire)

combined_test(ts_Poissons_fruitsdemer)
seasdum(ts_Poissons_fruitsdemer) #Seasonal Dummies

adf.test(ts_Poissons_fruitsdemer)
kpss.test(ts_Poissons_fruitsdemer)

xss <-diff(ts_Poissons_fruitsdemer)
adf.test(xss)
kpss.test(xss) # La serie est stationnaire ap diff :)

myregx13_01_1_3 <- regarima_x13(ts_Poissons_fruitsdemer, spec ="RG5c")
s_transform(myregx13_01_1_3)  # test log/level
# Pas de transformation en log.

### - CVS-

# Avec RJDmetra
s_transform(myregx13_01_1_3)
myspec <- x13_spec("RSA5c")
mysax13 <- x13(ts_Poissons_fruitsdemer, myspec) # SCV de RJGDmetra
#plot(mysax13$final) # JTM

# Extraire la série désaisonnalisée (CVS)
tsIPC01_1_3_CVS_RJDmetra <- ts(mysax13$final$series[, "sa"],
                               start = start(ts_Poissons_fruitsdemer), frequency = frequency(ts_Poissons_fruitsdemer))

dfcomp_01_1_3 <- data.frame(
 Date = as.Date(as.yearmon(time(ts_Poissons_fruitsdemer)), frac = 1),
 Originale = as.numeric(ts_Poissons_fruitsdemer),
 CVS_RJD = as.numeric(tsIPC01_1_3_CVS_RJDmetra)
)

### Outliers-

outliers01_1_3 <- tso(tsIPC01_1_3_CVS_RJDmetra)
print(outliers01_1_3)

plot(outliers01_1_3)
show(outliers01_1_3)

tsIPC01_1_3_CVS_RJDmetra_corr <- outliers01_1_3$yadj
dfcomp_01_1_3$CVS_RJD_CORR <- as.numeric(tsIPC01_1_3_CVS_RJDmetra_corr)

### Virification de la stationnarité-

combined_test(tsIPC01_1_3_CVS_RJDmetra_corr)
seasdum(tsIPC01_1_3_CVS_RJDmetra_corr) #Seasonal Dummies
# Toujours CVS logique

adf.test(tsIPC01_1_3_CVS_RJDmetra_corr)
kpss.test(tsIPC01_1_3_CVS_RJDmetra_corr)

tsIPC01_1_3_CVS_RJDmetra_corr_diff <- diff(tsIPC01_1_3_CVS_RJDmetra_corr)
adf.test(tsIPC01_1_3_CVS_RJDmetra_corr_diff)
kpss.test(tsIPC01_1_3_CVS_RJDmetra_corr_diff)


autoplot(tsIPC01_1_3_CVS_RJDmetra_corr_diff) +
 ggtitle("IPC - Coicop :  01.1.3 - serie différenciée") +
 xlab("Date") + ylab("IPC 01.1.3")+
 theme_minimal()+
 geom_line(color = "blue")


dfcomp_01_1_3 <- dfcomp_01_1_3[-1, ]  # on retire la première ligne
dfcomp_01_1_3$CVS_RJD_CORR_DIFF <- as.numeric(tsIPC01_1_3_CVS_RJDmetra_corr_diff)

#### 2. Estimation des modèles-----

# a.  Estimer et commenter les paramètres des modèles AR(1), AR(p) et ARIMA(p,d,q) et de la méthode LED Holt-Winters, ADAM ETS, ADAM ETS ARIMA, SSARIMA et CES
# b.  Paramètres : présenter sous forme de tableau les paramètres des modèles précédents. Déterminer et commenter le meilleur modèle d’après les critères AIC et AICc

# On entraine nos modele jusqu'a 2022 pour faire la prevision en 2023.

ts_train01_1_3 <- window(tsIPC01_1_3_CVS_RJDmetra_corr_diff, start = c(2010, 2), end = c(2022, 12))
ts_test01_1_3 <- window(tsIPC01_1_3_CVS_RJDmetra_corr_diff, start = c(2023, 1), end = c(2023, 12))


### 2. Estimation du modèle

###. 2.1 AR/ARMA-

### 2.1.1 ARMA-

modele_arima_01_1_3 <- auto.arima(ts_train01_1_3, stepwise = FALSE, approximation = FALSE)

summary(modele_arima_01_1_3)

j <- ncol(modele_arima_01_1_3$var.coef)
tstat <- matrix(nrow=j, ncol=1)
for(i in 1:j)
{
 tstat[i,1] <- modele_arima_01_1_3$coef[i]/sqrt(modele_arima_01_1_3$var.coef[i,i])
}
tstat

# Graphique des valeurs ajustées
plot(modele_arima_01_1_3$fitted)


prev_arima_01_1_3 <- forecast(modele_arima_01_1_3, h=12)
prev_arima_01_1_3
plot(prev_arima_01_1_3)

aic_arma <- AIC(modele_arima_01_1_3)
cat("AIC du modèle SSARIMA :", round(aic_arma, 2), "\n")

aicc_arma <- AICc(modele_arima_01_1_3)
cat("AICc du modèle SSARIMA :", round(aicc_arma, 2), "\n")

## 2.1.2 AR(1)-

modele_ar1_01_1_3 <- Arima(ts_train01_1_3, order = c(1, 0, 0), include.mean = TRUE)

summary(modele_ar1_01_1_3)

j <- ncol(modele_ar1_01_1_3$var.coef)
tstat <- matrix(nrow=j, ncol=1)
for(i in 1:j)
{
 tstat[i,1] <- modele_ar1_01_1_3$coef[i]/sqrt(modele_ar1_01_1_3$var.coef[i,i])
}
tstat

fitted_ar1 <- ts_train01_1_3 - residuals(modele_ar1_01_1_3)
plot(fitted_ar1, type = "l", col = "blue", main = "Valeurs ajustées - AR(1)", ylab = "Prévisions")

prev_ar1_01_1_3 <- forecast(modele_ar1_01_1_3, h = 12)
prev_ar1_01_1_3
plot(prev_ar1_01_1_3)

aic_ar1 <- AIC(modele_ar1_01_1_3)
cat("AIC du modèle AR(1) :", round(aic_ar1, 2), "\n")

aicc_ar1 <- AICc(modele_ar1_01_1_3)
cat("AICc du modèle AR(1) :", round(aicc_ar1, 2), "\n")

### 2.1.3 AR(P)-
modele_arp_01_1_3 <- auto.arima(ts_train01_1_3,
                                d = 0,
                                max.p = 10,        # tu peux ajuster ce maximum
                                max.q = 0,         # pas de MA
                                stationary = TRUE,
                                seasonal = FALSE,
                                stepwise = FALSE,
                                approximation = FALSE)


# Résumé du modèle
summary(modele_arp_01_1_3)

# t-stat des coefficients
j <- length(modele_arp_01_1_3$coef)
tstat_arp <- numeric(j)
for(i in 1:j) {
 tstat_arp[i] <- modele_arp_01_1_3$coef[i] / sqrt(modele_arp_01_1_3$var.coef[i, i])
}
tstat_arp

fitted_arP <- ts_train01_1_3 - residuals(modele_arp_01_1_3)
plot(fitted_arP, type = "l", col = "blue", main = "Valeurs ajustées - AR(1)", ylab = "Prévisions")

prev_arP_01_1_3 <- forecast(modele_arp_01_1_3, h = 12)
prev_arP_01_1_3
plot(prev_arP_01_1_3)

aic_arp <- AIC(modele_arp_01_1_3)
cat("AIC du modèle SSARIMA :", round(aic_arp, 2), "\n")

aicc_arp <- AICc(modele_arp_01_1_3)
cat("AICc du modèle SSARIMA :", round(aicc_arp, 2), "\n")

#### X13-

myregx13_01_1_3 <- regarima_x13(ts_train01_1_3, spec ="RG5c")
summary(myregx13_01_1_3)
s_transform(myregx13_01_1_3)
#plot(myregx13_01_1_3)
myregx13_01_1_3$forecast
forex13_01_1_3 <- matrix(myregx13_01_1_3$forecast[1:12])
forex13_01_1_3

plot(ts_train01_1_3,
     xlim = c(time(ts_train01_1_3)[1], tail(time(ts_train01_1_3), 1) + 1),
     ylim = range(ts_train01_1_3, forex13_01_1_3),
     main = "Prévisions issues de X13-ARIMA-SEATS",
     ylab = "Valeurs", xlab = "Temps")
lines(ts(forex13_01_1_3,
         start = c(2023, 1),  # à adapter si nécessaire
         frequency = frequency(ts_train01_1_3)),
      col = "blue", lwd = 2, lty = 2)
legend("topleft", legend = c("Série observée", "Prévision X13"),
       col = c("black", "blue"), lty = c(1,2), lwd = 2)


#### stl-

decomp = stlm(ts_train01_1_3, s.window="periodic")
show(decomp)
#

fitstl_01_1_3 = stlm(ts_train01_1_3)
prevstl_01_1_3 <- forecast(fitstl_01_1_3,12) #période d'une année

plot(prevstl_01_1_3)
summary(prevstl_01_1_3)


### .2.2 HoltWinters--

WH_add_01_1_3 <- HoltWinters(ts_train01_1_3, gamma = FALSE)
WH_add_01_1_3
show(WH_add_01_1_3)
WH_add_01_1_3$coefficients
plot(WH_add_01_1_3)
forecast_hw01 <- forecast(WH_add_01_1_3, h = 12)
plot(forecast_hw01)
forecast_hw01

# Nombre d'observations
res <- residuals(WH_add_01_1_3)
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


#### ADAM ETS-

fit_ADAM_ETS_01_1_3 <- auto.adam(ts_train01_1_3,
                                 model = "ZZZ",
                                 lags = c(1, 12),
                                 silent = TRUE)          
# ZZZ car ne spécifie rien (tendance, saisonnalité, erreur)
fit_ADAM_ETS_01_1_3
summary(fit_ADAM_ETS_01_1_3)

par(mfcol=c(2,2))
plot(fit_ADAM_ETS_01_1_3)
par(mfcol=c(1,1))


plot(fit_ADAM_ETS_01_1_3$states)
plot(fit_ADAM_ETS_01_1_3$residuals)

prev_ADAM_ETS_01_1_3 <- forecast(fit_ADAM_ETS_01_1_3, h=12)
show(prev_ADAM_ETS_01_1_3)
plot(prev_ADAM_ETS_01_1_3)

#### ADAM ETS + SARIMA-

fitadam3_01_1_3 <- auto.adam(ts_train01_1_3,
                             model = "ZZZ",
                             lags = c(1, 12),
                             orders = list(ar = c(3,3), i = 1, ma = c(3,3)),
                             select = TRUE,
                             bootstrap = TRUE)
fitadam3_01_1_3

summary(fitadam3_01_1_3)

par(mfcol=c(2,2))
plot(fitadam3_01_1_3)

par(mfcol=c(1,1))
plot(fitadam3_01_1_3$residuals)

prev_AES_01_1_3 <- forecast(fitadam3_01_1_3,12)
show(prev_AES_01_1_3)
plot(prev_AES_01_1_3)


### .2.5. SSARIMA-

fit_SSARIMA_01_1_3 <- auto.ssarima(ts_train01_1_3, lags=c(1,12), orders=list(ar=c(3,3), i=(2), ma=c(3,3), select=TRUE))
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


#### naïves-

pred_naive_01_1_3 <- naive(ts_train01_1_3, h=12)
show(pred_naive_01_1_3)
plot(pred_naive_01_1_3)




#### 3. évolution des prévisions----

ts_2023_01_1_3 <- window(tsIPC01_1_3_CVS_RJDmetra_corr_diff, start = c(2023, 1), end = c(2023, 12))

# Fonction mise à jour : transforme la date en format mois
extract_forecast_df <- function(fcast_obj, name) {
 df <- data.frame(
  date = as.Date(as.yearmon(time(fcast_obj$mean))),  # conversion en date
  value = as.numeric(fcast_obj$mean),
  model = name
 )
 df %>% slice_head(n = 12)
}

ts_forex13_01_1_3 <- ts(as.numeric(forex13_01_1_3), start = c(2023, 1), frequency = 12)

# 2. Construire un objet forecast manuellement (sans recalculer)
forecast_x13_01_1_3 <- structure(list(
 mean = ts_forex13_01_1_3,  # ici, un vrai ts avec start/end
 lower = matrix(NA, nrow = 12, ncol = 2, dimnames = list(NULL, c("80%", "95%"))),
 upper = matrix(NA, nrow = 12, ncol = 2, dimnames = list(NULL, c("80%", "95%"))),
 level = c(80, 95),
 x = NULL,
 method = "X13",
 fitted = NULL,
 residuals = NULL
), class = "forecast")

dfs_01_1_3 <- list(
 extract_forecast_df(prev_arima_01_1_3, "ARMA"),
 extract_forecast_df(prev_ar1_01_1_3, "AR(1)"),
 extract_forecast_df(prev_arP_01_1_3, "AR(P)"),
 extract_forecast_df(forecast_x13_01_1_3, "X13"),
 extract_forecast_df(forecast_hw01, "HoltWinters"),
 extract_forecast_df(prev_ADAM_ETS_01_1_3, "ADAM_ETS"),
 extract_forecast_df(prev_AES_01_1_3, "ADAM_ETS+SARIMA"),
 extract_forecast_df(prev_SSARIMA_01_1_3, "SSARIMA"),
 extract_forecast_df(pred_naive_01_1_3, "Naïve")
)

#Je vais comparer aveclesvaleur reel grace a ts_test01_1_3
observed_df_01_1_3 <- data.frame(
 date = as.Date(as.yearmon(time(ts_test01_1_3))),
 value = as.numeric(ts_test01_1_3),
 model = "Observée")

# Combine toutes les prévisions et l'observée
df_all_01_1_3 <- bind_rows(dfs_01_1_3, observed_df_01_1_3)
df_all_01_1_3$date <- as.Date(df_all_01_1_3$date, format = "%Y-%m-%d")
library(ggplot2)

# 1. Graphe simple avec axe en numérique
ggplot(df_all_01_1_3, aes(x = as.numeric(date), y = value, color = model)) +
 geom_line(size = 1.2) +
 scale_x_continuous(
  breaks = as.numeric(df_all_01_1_3$date[1:12]),
  labels = format(df_all_01_1_3$date[1:12], "%b %Y")
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
ggplot(df_all_01_1_3, aes(x = date, y = value, color = model, linetype = model)) +
 geom_line(size = 1.2) +
 labs(
  title = "Prévisions - 01.1.3 : Poissons et fruits de mer - Sur 12 mois (2023) par modèle",
  x = "Mois", y = "Valeur prévue", color = "Modèle", linetype = "Modèle"
 ) +
 scale_color_manual(values = c(
  "ADAM_ETS" = "red",
  "ADAM_ETS+SARIMA" = "orange",
  "HoltWinters" = "yellow",
  "Naïve" = "black",
  "SSARIMA" = "purple",
  "X13" = "darkblue",
  "ARMA" = "forestgreen",
  "AR(1)" = "brown",
  "AR(P)" = "darkcyan",
  "Observée" = "black"
 )) +
 scale_linetype_manual(values = c(
  "ADAM_ETS" = "solid",
  "ADAM_ETS+SARIMA" = "solid",
  "HoltWinters" = "solid",
  "Naïve" = "solid",
  "SSARIMA" = "solid",
  "X13" = "dashed",
  "ARMA" = "dotted",
  "AR(1)" = "dotted",
  "AR(P)" = "dotted",
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

#### 4 qualité de prevision -----

## MSE & R²OOS
## MSE & R²OOS avec le modèle Naïf comme référence

# Observations réelles
actual_values01_1_3 <- ts_test01_1_3

# Liste des prévisions pour chaque modèle
forecasts_01_1_3 <- list(
 prev_arima_01_1_3$mean,
 prev_ar1_01_1_3$mean,
 prev_arP_01_1_3$mean,
 forecast_x13_01_1_3$mean,
 forecast_hw01$mean,
 prev_ADAM_ETS_01_1_3$mean,
 prev_AES_01_1_3$mean,
 prev_SSARIMA_01_1_3$mean,
 pred_naive_01_1_3$mean
)


# Fonction MSE + R²OOS avec benchmark explicite
calculate_metrics_01_1_3 <- function(actual, forecast, benchmark_forecast) {
 mse <- mean((actual - forecast)^2)
 sst <- sum((actual - benchmark_forecast)^2)
 sse <- sum((actual - forecast)^2)
 r2oos <- 1 - (sse / sst)
 return(list(mse = mse, r2oos = r2oos))
}

# Naïve utilisé comme benchmark
benchmark_forecast <- as.numeric(pred_naive_01_1_3$mean)

# Calcul des métriques
metrics <- lapply(forecasts_01_1_3, function(fcast) {
 calculate_metrics_01_1_3(as.numeric(actual_values01_1_3), as.numeric(fcast), benchmark_forecast)
})

# Data frame de résultats
metrics_df_01_1_3 <- as.data.frame(do.call(rbind, metrics))
rownames(metrics_df_01_1_3) <- c("ARMA", "AR(1)", "AR(P)","X13",  "HoltWinters","ADAM_ETS", "ADAM_ETS+SARIMA", "SSARIMA", 
                                 "Naïve")

print(metrics_df_01_1_3)

## CSPE

# Fonction CSPE
calculate_cspe <- function(actual, forecast) {
 errors <- (actual - forecast)^2
 cspe <- cumsum(errors)
 return(cspe)
}

# CSPE pour chaque modèle
cspe_list <- lapply(forecasts_01_1_3, function(fcast) {
 calculate_cspe(as.numeric(actual_values01_1_3), as.numeric(fcast))
})

# Création du data.frame CSPE
cspe_df <- do.call(cbind, cspe_list)
colnames(cspe_df) <- rownames(metrics_df_01_1_3)
cspe_df <- cbind(Date = as.Date(time(actual_values01_1_3)), cspe_df)
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
  "ARMA" = "forestgreen", "AR(1)" = "brown", "AR(P)" = "darkcyan","X13" = "darkblue",
  "HoltWinters" = "yellow", "ADAM_ETS" = "red", "ADAM_ETS+SARIMA" = "orange",
  "SSARIMA" = "purple", "Naïve" = "black"
 )) +
 scale_linetype_manual(values = c(
  "ARMA" = "dashed", "AR(1)" = "dotted", "AR(P)" = "solid","X13" = "dashed",
  "HoltWinters" = "solid", "ADAM_ETS" = "solid", "ADAM_ETS+SARIMA" = "solid",
  "SSARIMA" = "solid", "Naïve" = "dotdash"
 )) +
 
 
 scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
 theme_minimal(base_size = 13) +
 theme(axis.text.x = element_text(angle = 45, hjust = 1),
       legend.position = "bottom",
       legend.title = element_text(face = "bold"),
       plot.title = element_text(face = "bold"))



##### 5 Test de précision-----

# Définir la période de temps
start_date <- c(2023, 1)
end_date <- c(2023, 12)

# Fonction pour ajuster les séries temporelles
adjust_time_series <- function(ts_data) {
 return(window(ts_data, start = start_date, end = end_date))
}

# Ajuster les séries temporelles pour chaque modèle
for_observed <- adjust_time_series(ts_2023_01_1_3)
for_ARMA     <- adjust_time_series(prev_arima_01_1_3$mean)
for_AR1      <- adjust_time_series(prev_ar1_01_1_3$mean)
for_ARP      <- adjust_time_series(prev_arP_01_1_3$mean)
for_X13      <- adjust_time_series(forecast_x13_01_1_3$mean)
for_HW       <- adjust_time_series(forecast_hw01$mean)
for_ADAM_ETS     <- adjust_time_series(prev_ADAM_ETS_01_1_3$mean)
for_AES      <- adjust_time_series(prev_AES_01_1_3$mean)
for_SSARIMA  <- adjust_time_series(prev_SSARIMA_01_1_3$mean)
for_NAIVE    <- adjust_time_series(pred_naive_01_1_3$mean)



# Vérifiez que toutes les séries temporelles ont la bonne longueur

ts_2023_01_1_3 <- window(tsIPC01_1_3_CVS_RJDmetra_corr_diff, start = c(2023, 1), end = c(2023, 12))
ts_test01_1_3 <- window(tsIPC01_1_3_CVS_RJDmetra_corr_diff, start = c(2023, 1), end = c(2023, 12))
print(length(ts_2023_01_1_3))

print(length(for_observed))
print(length(for_ARMA))
print(length(for_AR1))
print(length(for_ARP))
print(length(for_X13))

print(length(for_HW))
print(length(for_ADAM_ETS))
print(length(for_AES))
print(length(for_SSARIMA))
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

# Calculer le test DM

dm.test(error_NAIVE, error_ARMA, h = length(for_observed))
dm.test(error_NAIVE, error_AR1, h = length(for_observed))
dm.test(error_NAIVE, error_ARP, h = length(for_observed))
dm.test(error_NAIVE, error_X13, h = length(for_observed))
dm.test(error_NAIVE, error_HW, h = length(for_observed))
dm.test(error_NAIVE, error_ADAM_ETS, h = length(for_observed))
dm.test(error_NAIVE, error_AES, h = length(for_observed))
dm.test(error_NAIVE, error_SSARIMA, h = length(for_observed))


# Test de Diebold-Mariano avec h = 1
dm.test(error_NAIVE, error_ARMA, h = 1)
dm.test(error_NAIVE, error_AR1, h = 1)
dm.test(error_NAIVE, error_ARP, h = 1)
dm.test(error_NAIVE, error_X13, h = 1)
dm.test(error_NAIVE, error_HW, h = 1)
dm.test(error_NAIVE, error_ADAM_ETS, h = 1)
dm.test(error_NAIVE, error_AES, h = 1)
dm.test(error_NAIVE, error_SSARIMA, h = 1)


## le meilleur modèle est SSARIMA cool




#-------------- 01.1.4 - Lait, fromage et oeufs ---------


ts_Lait_fromage_oeufs <- ts(dfcomp$Lait_fromage_oeufs, start = c(2010, 1), frequency = 12)

autoplot(ts_Lait_fromage_oeufs) +
 ggtitle("IPC - Coicop :  01.1.4 - Lait, fromage et oeufs") +
 xlab("Date") + ylab("IPC - 01.1.4")+
 theme_minimal()+
 geom_line(color = "blue")


### - tests (CVS stationnaire)

combined_test(ts_Lait_fromage_oeufs)
seasdum(ts_Lait_fromage_oeufs) #Seasonal Dummies

adf.test(ts_Lait_fromage_oeufs)
kpss.test(ts_Lait_fromage_oeufs)

xss <-diff(ts_Lait_fromage_oeufs)
adf.test(xss)
kpss.test(xss) # La serie est stationnaire ap diff :)

myregx13_01_1_4 <- regarima_x13(ts_Lait_fromage_oeufs, spec ="RG5c")
s_transform(myregx13_01_1_4)  # test log/level
# Pas de transformation en log.

### - CVS-

# Avec RJDmetra
s_transform(myregx13_01_1_4)
myspec <- x13_spec("RSA5c")
mysax13 <- x13(ts_Lait_fromage_oeufs, myspec) # SCV de RJGDmetra
#plot(mysax13$final) # JTM

# Extraire la série désaisonnalisée (CVS)
tsIPC01_1_4_CVS_RJDmetra <- ts(mysax13$final$series[, "sa"],
                               start = start(ts_Lait_fromage_oeufs), frequency = frequency(ts_Lait_fromage_oeufs))





dfcomp_01_1_4 <- data.frame(
 Date = as.Date(as.yearmon(time(ts_Lait_fromage_oeufs)), frac = 1),
 Originale = as.numeric(ts_Lait_fromage_oeufs),
 CVS_RJD = as.numeric(tsIPC01_1_4_CVS_RJDmetra)
)

### Outliers-

outliers01_1_4 <- tso(tsIPC01_1_4_CVS_RJDmetra)

print(outliers01_1_4)

plot(outliers01_1_4)
show(outliers01_1_4)



tsIPC01_1_4_CVS_RJDmetra_corr <- outliers01_1_4$yadj
dfcomp_01_1_4$CVS_RJD_CORR <- as.numeric(tsIPC01_1_4_CVS_RJDmetra_corr)

### Virification de la stationnarité-

combined_test(tsIPC01_1_4_CVS_RJDmetra_corr)
seasdum(tsIPC01_1_4_CVS_RJDmetra_corr) #Seasonal Dummies
# Toujours CVS logique

adf.test(tsIPC01_1_4_CVS_RJDmetra_corr)
kpss.test(tsIPC01_1_4_CVS_RJDmetra_corr)

tsIPC01_1_4_CVS_RJDmetra_corr_diff <- diff(tsIPC01_1_4_CVS_RJDmetra_corr)
adf.test(tsIPC01_1_4_CVS_RJDmetra_corr_diff)
kpss.test(tsIPC01_1_4_CVS_RJDmetra_corr_diff)


autoplot(tsIPC01_1_4_CVS_RJDmetra_corr_diff) +
 ggtitle("IPC - Coicop :  01.1.4 - serie différenciée") +
 xlab("Date") + ylab("IPC 01.1.4")+
 theme_minimal()+
 geom_line(color = "blue")


dfcomp_01_1_4 <- dfcomp_01_1_4[-1, ]  # on retire la première ligne
dfcomp_01_1_4$CVS_RJD_CORR_DIFF <- as.numeric(tsIPC01_1_4_CVS_RJDmetra_corr_diff)

#### 2. Estimation des modèles-----

# a.  Estimer et commenter les paramètres des modèles AR(1), AR(p) et ARIMA(p,d,q) et de la méthode LED Holt-Winters, ADAM ETS, ADAM ETS ARIMA, SSARIMA et CES
# b.  Paramètres : présenter sous forme de tableau les paramètres des modèles précédents. Déterminer et commenter le meilleur modèle d’après les critères AIC et AICc

# On entraine nos modele jusqu'a 2022 pour faire la prevision en 2023.

ts_train01_1_4 <- window(tsIPC01_1_4_CVS_RJDmetra_corr_diff, start = c(2010, 2), end = c(2022, 12))
ts_test01_1_4 <- window(tsIPC01_1_4_CVS_RJDmetra_corr_diff, start = c(2023, 1), end = c(2023, 12))


### 2. Estimation du modèle

###. 2.1 AR/ARMA-

### 2.1.1 ARMA-

modele_arima_01_1_4 <- auto.arima(ts_train01_1_4, stepwise = FALSE, approximation = FALSE)

summary(modele_arima_01_1_4)

j <- ncol(modele_arima_01_1_4$var.coef)
tstat <- matrix(nrow=j, ncol=1)
for(i in 1:j)
{
 tstat[i,1] <- modele_arima_01_1_4$coef[i]/sqrt(modele_arima_01_1_4$var.coef[i,i])
}
tstat

# Graphique des valeurs ajustées
plot(modele_arima_01_1_4$fitted)


prev_arima_01_1_4 <- forecast(modele_arima_01_1_4, h=12)
prev_arima_01_1_4
plot(prev_arima_01_1_4)

aic_arma <- AIC(modele_arima_01_1_4)
cat("AIC du modèle SSARIMA :", round(aic_arma, 2), "\n")

aicc_arma <- AICc(modele_arima_01_1_4)
cat("AICc du modèle SSARIMA :", round(aicc_arma, 2), "\n")

## 2.1.2 AR(1)-

modele_ar1_01_1_4 <- Arima(ts_train01_1_4, order = c(1, 0, 0), include.mean = TRUE)

summary(modele_ar1_01_1_4)

j <- ncol(modele_ar1_01_1_4$var.coef)
tstat <- matrix(nrow=j, ncol=1)
for(i in 1:j)
{
 tstat[i,1] <- modele_ar1_01_1_4$coef[i]/sqrt(modele_ar1_01_1_4$var.coef[i,i])
}
tstat

fitted_ar1 <- ts_train01_1_4 - residuals(modele_ar1_01_1_4)
plot(fitted_ar1, type = "l", col = "blue", main = "Valeurs ajustées - AR(1)", ylab = "Prévisions")

prev_ar1_01_1_4 <- forecast(modele_ar1_01_1_4, h = 12)
prev_ar1_01_1_4
plot(prev_ar1_01_1_4)

aic_ar1 <- AIC(modele_ar1_01_1_4)
cat("AIC du modèle AR(1) :", round(aic_ar1, 2), "\n")

aicc_ar1 <- AICc(modele_ar1_01_1_4)
cat("AICc du modèle AR(1) :", round(aicc_ar1, 2), "\n")

### 2.1.3 AR(P)-
modele_arp_01_1_4 <- auto.arima(ts_train01_1_4,
                                d = 0,
                                max.p = 10,        # tu peux ajuster ce maximum
                                max.q = 0,         # pas de MA
                                stationary = TRUE,
                                seasonal = FALSE,
                                stepwise = FALSE,
                                approximation = FALSE)


# Résumé du modèle
summary(modele_arp_01_1_4)

# t-stat des coefficients
j <- length(modele_arp_01_1_4$coef)
tstat_arp <- numeric(j)
for(i in 1:j) {
 tstat_arp[i] <- modele_arp_01_1_4$coef[i] / sqrt(modele_arp_01_1_4$var.coef[i, i])
}
tstat_arp

fitted_arP <- ts_train01_1_4 - residuals(modele_arp_01_1_4)
plot(fitted_arP, type = "l", col = "blue", main = "Valeurs ajustées - AR(1)", ylab = "Prévisions")

prev_arP_01_1_4 <- forecast(modele_arp_01_1_4, h = 12)
prev_arP_01_1_4
plot(prev_arP_01_1_4)

aic_arp <- AIC(modele_arp_01_1_4)
cat("AIC du modèle SSARIMA :", round(aic_arp, 2), "\n")

aicc_arp <- AICc(modele_arp_01_1_4)
cat("AICc du modèle SSARIMA :", round(aicc_arp, 2), "\n")

#### X13-

myregx13_01_1_4 <- regarima_x13(ts_train01_1_4, spec ="RG5c")
summary(myregx13_01_1_4)
s_transform(myregx13_01_1_4)
#plot(myregx13_01_1_4)
myregx13_01_1_4$forecast
forex13_01_1_4 <- matrix(myregx13_01_1_4$forecast[1:12])
forex13_01_1_4

plot(ts_train01_1_4,
     xlim = c(time(ts_train01_1_4)[1], tail(time(ts_train01_1_4), 1) + 1),
     ylim = range(ts_train01_1_4, forex13_01_1_4),
     main = "Prévisions issues de X13-ARIMA-SEATS",
     ylab = "Valeurs", xlab = "Temps")
lines(ts(forex13_01_1_4,
         start = c(2023, 1),  # à adapter si nécessaire
         frequency = frequency(ts_train01_1_4)),
      col = "blue", lwd = 2, lty = 2)
legend("topleft", legend = c("Série observée", "Prévision X13"),
       col = c("black", "blue"), lty = c(1,2), lwd = 2)


#### stl-

decomp = stlm(ts_train01_1_4, s.window="periodic")
show(decomp)
#

fitstl_01_1_4 = stlm(ts_train01_1_4)
prevstl_01_1_4 <- forecast(fitstl_01_1_4,12) #période d'une année

plot(prevstl_01_1_4)
summary(prevstl_01_1_4)


### .2.2 HoltWinters--

WH_add_01_1_4 <- HoltWinters(ts_train01_1_4, gamma = FALSE)
WH_add_01_1_4
show(WH_add_01_1_4)
WH_add_01_1_4$coefficients
plot(WH_add_01_1_4)
forecast_hw01 <- forecast(WH_add_01_1_4, h = 12)
plot(forecast_hw01)
forecast_hw01

# Nombre d'observations
res <- residuals(WH_add_01_1_4)
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


#### ADAM ETS-

fit_ADAM_ETS_01_1_4 <- auto.adam(ts_train01_1_4,
                                 model = "ZZZ",
                                 lags = c(1, 12),
                                 silent = TRUE)          
# ZZZ car ne spécifie rien (tendance, saisonnalité, erreur)
fit_ADAM_ETS_01_1_4
summary(fit_ADAM_ETS_01_1_4)

par(mfcol=c(2,2))
plot(fit_ADAM_ETS_01_1_4)
par(mfcol=c(1,1))


plot(fit_ADAM_ETS_01_1_4$states)
plot(fit_ADAM_ETS_01_1_4$residuals)

prev_ADAM_ETS_01_1_4 <- forecast(fit_ADAM_ETS_01_1_4, h=12)
show(prev_ADAM_ETS_01_1_4)
plot(prev_ADAM_ETS_01_1_4)

#### ADAM ETS + SARIMA-

fitadam3_01_1_4 <- auto.adam(ts_train01_1_4,
                             model = "ZZZ",
                             lags = c(1, 12),
                             orders = list(ar = c(3,3), i = 1, ma = c(3,3)),
                             select = TRUE,
                             bootstrap = TRUE)
fitadam3_01_1_4

summary(fitadam3_01_1_4)

par(mfcol=c(2,2))
plot(fitadam3_01_1_4)

par(mfcol=c(1,1))
plot(fitadam3_01_1_4$residuals)

prev_AES_01_1_4 <- forecast(fitadam3_01_1_4,12)
show(prev_AES_01_1_4)
plot(prev_AES_01_1_4)


### .2.5. SSARIMA-

fit_SSARIMA_01_1_4 <- auto.ssarima(ts_train01_1_4, lags=c(1,12), orders=list(ar=c(3,3), i=(2), ma=c(3,3), select=TRUE))
summary(fit_SSARIMA_01_1_4)

par(mfcol=c(2,2))
plot(fit_SSARIMA_01_1_4)

par(mfcol=c(1,1))

plot(fit_SSARIMA_01_1_4$residuals)

prev_SSARIMA_01_1_4 <- forecast(fit_SSARIMA_01_1_4, h=12)
prev_SSARIMA_01_1_4
plot(prev_SSARIMA_01_1_4)

aic_ssarima <- AIC(fit_SSARIMA_01_1_4)
cat("AIC du modèle SSARIMA :", round(aic_ssarima, 2), "\n")

aicc_ssarima <- AICc(fit_SSARIMA_01_1_4)
cat("AICc du modèle SSARIMA :", round(aicc_ssarima, 2), "\n")


#### naïves-

pred_naive_01_1_4 <- naive(ts_train01_1_4, h=12)
show(pred_naive_01_1_4)
plot(pred_naive_01_1_4)




#### 3. évolution des prévisions----

ts_2023_01_1_4 <- window(tsIPC01_1_4_CVS_RJDmetra_corr_diff, start = c(2023, 1), end = c(2023, 12))

# Fonction mise à jour : transforme la date en format mois
extract_forecast_df <- function(fcast_obj, name) {
 df <- data.frame(
  date = as.Date(as.yearmon(time(fcast_obj$mean))),  # conversion en date
  value = as.numeric(fcast_obj$mean),
  model = name
 )
 df %>% slice_head(n = 12)
}

ts_forex13_01_1_4 <- ts(as.numeric(forex13_01_1_4), start = c(2023, 1), frequency = 12)

# 2. Construire un objet forecast manuellement (sans recalculer)
forecast_x13_01_1_4 <- structure(list(
 mean = ts_forex13_01_1_4,  # ici, un vrai ts avec start/end
 lower = matrix(NA, nrow = 12, ncol = 2, dimnames = list(NULL, c("80%", "95%"))),
 upper = matrix(NA, nrow = 12, ncol = 2, dimnames = list(NULL, c("80%", "95%"))),
 level = c(80, 95),
 x = NULL,
 method = "X13",
 fitted = NULL,
 residuals = NULL
), class = "forecast")

dfs_01_1_4 <- list(
 extract_forecast_df(prev_arima_01_1_4, "ARMA"),
 extract_forecast_df(prev_ar1_01_1_4, "AR(1)"),
 extract_forecast_df(prev_arP_01_1_4, "AR(P)"),
 extract_forecast_df(forecast_x13_01_1_4, "X13"),
 extract_forecast_df(forecast_hw01, "HoltWinters"),
 extract_forecast_df(prev_ADAM_ETS_01_1_4, "ADAM_ETS"),
 extract_forecast_df(prev_AES_01_1_4, "ADAM_ETS+SARIMA"),
 extract_forecast_df(prev_SSARIMA_01_1_4, "SSARIMA"),
 extract_forecast_df(pred_naive_01_1_4, "Naïve")
)

#Je vais comparer aveclesvaleur reel grace a ts_test01_1_4
observed_df_01_1_4 <- data.frame(
 date = as.Date(as.yearmon(time(ts_test01_1_4))),
 value = as.numeric(ts_test01_1_4),
 model = "Observée")

# Combine toutes les prévisions et l'observée
df_all_01_1_4 <- bind_rows(dfs_01_1_4, observed_df_01_1_4)
df_all_01_1_4$date <- as.Date(df_all_01_1_4$date, format = "%Y-%m-%d")
library(ggplot2)

# 1. Graphe simple avec axe en numérique
ggplot(df_all_01_1_4, aes(x = as.numeric(date), y = value, color = model)) +
 geom_line(size = 1.2) +
 scale_x_continuous(
  breaks = as.numeric(df_all_01_1_4$date[1:12]),
  labels = format(df_all_01_1_4$date[1:12], "%b %Y")
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
ggplot(df_all_01_1_4, aes(x = date, y = value, color = model, linetype = model)) +
 geom_line(size = 1.2) +
 labs(
  title = "Prévisions - 01.1.4 : Lait, fromage et oeufs - Sur 12 mois (2023) par modèle",
  x = "Mois", y = "Valeur prévue", color = "Modèle", linetype = "Modèle"
 ) +
 scale_color_manual(values = c(
  "ADAM_ETS" = "red",
  "ADAM_ETS+SARIMA" = "orange",
  "HoltWinters" = "yellow",
  "Naïve" = "black",
  "SSARIMA" = "purple",
  "X13" = "darkblue",
  "ARMA" = "forestgreen",
  "AR(1)" = "brown",
  "AR(P)" = "darkcyan",
  "Observée" = "black"
 )) +
 scale_linetype_manual(values = c(
  "ADAM_ETS" = "solid",
  "ADAM_ETS+SARIMA" = "solid",
  "HoltWinters" = "solid",
  "Naïve" = "solid",
  "SSARIMA" = "solid",
  "X13" = "dashed",
  "ARMA" = "dotted",
  "AR(1)" = "dotted",
  "AR(P)" = "dotted",
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

#### 4 qualité de prevision -----

## MSE & R²OOS
## MSE & R²OOS avec le modèle Naïf comme référence

# Observations réelles
actual_values01_1_4 <- ts_test01_1_4

# Liste des prévisions pour chaque modèle
forecasts_01_1_4 <- list(
 prev_arima_01_1_4$mean,
 prev_ar1_01_1_4$mean,
 prev_arP_01_1_4$mean,
 forecast_x13_01_1_4$mean,
 forecast_hw01$mean,
 prev_ADAM_ETS_01_1_4$mean,
 prev_AES_01_1_4$mean,
 prev_SSARIMA_01_1_4$mean,
 pred_naive_01_1_4$mean
)


# Fonction MSE + R²OOS avec benchmark explicite
calculate_metrics_01_1_4 <- function(actual, forecast, benchmark_forecast) {
 mse <- mean((actual - forecast)^2)
 sst <- sum((actual - benchmark_forecast)^2)
 sse <- sum((actual - forecast)^2)
 r2oos <- 1 - (sse / sst)
 return(list(mse = mse, r2oos = r2oos))
}

# Naïve utilisé comme benchmark
benchmark_forecast <- as.numeric(pred_naive_01_1_4$mean)

# Calcul des métriques
metrics <- lapply(forecasts_01_1_4, function(fcast) {
 calculate_metrics_01_1_4(as.numeric(actual_values01_1_4), as.numeric(fcast), benchmark_forecast)
})

# Data frame de résultats
metrics_df_01_1_4 <- as.data.frame(do.call(rbind, metrics))
rownames(metrics_df_01_1_4) <- c("ARMA", "AR(1)", "AR(P)","X13",  "HoltWinters","ADAM_ETS", "ADAM_ETS+SARIMA", "SSARIMA", 
                                 "Naïve")

print(metrics_df_01_1_4)

## CSPE

# Fonction CSPE
calculate_cspe <- function(actual, forecast) {
 errors <- (actual - forecast)^2
 cspe <- cumsum(errors)
 return(cspe)
}

# CSPE pour chaque modèle
cspe_list <- lapply(forecasts_01_1_4, function(fcast) {
 calculate_cspe(as.numeric(actual_values01_1_4), as.numeric(fcast))
})

# Création du data.frame CSPE
cspe_df <- do.call(cbind, cspe_list)
colnames(cspe_df) <- rownames(metrics_df_01_1_4)
cspe_df <- cbind(Date = as.Date(time(actual_values01_1_4)), cspe_df)
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
  "ARMA" = "forestgreen", "AR(1)" = "brown", "AR(P)" = "darkcyan","X13" = "darkblue",
  "HoltWinters" = "yellow", "ADAM_ETS" = "red", "ADAM_ETS+SARIMA" = "orange",
  "SSARIMA" = "purple", "Naïve" = "black"
 )) +
 scale_linetype_manual(values = c(
  "ARMA" = "dashed", "AR(1)" = "dotted", "AR(P)" = "solid","X13" = "dashed",
  "HoltWinters" = "solid", "ADAM_ETS" = "solid", "ADAM_ETS+SARIMA" = "solid",
  "SSARIMA" = "solid", "Naïve" = "dotdash"
 )) +
 
 
 scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
 theme_minimal(base_size = 13) +
 theme(axis.text.x = element_text(angle = 45, hjust = 1),
       legend.position = "bottom",
       legend.title = element_text(face = "bold"),
       plot.title = element_text(face = "bold"))



##### 5 Test de précision-----

# Définir la période de temps
start_date <- c(2023, 1)
end_date <- c(2023, 12)

# Fonction pour ajuster les séries temporelles
adjust_time_series <- function(ts_data) {
 return(window(ts_data, start = start_date, end = end_date))
}

# Ajuster les séries temporelles pour chaque modèle
for_observed <- adjust_time_series(ts_2023_01_1_4)
for_ARMA     <- adjust_time_series(prev_arima_01_1_4$mean)
for_AR1      <- adjust_time_series(prev_ar1_01_1_4$mean)
for_ARP      <- adjust_time_series(prev_arP_01_1_4$mean)
for_X13      <- adjust_time_series(forecast_x13_01_1_4$mean)
for_HW       <- adjust_time_series(forecast_hw01$mean)
for_ADAM_ETS     <- adjust_time_series(prev_ADAM_ETS_01_1_4$mean)
for_AES      <- adjust_time_series(prev_AES_01_1_4$mean)
for_SSARIMA  <- adjust_time_series(prev_SSARIMA_01_1_4$mean)
for_NAIVE    <- adjust_time_series(pred_naive_01_1_4$mean)



# Vérifiez que toutes les séries temporelles ont la bonne longueur

ts_2023_01_1_4 <- window(tsIPC01_1_4_CVS_RJDmetra_corr_diff, start = c(2023, 1), end = c(2023, 12))
ts_test01_1_4 <- window(tsIPC01_1_4_CVS_RJDmetra_corr_diff, start = c(2023, 1), end = c(2023, 12))
print(length(ts_2023_01_1_4))

print(length(for_observed))
print(length(for_ARMA))
print(length(for_AR1))
print(length(for_ARP))
print(length(for_X13))
print(length(for_HW))
print(length(for_ADAM_ETS))
print(length(for_AES))
print(length(for_SSARIMA))
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

# Calculer le test DM

dm.test(error_NAIVE, error_ARMA, h = length(for_observed))
dm.test(error_NAIVE, error_AR1, h = length(for_observed))
dm.test(error_NAIVE, error_ARP, h = length(for_observed))
dm.test(error_NAIVE, error_X13, h = length(for_observed))
dm.test(error_NAIVE, error_HW, h = length(for_observed))
dm.test(error_NAIVE, error_ADAM_ETS, h = length(for_observed))
dm.test(error_NAIVE, error_AES, h = length(for_observed))
dm.test(error_NAIVE, error_SSARIMA, h = length(for_observed))


# Test de Diebold-Mariano avec h = 1
dm.test(error_NAIVE, error_ARMA, h = 1)
dm.test(error_NAIVE, error_AR1, h = 1)
dm.test(error_NAIVE, error_ARP, h = 1)
dm.test(error_NAIVE, error_X13, h = 1)
dm.test(error_NAIVE, error_HW, h = 1)
dm.test(error_NAIVE, error_ADAM_ETS, h = 1)
dm.test(error_NAIVE, error_AES, h = 1)
dm.test(error_NAIVE, error_SSARIMA, h = 1)


## le meilleur modèle est HoltWinters cool

#-------------- 01.1.5 - Huiles et graisses ---------

ts_Huiles_graisses <- ts(dfcomp$Huiles_graisses, start = c(2010, 1), frequency = 12)

autoplot(ts_Huiles_graisses) +
 ggtitle("IPC - Coicop :  01.1.5 - Huiles et graisses") +
 xlab("Date") + ylab("IPC - 01.1.5")+
 theme_minimal()+
 geom_line(color = "blue")


### - tests (CVS stationnaire)

combined_test(ts_Huiles_graisses)
seasdum(ts_Huiles_graisses) #Seasonal Dummies

adf.test(ts_Huiles_graisses)
kpss.test(ts_Huiles_graisses)

xss <-diff(ts_Huiles_graisses)
adf.test(xss)
kpss.test(xss) # La serie est stationnaire ap diff :)

myregx13_01_1_5 <- regarima_x13(ts_Huiles_graisses, spec ="RG5c")
s_transform(myregx13_01_1_5)  # test log/level
# Pas de transformation en log.

### - CVS-

# Avec RJDmetra
s_transform(myregx13_01_1_5)
myspec <- x13_spec("RSA5c")
mysax13 <- x13(ts_Huiles_graisses, myspec) # SCV de RJGDmetra
#plot(mysax13$final) # JTM

# Extraire la série désaisonnalisée (CVS)
tsIPC01_1_5_CVS_RJDmetra <- ts(mysax13$final$series[, "sa"],
                               start = start(ts_Huiles_graisses), frequency = frequency(ts_Huiles_graisses))

dfcomp_01_1_5 <- data.frame(
 Date = as.Date(as.yearmon(time(ts_Huiles_graisses)), frac = 1),
 Originale = as.numeric(ts_Huiles_graisses),
 CVS_RJD = as.numeric(tsIPC01_1_5_CVS_RJDmetra)
)

### Outliers-

outliers01_1_5 <- tso(tsIPC01_1_5_CVS_RJDmetra)
print(outliers01_1_5)

plot(outliers01_1_5)
show(outliers01_1_5)

tsIPC01_1_5_CVS_RJDmetra_corr <- outliers$yadj
dfcomp_01_1_5$CVS_RJD_CORR <- as.numeric(tsIPC01_1_5_CVS_RJDmetra_corr)

### Virification de la stationnarité-

combined_test(tsIPC01_1_5_CVS_RJDmetra_corr)
seasdum(tsIPC01_1_5_CVS_RJDmetra_corr) #Seasonal Dummies
# Toujours CVS logique

adf.test(tsIPC01_1_5_CVS_RJDmetra_corr)
kpss.test(tsIPC01_1_5_CVS_RJDmetra_corr)

tsIPC01_1_5_CVS_RJDmetra_corr_diff <- diff(tsIPC01_1_5_CVS_RJDmetra_corr)
adf.test(tsIPC01_1_5_CVS_RJDmetra_corr_diff)
kpss.test(tsIPC01_1_5_CVS_RJDmetra_corr_diff)


autoplot(tsIPC01_1_5_CVS_RJDmetra_corr_diff) +
 ggtitle("IPC - Coicop : 01.1.5 - serie différenciée") +
 xlab("Date") + ylab("IPC 01.1.5")+
 theme_minimal()+
 geom_line(color = "blue")


dfcomp_01_1_5 <- dfcomp_01_1_5[-1, ]  # on retire la première ligne
dfcomp_01_1_5$CVS_RJD_CORR_DIFF <- as.numeric(tsIPC01_1_5_CVS_RJDmetra_corr_diff)

#### 2. Estimation des modèles-----

# a.  Estimer et commenter les paramètres des modèles AR(1), AR(p) et ARIMA(p,d,q) et de la méthode LED Holt-Winters, ADAM ETS, ADAM ETS ARIMA, SSARIMA et CES
# b.  Paramètres : présenter sous forme de tableau les paramètres des modèles précédents. Déterminer et commenter le meilleur modèle d’après les critères AIC et AICc

# On entraine nos modele jusqu'a 2022 pour faire la prevision en 2023.

ts_train01_1_5 <- window(tsIPC01_1_5_CVS_RJDmetra_corr_diff, start = c(2010, 2), end = c(2022, 12))
ts_test01_1_5 <- window(tsIPC01_1_5_CVS_RJDmetra_corr_diff, start = c(2023, 1), end = c(2023, 12))


### 2. Estimation du modèle

###. 2.1 AR/ARMA-

### 2.1.1 ARMA-

modele_arima_01_1_5 <- auto.arima(ts_train01_1_5, stepwise = FALSE, approximation = FALSE)

summary(modele_arima_01_1_5)

j <- ncol(modele_arima_01_1_5$var.coef)
tstat <- matrix(nrow=j, ncol=1)
for(i in 1:j)
{
 tstat[i,1] <- modele_arima_01_1_5$coef[i]/sqrt(modele_arima_01_1_5$var.coef[i,i])
}
tstat

# Graphique des valeurs ajustées
plot(modele_arima_01_1_5$fitted)


prev_arima_01_1_5 <- forecast(modele_arima_01_1_5, h=12)
prev_arima_01_1_5
plot(prev_arima_01_1_5)

aic_arma <- AIC(modele_arima_01_1_5)
cat("AIC du modèle SSARIMA :", round(aic_arma, 2), "\n")

aicc_arma <- AICc(modele_arima_01_1_5)
cat("AICc du modèle SSARIMA :", round(aicc_arma, 2), "\n")

## 2.1.2 AR(1)-

modele_ar1_01_1_5 <- Arima(ts_train01_1_5, order = c(1, 0, 0), include.mean = TRUE)

summary(modele_ar1_01_1_5)

j <- ncol(modele_ar1_01_1_5$var.coef)
tstat <- matrix(nrow=j, ncol=1)
for(i in 1:j)
{
 tstat[i,1] <- modele_ar1_01_1_5$coef[i]/sqrt(modele_ar1_01_1_5$var.coef[i,i])
}
tstat

fitted_ar1 <- ts_train01_1_5 - residuals(modele_ar1_01_1_5)
plot(fitted_ar1, type = "l", col = "blue", main = "Valeurs ajustées - AR(1)", ylab = "Prévisions")

prev_ar1_01_1_5 <- forecast(modele_ar1_01_1_5, h = 12)
prev_ar1_01_1_5
plot(prev_ar1_01_1_5)

aic_ar1 <- AIC(modele_ar1_01_1_5)
cat("AIC du modèle AR(1) :", round(aic_ar1, 2), "\n")

aicc_ar1 <- AICc(modele_ar1_01_1_5)
cat("AICc du modèle AR(1) :", round(aicc_ar1, 2), "\n")

### 2.1.3 AR(P)-
modele_arp_01_1_5 <- auto.arima(ts_train01_1_5,
                                d = 0,
                                max.p = 10,        # tu peux ajuster ce maximum
                                max.q = 0,         # pas de MA
                                stationary = TRUE,
                                seasonal = FALSE,
                                stepwise = FALSE,
                                approximation = FALSE)


# Résumé du modèle
summary(modele_arp_01_1_5)

# t-stat des coefficients
j <- length(modele_arp_01_1_5$coef)
tstat_arp <- numeric(j)
for(i in 1:j) {
 tstat_arp[i] <- modele_arp_01_1_5$coef[i] / sqrt(modele_arp_01_1_5$var.coef[i, i])
}
tstat_arp

fitted_arP <- ts_train01_1_5 - residuals(modele_arp_01_1_5)
plot(fitted_arP, type = "l", col = "blue", main = "Valeurs ajustées - AR(1)", ylab = "Prévisions")

prev_arP_01_1_5 <- forecast(modele_arp_01_1_5, h = 12)
prev_arP_01_1_5
plot(prev_arP_01_1_5)

aic_arp <- AIC(modele_arp_01_1_5)
cat("AIC du modèle SSARIMA :", round(aic_arp, 2), "\n")

aicc_arp <- AICc(modele_arp_01_1_5)
cat("AICc du modèle SSARIMA :", round(aicc_arp, 2), "\n")

#### X13-

myregx13_01_1_5 <- regarima_x13(ts_train01_1_5, spec ="RG5c")
summary(myregx13_01_1_5)
s_transform(myregx13_01_1_5)
#plot(myregx13_01_1_5)
myregx13_01_1_5$forecast
forex13_01_1_5 <- matrix(myregx13_01_1_5$forecast[1:12])
forex13_01_1_5

plot(ts_train01_1_5,
     xlim = c(time(ts_train01_1_5)[1], tail(time(ts_train01_1_5), 1) + 1),
     ylim = range(ts_train01_1_5, forex13_01_1_5),
     main = "Prévisions issues de X13-ARIMA-SEATS",
     ylab = "Valeurs", xlab = "Temps")
lines(ts(forex13_01_1_5,
         start = c(2023, 1),  # à adapter si nécessaire
         frequency = frequency(ts_train01_1_5)),
      col = "blue", lwd = 2, lty = 2)
legend("topleft", legend = c("Série observée", "Prévision X13"),
       col = c("black", "blue"), lty = c(1,2), lwd = 2)


#### stl-

decomp = stlm(ts_train01_1_5, s.window="periodic")
show(decomp)
#

fitstl_01_1_5 = stlm(ts_train01_1_5)
prevstl_01_1_5 <- forecast(fitstl_01_1_5,12) #période d'une année

plot(prevstl_01_1_5)
summary(prevstl_01_1_5)


### .2.2 HoltWinters--

WH_add_01_1_5 <- HoltWinters(ts_train01_1_5, gamma = FALSE)
WH_add_01_1_5
show(WH_add_01_1_5)
WH_add_01_1_5$coefficients
plot(WH_add_01_1_5)
forecast_hw01 <- forecast(WH_add_01_1_5, h = 12)
plot(forecast_hw01)
forecast_hw01

# Nombre d'observations
res <- residuals(WH_add_01_1_5)
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


#### ADAM ETS-

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

#### ADAM ETS + SARIMA-

fitadam3_01_1_5 <- auto.adam(ts_train01_1_5,
                             model = "ZZZ",
                             lags = c(1, 12),
                             orders = list(ar = c(3,3), i = 1, ma = c(3,3)),
                             select = TRUE,
                             bootstrap = TRUE)
fitadam3_01_1_5

summary(fitadam3_01_1_5)

par(mfcol=c(2,2))
plot(fitadam3_01_1_5)

par(mfcol=c(1,1))
plot(fitadam3_01_1_5$residuals)

prev_AES_01_1_5 <- forecast(fitadam3_01_1_5,12)
show(prev_AES_01_1_5)
plot(prev_AES_01_1_5)


### .2.5. SSARIMA-

fit_SSARIMA_01_1_5 <- auto.ssarima(ts_train01_1_5, lags=c(1,12), orders=list(ar=c(3,3), i=(2), ma=c(3,3), select=TRUE))
summary(fit_SSARIMA_01_1_5)

par(mfcol=c(2,2))
plot(fit_SSARIMA_01_1_5)

par(mfcol=c(1,1))

plot(fit_SSARIMA_01_1_5$residuals)

prev_SSARIMA_01_1_5 <- forecast(fit_SSARIMA_01_1_5, h=12)
prev_SSARIMA_01_1_5
plot(prev_SSARIMA_01_1_5)

aic_ssarima <- AIC(fit_SSARIMA_01_1_5)
cat("AIC du modèle SSARIMA :", round(aic_ssarima, 2), "\n")

aicc_ssarima <- AICc(fit_SSARIMA_01_1_5)
cat("AICc du modèle SSARIMA :", round(aicc_ssarima, 2), "\n")


#### naïves-

pred_naive_01_1_5 <- naive(ts_train01_1_5, h=12)
show(pred_naive_01_1_5)
plot(pred_naive_01_1_5)




#### 3. évolution des prévisions----

ts_2023_01_1_5 <- window(tsIPC01_1_5_CVS_RJDmetra_corr_diff, start = c(2023, 1), end = c(2023, 12))

# Fonction mise à jour : transforme la date en format mois
extract_forecast_df <- function(fcast_obj, name) {
 df <- data.frame(
  date = as.Date(as.yearmon(time(fcast_obj$mean))),  # conversion en date
  value = as.numeric(fcast_obj$mean),
  model = name
 )
 df %>% slice_head(n = 12)
}

ts_forex13_01_1_5 <- ts(as.numeric(forex13_01_1_5), start = c(2023, 1), frequency = 12)

# 2. Construire un objet forecast manuellement (sans recalculer)
forecast_x13_01_1_5 <- structure(list(
 mean = ts_forex13_01_1_5,  # ici, un vrai ts avec start/end
 lower = matrix(NA, nrow = 12, ncol = 2, dimnames = list(NULL, c("80%", "95%"))),
 upper = matrix(NA, nrow = 12, ncol = 2, dimnames = list(NULL, c("80%", "95%"))),
 level = c(80, 95),
 x = NULL,
 method = "X13",
 fitted = NULL,
 residuals = NULL
), class = "forecast")

dfs_01_1_5 <- list(
 extract_forecast_df(prev_arima_01_1_5, "ARMA"),
 extract_forecast_df(prev_ar1_01_1_5, "AR(1)"),
 extract_forecast_df(prev_arP_01_1_5, "AR(P)"),
 extract_forecast_df(forecast_x13_01_1_5, "X13"),
 extract_forecast_df(forecast_hw01, "HoltWinters"),
 extract_forecast_df(prev_ADAM_ETS_01_1_5, "ADAM_ETS"),
 extract_forecast_df(prev_AES_01_1_5, "ADAM_ETS+SARIMA"),
 extract_forecast_df(prev_SSARIMA_01_1_5, "SSARIMA"),
 extract_forecast_df(pred_naive_01_1_5, "Naïve")
)

#Je vais comparer aveclesvaleur reel grace a ts_test01_1_5
observed_df_01_1_5 <- data.frame(
 date = as.Date(as.yearmon(time(ts_test01_1_5))),
 value = as.numeric(ts_test01_1_5),
 model = "Observée")

# Combine toutes les prévisions et l'observée
df_all_01_1_5 <- bind_rows(dfs_01_1_5, observed_df_01_1_5)
df_all_01_1_5$date <- as.Date(df_all_01_1_5$date, format = "%Y-%m-%d")
library(ggplot2)

# 1. Graphe simple avec axe en numérique
ggplot(df_all_01_1_5, aes(x = as.numeric(date), y = value, color = model)) +
 geom_line(size = 1.2) +
 scale_x_continuous(
  breaks = as.numeric(df_all_01_1_5$date[1:12]),
  labels = format(df_all_01_1_5$date[1:12], "%b %Y")
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
ggplot(df_all_01_1_5, aes(x = date, y = value, color = model, linetype = model)) +
 geom_line(size = 1.2) +
 labs(
  title = "Prévisions - 01.1.5 : Huiles et graisses - Sur 12 mois (2023) par modèle",
  x = "Mois", y = "Valeur prévue", color = "Modèle", linetype = "Modèle"
 ) +
 scale_color_manual(values = c(
  "ADAM_ETS" = "red",
  "ADAM_ETS+SARIMA" = "orange",
  "HoltWinters" = "yellow",
  "Naïve" = "black",
  "SSARIMA" = "purple",
  "X13" = "darkblue",
  "ARMA" = "forestgreen",
  "AR(1)" = "brown",
  "AR(P)" = "darkcyan",
  "Observée" = "black"
 )) +
 scale_linetype_manual(values = c(
  "ADAM_ETS" = "solid",
  "ADAM_ETS+SARIMA" = "solid",
  "HoltWinters" = "solid",
  "Naïve" = "solid",
  "SSARIMA" = "solid",
  "X13" = "dashed",
  "ARMA" = "dotted",
  "AR(1)" = "dotted",
  "AR(P)" = "dotted",
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

#### 4 qualité de prevision -----

## MSE & R²OOS
## MSE & R²OOS avec le modèle Naïf comme référence

# Observations réelles
actual_values01_1_5 <- ts_test01_1_5

# Liste des prévisions pour chaque modèle
forecasts_01_1_5 <- list(
 prev_arima_01_1_5$mean,
 prev_ar1_01_1_5$mean,
 prev_arP_01_1_5$mean,
 forecast_x13_01_1_5$mean,
 forecast_hw01$mean,
 prev_ADAM_ETS_01_1_5$mean,
 prev_AES_01_1_5$mean,
 prev_SSARIMA_01_1_5$mean,
 pred_naive_01_1_5$mean
)


# Fonction MSE + R²OOS avec benchmark explicite
calculate_metrics_01_1_5 <- function(actual, forecast, benchmark_forecast) {
 mse <- mean((actual - forecast)^2)
 sst <- sum((actual - benchmark_forecast)^2)
 sse <- sum((actual - forecast)^2)
 r2oos <- 1 - (sse / sst)
 return(list(mse = mse, r2oos = r2oos))
}

# Naïve utilisé comme benchmark
benchmark_forecast <- as.numeric(pred_naive_01_1_5$mean)

# Calcul des métriques
metrics <- lapply(forecasts_01_1_5, function(fcast) {
 calculate_metrics_01_1_5(as.numeric(actual_values01_1_5), as.numeric(fcast), benchmark_forecast)
})

# Data frame de résultats
metrics_df_01_1_5 <- as.data.frame(do.call(rbind, metrics))
rownames(metrics_df_01_1_5) <- c("ARMA", "AR(1)", "AR(P)","X13",  "HoltWinters","ADAM_ETS", "ADAM_ETS+SARIMA", "SSARIMA", 
                                 "Naïve")

print(metrics_df_01_1_5)

## CSPE

# Fonction CSPE
calculate_cspe <- function(actual, forecast) {
 errors <- (actual - forecast)^2
 cspe <- cumsum(errors)
 return(cspe)
}

# CSPE pour chaque modèle
cspe_list <- lapply(forecasts_01_1_5, function(fcast) {
 calculate_cspe(as.numeric(actual_values01_1_5), as.numeric(fcast))
})

# Création du data.frame CSPE
cspe_df <- do.call(cbind, cspe_list)
colnames(cspe_df) <- rownames(metrics_df_01_1_5)
cspe_df <- cbind(Date = as.Date(time(actual_values01_1_5)), cspe_df)
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
  "ARMA" = "forestgreen", "AR(1)" = "brown", "AR(P)" = "darkcyan","X13" = "darkblue",
  "HoltWinters" = "yellow", "ADAM_ETS" = "red", "ADAM_ETS+SARIMA" = "orange",
  "SSARIMA" = "purple", "Naïve" = "black"
 )) +
 scale_linetype_manual(values = c(
  "ARMA" = "dashed", "AR(1)" = "dotted", "AR(P)" = "solid","X13" = "dashed",
  "HoltWinters" = "solid", "ADAM_ETS" = "solid", "ADAM_ETS+SARIMA" = "solid",
  "SSARIMA" = "solid", "Naïve" = "dotdash"
 )) +
 
 
 scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
 theme_minimal(base_size = 13) +
 theme(axis.text.x = element_text(angle = 45, hjust = 1),
       legend.position = "bottom",
       legend.title = element_text(face = "bold"),
       plot.title = element_text(face = "bold"))



##### 5 Test de précision-----

# Définir la période de temps
start_date <- c(2023, 1)
end_date <- c(2023, 12)

# Fonction pour ajuster les séries temporelles
adjust_time_series <- function(ts_data) {
 return(window(ts_data, start = start_date, end = end_date))
}

# Ajuster les séries temporelles pour chaque modèle
for_observed <- adjust_time_series(ts_2023_01_1_5)
for_ARMA     <- adjust_time_series(prev_arima_01_1_5$mean)
for_AR1      <- adjust_time_series(prev_ar1_01_1_5$mean)
for_ARP      <- adjust_time_series(prev_arP_01_1_5$mean)
for_X13      <- adjust_time_series(forecast_x13_01_1_5$mean)
for_HW       <- adjust_time_series(forecast_hw01$mean)
for_ADAM_ETS     <- adjust_time_series(prev_ADAM_ETS_01_1_5$mean)
for_AES      <- adjust_time_series(prev_AES_01_1_5$mean)
for_SSARIMA  <- adjust_time_series(prev_SSARIMA_01_1_5$mean)
for_NAIVE    <- adjust_time_series(pred_naive_01_1_5$mean)



# Vérifiez que toutes les séries temporelles ont la bonne longueur

ts_2023_01_1_5 <- window(tsIPC01_1_5_CVS_RJDmetra_corr_diff, start = c(2023, 1), end = c(2023, 12))
ts_test01_1_5 <- window(tsIPC01_1_5_CVS_RJDmetra_corr_diff, start = c(2023, 1), end = c(2023, 12))
print(length(ts_2023_01_1_5))

print(length(for_observed))
print(length(for_ARMA))
print(length(for_AR1))
print(length(for_ARP))
print(length(for_X13))
print(length(for_HW))
print(length(for_ADAM_ETS))
print(length(for_AES))
print(length(for_SSARIMA))
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

# Calculer le test DM

dm.test(error_NAIVE, error_ARMA, h = length(for_observed))
dm.test(error_NAIVE, error_AR1, h = length(for_observed))
dm.test(error_NAIVE, error_ARP, h = length(for_observed))
dm.test(error_NAIVE, error_X13, h = length(for_observed))
dm.test(error_NAIVE, error_HW, h = length(for_observed))
dm.test(error_NAIVE, error_ADAM_ETS, h = length(for_observed))
dm.test(error_NAIVE, error_AES, h = length(for_observed))
dm.test(error_NAIVE, error_SSARIMA, h = length(for_observed))


# Test de Diebold-Mariano avec h = 1
dm.test(error_NAIVE, error_ARMA, h = 1)
dm.test(error_NAIVE, error_AR1, h = 1)
dm.test(error_NAIVE, error_ARP, h = 1)
dm.test(error_NAIVE, error_X13, h = 1)
dm.test(error_NAIVE, error_HW, h = 1)
dm.test(error_NAIVE, error_ADAM_ETS, h = 1)
dm.test(error_NAIVE, error_AES, h = 1)
dm.test(error_NAIVE, error_SSARIMA, h = 1)


## le meilleur modèle est ADAM_ETS cool




#-------------- 01.1.6 - Fruits ---------

ts_Fruits <- ts(dfcomp$Fruits, start = c(2010, 1), frequency = 12)

autoplot(ts_Fruits) +
 ggtitle("IPC - Coicop :  01.1.6 - Fruits") +
 xlab("Date") + ylab("IPC - 01.1.6")+
 theme_minimal()+
 geom_line(color = "blue")


### - tests (CVS stationnaire)

combined_test(ts_Fruits)
seasdum(ts_Fruits) #Seasonal Dummies

adf.test(ts_Fruits)
kpss.test(ts_Fruits)

xss <-diff(ts_Fruits)
adf.test(xss)
kpss.test(xss) # La serie est stationnaire ap diff :)

myregx13_01_1_6 <- regarima_x13(ts_Fruits, spec ="RG5c")
s_transform(myregx13_01_1_6)  # test log/level
# Pas de transformation en log.

### - CVS-

# Avec RJDmetra
s_transform(myregx13_01_1_6)
myspec <- x13_spec("RSA5c")
mysax13 <- x13(ts_Fruits, myspec) # SCV de RJGDmetra
#plot(mysax13$final) # JTM

# Extraire la série désaisonnalisée (CVS)
tsIPC01_1_6_CVS_RJDmetra <- ts(mysax13$final$series[, "sa"],
                               start = start(ts_Fruits), frequency = frequency(ts_Fruits))

dfcomp_01_1_6 <- data.frame(
 Date = as.Date(as.yearmon(time(ts_Fruits)), frac = 1),
 Originale = as.numeric(ts_Fruits),
 CVS_RJD = as.numeric(tsIPC01_1_6_CVS_RJDmetra)
)

### Outliers-

outliers01_1_6 <- tso(tsIPC01_1_6_CVS_RJDmetra)
print(outliers)

plot(outliers01_1_6)
show(outliers01_1_6)


tsIPC01_1_6_CVS_RJDmetra_corr <- outliers01_1_6$yadj
dfcomp_01_1_6$CVS_RJD_CORR <- as.numeric(tsIPC01_1_6_CVS_RJDmetra_corr)

### Virification de la stationnarité-

combined_test(tsIPC01_1_6_CVS_RJDmetra_corr)
seasdum(tsIPC01_1_6_CVS_RJDmetra_corr) #Seasonal Dummies
# Toujours CVS logique

adf.test(tsIPC01_1_6_CVS_RJDmetra_corr)
kpss.test(tsIPC01_1_6_CVS_RJDmetra_corr)

tsIPC01_1_6_CVS_RJDmetra_corr_diff <- diff(tsIPC01_1_6_CVS_RJDmetra_corr)
adf.test(tsIPC01_1_6_CVS_RJDmetra_corr_diff)
kpss.test(tsIPC01_1_6_CVS_RJDmetra_corr_diff)


autoplot(tsIPC01_1_6_CVS_RJDmetra_corr_diff) +
 ggtitle("IPC - Coicop : 01.1.6 - serie différenciée") +
 xlab("Date") + ylab("IPC 01.1.6")+
 theme_minimal()+
 geom_line(color = "blue")


dfcomp_01_1_6 <- dfcomp_01_1_6[-1, ]  # on retire la première ligne
dfcomp_01_1_6$CVS_RJD_CORR_DIFF <- as.numeric(tsIPC01_1_6_CVS_RJDmetra_corr_diff)

#### 2. Estimation des modèles-----

# a.  Estimer et commenter les paramètres des modèles AR(1), AR(p) et ARIMA(p,d,q) et de la méthode LED Holt-Winters, ADAM ETS, ADAM ETS ARIMA, SSARIMA et CES
# b.  Paramètres : présenter sous forme de tableau les paramètres des modèles précédents. Déterminer et commenter le meilleur modèle d’après les critères AIC et AICc

# On entraine nos modele jusqu'a 2022 pour faire la prevision en 2023.

ts_train01_1_6 <- window(tsIPC01_1_6_CVS_RJDmetra_corr_diff, start = c(2010, 2), end = c(2022, 12))
ts_test01_1_6 <- window(tsIPC01_1_6_CVS_RJDmetra_corr_diff, start = c(2023, 1), end = c(2023, 12))

### 2. Estimation du modèle


###. 2.1 AR/ARMA-

### 2.1.1 ARMA-

modele_arima_01_1_6 <- auto.arima(ts_train01_1_6, stepwise = FALSE, approximation = FALSE)

summary(modele_arima_01_1_6)

j <- ncol(modele_arima_01_1_6$var.coef)
tstat <- matrix(nrow=j, ncol=1)
for(i in 1:j)
{
 tstat[i,1] <- modele_arima_01_1_6$coef[i]/sqrt(modele_arima_01_1_6$var.coef[i,i])
}
tstat

# Graphique des valeurs ajustées
plot(modele_arima_01_1_6$fitted)


prev_arima_01_1_6 <- forecast(modele_arima_01_1_6, h=12)
prev_arima_01_1_6
plot(prev_arima_01_1_6)

aic_arma <- AIC(modele_arima_01_1_6)
cat("AIC du modèle SSARIMA :", round(aic_arma, 2), "\n")

aicc_arma <- AICc(modele_arima_01_1_6)
cat("AICc du modèle SSARIMA :", round(aicc_arma, 2), "\n")

## 2.1.2 AR(1)-

modele_ar1_01_1_6 <- Arima(ts_train01_1_6, order = c(1, 0, 0), include.mean = TRUE)

summary(modele_ar1_01_1_6)

j <- ncol(modele_ar1_01_1_6$var.coef)
tstat <- matrix(nrow=j, ncol=1)
for(i in 1:j)
{
 tstat[i,1] <- modele_ar1_01_1_6$coef[i]/sqrt(modele_ar1_01_1_6$var.coef[i,i])
}
tstat

fitted_ar1 <- ts_train01_1_6 - residuals(modele_ar1_01_1_6)
plot(fitted_ar1, type = "l", col = "blue", main = "Valeurs ajustées - AR(1)", ylab = "Prévisions")

prev_ar1_01_1_6 <- forecast(modele_ar1_01_1_6, h = 12)
prev_ar1_01_1_6
plot(prev_ar1_01_1_6)

aic_ar1 <- AIC(modele_ar1_01_1_6)
cat("AIC du modèle AR(1) :", round(aic_ar1, 2), "\n")

aicc_ar1 <- AICc(modele_ar1_01_1_6)
cat("AICc du modèle AR(1) :", round(aicc_ar1, 2), "\n")

### 2.1.3 AR(P)-
modele_arp_01_1_6 <- auto.arima(ts_train01_1_6,
                                d = 0,
                                max.p = 10,        # tu peux ajuster ce maximum
                                max.q = 0,         # pas de MA
                                stationary = TRUE,
                                seasonal = FALSE,
                                stepwise = FALSE,
                                approximation = FALSE)


# Résumé du modèle
summary(modele_arp_01_1_6)

# t-stat des coefficients
# j <- length(modele_arp_01_1_6$coef)
# tstat_arp <- numeric(j)
# for(i in 1:j) {
#  tstat_arp[i] <- modele_arp_01_1_6$coef[i] / sqrt(modele_arp_01_1_6$var.coef[i, i])
# }
# tstat_arp

fitted_arP <- ts_train01_1_6 - residuals(modele_arp_01_1_6)
plot(fitted_arP, type = "l", col = "blue", main = "Valeurs ajustées - AR(1)", ylab = "Prévisions")

prev_arP_01_1_6 <- forecast(modele_arp_01_1_6, h = 12)
prev_arP_01_1_6
plot(prev_arP_01_1_6)

aic_arp <- AIC(modele_arp_01_1_6)
cat("AIC du modèle SSARIMA :", round(aic_arp, 2), "\n")


aicc_arp <- AICc(modele_arp_01_1_6)
cat("AICc du modèle SSARIMA :", round(aicc_arp, 2), "\n")

#### X13-

myregx13_01_1_6 <- regarima_x13(ts_train01_1_6, spec ="RG5c")
summary(myregx13_01_1_6)
s_transform(myregx13_01_1_6)
#plot(myregx13_01_1_6)
myregx13_01_1_6$forecast
forex13_01_1_6 <- matrix(myregx13_01_1_6$forecast[1:12])
forex13_01_1_6

plot(ts_train01_1_6,
     xlim = c(time(ts_train01_1_6)[1], tail(time(ts_train01_1_6), 1) + 1),
     ylim = range(ts_train01_1_6, forex13_01_1_6),
     main = "Prévisions issues de X13-ARIMA-SEATS",
     ylab = "Valeurs", xlab = "Temps")
lines(ts(forex13_01_1_6,
         start = c(2023, 1),  # à adapter si nécessaire
         frequency = frequency(ts_train01_1_6)),
      col = "blue", lwd = 2, lty = 2)
legend("topleft", legend = c("Série observée", "Prévision X13"),
       col = c("black", "blue"), lty = c(1,2), lwd = 2)


#### stl-

decomp = stlm(ts_train01_1_6, s.window="periodic")
show(decomp)
#

fitstl_01_1_6 = stlm(ts_train01_1_6)
prevstl_01_1_6 <- forecast(fitstl_01_1_6,12) #période d'une année

plot(prevstl_01_1_6)
summary(prevstl_01_1_6)


### .2.2 HoltWinters--

WH_add_01_1_6 <- HoltWinters(ts_train01_1_6, gamma = FALSE)
WH_add_01_1_6
show(WH_add_01_1_6)
WH_add_01_1_6$coefficients
plot(WH_add_01_1_6)
forecast_hw01 <- forecast(WH_add_01_1_6, h = 12)
plot(forecast_hw01)
forecast_hw01

# Nombre d'observations
res <- residuals(WH_add_01_1_6)
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


#### ADAM ETS-

fit_ADAM_ETS_01_1_6 <- auto.adam(ts_train01_1_6,
                                 model = "ZZZ",
                                 lags = c(1, 12),
                                 silent = TRUE)          
# ZZZ car ne spécifie rien (tendance, saisonnalité, erreur)
fit_ADAM_ETS_01_1_6
summary(fit_ADAM_ETS_01_1_6)

par(mfcol=c(2,2))
plot(fit_ADAM_ETS_01_1_6)
par(mfcol=c(1,1))


plot(fit_ADAM_ETS_01_1_6$states)
plot(fit_ADAM_ETS_01_1_6$residuals)

prev_ADAM_ETS_01_1_6 <- forecast(fit_ADAM_ETS_01_1_6, h=12)
show(prev_ADAM_ETS_01_1_6)
plot(prev_ADAM_ETS_01_1_6)

#### ADAM ETS + SARIMA-

fitadam3_01_1_6 <- auto.adam(ts_train01_1_6,
                             model = "ZZZ",
                             lags = c(1, 12),
                             orders = list(ar = c(3,3), i = 1, ma = c(3,3)),
                             select = TRUE,
                             bootstrap = TRUE)
fitadam3_01_1_6

summary(fitadam3_01_1_6)

par(mfcol=c(2,2))
plot(fitadam3_01_1_6)

par(mfcol=c(1,1))
plot(fitadam3_01_1_6$residuals)

prev_AES_01_1_6 <- forecast(fitadam3_01_1_6,12)
show(prev_AES_01_1_6)
plot(prev_AES_01_1_6)


### .2.5. SSARIMA-

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


#### naïves-

pred_naive_01_1_6 <- naive(ts_train01_1_6, h=12)
show(pred_naive_01_1_6)
plot(pred_naive_01_1_6)




#### 3. évolution des prévisions----

ts_2023_01_1_6 <- window(tsIPC01_1_6_CVS_RJDmetra_corr_diff, start = c(2023, 1), end = c(2023, 12))

# Fonction mise à jour : transforme la date en format mois
extract_forecast_df <- function(fcast_obj, name) {
 df <- data.frame(
  date = as.Date(as.yearmon(time(fcast_obj$mean))),  # conversion en date
  value = as.numeric(fcast_obj$mean),
  model = name
 )
 df %>% slice_head(n = 12)
}

ts_forex13_01_1_6 <- ts(as.numeric(forex13_01_1_6), start = c(2023, 1), frequency = 12)

# 2. Construire un objet forecast manuellement (sans recalculer)
forecast_x13_01_1_6 <- structure(list(
 mean = ts_forex13_01_1_6,  # ici, un vrai ts avec start/end
 lower = matrix(NA, nrow = 12, ncol = 2, dimnames = list(NULL, c("80%", "95%"))),
 upper = matrix(NA, nrow = 12, ncol = 2, dimnames = list(NULL, c("80%", "95%"))),
 level = c(80, 95),
 x = NULL,
 method = "X13",
 fitted = NULL,
 residuals = NULL
), class = "forecast")

dfs_01_1_6 <- list(
 extract_forecast_df(prev_arima_01_1_6, "ARMA"),
 extract_forecast_df(prev_ar1_01_1_6, "AR(1)"),
 extract_forecast_df(prev_arP_01_1_6, "AR(P)"),
 extract_forecast_df(forecast_x13_01_1_6, "X13"),
 extract_forecast_df(forecast_hw01, "HoltWinters"),
 extract_forecast_df(prev_ADAM_ETS_01_1_6, "ADAM_ETS"),
 extract_forecast_df(prev_AES_01_1_6, "ADAM_ETS+SARIMA"),
 extract_forecast_df(prev_SSARIMA_01_1_6, "SSARIMA"),
 extract_forecast_df(pred_naive_01_1_6, "Naïve")
)

#Je vais comparer aveclesvaleur reel grace a ts_test01_1_6
observed_df_01_1_6 <- data.frame(
 date = as.Date(as.yearmon(time(ts_test01_1_6))),
 value = as.numeric(ts_test01_1_6),
 model = "Observée")

# Combine toutes les prévisions et l'observée
df_all_01_1_6 <- bind_rows(dfs_01_1_6, observed_df_01_1_6)
df_all_01_1_6$date <- as.Date(df_all_01_1_6$date, format = "%Y-%m-%d")
library(ggplot2)

# 1. Graphe simple avec axe en numérique
ggplot(df_all_01_1_6, aes(x = as.numeric(date), y = value, color = model)) +
 geom_line(size = 1.2) +
 scale_x_continuous(
  breaks = as.numeric(df_all_01_1_6$date[1:12]),
  labels = format(df_all_01_1_6$date[1:12], "%b %Y")
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
ggplot(df_all_01_1_6, aes(x = date, y = value, color = model, linetype = model)) +
 geom_line(size = 1.2) +
 labs(
  title = "Prévisions - 01.1.6 : Fruits - Sur 12 mois (2023) par modèle",
  x = "Mois", y = "Valeur prévue", color = "Modèle", linetype = "Modèle"
 ) +
 scale_color_manual(values = c(
  "ADAM_ETS" = "red",
  "ADAM_ETS+SARIMA" = "orange",
  "HoltWinters" = "yellow",
  "Naïve" = "black",
  "SSARIMA" = "purple",
  "X13" = "darkblue",
  "ARMA" = "forestgreen",
  "AR(1)" = "brown",
  "AR(P)" = "darkcyan",
  "Observée" = "black"
 )) +
 scale_linetype_manual(values = c(
  "ADAM_ETS" = "solid",
  "ADAM_ETS+SARIMA" = "solid",
  "HoltWinters" = "solid",
  "Naïve" = "solid",
  "SSARIMA" = "solid",
  "X13" = "dashed",
  "ARMA" = "dotted",
  "AR(1)" = "dotted",
  "AR(P)" = "dotted",
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

#### 4 qualité de prevision -----

## MSE & R²OOS
## MSE & R²OOS avec le modèle Naïf comme référence

# Observations réelles
actual_values01_1_6 <- ts_test01_1_6

# Liste des prévisions pour chaque modèle
forecasts_01_1_6 <- list(
 prev_arima_01_1_6$mean,
 prev_ar1_01_1_6$mean,
 prev_arP_01_1_6$mean,
 forecast_x13_01_1_6$mean,
 forecast_hw01$mean,
 prev_ADAM_ETS_01_1_6$mean,
 prev_AES_01_1_6$mean,
 prev_SSARIMA_01_1_6$mean,
 pred_naive_01_1_6$mean
)


# Fonction MSE + R²OOS avec benchmark explicite
calculate_metrics_01_1_6 <- function(actual, forecast, benchmark_forecast) {
 mse <- mean((actual - forecast)^2)
 sst <- sum((actual - benchmark_forecast)^2)
 sse <- sum((actual - forecast)^2)
 r2oos <- 1 - (sse / sst)
 return(list(mse = mse, r2oos = r2oos))
}

# Naïve utilisé comme benchmark
benchmark_forecast <- as.numeric(pred_naive_01_1_6$mean)

# Calcul des métriques
metrics <- lapply(forecasts_01_1_6, function(fcast) {
 calculate_metrics_01_1_6(as.numeric(actual_values01_1_6), as.numeric(fcast), benchmark_forecast)
})

# Data frame de résultats
metrics_df_01_1_6 <- as.data.frame(do.call(rbind, metrics))
rownames(metrics_df_01_1_6) <- c("ARMA", "AR(1)", "AR(P)","X13",  "HoltWinters","ADAM_ETS", "ADAM_ETS+SARIMA", "SSARIMA", 
                                 "Naïve")

print(metrics_df_01_1_6)

## CSPE

# Fonction CSPE
calculate_cspe <- function(actual, forecast) {
 errors <- (actual - forecast)^2
 cspe <- cumsum(errors)
 return(cspe)
}

# CSPE pour chaque modèle
cspe_list <- lapply(forecasts_01_1_6, function(fcast) {
 calculate_cspe(as.numeric(actual_values01_1_6), as.numeric(fcast))
})

# Création du data.frame CSPE
cspe_df <- do.call(cbind, cspe_list)
colnames(cspe_df) <- rownames(metrics_df_01_1_6)
cspe_df <- cbind(Date = as.Date(time(actual_values01_1_6)), cspe_df)
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
  "ARMA" = "forestgreen", "AR(1)" = "brown", "AR(P)" = "darkcyan","X13" = "darkblue",
  "HoltWinters" = "yellow", "ADAM_ETS" = "red", "ADAM_ETS+SARIMA" = "orange",
  "SSARIMA" = "purple", "Naïve" = "black"
 )) +
 scale_linetype_manual(values = c(
  "ARMA" = "dashed", "AR(1)" = "dotted", "AR(P)" = "solid","X13" = "dashed",
  "HoltWinters" = "solid", "ADAM_ETS" = "solid", "ADAM_ETS+SARIMA" = "solid",
  "SSARIMA" = "solid", "Naïve" = "dotdash"
 )) +
 
 
 scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
 theme_minimal(base_size = 13) +
 theme(axis.text.x = element_text(angle = 45, hjust = 1),
       legend.position = "bottom",
       legend.title = element_text(face = "bold"),
       plot.title = element_text(face = "bold"))



##### 5 Test de précision-----

# Définir la période de temps
start_date <- c(2023, 1)
end_date <- c(2023, 12)

# Fonction pour ajuster les séries temporelles
adjust_time_series <- function(ts_data) {
 return(window(ts_data, start = start_date, end = end_date))
}

# Ajuster les séries temporelles pour chaque modèle
for_observed <- adjust_time_series(ts_2023_01_1_6)
for_ARMA     <- adjust_time_series(prev_arima_01_1_6$mean)
for_AR1      <- adjust_time_series(prev_ar1_01_1_6$mean)
for_ARP      <- adjust_time_series(prev_arP_01_1_6$mean)
for_X13      <- adjust_time_series(forecast_x13_01_1_6$mean)
for_HW       <- adjust_time_series(forecast_hw01$mean)
for_ADAM_ETS     <- adjust_time_series(prev_ADAM_ETS_01_1_6$mean)
for_AES      <- adjust_time_series(prev_AES_01_1_6$mean)
for_SSARIMA  <- adjust_time_series(prev_SSARIMA_01_1_6$mean)
for_NAIVE    <- adjust_time_series(pred_naive_01_1_6$mean)



# Vérifiez que toutes les séries temporelles ont la bonne longueur

ts_2023_01_1_6 <- window(tsIPC01_1_6_CVS_RJDmetra_corr_diff, start = c(2023, 1), end = c(2023, 12))
ts_test01_1_6 <- window(tsIPC01_1_6_CVS_RJDmetra_corr_diff, start = c(2023, 1), end = c(2023, 12))
print(length(ts_2023_01_1_6))

print(length(for_observed))
print(length(for_ARMA))
print(length(for_AR1))
print(length(for_ARP))
print(length(for_X13))
print(length(for_HW))
print(length(for_ADAM_ETS))
print(length(for_AES))
print(length(for_SSARIMA))
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

# Calculer le test DM

dm.test(error_NAIVE, error_ARMA, h = length(for_observed))
dm.test(error_NAIVE, error_AR1, h = length(for_observed))
dm.test(error_NAIVE, error_ARP, h = length(for_observed))
dm.test(error_NAIVE, error_X13, h = length(for_observed))
dm.test(error_NAIVE, error_HW, h = length(for_observed))
dm.test(error_NAIVE, error_ADAM_ETS, h = length(for_observed))
dm.test(error_NAIVE, error_AES, h = length(for_observed))
dm.test(error_NAIVE, error_SSARIMA, h = length(for_observed))


# Test de Diebold-Mariano avec h = 1
dm.test(error_NAIVE, error_ARMA, h = 1)
dm.test(error_NAIVE, error_AR1, h = 1)
dm.test(error_NAIVE, error_ARP, h = 1)
dm.test(error_NAIVE, error_X13, h = 1)
dm.test(error_NAIVE, error_HW, h = 1)
dm.test(error_NAIVE, error_ADAM_ETS, h = 1)
dm.test(error_NAIVE, error_AES, h = 1)
dm.test(error_NAIVE, error_SSARIMA, h = 1)


## le meilleur modèle est SSARIMA cool


#-------------- 01.1.7 - Légumes ---------

ts_Legumes <- ts(dfcomp$Legumes, start = c(2010, 1), frequency = 12)

autoplot(ts_Legumes) +
 ggtitle("IPC - Coicop :  01.1.7 - Légumes") +
 xlab("Date") + ylab("IPC - 01.1.7")+
 theme_minimal()+
 geom_line(color = "blue")


### - tests (CVS stationnaire)

combined_test(ts_Legumes)

seasdum(ts_Legumes) #Seasonal Dummies

adf.test(ts_Legumes)
kpss.test(ts_Legumes)

xss <-diff(ts_Legumes)
adf.test(xss)
kpss.test(xss) # La serie est stationnaire ap diff :)

myregx13_01_1_7 <- regarima_x13(ts_Legumes, spec ="RG5c")
s_transform(myregx13_01_1_7)  # test log/level
# Pas de transformation en log.

### - CVS-

# Avec RJDmetra
s_transform(myregx13_01_1_7)
myspec <- x13_spec("RSA5c")
mysax13 <- x13(ts_Legumes, myspec) # SCV de RJGDmetra
#plot(mysax13$final) # JTM

# Extraire la série désaisonnalisée (CVS)
tsIPC01_1_7_CVS_RJDmetra <- ts(mysax13$final$series[, "sa"],
                               start = start(ts_Legumes), frequency = frequency(ts_Legumes))

dfcomp_01_1_7 <- data.frame(
 Date = as.Date(as.yearmon(time(ts_Legumes)), frac = 1),
 Originale = as.numeric(ts_Legumes),
 CVS_RJD = as.numeric(tsIPC01_1_7_CVS_RJDmetra)
)

### Outliers------

outliers01_1_7 <- tso(tsIPC01_1_7_CVS_RJDmetra)
print(outliers01_1_7)

plot(outliers01_1_7)
show(outliers01_1_7)



tsIPC01_1_7_CVS_RJDmetra_corr <- outliers$yadj
dfcomp_01_1_7$CVS_RJD_CORR <- as.numeric(tsIPC01_1_7_CVS_RJDmetra_corr)

### Virification de la stationnarité-

combined_test(tsIPC01_1_7_CVS_RJDmetra_corr)
seasdum(tsIPC01_1_7_CVS_RJDmetra_corr) #Seasonal Dummies
# Toujours CVS logique

adf.test(tsIPC01_1_7_CVS_RJDmetra_corr)
kpss.test(tsIPC01_1_7_CVS_RJDmetra_corr)

tsIPC01_1_7_CVS_RJDmetra_corr_diff <- diff(tsIPC01_1_7_CVS_RJDmetra_corr)
adf.test(tsIPC01_1_7_CVS_RJDmetra_corr_diff)
kpss.test(tsIPC01_1_7_CVS_RJDmetra_corr_diff)


autoplot(tsIPC01_1_7_CVS_RJDmetra_corr_diff) +
 ggtitle("IPC - Coicop : 01.1.7 - serie différenciée") +
 xlab("Date") + ylab("IPC 01.1.7")+
 theme_minimal()+
 geom_line(color = "blue")


dfcomp_01_1_7 <- dfcomp_01_1_7[-1, ]  # on retire la première ligne
dfcomp_01_1_7$CVS_RJD_CORR_DIFF <- as.numeric(tsIPC01_1_7_CVS_RJDmetra_corr_diff)

#### 2. Estimation des modèles-----

# a.  Estimer et commenter les paramètres des modèles AR(1), AR(p) et ARIMA(p,d,q) et de la méthode LED Holt-Winters, ADAM ETS, ADAM ETS ARIMA, SSARIMA et CES
# b.  Paramètres : présenter sous forme de tableau les paramètres des modèles précédents. Déterminer et commenter le meilleur modèle d’après les critères AIC et AICc

# On entraine nos modele jusqu'a 2022 pour faire la prevision en 2023.

ts_train01_1_7 <- window(tsIPC01_1_7_CVS_RJDmetra_corr_diff, start = c(2010, 2), end = c(2022, 12))
ts_test01_1_7 <- window(tsIPC01_1_7_CVS_RJDmetra_corr_diff, start = c(2023, 1), end = c(2023, 12))

### 2. Estimation du modèle

###. 2.1 AR/ARMA-

### 2.1.1 ARMA-

modele_arima_01_1_7 <- auto.arima(ts_train01_1_7, stepwise = FALSE, approximation = FALSE)

summary(modele_arima_01_1_7)

j <- ncol(modele_arima_01_1_7$var.coef)
tstat <- matrix(nrow=j, ncol=1)
for(i in 1:j)
{
 tstat[i,1] <- modele_arima_01_1_7$coef[i]/sqrt(modele_arima_01_1_7$var.coef[i,i])
}
tstat

# Graphique des valeurs ajustées
plot(modele_arima_01_1_7$fitted)
prev_arima_01_1_7 <- forecast(modele_arima_01_1_7, h=12)
prev_arima_01_1_7
plot(prev_arima_01_1_7)

aic_arma <- AIC(modele_arima_01_1_7)
cat("AIC du modèle SSARIMA :", round(aic_arma, 2), "\n")

aicc_arma <- AICc(modele_arima_01_1_7)
cat("AICc du modèle SSARIMA :", round(aicc_arma, 2), "\n")

## 2.1.2 AR(1)-

modele_ar1_01_1_7 <- Arima(ts_train01_1_7, order = c(1, 0, 0), include.mean = TRUE)

summary(modele_ar1_01_1_7)

j <- ncol(modele_ar1_01_1_7$var.coef)
tstat <- matrix(nrow=j, ncol=1)
for(i in 1:j)
{
 tstat[i,1] <- modele_ar1_01_1_7$coef[i]/sqrt(modele_ar1_01_1_7$var.coef[i,i])
}
tstat

fitted_ar1 <- ts_train01_1_7 - residuals(modele_ar1_01_1_7)
plot(fitted_ar1, type = "l", col = "blue", main = "Valeurs ajustées - AR(1)", ylab = "Prévisions")

prev_ar1_01_1_7 <- forecast(modele_ar1_01_1_7, h = 12)
prev_ar1_01_1_7
plot(prev_ar1_01_1_7)

aic_ar1 <- AIC(modele_ar1_01_1_7)
cat("AIC du modèle AR(1) :", round(aic_ar1, 2), "\n")

aicc_ar1 <- AICc(modele_ar1_01_1_7)
cat("AICc du modèle AR(1) :", round(aicc_ar1, 2), "\n")

### 2.1.3 AR(P)-
modele_arp_01_1_7 <- auto.arima(ts_train01_1_7,
                                d = 0,
                                max.p = 10,        # tu peux ajuster ce maximum
                                max.q = 0,         # pas de MA
                                stationary = TRUE,
                                seasonal = FALSE,
                                stepwise = FALSE,
                                approximation = FALSE)


# Résumé du modèle
summary(modele_arp_01_1_7)

# t-stat des coefficients
j <- length(modele_arp_01_1_7$coef)
tstat_arp <- numeric(j)
for(i in 1:j) {
 tstat_arp[i] <- modele_arp_01_1_7$coef[i] / sqrt(modele_arp_01_1_7$var.coef[i, i])
}
tstat_arp

fitted_arP <- ts_train01_1_7 - residuals(modele_arp_01_1_7)
plot(fitted_arP, type = "l", col = "blue", main = "Valeurs ajustées - AR(1)", ylab = "Prévisions")

prev_arP_01_1_7 <- forecast(modele_arp_01_1_7, h = 12)
prev_arP_01_1_7
plot(prev_arP_01_1_7)

aic_arp <- AIC(modele_arp_01_1_7)
cat("AIC du modèle SSARIMA :", round(aic_arp, 2), "\n")


aicc_arp <- AICc(modele_arp_01_1_7)
cat("AICc du modèle SSARIMA :", round(aicc_arp, 2), "\n")

#### X13-

myregx13_01_1_7 <- regarima_x13(ts_train01_1_7, spec ="RG5c")
summary(myregx13_01_1_7)
s_transform(myregx13_01_1_7)
#plot(myregx13_01_1_7)
myregx13_01_1_7$forecast
forex13_01_1_7 <- matrix(myregx13_01_1_7$forecast[1:12])
forex13_01_1_7

plot(ts_train01_1_7,
     xlim = c(time(ts_train01_1_7)[1], tail(time(ts_train01_1_7), 1) + 1),
     ylim = range(ts_train01_1_7, forex13_01_1_7),
     main = "Prévisions issues de X13-ARIMA-SEATS",
     ylab = "Valeurs", xlab = "Temps")
lines(ts(forex13_01_1_7,
         start = c(2023, 1),  # à adapter si nécessaire
         frequency = frequency(ts_train01_1_7)),
      col = "blue", lwd = 2, lty = 2)
legend("topleft", legend = c("Série observée", "Prévision X13"),
       col = c("black", "blue"), lty = c(1,2), lwd = 2)


#### stl-

decomp = stlm(ts_train01_1_7, s.window="periodic")
show(decomp)
#

fitstl_01_1_7 = stlm(ts_train01_1_7)
prevstl_01_1_7 <- forecast(fitstl_01_1_7,12) #période d'une année

plot(prevstl_01_1_7)
summary(prevstl_01_1_7)


### .2.2 HoltWinters--

WH_add_01_1_7 <- HoltWinters(ts_train01_1_7, gamma = FALSE)
WH_add_01_1_7
show(WH_add_01_1_7)
WH_add_01_1_7$coefficients
plot(WH_add_01_1_7)
forecast_hw01 <- forecast(WH_add_01_1_7, h = 12)
plot(forecast_hw01)
forecast_hw01

# Nombre d'observations
res <- residuals(WH_add_01_1_7)
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


#### ADAM ETS-

fit_ADAM_ETS_01_1_7 <- auto.adam(ts_train01_1_7,
                                 model = "ZZZ",
                                 lags = c(1, 12),
                                 silent = TRUE)          
# ZZZ car ne spécifie rien (tendance, saisonnalité, erreur)
fit_ADAM_ETS_01_1_7
summary(fit_ADAM_ETS_01_1_7)
par(mfcol=c(2,2))
plot(fit_ADAM_ETS_01_1_7)
par(mfcol=c(1,1))


plot(fit_ADAM_ETS_01_1_7$residuals)

prev_ADAM_ETS_01_1_7 <- forecast(fit_ADAM_ETS_01_1_7, h=12)
show(prev_ADAM_ETS_01_1_7)
plot(prev_ADAM_ETS_01_1_7)

#### ADAM ETS + SARIMA-

fitadam3_01_1_7 <- auto.adam(ts_train01_1_7,
                             model = "ZZZ",
                             lags = c(1, 12),
                             orders = list(ar = c(3,3), i = 1, ma = c(3,3)),
                             select = TRUE,
                             bootstrap = TRUE)
fitadam3_01_1_7

summary(fitadam3_01_1_7)

par(mfcol=c(2,2))
plot(fitadam3_01_1_7)
par(mfcol=c(1,1))
plot(fitadam3_01_1_7$residuals)

prev_AES_01_1_7 <- forecast(fitadam3_01_1_7,12)
show(prev_AES_01_1_7)
plot(prev_AES_01_1_7)


### .2.5. SSARIMA-

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


#### naïves-

pred_naive_01_1_7 <- naive(ts_train01_1_7, h=12)
show(pred_naive_01_1_7)
plot(pred_naive_01_1_7)




#### 3. évolution des prévisions----

ts_2023_01_1_7 <- window(tsIPC01_1_7_CVS_RJDmetra_corr_diff, start = c(2023, 1), end = c(2023, 12))

# Fonction mise à jour : transforme la date en format mois
extract_forecast_df <- function(fcast_obj, name) {
 df <- data.frame(
  date = as.Date(as.yearmon(time(fcast_obj$mean))),  # conversion en date
  value = as.numeric(fcast_obj$mean),
  model = name
 )
 df %>% slice_head(n = 12)
}

ts_forex13_01_1_7 <- ts(as.numeric(forex13_01_1_7), start = c(2023, 1), frequency = 12)

# 2. Construire un objet forecast manuellement (sans recalculer)
forecast_x13_01_1_7 <- structure(list(
 mean = ts_forex13_01_1_7,  # ici, un vrai ts avec start/end
 lower = matrix(NA, nrow = 12, ncol = 2, dimnames = list(NULL, c("80%", "95%"))),
 upper = matrix(NA, nrow = 12, ncol = 2, dimnames = list(NULL, c("80%", "95%"))),
 level = c(80, 95),
 x = NULL,
 method = "X13",
 fitted = NULL,
 residuals = NULL
), class = "forecast")

dfs_01_1_7 <- list(
 extract_forecast_df(prev_arima_01_1_7, "ARMA"),
 extract_forecast_df(prev_ar1_01_1_7, "AR(1)"),
 extract_forecast_df(prev_arP_01_1_7, "AR(P)"),
 extract_forecast_df(forecast_x13_01_1_7, "X13"),
 extract_forecast_df(forecast_hw01, "HoltWinters"),
 extract_forecast_df(prev_ADAM_ETS_01_1_7, "ADAM_ETS"),
 extract_forecast_df(prev_AES_01_1_7, "ADAM_ETS+SARIMA"),
 extract_forecast_df(prev_SSARIMA_01_1_7, "SSARIMA"),
 extract_forecast_df(pred_naive_01_1_7, "Naïve")
)

#Je vais comparer aveclesvaleur reel grace a ts_test01_1_7
observed_df_01_1_7 <- data.frame(
 date = as.Date(as.yearmon(time(ts_test01_1_7))),
 value = as.numeric(ts_test01_1_7),
 model = "Observée")

# Combine toutes les prévisions et l'observée
df_all_01_1_7 <- bind_rows(dfs_01_1_7, observed_df_01_1_7)
df_all_01_1_7$date <- as.Date(df_all_01_1_7$date, format = "%Y-%m-%d")
library(ggplot2)

# 1. Graphe simple avec axe en numérique
ggplot(df_all_01_1_7, aes(x = as.numeric(date), y = value, color = model)) +
 geom_line(size = 1.2) +
 scale_x_continuous(
  breaks = as.numeric(df_all_01_1_7$date[1:12]),
  labels = format(df_all_01_1_7$date[1:12], "%b %Y")
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
ggplot(df_all_01_1_7, aes(x = date, y = value, color = model, linetype = model)) +
 geom_line(size = 1.2) +
 labs(
  title = "Prévisions - 01.1.7 : Légumes - Sur 12 mois (2023) par modèle",
  x = "Mois", y = "Valeur prévue", color = "Modèle", linetype = "Modèle"
 ) +
 scale_color_manual(values = c(
  "ADAM_ETS" = "red",
  "ADAM_ETS+SARIMA" = "orange",
  "HoltWinters" = "yellow",
  "Naïve" = "black",
  "SSARIMA" = "purple",
  "X13" = "darkblue",
  "ARMA" = "forestgreen",
  "AR(1)" = "brown",
  "AR(P)" = "darkcyan",
  "Observée" = "black"
 )) +
 scale_linetype_manual(values = c(
  "ADAM_ETS" = "solid",
  "ADAM_ETS+SARIMA" = "solid",
  "HoltWinters" = "solid",
  "Naïve" = "solid",
  "SSARIMA" = "solid",
  "X13" = "dashed",
  "ARMA" = "dotted",
  "AR(1)" = "dotted",
  "AR(P)" = "dotted",
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

#### 4 qualité de prevision -----

## MSE & R²OOS
## MSE & R²OOS avec le modèle Naïf comme référence

# Observations réelles
actual_values01_1_7 <- ts_test01_1_7

# Liste des prévisions pour chaque modèle
forecasts_01_1_7 <- list(
 prev_arima_01_1_7$mean,
 prev_ar1_01_1_7$mean,
 prev_arP_01_1_7$mean,
 forecast_x13_01_1_7$mean,
 forecast_hw01$mean,
 prev_ADAM_ETS_01_1_7$mean,
 prev_AES_01_1_7$mean,
 prev_SSARIMA_01_1_7$mean,
 pred_naive_01_1_7$mean
)


# Fonction MSE + R²OOS avec benchmark explicite
calculate_metrics_01_1_7 <- function(actual, forecast, benchmark_forecast) {
 mse <- mean((actual - forecast)^2)
 sst <- sum((actual - benchmark_forecast)^2)
 sse <- sum((actual - forecast)^2)
 r2oos <- 1 - (sse / sst)
 return(list(mse = mse, r2oos = r2oos))
}

# Naïve utilisé comme benchmark
benchmark_forecast <- as.numeric(pred_naive_01_1_7$mean)

# Calcul des métriques
metrics <- lapply(forecasts_01_1_7, function(fcast) {
 calculate_metrics_01_1_7(as.numeric(actual_values01_1_7), as.numeric(fcast), benchmark_forecast)
})

# Data frame de résultats
metrics_df_01_1_7 <- as.data.frame(do.call(rbind, metrics))
rownames(metrics_df_01_1_7) <- c("ARMA", "AR(1)", "AR(P)","X13",  "HoltWinters","ADAM_ETS", "ADAM_ETS+SARIMA", "SSARIMA", 
                                 "Naïve")

print(metrics_df_01_1_7)

## CSPE

# Fonction CSPE
calculate_cspe <- function(actual, forecast) {
 errors <- (actual - forecast)^2
 cspe <- cumsum(errors)
 return(cspe)
}

# CSPE pour chaque modèle
cspe_list <- lapply(forecasts_01_1_7, function(fcast) {
 calculate_cspe(as.numeric(actual_values01_1_7), as.numeric(fcast))
})

# Création du data.frame CSPE
cspe_df <- do.call(cbind, cspe_list)
colnames(cspe_df) <- rownames(metrics_df_01_1_7)
cspe_df <- cbind(Date = as.Date(time(actual_values01_1_7)), cspe_df)
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
  "ARMA" = "forestgreen", "AR(1)" = "brown", "AR(P)" = "darkcyan","X13" = "darkblue",
  "HoltWinters" = "yellow", "ADAM_ETS" = "red", "ADAM_ETS+SARIMA" = "orange",
  "SSARIMA" = "purple", "Naïve" = "black"
 )) +
 scale_linetype_manual(values = c(
  "ARMA" = "dashed", "AR(1)" = "dotted", "AR(P)" = "solid","X13" = "dashed",
  "HoltWinters" = "solid", "ADAM_ETS" = "solid", "ADAM_ETS+SARIMA" = "solid",
  "SSARIMA" = "solid", "Naïve" = "dotdash"
 )) +
 
 
 scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
 theme_minimal(base_size = 13) +
 theme(axis.text.x = element_text(angle = 45, hjust = 1),
       legend.position = "bottom",
       legend.title = element_text(face = "bold"),
       plot.title = element_text(face = "bold"))



##### 5 Test de précision-----

# Définir la période de temps
start_date <- c(2023, 1)
end_date <- c(2023, 12)

# Fonction pour ajuster les séries temporelles
adjust_time_series <- function(ts_data) {
 return(window(ts_data, start = start_date, end = end_date))
}

# Ajuster les séries temporelles pour chaque modèle
for_observed <- adjust_time_series(ts_2023_01_1_7)
for_ARMA     <- adjust_time_series(prev_arima_01_1_7$mean)
for_AR1      <- adjust_time_series(prev_ar1_01_1_7$mean)
for_ARP      <- adjust_time_series(prev_arP_01_1_7$mean)
for_X13      <- adjust_time_series(forecast_x13_01_1_7$mean)
for_HW       <- adjust_time_series(forecast_hw01$mean)
for_ADAM_ETS     <- adjust_time_series(prev_ADAM_ETS_01_1_7$mean)
for_AES      <- adjust_time_series(prev_AES_01_1_7$mean)
for_SSARIMA  <- adjust_time_series(prev_SSARIMA_01_1_7$mean)
for_NAIVE    <- adjust_time_series(pred_naive_01_1_7$mean)



# Vérifiez que toutes les séries temporelles ont la bonne longueur

ts_2023_01_1_7 <- window(tsIPC01_1_7_CVS_RJDmetra_corr_diff, start = c(2023, 1), end = c(2023, 12))
ts_test01_1_7 <- window(tsIPC01_1_7_CVS_RJDmetra_corr_diff, start = c(2023, 1), end = c(2023, 12))
print(length(ts_2023_01_1_7))

print(length(for_observed))
print(length(for_ARMA))
print(length(for_AR1))
print(length(for_ARP))
print(length(for_X13))
print(length(for_HW))
print(length(for_ADAM_ETS))
print(length(for_AES))
print(length(for_SSARIMA))
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

# Calculer le test DM

dm.test(error_NAIVE, error_ARMA, h = length(for_observed))
dm.test(error_NAIVE, error_AR1, h = length(for_observed))
dm.test(error_NAIVE, error_ARP, h = length(for_observed))
dm.test(error_NAIVE, error_X13, h = length(for_observed))
dm.test(error_NAIVE, error_HW, h = length(for_observed))
dm.test(error_NAIVE, error_ADAM_ETS, h = length(for_observed))
dm.test(error_NAIVE, error_AES, h = length(for_observed))
dm.test(error_NAIVE, error_SSARIMA, h = length(for_observed))


# Test de Diebold-Mariano avec h = 1
dm.test(error_NAIVE, error_ARMA, h = 1)
dm.test(error_NAIVE, error_AR1, h = 1)
dm.test(error_NAIVE, error_ARP, h = 1)
dm.test(error_NAIVE, error_X13, h = 1)
dm.test(error_NAIVE, error_HW, h = 1)
dm.test(error_NAIVE, error_ADAM_ETS, h = 1)
dm.test(error_NAIVE, error_AES, h = 1)
dm.test(error_NAIVE, error_SSARIMA, h = 1)


## le meilleur modèle est SSARIMA cool

#-------------- 01.1.8 - Sucre, confiture, miel, chocolat et confiserie ---------

ts_Sucre_confiture_confiserie <- ts(dfcomp$Sucre_confiture_confiserie, start = c(2010, 1), frequency = 12)

autoplot(ts_Sucre_confiture_confiserie) +
 ggtitle("IPC - Coicop :  01.1.8 - Sucre, confiture et confiserie") +
 xlab("Date") + ylab("IPC - 01.1.8")+
 theme_minimal()+
 geom_line(color = "blue")


### - tests (CVS stationnaire)

combined_test(ts_Sucre_confiture_confiserie)

seasdum(ts_Sucre_confiture_confiserie) #Seasonal Dummies

adf.test(ts_Sucre_confiture_confiserie)
kpss.test(ts_Sucre_confiture_confiserie)

xss <-diff(ts_Sucre_confiture_confiserie)
adf.test(xss)
kpss.test(xss) # La serie est stationnaire ap diff :)

myregx13_01_1_8 <- regarima_x13(ts_Sucre_confiture_confiserie, spec ="RG5c")
s_transform(myregx13_01_1_8)  # test log/level
# Pas de transformation en log.

### - CVS-

# Avec RJDmetra
s_transform(myregx13_01_1_8)
myspec <- x13_spec("RSA5c")
mysax13 <- x13(ts_Sucre_confiture_confiserie, myspec) # SCV de RJGDmetra
#plot(mysax13$final) # JTM

# Extraire la série désaisonnalisée (CVS)
tsIPC01_1_8_CVS_RJDmetra <- ts(mysax13$final$series[, "sa"],
                               start = start(ts_Sucre_confiture_confiserie), frequency = frequency(ts_Sucre_confiture_confiserie))

dfcomp_01_1_8 <- data.frame(
 Date = as.Date(as.yearmon(time(ts_Sucre_confiture_confiserie)), frac = 1),
 Originale = as.numeric(ts_Sucre_confiture_confiserie),
 CVS_RJD = as.numeric(tsIPC01_1_8_CVS_RJDmetra)
)

### Outliers-

outliers01_1_8 <- tso(tsIPC01_1_8_CVS_RJDmetra)
print(outliers01_1_8)

plot(outliers01_1_8)
show(outliers01_1_8)

tsIPC01_1_8_CVS_RJDmetra_corr <- outliers01_1_8$yadj
dfcomp_01_1_8$CVS_RJD_CORR <- as.numeric(tsIPC01_1_8_CVS_RJDmetra_corr)

### Virification de la stationnarité-

combined_test(tsIPC01_1_8_CVS_RJDmetra_corr)
seasdum(tsIPC01_1_8_CVS_RJDmetra_corr) #Seasonal Dummies
# Toujours CVS logique

adf.test(tsIPC01_1_8_CVS_RJDmetra_corr)
kpss.test(tsIPC01_1_8_CVS_RJDmetra_corr)

tsIPC01_1_8_CVS_RJDmetra_corr_diff <- diff(tsIPC01_1_8_CVS_RJDmetra_corr)
adf.test(tsIPC01_1_8_CVS_RJDmetra_corr_diff)
kpss.test(tsIPC01_1_8_CVS_RJDmetra_corr_diff)


autoplot(tsIPC01_1_8_CVS_RJDmetra_corr_diff) +
 ggtitle("IPC - Coicop : 01.1.8 - serie différenciée") +
 xlab("Date") + ylab("IPC 01.1.8")+
 theme_minimal()+
 geom_line(color = "blue")


dfcomp_01_1_8 <- dfcomp_01_1_8[-1, ]  # on retire la première ligne
dfcomp_01_1_8$CVS_RJD_CORR_DIFF <- as.numeric(tsIPC01_1_8_CVS_RJDmetra_corr_diff)

#### 2. Estimation des modèles-----

# a.  Estimer et commenter les paramètres des modèles AR(1), AR(p) et ARIMA(p,d,q) et de la méthode LED Holt-Winters, ADAM ETS, ADAM ETS ARIMA, SSARIMA et CES
# b.  Paramètres : présenter sous forme de tableau les paramètres des modèles précédents. Déterminer et commenter le meilleur modèle d’après les critères AIC et AICc

# On entraine nos modele jusqu'a 2022 pour faire la prevision en 2023.

ts_train01_1_8 <- window(tsIPC01_1_8_CVS_RJDmetra_corr_diff, start = c(2010, 2), end = c(2022, 12))
ts_test01_1_8 <- window(tsIPC01_1_8_CVS_RJDmetra_corr_diff, start = c(2023, 1), end = c(2023, 12))

### 2. Estimation du modèle

###. 2.1 AR/ARMA-

### 2.1.1 ARMA-

modele_arima_01_1_8 <- auto.arima(ts_train01_1_8, stepwise = FALSE, approximation = FALSE)

summary(modele_arima_01_1_8)

j <- ncol(modele_arima_01_1_8$var.coef)
tstat <- matrix(nrow=j, ncol=1)
for(i in 1:j)
{
 tstat[i,1] <- modele_arima_01_1_8$coef[i]/sqrt(modele_arima_01_1_8$var.coef[i,i])
}
tstat

# Graphique des valeurs ajustées
plot(modele_arima_01_1_8$fitted)
prev_arima_01_1_8 <- forecast(modele_arima_01_1_8, h=12)
prev_arima_01_1_8
plot(prev_arima_01_1_8)

aic_arma <- AIC(modele_arima_01_1_8)
cat("AIC du modèle SSARIMA :", round(aic_arma, 2), "\n")

aicc_arma <- AICc(modele_arima_01_1_8)
cat("AICc du modèle SSARIMA :", round(aicc_arma, 2), "\n")

## 2.1.2 AR(1)-

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

aic_ar1 <- AIC(modele_ar1_01_1_8)
cat("AIC du modèle AR(1) :", round(aic_ar1, 2), "\n")

aicc_ar1 <- AICc(modele_ar1_01_1_8)
cat("AICc du modèle AR(1) :", round(aicc_ar1, 2), "\n")

### 2.1.3 AR(P)-
modele_arp_01_1_8 <- auto.arima(ts_train01_1_8,
                                d = 0,
                                max.p = 10,        # tu peux ajuster ce maximum
                                max.q = 0,         # pas de MA
                                stationary = TRUE,
                                seasonal = FALSE,
                                stepwise = FALSE,
                                approximation = FALSE)


# Résumé du modèle
summary(modele_arp_01_1_8)

# t-stat des coefficients
j <- length(modele_arp_01_1_8$coef)
tstat_arp <- numeric(j)
for(i in 1:j) {
 tstat_arp[i] <- modele_arp_01_1_8$coef[i] / sqrt(modele_arp_01_1_8$var.coef[i, i])
}
tstat_arp

fitted_arP <- ts_train01_1_8 - residuals(modele_arp_01_1_8)
plot(fitted_arP, type = "l", col = "blue", main = "Valeurs ajustées - AR(1)", ylab = "Prévisions")

prev_arP_01_1_8 <- forecast(modele_arp_01_1_8, h = 12)
prev_arP_01_1_8
plot(prev_arP_01_1_8)

aic_arp <- AIC(modele_arp_01_1_8)
cat("AIC du modèle SSARIMA :", round(aic_arp, 2), "\n")


aicc_arp <- AICc(modele_arp_01_1_8)
cat("AICc du modèle SSARIMA :", round(aicc_arp, 2), "\n")

#### X13-

myregx13_01_1_8 <- regarima_x13(ts_train01_1_8, spec ="RG5c")
summary(myregx13_01_1_8)
s_transform(myregx13_01_1_8)
#plot(myregx13_01_1_8)
myregx13_01_1_8$forecast
forex13_01_1_8 <- matrix(myregx13_01_1_8$forecast[1:12])
forex13_01_1_8

plot(ts_train01_1_8,
     xlim = c(time(ts_train01_1_8)[1], tail(time(ts_train01_1_8), 1) + 1),
     ylim = range(ts_train01_1_8, forex13_01_1_8),
     main = "Prévisions issues de X13-ARIMA-SEATS",
     ylab = "Valeurs", xlab = "Temps")
lines(ts(forex13_01_1_8,
         start = c(2023, 1),  # à adapter si nécessaire
         frequency = frequency(ts_train01_1_8)),
      col = "blue", lwd = 2, lty = 2)
legend("topleft", legend = c("Série observée", "Prévision X13"),
       col = c("black", "blue"), lty = c(1,2), lwd = 2)


#### stl-

decomp = stlm(ts_train01_1_8, s.window="periodic")
show(decomp)
#

fitstl_01_1_8 = stlm(ts_train01_1_8)
prevstl_01_1_8 <- forecast(fitstl_01_1_8,12) #période d'une année

plot(prevstl_01_1_8)
summary(prevstl_01_1_8)


### .2.2 HoltWinters--

WH_add_01_1_8 <- HoltWinters(ts_train01_1_8, gamma = FALSE)
WH_add_01_1_8
show(WH_add_01_1_8)
WH_add_01_1_8$coefficients
plot(WH_add_01_1_8)
forecast_hw01 <- forecast(WH_add_01_1_8, h = 12)
plot(forecast_hw01)
forecast_hw01

# Nombre d'observations
res <- residuals(WH_add_01_1_8)
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


#### ADAM ETS-

fit_ADAM_ETS_01_1_8 <- auto.adam(ts_train01_1_8,
                                 model = "ZZZ",
                                 lags = c(1, 12),
                                 silent = TRUE)          
# ZZZ car ne spécifie rien (tendance, saisonnalité, erreur)
fit_ADAM_ETS_01_1_8
summary(fit_ADAM_ETS_01_1_8)
par(mfcol=c(2,2))
plot(fit_ADAM_ETS_01_1_8)
par(mfcol=c(1,1))


plot(fit_ADAM_ETS_01_1_8$states)
plot(fit_ADAM_ETS_01_1_8$residuals)

prev_ADAM_ETS_01_1_8 <- forecast(fit_ADAM_ETS_01_1_8, h=12)
show(prev_ADAM_ETS_01_1_8)
plot(prev_ADAM_ETS_01_1_8)

#### ADAM ETS + SARIMA-

fitadam3_01_1_8 <- auto.adam(ts_train01_1_8,
                             model = "ZZZ",
                             lags = c(1, 12),
                             orders = list(ar = c(3,3), i = 1, ma = c(3,3)),
                             select = TRUE,
                             bootstrap = TRUE)
fitadam3_01_1_8

summary(fitadam3_01_1_8)

par(mfcol=c(2,2))
plot(fitadam3_01_1_8)
par(mfcol=c(1,1))
plot(fitadam3_01_1_8$residuals)

prev_AES_01_1_8 <- forecast(fitadam3_01_1_8,12)
show(prev_AES_01_1_8)
plot(prev_AES_01_1_8)


### .2.5. SSARIMA-

fit_SSARIMA_01_1_8 <- auto.ssarima(ts_train01_1_8, lags=c(1,12), orders=list(ar=c(3,3), i=(2), ma=c(3,3), select=TRUE))
summary(fit_SSARIMA_01_1_8)

par(mfcol=c(2,2))
plot(fit_SSARIMA_01_1_8)

par(mfcol=c(1,1))

plot(fit_SSARIMA_01_1_8$residuals)

prev_SSARIMA_01_1_8 <- forecast(fit_SSARIMA_01_1_8, h=12)
prev_SSARIMA_01_1_8
plot(prev_SSARIMA_01_1_8)

aic_ssarima <- AIC(fit_SSARIMA_01_1_8)
cat("AIC du modèle SSARIMA :", round(aic_ssarima, 2), "\n")

aicc_ssarima <- AICc(fit_SSARIMA_01_1_8)
cat("AICc du modèle SSARIMA :", round(aicc_ssarima, 2), "\n")


#### naïves-

pred_naive_01_1_8 <- naive(ts_train01_1_8, h=12)
show(pred_naive_01_1_8)
plot(pred_naive_01_1_8)




#### 3. évolution des prévisions----

ts_2023_01_1_8 <- window(tsIPC01_1_8_CVS_RJDmetra_corr_diff, start = c(2023, 1), end = c(2023, 12))

# Fonction mise à jour : transforme la date en format mois
extract_forecast_df <- function(fcast_obj, name) {
 df <- data.frame(
  date = as.Date(as.yearmon(time(fcast_obj$mean))),  # conversion en date
  value = as.numeric(fcast_obj$mean),
  model = name
 )
 df %>% slice_head(n = 12)
}

ts_forex13_01_1_8 <- ts(as.numeric(forex13_01_1_8), start = c(2023, 1), frequency = 12)

# 2. Construire un objet forecast manuellement (sans recalculer)
forecast_x13_01_1_8 <- structure(list(
 mean = ts_forex13_01_1_8,  # ici, un vrai ts avec start/end
 lower = matrix(NA, nrow = 12, ncol = 2, dimnames = list(NULL, c("80%", "95%"))),
 upper = matrix(NA, nrow = 12, ncol = 2, dimnames = list(NULL, c("80%", "95%"))),
 level = c(80, 95),
 x = NULL,
 method = "X13",
 fitted = NULL,
 residuals = NULL
), class = "forecast")

dfs_01_1_8 <- list(
 extract_forecast_df(prev_arima_01_1_8, "ARMA"),
 extract_forecast_df(prev_ar1_01_1_8, "AR(1)"),
 extract_forecast_df(prev_arP_01_1_8, "AR(P)"),
 extract_forecast_df(forecast_x13_01_1_8, "X13"),
 extract_forecast_df(forecast_hw01, "HoltWinters"),
 extract_forecast_df(prev_ADAM_ETS_01_1_8, "ADAM_ETS"),
 extract_forecast_df(prev_AES_01_1_8, "ADAM_ETS+SARIMA"),
 extract_forecast_df(prev_SSARIMA_01_1_8, "SSARIMA"),
 extract_forecast_df(pred_naive_01_1_8, "Naïve")
)

#Je vais comparer aveclesvaleur reel grace a ts_test01_1_8
observed_df_01_1_8 <- data.frame(
 date = as.Date(as.yearmon(time(ts_test01_1_8))),
 value = as.numeric(ts_test01_1_8),
 model = "Observée")

# Combine toutes les prévisions et l'observée
df_all_01_1_8 <- bind_rows(dfs_01_1_8, observed_df_01_1_8)
df_all_01_1_8$date <- as.Date(df_all_01_1_8$date, format = "%Y-%m-%d")
library(ggplot2)

# 1. Graphe simple avec axe en numérique
ggplot(df_all_01_1_8, aes(x = as.numeric(date), y = value, color = model)) +
 geom_line(size = 1.2) +
 scale_x_continuous(
  breaks = as.numeric(df_all_01_1_8$date[1:12]),
  labels = format(df_all_01_1_8$date[1:12], "%b %Y")
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
ggplot(df_all_01_1_8, aes(x = date, y = value, color = model, linetype = model)) +
 geom_line(size = 1.2) +
 labs(
  title = "Prévisions - 01.1.8 : Légumes - Sur 12 mois (2023) par modèle",
  x = "Mois", y = "Valeur prévue", color = "Modèle", linetype = "Modèle"
 ) +
 scale_color_manual(values = c(
  "ADAM_ETS" = "red",
  "ADAM_ETS+SARIMA" = "orange",
  "HoltWinters" = "yellow",
  "Naïve" = "black",
  "SSARIMA" = "purple",
  "X13" = "darkblue",
  "ARMA" = "forestgreen",
  "AR(1)" = "brown",
  "AR(P)" = "darkcyan",
  "Observée" = "black"
 )) +
 scale_linetype_manual(values = c(
  "ADAM_ETS" = "solid",
  "ADAM_ETS+SARIMA" = "solid",
  "HoltWinters" = "solid",
  "Naïve" = "solid",
  "SSARIMA" = "solid",
  "X13" = "dashed",
  "ARMA" = "dotted",
  "AR(1)" = "dotted",
  "AR(P)" = "dotted",
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

#### 4 qualité de prevision -----

## MSE & R²OOS
## MSE & R²OOS avec le modèle Naïf comme référence

# Observations réelles
actual_values01_1_8 <- ts_test01_1_8

# Liste des prévisions pour chaque modèle
forecasts_01_1_8 <- list(
 prev_arima_01_1_8$mean,
 prev_ar1_01_1_8$mean,
 prev_arP_01_1_8$mean,
 forecast_x13_01_1_8$mean,
 forecast_hw01$mean,
 prev_ADAM_ETS_01_1_8$mean,
 prev_AES_01_1_8$mean,
 prev_SSARIMA_01_1_8$mean,
 pred_naive_01_1_8$mean
)


# Fonction MSE + R²OOS avec benchmark explicite
calculate_metrics_01_1_8 <- function(actual, forecast, benchmark_forecast) {
 mse <- mean((actual - forecast)^2)
 sst <- sum((actual - benchmark_forecast)^2)
 sse <- sum((actual - forecast)^2)
 r2oos <- 1 - (sse / sst)
 return(list(mse = mse, r2oos = r2oos))
}

# Naïve utilisé comme benchmark
benchmark_forecast <- as.numeric(pred_naive_01_1_8$mean)

# Calcul des métriques
metrics <- lapply(forecasts_01_1_8, function(fcast) {
 calculate_metrics_01_1_8(as.numeric(actual_values01_1_8), as.numeric(fcast), benchmark_forecast)
})

# Data frame de résultats
metrics_df_01_1_8 <- as.data.frame(do.call(rbind, metrics))
rownames(metrics_df_01_1_8) <- c("ARMA", "AR(1)", "AR(P)","X13",  "HoltWinters","ADAM_ETS", "ADAM_ETS+SARIMA", "SSARIMA", 
                                 "Naïve")

print(metrics_df_01_1_8)

## CSPE

# Fonction CSPE
calculate_cspe <- function(actual, forecast) {
 errors <- (actual - forecast)^2
 cspe <- cumsum(errors)
 return(cspe)
}

# CSPE pour chaque modèle
cspe_list <- lapply(forecasts_01_1_8, function(fcast) {
 calculate_cspe(as.numeric(actual_values01_1_8), as.numeric(fcast))
})

# Création du data.frame CSPE
cspe_df <- do.call(cbind, cspe_list)
colnames(cspe_df) <- rownames(metrics_df_01_1_8)
cspe_df <- cbind(Date = as.Date(time(actual_values01_1_8)), cspe_df)
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
  "ARMA" = "forestgreen", "AR(1)" = "brown", "AR(P)" = "darkcyan","X13" = "darkblue",
  "HoltWinters" = "yellow", "ADAM_ETS" = "red", "ADAM_ETS+SARIMA" = "orange",
  "SSARIMA" = "purple", "Naïve" = "black"
 )) +
 scale_linetype_manual(values = c(
  "ARMA" = "dashed", "AR(1)" = "dotted", "AR(P)" = "solid","X13" = "dashed",
  "HoltWinters" = "solid", "ADAM_ETS" = "solid", "ADAM_ETS+SARIMA" = "solid",
  "SSARIMA" = "solid", "Naïve" = "dotdash"
 )) +
 
 
 scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
 theme_minimal(base_size = 13) +
 theme(axis.text.x = element_text(angle = 45, hjust = 1),
       legend.position = "bottom",
       legend.title = element_text(face = "bold"),
       plot.title = element_text(face = "bold"))



##### 5 Test de précision-----

# Définir la période de temps
start_date <- c(2023, 1)
end_date <- c(2023, 12)

# Fonction pour ajuster les séries temporelles
adjust_time_series <- function(ts_data) {
 return(window(ts_data, start = start_date, end = end_date))
}

# Ajuster les séries temporelles pour chaque modèle
for_observed <- adjust_time_series(ts_2023_01_1_8)
for_ARMA     <- adjust_time_series(prev_arima_01_1_8$mean)
for_AR1      <- adjust_time_series(prev_ar1_01_1_8$mean)
for_ARP      <- adjust_time_series(prev_arP_01_1_8$mean)
for_X13      <- adjust_time_series(forecast_x13_01_1_8$mean)
for_HW       <- adjust_time_series(forecast_hw01$mean)
for_ADAM_ETS     <- adjust_time_series(prev_ADAM_ETS_01_1_8$mean)
for_AES      <- adjust_time_series(prev_AES_01_1_8$mean)
for_SSARIMA  <- adjust_time_series(prev_SSARIMA_01_1_8$mean)
for_NAIVE    <- adjust_time_series(pred_naive_01_1_8$mean)



# Vérifiez que toutes les séries temporelles ont la bonne longueur

ts_2023_01_1_8 <- window(tsIPC01_1_8_CVS_RJDmetra_corr_diff, start = c(2023, 1), end = c(2023, 12))
ts_test01_1_8 <- window(tsIPC01_1_8_CVS_RJDmetra_corr_diff, start = c(2023, 1), end = c(2023, 12))
print(length(ts_2023_01_1_8))

print(length(for_observed))
print(length(for_ARMA))
print(length(for_AR1))
print(length(for_ARP))
print(length(for_X13))
print(length(for_HW))
print(length(for_ADAM_ETS))
print(length(for_AES))
print(length(for_SSARIMA))
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

# Calculer le test DM

dm.test(error_NAIVE, error_ARMA, h = length(for_observed))
dm.test(error_NAIVE, error_AR1, h = length(for_observed))
dm.test(error_NAIVE, error_ARP, h = length(for_observed))
dm.test(error_NAIVE, error_X13, h = length(for_observed))
dm.test(error_NAIVE, error_HW, h = length(for_observed))
dm.test(error_NAIVE, error_ADAM_ETS, h = length(for_observed))
dm.test(error_NAIVE, error_AES, h = length(for_observed))
dm.test(error_NAIVE, error_SSARIMA, h = length(for_observed))


# Test de Diebold-Mariano avec h = 1
dm.test(error_NAIVE, error_ARMA, h = 1)
dm.test(error_NAIVE, error_AR1, h = 1)
dm.test(error_NAIVE, error_ARP, h = 1)
dm.test(error_NAIVE, error_X13, h = 1)
dm.test(error_NAIVE, error_HW, h = 1)
dm.test(error_NAIVE, error_ADAM_ETS, h = 1)
dm.test(error_NAIVE, error_AES, h = 1)
dm.test(error_NAIVE, error_SSARIMA, h = 1)


## le meilleur modèle est AR1 cool


#--------------  01.1.9 - Produits alimentaires n.c.a. ---------

ts_n_c_a <- ts(dfcomp$n_c_a, start = c(2010, 1), frequency = 12)

autoplot(ts_n_c_a) +
 ggtitle("IPC - Coicop :  01.1.9 - Produits alimentaires n.c.a.") +
 xlab("Date") + ylab("IPC - 01.1.9")+
 theme_minimal()+
 geom_line(color = "blue")


### - tests (CVS stationnaire)

combined_test(ts_n_c_a)

seasdum(ts_n_c_a) #Seasonal Dummies

adf.test(ts_n_c_a)
kpss.test(ts_n_c_a)

xss <-diff(ts_n_c_a)
adf.test(xss)
kpss.test(xss) # La serie est stationnaire ap diff :)

myregx13_01_1_9 <- regarima_x13(ts_n_c_a, spec ="RG5c")
s_transform(myregx13_01_1_9)  # test log/level
# Pas de transformation en log.


### Outliers-


outliers01_1_9 <- tso(ts_n_c_a)
print(outliers01_1_9)

plot(outliers01_1_9)
show(outliers01_1_9)

tsIPC01_1_9_corr <- outliers01_1_9$yadj

dfcomp_01_1_9 <- data.frame(
 Date = as.Date(as.yearmon(time(ts_n_c_a)), frac = 1),
 Originale = as.numeric(ts_n_c_a),
 ts_corr = as.numeric(tsIPC01_1_9_corr)
)

### Virification de la stationnarité-

combined_test(tsIPC01_1_9_corr)
seasdum(tsIPC01_1_9_corr) #Seasonal Dummies
# Toujours CVS logique

adf.test(tsIPC01_1_9_corr)
kpss.test(tsIPC01_1_9_corr)

tsIPC01_1_9_corr_diff <- diff(tsIPC01_1_9_corr)
adf.test(tsIPC01_1_9_corr_diff)
kpss.test(tsIPC01_1_9_corr_diff)


autoplot(tsIPC01_1_9_corr_diff) +
 ggtitle("IPC - Coicop : 01.1.9 - serie différenciée") +
 xlab("Date") + ylab("IPC 01.1.9")+
 theme_minimal()+
 geom_line(color = "blue")


dfcomp_01_1_9 <- dfcomp_01_1_9[-1, ]  # on retire la première ligne
dfcomp_01_1_9$CVS_RJD_CORR_DIFF <- as.numeric(tsIPC01_1_9_corr_diff)

#### 2. Estimation des modèles-----

# a.  Estimer et commenter les paramètres des modèles AR(1), AR(p) et ARIMA(p,d,q) et de la méthode LED Holt-Winters, ADAM ETS, ADAM ETS ARIMA, SSARIMA et CES
# b.  Paramètres : présenter sous forme de tableau les paramètres des modèles précédents. Déterminer et commenter le meilleur modèle d’après les critères AIC et AICc

# On entraine nos modele jusqu'a 2022 pour faire la prevision en 2023.

ts_train01_1_9 <- window(tsIPC01_1_9_corr_diff, start = c(2010, 2), end = c(2022, 12))
ts_test01_1_9 <- window(tsIPC01_1_9_corr_diff, start = c(2023, 1), end = c(2023, 12))

### 2. Estimation du modèle

###. 2.1 AR/ARMA-

### 2.1.1 ARMA-

modele_arima_01_1_9 <- auto.arima(ts_train01_1_9, stepwise = FALSE, approximation = FALSE)

summary(modele_arima_01_1_9)

j <- ncol(modele_arima_01_1_9$var.coef)
tstat <- matrix(nrow=j, ncol=1)
for(i in 1:j)
{
 tstat[i,1] <- modele_arima_01_1_9$coef[i]/sqrt(modele_arima_01_1_9$var.coef[i,i])
}
tstat

# Graphique des valeurs ajustées
plot(modele_arima_01_1_9$fitted)
prev_arima_01_1_9 <- forecast(modele_arima_01_1_9, h=12)
prev_arima_01_1_9
plot(prev_arima_01_1_9)

aic_arma <- AIC(modele_arima_01_1_9)
cat("AIC du modèle SSARIMA :", round(aic_arma, 2), "\n")

aicc_arma <- AICc(modele_arima_01_1_9)
cat("AICc du modèle SSARIMA :", round(aicc_arma, 2), "\n")

## 2.1.2 AR(1)-

modele_ar1_01_1_9 <- Arima(ts_train01_1_9, order = c(1, 0, 0), include.mean = TRUE)

summary(modele_ar1_01_1_9)

j <- ncol(modele_ar1_01_1_9$var.coef)
tstat <- matrix(nrow=j, ncol=1)
for(i in 1:j)
{
 tstat[i,1] <- modele_ar1_01_1_9$coef[i]/sqrt(modele_ar1_01_1_9$var.coef[i,i])
}
tstat

fitted_ar1 <- ts_train01_1_9 - residuals(modele_ar1_01_1_9)
plot(fitted_ar1, type = "l", col = "blue", main = "Valeurs ajustées - AR(1)", ylab = "Prévisions")

prev_ar1_01_1_9 <- forecast(modele_ar1_01_1_9, h = 12)
prev_ar1_01_1_9
plot(prev_ar1_01_1_9)

aic_ar1 <- AIC(modele_ar1_01_1_9)
cat("AIC du modèle AR(1) :", round(aic_ar1, 2), "\n")

aicc_ar1 <- AICc(modele_ar1_01_1_9)
cat("AICc du modèle AR(1) :", round(aicc_ar1, 2), "\n")

### 2.1.3 AR(P)-
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

aic_arp <- AIC(modele_arp_01_1_9)
cat("AIC du modèle SSARIMA :", round(aic_arp, 2), "\n")

aicc_arp <- AICc(modele_arp_01_1_9)
cat("AICc du modèle SSARIMA :", round(aicc_arp, 2), "\n")

#### X13-

myregx13_01_1_9 <- regarima_x13(ts_train01_1_9, spec ="RG5c")
summary(myregx13_01_1_9)
s_transform(myregx13_01_1_9)
#plot(myregx13_01_1_9)
myregx13_01_1_9$forecast
forex13_01_1_9 <- matrix(myregx13_01_1_9$forecast[1:12])
forex13_01_1_9

plot(ts_train01_1_9,
     xlim = c(time(ts_train01_1_9)[1], tail(time(ts_train01_1_9), 1) + 1),
     ylim = range(ts_train01_1_9, forex13_01_1_9),
     main = "Prévisions issues de X13-ARIMA-SEATS",
     ylab = "Valeurs", xlab = "Temps")
lines(ts(forex13_01_1_9,
         start = c(2023, 1),  # à adapter si nécessaire
         frequency = frequency(ts_train01_1_9)),
      col = "blue", lwd = 2, lty = 2)
legend("topleft", legend = c("Série observée", "Prévision X13"),
       col = c("black", "blue"), lty = c(1,2), lwd = 2)


#### stl-

decomp = stlm(ts_train01_1_9, s.window="periodic")
show(decomp)
#

fitstl_01_1_9 = stlm(ts_train01_1_9)
prevstl_01_1_9 <- forecast(fitstl_01_1_9,12) #période d'une année

plot(prevstl_01_1_9)
summary(prevstl_01_1_9)


### .2.2 HoltWinters--

WH_add_01_1_9 <- HoltWinters(ts_train01_1_9, gamma = FALSE)
WH_add_01_1_9
show(WH_add_01_1_9)
WH_add_01_1_9$coefficients
plot(WH_add_01_1_9)
forecast_hw01 <- forecast(WH_add_01_1_9, h = 12)
plot(forecast_hw01)
forecast_hw01

# Nombre d'observations
res <- residuals(WH_add_01_1_9)
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


#### ADAM ETS-

fit_ADAM_ETS_01_1_9 <- auto.adam(ts_train01_1_9,
                                 model = "ZZZ",
                                 lags = c(1, 12),
                                 silent = TRUE)          
# ZZZ car ne spécifie rien (tendance, saisonnalité, erreur)
fit_ADAM_ETS_01_1_9
summary(fit_ADAM_ETS_01_1_9)
par(mfcol=c(2,2))
plot(fit_ADAM_ETS_01_1_9)
par(mfcol=c(1,1))


plot(fit_ADAM_ETS_01_1_9$states)
plot(fit_ADAM_ETS_01_1_9$residuals)

prev_ADAM_ETS_01_1_9 <- forecast(fit_ADAM_ETS_01_1_9, h=12)
show(prev_ADAM_ETS_01_1_9)
plot(prev_ADAM_ETS_01_1_9)

#### ADAM ETS + SARIMA-

fitadam3_01_1_9 <- auto.adam(ts_train01_1_9,
                             model = "ZZZ",
                             lags = c(1, 12),
                             orders = list(ar = c(3,3), i = 1, ma = c(3,3)),
                             select = TRUE,
                             bootstrap = TRUE)
fitadam3_01_1_9

summary(fitadam3_01_1_9)

par(mfcol=c(2,2))
plot(fitadam3_01_1_9)
par(mfcol=c(1,1))
plot(fitadam3_01_1_9$residuals)

prev_AES_01_1_9 <- forecast(fitadam3_01_1_9,12)
show(prev_AES_01_1_9)
plot(prev_AES_01_1_9)


### .2.5. SSARIMA-

fit_SSARIMA_01_1_9 <- auto.ssarima(ts_train01_1_9, lags=c(1,12), orders=list(ar=c(3,3), i=(2), ma=c(3,3), select=TRUE))
summary(fit_SSARIMA_01_1_9)

par(mfcol=c(2,2))
plot(fit_SSARIMA_01_1_9)

par(mfcol=c(1,1))

plot(fit_SSARIMA_01_1_9$residuals)

prev_SSARIMA_01_1_9 <- forecast(fit_SSARIMA_01_1_9, h=12)
prev_SSARIMA_01_1_9
plot(prev_SSARIMA_01_1_9)

aic_ssarima <- AIC(fit_SSARIMA_01_1_9)
cat("AIC du modèle SSARIMA :", round(aic_ssarima, 2), "\n")

aicc_ssarima <- AICc(fit_SSARIMA_01_1_9)
cat("AICc du modèle SSARIMA :", round(aicc_ssarima, 2), "\n")


#### naïves-

pred_naive_01_1_9 <- naive(ts_train01_1_9, h=12)
show(pred_naive_01_1_9)
plot(pred_naive_01_1_9)




#### 3. évolution des prévisions----

ts_2023_01_1_9 <- window(tsIPC01_1_9_corr_diff, start = c(2023, 1), end = c(2023, 12))

# Fonction mise à jour : transforme la date en format mois
extract_forecast_df <- function(fcast_obj, name) {
 df <- data.frame(
  date = as.Date(as.yearmon(time(fcast_obj$mean))),  # conversion en date
  value = as.numeric(fcast_obj$mean),
  model = name
 )
 df %>% slice_head(n = 12)
}

ts_forex13_01_1_9 <- ts(as.numeric(forex13_01_1_9), start = c(2023, 1), frequency = 12)

# 2. Construire un objet forecast manuellement (sans recalculer)
forecast_x13_01_1_9 <- structure(list(
 mean = ts_forex13_01_1_9,  # ici, un vrai ts avec start/end
 lower = matrix(NA, nrow = 12, ncol = 2, dimnames = list(NULL, c("80%", "95%"))),
 upper = matrix(NA, nrow = 12, ncol = 2, dimnames = list(NULL, c("80%", "95%"))),
 level = c(80, 95),
 x = NULL,
 method = "X13",
 fitted = NULL,
 residuals = NULL
), class = "forecast")

dfs_01_1_9 <- list(
 extract_forecast_df(prev_arima_01_1_9, "ARMA"),
 extract_forecast_df(prev_ar1_01_1_9, "AR(1)"),
 extract_forecast_df(prev_arP_01_1_9, "AR(P)"),
 extract_forecast_df(forecast_x13_01_1_9, "X13"),
 extract_forecast_df(forecast_hw01, "HoltWinters"),
 extract_forecast_df(prev_ADAM_ETS_01_1_9, "ADAM_ETS"),
 extract_forecast_df(prev_AES_01_1_9, "ADAM_ETS+SARIMA"),
 extract_forecast_df(prev_SSARIMA_01_1_9, "SSARIMA"),
 extract_forecast_df(pred_naive_01_1_9, "Naïve")
)

#Je vais comparer aveclesvaleur reel grace a ts_test01_1_9
observed_df_01_1_9 <- data.frame(
 date = as.Date(as.yearmon(time(ts_test01_1_9))),
 value = as.numeric(ts_test01_1_9),
 model = "Observée")

# Combine toutes les prévisions et l'observée
df_all_01_1_9 <- bind_rows(dfs_01_1_9, observed_df_01_1_9)
df_all_01_1_9$date <- as.Date(df_all_01_1_9$date, format = "%Y-%m-%d")
library(ggplot2)

# 1. Graphe simple avec axe en numérique
ggplot(df_all_01_1_9, aes(x = as.numeric(date), y = value, color = model)) +
 geom_line(size = 1.2) +
 scale_x_continuous(
  breaks = as.numeric(df_all_01_1_9$date[1:12]),
  labels = format(df_all_01_1_9$date[1:12], "%b %Y")
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
ggplot(df_all_01_1_9, aes(x = date, y = value, color = model, linetype = model)) +
 geom_line(size = 1.2) +
 labs(
  title = "Prévisions - 01.1.9 : Produits alimentaires n.c.a. - Sur 12 mois (2023) par modèle",
  x = "Mois", y = "Valeur prévue", color = "Modèle", linetype = "Modèle"
 ) +
 scale_color_manual(values = c(
  "ADAM_ETS" = "red",
  "ADAM_ETS+SARIMA" = "orange",
  "HoltWinters" = "yellow",
  "Naïve" = "black",
  "SSARIMA" = "purple",
  "X13" = "darkblue",
  "ARMA" = "forestgreen",
  "AR(1)" = "brown",
  "AR(P)" = "darkcyan",
  "Observée" = "black"
 )) +
 scale_linetype_manual(values = c(
  "ADAM_ETS" = "solid",
  "ADAM_ETS+SARIMA" = "solid",
  "HoltWinters" = "solid",
  "Naïve" = "solid",
  "SSARIMA" = "solid",
  "X13" = "dashed",
  "ARMA" = "dotted",
  "AR(1)" = "dotted",
  "AR(P)" = "dotted",
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


#### 4 qualité de prevision -----

## MSE & R²OOS
## MSE & R²OOS avec le modèle Naïf comme référence

# Observations réelles
actual_values01_1_9 <- ts_test01_1_9

# Liste des prévisions pour chaque modèle
forecasts_01_1_9 <- list(
 prev_arima_01_1_9$mean,
 prev_ar1_01_1_9$mean,
 prev_arP_01_1_9$mean,
 forecast_x13_01_1_9$mean,
 forecast_hw01$mean,
 prev_ADAM_ETS_01_1_9$mean,
 prev_AES_01_1_9$mean,
 prev_SSARIMA_01_1_9$mean,
 pred_naive_01_1_9$mean
)


# Fonction MSE + R²OOS avec benchmark explicite
calculate_metrics_01_1_9 <- function(actual, forecast, benchmark_forecast) {
 mse <- mean((actual - forecast)^2)
 sst <- sum((actual - benchmark_forecast)^2)
 sse <- sum((actual - forecast)^2)
 r2oos <- 1 - (sse / sst)
 return(list(mse = mse, r2oos = r2oos))
}

# Naïve utilisé comme benchmark
benchmark_forecast <- as.numeric(pred_naive_01_1_9$mean)

# Calcul des métriques
metrics <- lapply(forecasts_01_1_9, function(fcast) {
 calculate_metrics_01_1_9(as.numeric(actual_values01_1_9), as.numeric(fcast), benchmark_forecast)
})

# Data frame de résultats
metrics_df_01_1_9 <- as.data.frame(do.call(rbind, metrics))
rownames(metrics_df_01_1_9) <- c("ARMA", "AR(1)", "AR(P)","X13",  "HoltWinters","ADAM_ETS", "ADAM_ETS+SARIMA", "SSARIMA", 
                                 "Naïve")

print(metrics_df_01_1_9)

## CSPE

# Fonction CSPE
calculate_cspe <- function(actual, forecast) {
 errors <- (actual - forecast)^2
 cspe <- cumsum(errors)
 return(cspe)
}

# CSPE pour chaque modèle
cspe_list <- lapply(forecasts_01_1_9, function(fcast) {
 calculate_cspe(as.numeric(actual_values01_1_9), as.numeric(fcast))
})

# Création du data.frame CSPE
cspe_df <- do.call(cbind, cspe_list)
colnames(cspe_df) <- rownames(metrics_df_01_1_9)
cspe_df <- cbind(Date = as.Date(time(actual_values01_1_9)), cspe_df)
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
  "ARMA" = "forestgreen", "AR(1)" = "brown", "AR(P)" = "darkcyan","X13" = "darkblue",
  "HoltWinters" = "yellow", "ADAM_ETS" = "red", "ADAM_ETS+SARIMA" = "orange",
  "SSARIMA" = "purple", "Naïve" = "black"
 )) +
 scale_linetype_manual(values = c(
  "ARMA" = "dashed", "AR(1)" = "dotted", "AR(P)" = "solid","X13" = "dashed",
  "HoltWinters" = "solid", "ADAM_ETS" = "solid", "ADAM_ETS+SARIMA" = "solid",
  "SSARIMA" = "solid", "Naïve" = "dotdash"
 )) +
 
 
 scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
 theme_minimal(base_size = 13) +
 theme(axis.text.x = element_text(angle = 45, hjust = 1),
       legend.position = "bottom",
       legend.title = element_text(face = "bold"),
       plot.title = element_text(face = "bold"))



##### 5 Test de précision-----

# Définir la période de temps
start_date <- c(2023, 1)
end_date <- c(2023, 12)

# Fonction pour ajuster les séries temporelles
adjust_time_series <- function(ts_data) {
 return(window(ts_data, start = start_date, end = end_date))
}

# Ajuster les séries temporelles pour chaque modèle
for_observed <- adjust_time_series(ts_2023_01_1_9)
for_ARMA     <- adjust_time_series(prev_arima_01_1_9$mean)
for_AR1      <- adjust_time_series(prev_ar1_01_1_9$mean)
for_ARP      <- adjust_time_series(prev_arP_01_1_9$mean)
for_X13      <- adjust_time_series(forecast_x13_01_1_9$mean)
for_HW       <- adjust_time_series(forecast_hw01$mean)
for_ADAM_ETS     <- adjust_time_series(prev_ADAM_ETS_01_1_9$mean)
for_AES      <- adjust_time_series(prev_AES_01_1_9$mean)
for_SSARIMA  <- adjust_time_series(prev_SSARIMA_01_1_9$mean)
for_NAIVE    <- adjust_time_series(pred_naive_01_1_9$mean)



# Vérifiez que toutes les séries temporelles ont la bonne longueur

ts_2023_01_1_9 <- window(tsIPC01_1_9_corr_diff, start = c(2023, 1), end = c(2023, 12))
ts_test01_1_9 <- window(tsIPC01_1_9_corr_diff, start = c(2023, 1), end = c(2023, 12))
print(length(ts_2023_01_1_9))

print(length(for_observed))
print(length(for_ARMA))
print(length(for_AR1))
print(length(for_ARP))
print(length(for_X13))
print(length(for_HW))
print(length(for_ADAM_ETS))
print(length(for_AES))
print(length(for_SSARIMA))
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

# Calculer le test DM

dm.test(error_NAIVE, error_ARMA, h = length(for_observed))
dm.test(error_NAIVE, error_AR1, h = length(for_observed))
dm.test(error_NAIVE, error_ARP, h = length(for_observed))
dm.test(error_NAIVE, error_X13, h = length(for_observed))
dm.test(error_NAIVE, error_HW, h = length(for_observed))
dm.test(error_NAIVE, error_ADAM_ETS, h = length(for_observed))
dm.test(error_NAIVE, error_AES, h = length(for_observed))
dm.test(error_NAIVE, error_SSARIMA, h = length(for_observed))


# Test de Diebold-Mariano avec h = 1
dm.test(error_NAIVE, error_ARMA, h = 1)
dm.test(error_NAIVE, error_AR1, h = 1)
dm.test(error_NAIVE, error_ARP, h = 1)
dm.test(error_NAIVE, error_X13, h = 1)
dm.test(error_NAIVE, error_HW, h = 1)
dm.test(error_NAIVE, error_ADAM_ETS, h = 1)
dm.test(error_NAIVE, error_AES, h = 1)
dm.test(error_NAIVE, error_SSARIMA, h = 1)


## le meilleur modèle est AR(P) cool


#-------------- 01.2.1 - Café, thé et cacao ---------

ts_Cafe_the_cacao <- ts(dfcomp$Cafe_the_cacao, start = c(2010, 1), frequency = 12)

autoplot(ts_Cafe_the_cacao) +
 ggtitle("IPC - Coicop :  01.2.1 - Sucre, confiture et confiserie") +
 xlab("Date") + ylab("IPC - 01.2.1")+
 theme_minimal()+
 geom_line(color = "blue")


### - tests (CVS stationnaire)

combined_test(ts_Cafe_the_cacao)

seasdum(ts_Cafe_the_cacao) #Seasonal Dummies

adf.test(ts_Cafe_the_cacao)
kpss.test(ts_Cafe_the_cacao)

xss <-diff(ts_Cafe_the_cacao)
adf.test(xss)
kpss.test(xss) # La serie est stationnaire ap diff :)

myregx13_01_2_1 <- regarima_x13(ts_Cafe_the_cacao, spec ="RG5c")
s_transform(myregx13_01_2_1)  # test log/level
# Pas de transformation en log.

### - CVS-

# Avec RJDmetra
s_transform(myregx13_01_2_1)
myspec <- x13_spec("RSA5c")
mysax13 <- x13(ts_Cafe_the_cacao, myspec) # SCV de RJGDmetra
#plot(mysax13$final) # JTM

# Extraire la série désaisonnalisée (CVS)
tsIPC01_2_1_CVS_RJDmetra <- ts(mysax13$final$series[, "sa"],
                               start = start(ts_Cafe_the_cacao), frequency = frequency(ts_Cafe_the_cacao))

dfcomp_01_2_1 <- data.frame(
 Date = as.Date(as.yearmon(time(ts_Cafe_the_cacao)), frac = 1),
 Originale = as.numeric(ts_Cafe_the_cacao),
 CVS_RJD = as.numeric(tsIPC01_2_1_CVS_RJDmetra)
)

### Outliers-

outliers01_2_1 <- tso(tsIPC01_2_1_CVS_RJDmetra)
print(outliers01_2_1)

plot(outliers01_2_1)
show(outliers01_2_1)

tsIPC01_2_1_CVS_RJDmetra_corr <- outliers01_2_1$yadj
dfcomp_01_2_1$CVS_RJD_CORR <- as.numeric(tsIPC01_2_1_CVS_RJDmetra_corr)

### Virification de la stationnarité-

combined_test(tsIPC01_2_1_CVS_RJDmetra_corr)
seasdum(tsIPC01_2_1_CVS_RJDmetra_corr) #Seasonal Dummies
# Toujours CVS logique

adf.test(tsIPC01_2_1_CVS_RJDmetra_corr)
kpss.test(tsIPC01_2_1_CVS_RJDmetra_corr)

tsIPC01_2_1_CVS_RJDmetra_corr_diff <- diff(tsIPC01_2_1_CVS_RJDmetra_corr)
adf.test(tsIPC01_2_1_CVS_RJDmetra_corr_diff)
kpss.test(tsIPC01_2_1_CVS_RJDmetra_corr_diff)


autoplot(tsIPC01_2_1_CVS_RJDmetra_corr_diff) +
 ggtitle("IPC - Coicop : 01.2.1 - serie différenciée") +
 xlab("Date") + ylab("IPC 01.2.1")+
 theme_minimal()+
 geom_line(color = "blue")


dfcomp_01_2_1 <- dfcomp_01_2_1[-1, ]  # on retire la première ligne
dfcomp_01_2_1$CVS_RJD_CORR_DIFF <- as.numeric(tsIPC01_2_1_CVS_RJDmetra_corr_diff)

#### 2. Estimation des modèles-----

# a.  Estimer et commenter les paramètres des modèles AR(1), AR(p) et ARIMA(p,d,q) et de la méthode LED Holt-Winters, ADAM ETS, ADAM ETS ARIMA, SSARIMA et CES
# b.  Paramètres : présenter sous forme de tableau les paramètres des modèles précédents. Déterminer et commenter le meilleur modèle d’après les critères AIC et AICc

# On entraine nos modele jusqu'a 2022 pour faire la prevision en 2023.

ts_train01_2_1 <- window(tsIPC01_2_1_CVS_RJDmetra_corr_diff, start = c(2010, 2), end = c(2022, 12))
ts_test01_2_1 <- window(tsIPC01_2_1_CVS_RJDmetra_corr_diff, start = c(2023, 1), end = c(2023, 12))


### 2. Estimation du modèle

###. 2.1 AR/ARMA-

### 2.1.1 ARMA-

modele_arima_01_2_1 <- auto.arima(ts_train01_2_1, stepwise = FALSE, approximation = FALSE)

summary(modele_arima_01_2_1)

j <- ncol(modele_arima_01_2_1$var.coef)
tstat <- matrix(nrow=j, ncol=1)
for(i in 1:j)
{
 tstat[i,1] <- modele_arima_01_2_1$coef[i]/sqrt(modele_arima_01_2_1$var.coef[i,i])
}
tstat

# Graphique des valeurs ajustées
plot(modele_arima_01_2_1$fitted)
prev_arima_01_2_1 <- forecast(modele_arima_01_2_1, h=12)
prev_arima_01_2_1
plot(prev_arima_01_2_1)

aic_arma <- AIC(modele_arima_01_2_1)
cat("AIC du modèle SSARIMA :", round(aic_arma, 2), "\n")

aicc_arma <- AICc(modele_arima_01_2_1)
cat("AICc du modèle SSARIMA :", round(aicc_arma, 2), "\n")

## 2.1.2 AR(1)-

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

aic_ar1 <- AIC(modele_ar1_01_2_1)
cat("AIC du modèle AR(1) :", round(aic_ar1, 2), "\n")

aicc_ar1 <- AICc(modele_ar1_01_2_1)
cat("AICc du modèle AR(1) :", round(aicc_ar1, 2), "\n")

### 2.1.3 AR(P)-
modele_arp_01_2_1 <- auto.arima(ts_train01_2_1,
                                d = 0,
                                max.p = 10,        # tu peux ajuster ce maximum
                                max.q = 0,         # pas de MA
                                stationary = TRUE,
                                seasonal = FALSE,
                                stepwise = FALSE,
                                approximation = FALSE)


# Résumé du modèle
summary(modele_arp_01_2_1)

# t-stat des coefficients
j <- length(modele_arp_01_2_1$coef)
tstat_arp <- numeric(j)
for(i in 1:j) {
 tstat_arp[i] <- modele_arp_01_2_1$coef[i] / sqrt(modele_arp_01_2_1$var.coef[i, i])
}
tstat_arp

fitted_arP <- ts_train01_2_1 - residuals(modele_arp_01_2_1)
plot(fitted_arP, type = "l", col = "blue", main = "Valeurs ajustées - AR(1)", ylab = "Prévisions")

prev_arP_01_2_1 <- forecast(modele_arp_01_2_1, h = 12)
prev_arP_01_2_1
plot(prev_arP_01_2_1)

aic_arp <- AIC(modele_arp_01_2_1)
cat("AIC du modèle SSARIMA :", round(aic_arp, 2), "\n")


aicc_arp <- AICc(modele_arp_01_2_1)
cat("AICc du modèle SSARIMA :", round(aicc_arp, 2), "\n")

#### X13-

myregx13_01_2_1 <- regarima_x13(ts_train01_2_1, spec ="RG5c")
summary(myregx13_01_2_1)
s_transform(myregx13_01_2_1)
#plot(myregx13_01_2_1)
myregx13_01_2_1$forecast
forex13_01_2_1 <- matrix(myregx13_01_2_1$forecast[1:12])
forex13_01_2_1

plot(ts_train01_2_1,
     xlim = c(time(ts_train01_2_1)[1], tail(time(ts_train01_2_1), 1) + 1),
     ylim = range(ts_train01_2_1, forex13_01_2_1),
     main = "Prévisions issues de X13-ARIMA-SEATS",
     ylab = "Valeurs", xlab = "Temps")
lines(ts(forex13_01_2_1,
         start = c(2023, 1),  # à adapter si nécessaire
         frequency = frequency(ts_train01_2_1)),
      col = "blue", lwd = 2, lty = 2)
legend("topleft", legend = c("Série observée", "Prévision X13"),
       col = c("black", "blue"), lty = c(1,2), lwd = 2)


#### stl-

decomp = stlm(ts_train01_2_1, s.window="periodic")
show(decomp)
#

fitstl_01_2_1 = stlm(ts_train01_2_1)
prevstl_01_2_1 <- forecast(fitstl_01_2_1,12) #période d'une année

plot(prevstl_01_2_1)
summary(prevstl_01_2_1)


### .2.2 HoltWinters--

WH_add_01_2_1 <- HoltWinters(ts_train01_2_1, gamma = FALSE)
WH_add_01_2_1
show(WH_add_01_2_1)
WH_add_01_2_1$coefficients
plot(WH_add_01_2_1)
forecast_hw01 <- forecast(WH_add_01_2_1, h = 12)
plot(forecast_hw01)
forecast_hw01

# Nombre d'observations
res <- residuals(WH_add_01_2_1)
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


#### ADAM ETS-

fit_ADAM_ETS_01_2_1 <- auto.adam(ts_train01_2_1,
                                 model = "ZZZ",
                                 lags = c(1, 12),
                                 silent = TRUE)          
# ZZZ car ne spécifie rien (tendance, saisonnalité, erreur)
fit_ADAM_ETS_01_2_1
summary(fit_ADAM_ETS_01_2_1)
par(mfcol=c(2,2))
plot(fit_ADAM_ETS_01_2_1)
par(mfcol=c(1,1))


plot(fit_ADAM_ETS_01_2_1$states)
plot(fit_ADAM_ETS_01_2_1$residuals)

prev_ADAM_ETS_01_2_1 <- forecast(fit_ADAM_ETS_01_2_1, h=12)
show(prev_ADAM_ETS_01_2_1)
plot(prev_ADAM_ETS_01_2_1)

#### ADAM ETS + SARIMA-

fitadam3_01_2_1 <- auto.adam(ts_train01_2_1,
                             model = "ZZZ",
                             lags = c(1, 12),
                             orders = list(ar = c(3,3), i = 1, ma = c(3,3)),
                             select = TRUE,
                             bootstrap = TRUE)
fitadam3_01_2_1

summary(fitadam3_01_2_1)

par(mfcol=c(2,2))
plot(fitadam3_01_2_1)
par(mfcol=c(1,1))
plot(fitadam3_01_2_1$residuals)

prev_AES_01_2_1 <- forecast(fitadam3_01_2_1,12)
show(prev_AES_01_2_1)
plot(prev_AES_01_2_1)

### .2.5. SSARIMA-

fit_SSARIMA_01_2_1 <- auto.ssarima(ts_train01_2_1, lags=c(1,12), orders=list(ar=c(3,3), i=(2), ma=c(3,3), select=TRUE))
summary(fit_SSARIMA_01_2_1)

par(mfcol=c(2,2))
plot(fit_SSARIMA_01_2_1)

par(mfcol=c(1,1))

plot(fit_SSARIMA_01_2_1$residuals)

prev_SSARIMA_01_2_1 <- forecast(fit_SSARIMA_01_2_1, h=12)
prev_SSARIMA_01_2_1
plot(prev_SSARIMA_01_2_1)

aic_ssarima <- AIC(fit_SSARIMA_01_2_1)
cat("AIC du modèle SSARIMA :", round(aic_ssarima, 2), "\n")

aicc_ssarima <- AICc(fit_SSARIMA_01_2_1)
cat("AICc du modèle SSARIMA :", round(aicc_ssarima, 2), "\n")


#### naïves-

pred_naive_01_2_1 <- naive(ts_train01_2_1, h=12)
show(pred_naive_01_2_1)
plot(pred_naive_01_2_1)




#### 3. évolution des prévisions----

ts_2023_01_2_1 <- window(tsIPC01_2_1_CVS_RJDmetra_corr_diff, start = c(2023, 1), end = c(2023, 12))

# Fonction mise à jour : transforme la date en format mois
extract_forecast_df <- function(fcast_obj, name) {
 df <- data.frame(
  date = as.Date(as.yearmon(time(fcast_obj$mean))),  # conversion en date
  value = as.numeric(fcast_obj$mean),
  model = name
 )
 df %>% slice_head(n = 12)
}

ts_forex13_01_2_1 <- ts(as.numeric(forex13_01_2_1), start = c(2023, 1), frequency = 12)

# 2. Construire un objet forecast manuellement (sans recalculer)
forecast_x13_01_2_1 <- structure(list(
 mean = ts_forex13_01_2_1,  # ici, un vrai ts avec start/end
 lower = matrix(NA, nrow = 12, ncol = 2, dimnames = list(NULL, c("80%", "95%"))),
 upper = matrix(NA, nrow = 12, ncol = 2, dimnames = list(NULL, c("80%", "95%"))),
 level = c(80, 95),
 x = NULL,
 method = "X13",
 fitted = NULL,
 residuals = NULL
), class = "forecast")

dfs_01_2_1 <- list(
 extract_forecast_df(prev_arima_01_2_1, "ARMA"),
 extract_forecast_df(prev_ar1_01_2_1, "AR(1)"),
 extract_forecast_df(prev_arP_01_2_1, "AR(P)"),
 extract_forecast_df(forecast_x13_01_2_1, "X13"),
 extract_forecast_df(forecast_hw01, "HoltWinters"),
 extract_forecast_df(prev_ADAM_ETS_01_2_1, "ADAM_ETS"),
 extract_forecast_df(prev_AES_01_2_1, "ADAM_ETS+SARIMA"),
 extract_forecast_df(prev_SSARIMA_01_2_1, "SSARIMA"),
 extract_forecast_df(pred_naive_01_2_1, "Naïve")
)

#Je vais comparer aveclesvaleur reel grace a ts_test01_2_1
observed_df_01_2_1 <- data.frame(
 date = as.Date(as.yearmon(time(ts_test01_2_1))),
 value = as.numeric(ts_test01_2_1),
 model = "Observée")

# Combine toutes les prévisions et l'observée
df_all_01_2_1 <- bind_rows(dfs_01_2_1, observed_df_01_2_1)
df_all_01_2_1$date <- as.Date(df_all_01_2_1$date, format = "%Y-%m-%d")
library(ggplot2)

# 1. Graphe simple avec axe en numérique
ggplot(df_all_01_2_1, aes(x = as.numeric(date), y = value, color = model)) +
 geom_line(size = 1.2) +
 scale_x_continuous(
  breaks = as.numeric(df_all_01_2_1$date[1:12]),
  labels = format(df_all_01_2_1$date[1:12], "%b %Y")
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
ggplot(df_all_01_2_1, aes(x = date, y = value, color = model, linetype = model)) +
 geom_line(size = 1.2) +
 labs(
  title = "Prévisions - 01.2.1 : Café, thé et cacao - Sur 12 mois (2023) par modèle",
  x = "Mois", y = "Valeur prévue", color = "Modèle", linetype = "Modèle"
 ) +
 scale_color_manual(values = c(
  "ADAM_ETS" = "red",
  "ADAM_ETS+SARIMA" = "orange",
  "HoltWinters" = "yellow",
  "Naïve" = "black",
  "SSARIMA" = "purple",
  "X13" = "darkblue",
  "ARMA" = "forestgreen",
  "AR(1)" = "brown",
  "AR(P)" = "darkcyan",
  "Observée" = "black"
 )) +
 scale_linetype_manual(values = c(
  "ADAM_ETS" = "solid",
  "ADAM_ETS+SARIMA" = "solid",
  "HoltWinters" = "solid",
  "Naïve" = "solid",
  "SSARIMA" = "solid",
  "X13" = "dashed",
  "ARMA" = "dotted",
  "AR(1)" = "dotted",
  "AR(P)" = "dotted",
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

#### 4 qualité de prevision -----

## MSE & R²OOS
## MSE & R²OOS avec le modèle Naïf comme référence

# Observations réelles
actual_values01_2_1 <- ts_test01_2_1

# Liste des prévisions pour chaque modèle
forecasts_01_2_1 <- list(
 prev_arima_01_2_1$mean,
 prev_ar1_01_2_1$mean,
 prev_arP_01_2_1$mean,
 forecast_x13_01_2_1$mean,
 forecast_hw01$mean,
 prev_ADAM_ETS_01_2_1$mean,
 prev_AES_01_2_1$mean,
 prev_SSARIMA_01_2_1$mean,
 pred_naive_01_2_1$mean
)


# Fonction MSE + R²OOS avec benchmark explicite
calculate_metrics_01_2_1 <- function(actual, forecast, benchmark_forecast) {
 mse <- mean((actual - forecast)^2)
 sst <- sum((actual - benchmark_forecast)^2)
 sse <- sum((actual - forecast)^2)
 r2oos <- 1 - (sse / sst)
 return(list(mse = mse, r2oos = r2oos))
}

# Naïve utilisé comme benchmark
benchmark_forecast <- as.numeric(pred_naive_01_2_1$mean)

# Calcul des métriques
metrics <- lapply(forecasts_01_2_1, function(fcast) {
 calculate_metrics_01_2_1(as.numeric(actual_values01_2_1), as.numeric(fcast), benchmark_forecast)
})

# Data frame de résultats
metrics_df_01_2_1 <- as.data.frame(do.call(rbind, metrics))
rownames(metrics_df_01_2_1) <- c("ARMA", "AR(1)", "AR(P)","X13",  "HoltWinters","ADAM_ETS", "ADAM_ETS+SARIMA", "SSARIMA", 
                                 "Naïve")

print(metrics_df_01_2_1)

## CSPE

# Fonction CSPE
calculate_cspe <- function(actual, forecast) {
 errors <- (actual - forecast)^2
 cspe <- cumsum(errors)
 return(cspe)
}

# CSPE pour chaque modèle
cspe_list <- lapply(forecasts_01_2_1, function(fcast) {
 calculate_cspe(as.numeric(actual_values01_2_1), as.numeric(fcast))
})

# Création du data.frame CSPE
cspe_df <- do.call(cbind, cspe_list)
colnames(cspe_df) <- rownames(metrics_df_01_2_1)
cspe_df <- cbind(Date = as.Date(time(actual_values01_2_1)), cspe_df)
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
  "ARMA" = "forestgreen", "AR(1)" = "brown", "AR(P)" = "darkcyan","X13" = "darkblue",
  "HoltWinters" = "yellow", "ADAM_ETS" = "red", "ADAM_ETS+SARIMA" = "orange",
  "SSARIMA" = "purple", "Naïve" = "black"
 )) +
 scale_linetype_manual(values = c(
  "ARMA" = "dashed", "AR(1)" = "dotted", "AR(P)" = "solid","X13" = "dashed",
  "HoltWinters" = "solid", "ADAM_ETS" = "solid", "ADAM_ETS+SARIMA" = "solid",
  "SSARIMA" = "solid", "Naïve" = "dotdash"
 )) +
 
 
 scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
 theme_minimal(base_size = 13) +
 theme(axis.text.x = element_text(angle = 45, hjust = 1),
       legend.position = "bottom",
       legend.title = element_text(face = "bold"),
       plot.title = element_text(face = "bold"))



##### 5 Test de précision-----

# Définir la période de temps
start_date <- c(2023, 1)
end_date <- c(2023, 12)

# Fonction pour ajuster les séries temporelles
adjust_time_series <- function(ts_data) {
 return(window(ts_data, start = start_date, end = end_date))
}

# Ajuster les séries temporelles pour chaque modèle
for_observed <- adjust_time_series(ts_2023_01_2_1)
for_ARMA     <- adjust_time_series(prev_arima_01_2_1$mean)
for_AR1      <- adjust_time_series(prev_ar1_01_2_1$mean)
for_ARP      <- adjust_time_series(prev_arP_01_2_1$mean)
for_X13      <- adjust_time_series(forecast_x13_01_2_1$mean)
for_HW       <- adjust_time_series(forecast_hw01$mean)
for_ADAM_ETS     <- adjust_time_series(prev_ADAM_ETS_01_2_1$mean)
for_AES      <- adjust_time_series(prev_AES_01_2_1$mean)
for_SSARIMA  <- adjust_time_series(prev_SSARIMA_01_2_1$mean)
for_NAIVE    <- adjust_time_series(pred_naive_01_2_1$mean)



# Vérifiez que toutes les séries temporelles ont la bonne longueur

ts_2023_01_2_1 <- window(tsIPC01_2_1_CVS_RJDmetra_corr_diff, start = c(2023, 1), end = c(2023, 12))
ts_test01_2_1 <- window(tsIPC01_2_1_CVS_RJDmetra_corr_diff, start = c(2023, 1), end = c(2023, 12))
print(length(ts_2023_01_2_1))

print(length(for_observed))
print(length(for_ARMA))
print(length(for_AR1))
print(length(for_ARP))
print(length(for_X13))
print(length(for_HW))
print(length(for_ADAM_ETS))
print(length(for_AES))
print(length(for_SSARIMA))
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

# Calculer le test DM

dm.test(error_NAIVE, error_ARMA, h = length(for_observed))
dm.test(error_NAIVE, error_AR1, h = length(for_observed))
dm.test(error_NAIVE, error_ARP, h = length(for_observed))
dm.test(error_NAIVE, error_X13, h = length(for_observed))
dm.test(error_NAIVE, error_HW, h = length(for_observed))
dm.test(error_NAIVE, error_ADAM_ETS, h = length(for_observed))
dm.test(error_NAIVE, error_AES, h = length(for_observed))
dm.test(error_NAIVE, error_SSARIMA, h = length(for_observed))


# Test de Diebold-Mariano avec h = 1
dm.test(error_NAIVE, error_ARMA, h = 1)
dm.test(error_NAIVE, error_AR1, h = 1)
dm.test(error_NAIVE, error_ARP, h = 1)
dm.test(error_NAIVE, error_X13, h = 1)
dm.test(error_NAIVE, error_HW, h = 1)
dm.test(error_NAIVE, error_ADAM_ETS, h = 1)
dm.test(error_NAIVE, error_AES, h = 1)
dm.test(error_NAIVE, error_SSARIMA, h = 1)


## le meilleur modèle est AR1 ou arma (plus ar1)


#-------------- 01.2.2 - Eaux minérales, boissons rafraîchissantes, jus de fruits et de légumes ---------

ts_Eauxminerales_boissonsrafraîchissantes <- ts(dfcomp$Eauxminerales_boissonsrafraîchissantes, start = c(2010, 1), frequency = 12)

autoplot(ts_Eauxminerales_boissonsrafraîchissantes) +
 ggtitle("IPC - Coicop :  01.2.2 - Eaux minérales, boissons et jus ") +
 xlab("Date") + ylab("IPC - 01.2.2")+
 theme_minimal()+
 geom_line(color = "blue")


### - tests (CVS stationnaire)

combined_test(ts_Eauxminerales_boissonsrafraîchissantes)

seasdum(ts_Eauxminerales_boissonsrafraîchissantes) #Seasonal Dummies

adf.test(ts_Eauxminerales_boissonsrafraîchissantes)
kpss.test(ts_Eauxminerales_boissonsrafraîchissantes)

xss <-diff(ts_Eauxminerales_boissonsrafraîchissantes)
adf.test(xss)
kpss.test(xss) # La serie est stationnaire ap diff :)

myregx13_01_2_2 <- regarima_x13(ts_Eauxminerales_boissonsrafraîchissantes, spec ="RG5c")
s_transform(myregx13_01_2_2)  # test log/level
# Pas de transformation en log.

### - CVS-

# Avec RJDmetra
s_transform(myregx13_01_2_2)
myspec <- x13_spec("RSA5c")
mysax13 <- x13(ts_Eauxminerales_boissonsrafraîchissantes, myspec) # SCV de RJGDmetra
#plot(mysax13$final) # JTM

# Extraire la série désaisonnalisée (CVS)
tsIPC01_2_2_CVS_RJDmetra <- ts(mysax13$final$series[, "sa"],
                               start = start(ts_Eauxminerales_boissonsrafraîchissantes), frequency = frequency(ts_Eauxminerales_boissonsrafraîchissantes))

dfcomp_01_2_2 <- data.frame(
 Date = as.Date(as.yearmon(time(ts_Eauxminerales_boissonsrafraîchissantes)), frac = 1),
 Originale = as.numeric(ts_Eauxminerales_boissonsrafraîchissantes),
 CVS_RJD = as.numeric(tsIPC01_2_2_CVS_RJDmetra)
)

### Outliers-

outliers01_2_2 <- tso(tsIPC01_2_2_CVS_RJDmetra)
print(outliers01_2_2)

plot(outliers01_2_2)
show(outliers01_2_2)

tsIPC01_2_2_CVS_RJDmetra_corr <- outliers$yadj
dfcomp_01_2_2$CVS_RJD_CORR <- as.numeric(tsIPC01_2_2_CVS_RJDmetra_corr)

### Virification de la stationnarité-

combined_test(tsIPC01_2_2_CVS_RJDmetra_corr)
seasdum(tsIPC01_2_2_CVS_RJDmetra_corr) #Seasonal Dummies
# Toujours CVS logique

adf.test(tsIPC01_2_2_CVS_RJDmetra_corr)
kpss.test(tsIPC01_2_2_CVS_RJDmetra_corr)

tsIPC01_2_2_CVS_RJDmetra_corr_diff <- diff(tsIPC01_2_2_CVS_RJDmetra_corr)
adf.test(tsIPC01_2_2_CVS_RJDmetra_corr_diff)
kpss.test(tsIPC01_2_2_CVS_RJDmetra_corr_diff)


autoplot(tsIPC01_2_2_CVS_RJDmetra_corr_diff) +
 ggtitle("IPC - Coicop : 01.2.2 - serie différenciée") +
 xlab("Date") + ylab("IPC 01.2.2")+
 theme_minimal()+
 geom_line(color = "blue")


dfcomp_01_2_2 <- dfcomp_01_2_2[-1, ]  # on retire la première ligne
dfcomp_01_2_2$CVS_RJD_CORR_DIFF <- as.numeric(tsIPC01_2_2_CVS_RJDmetra_corr_diff)

#### 2. Estimation des modèles-----

# a.  Estimer et commenter les paramètres des modèles AR(1), AR(p) et ARIMA(p,d,q) et de la méthode LED Holt-Winters, ADAM ETS, ADAM ETS ARIMA, SSARIMA et CES
# b.  Paramètres : présenter sous forme de tableau les paramètres des modèles précédents. Déterminer et commenter le meilleur modèle d’après les critères AIC et AICc

# On entraine nos modele jusqu'a 2022 pour faire la prevision en 2023.

ts_train01_2_2 <- window(tsIPC01_2_2_CVS_RJDmetra_corr_diff, start = c(2010, 2), end = c(2022, 12))
ts_test01_2_2 <- window(tsIPC01_2_2_CVS_RJDmetra_corr_diff, start = c(2023, 1), end = c(2023, 12))


### 2. Estimation du modèle

###. 2.1 AR/ARMA-

### 2.1.1 ARMA-

modele_arima_01_2_2 <- auto.arima(ts_train01_2_2, stepwise = FALSE, approximation = FALSE)

summary(modele_arima_01_2_2)

j <- ncol(modele_arima_01_2_2$var.coef)
tstat <- matrix(nrow=j, ncol=1)
for(i in 1:j)
{
 tstat[i,1] <- modele_arima_01_2_2$coef[i]/sqrt(modele_arima_01_2_2$var.coef[i,i])
}
tstat

# Graphique des valeurs ajustées
plot(modele_arima_01_2_2$fitted)
prev_arima_01_2_2 <- forecast(modele_arima_01_2_2, h=12)
prev_arima_01_2_2
plot(prev_arima_01_2_2)

aic_arma <- AIC(modele_arima_01_2_2)
cat("AIC du modèle SSARIMA :", round(aic_arma, 2), "\n")

aicc_arma <- AICc(modele_arima_01_2_2)
cat("AICc du modèle SSARIMA :", round(aicc_arma, 2), "\n")

## 2.1.2 AR(1)-

modele_ar1_01_2_2 <- Arima(ts_train01_2_2, order = c(1, 0, 0), include.mean = TRUE)

summary(modele_ar1_01_2_2)

j <- ncol(modele_ar1_01_2_2$var.coef)
tstat <- matrix(nrow=j, ncol=1)
for(i in 1:j)
{
 tstat[i,1] <- modele_ar1_01_2_2$coef[i]/sqrt(modele_ar1_01_2_2$var.coef[i,i])
}
tstat

fitted_ar1 <- ts_train01_2_2 - residuals(modele_ar1_01_2_2)
plot(fitted_ar1, type = "l", col = "blue", main = "Valeurs ajustées - AR(1)", ylab = "Prévisions")

prev_ar1_01_2_2 <- forecast(modele_ar1_01_2_2, h = 12)
prev_ar1_01_2_2
plot(prev_ar1_01_2_2)

aic_ar1 <- AIC(modele_ar1_01_2_2)
cat("AIC du modèle AR(1) :", round(aic_ar1, 2), "\n")

aicc_ar1 <- AICc(modele_ar1_01_2_2)
cat("AICc du modèle AR(1) :", round(aicc_ar1, 2), "\n")

### 2.1.3 AR(P)-
modele_arp_01_2_2 <- auto.arima(ts_train01_2_2,
                                d = 0,
                                max.p = 10,        # tu peux ajuster ce maximum
                                max.q = 0,         # pas de MA
                                stationary = TRUE,
                                seasonal = FALSE,
                                stepwise = FALSE,
                                approximation = FALSE)


# Résumé du modèle
summary(modele_arp_01_2_2)

# t-stat des coefficients
j <- length(modele_arp_01_2_2$coef)
tstat_arp <- numeric(j)
for(i in 1:j) {
 tstat_arp[i] <- modele_arp_01_2_2$coef[i] / sqrt(modele_arp_01_2_2$var.coef[i, i])
}
tstat_arp

fitted_arP <- ts_train01_2_2 - residuals(modele_arp_01_2_2)
plot(fitted_arP, type = "l", col = "blue", main = "Valeurs ajustées - AR(1)", ylab = "Prévisions")

prev_arP_01_2_2 <- forecast(modele_arp_01_2_2, h = 12)
prev_arP_01_2_2
plot(prev_arP_01_2_2)

aic_arp <- AIC(modele_arp_01_2_2)
cat("AIC du modèle SSARIMA :", round(aic_arp, 2), "\n")


aicc_arp <- AICc(modele_arp_01_2_2)
cat("AICc du modèle SSARIMA :", round(aicc_arp, 2), "\n")

#### X13-

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


#### stl-

decomp = stlm(ts_train01_2_2, s.window="periodic")
show(decomp)
#

fitstl_01_2_2 = stlm(ts_train01_2_2)
prevstl_01_2_2 <- forecast(fitstl_01_2_2,12) #période d'une année

plot(prevstl_01_2_2)
summary(prevstl_01_2_2)


### .2.2 HoltWinters--

WH_add_01_2_2 <- HoltWinters(ts_train01_2_2, gamma = FALSE)
WH_add_01_2_2
show(WH_add_01_2_2)
WH_add_01_2_2$coefficients
plot(WH_add_01_2_2)
forecast_hw01 <- forecast(WH_add_01_2_2, h = 12)
plot(forecast_hw01)
forecast_hw01

# Nombre d'observations
res <- residuals(WH_add_01_2_2)
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


#### ADAM ETS-

fit_ADAM_ETS_01_2_2 <- auto.adam(ts_train01_2_2,
                                 model = "ZZZ",
                                 lags = c(1, 12),
                                 silent = TRUE)          
# ZZZ car ne spécifie rien (tendance, saisonnalité, erreur)
fit_ADAM_ETS_01_2_2
summary(fit_ADAM_ETS_01_2_2)
par(mfcol=c(2,2))
plot(fit_ADAM_ETS_01_2_2)
par(mfcol=c(1,1))


plot(fit_ADAM_ETS_01_2_2$residuals)

prev_ADAM_ETS_01_2_2 <- forecast(fit_ADAM_ETS_01_2_2, h=12)
show(prev_ADAM_ETS_01_2_2)
plot(prev_ADAM_ETS_01_2_2)

#### ADAM ETS + SARIMA-

fitadam3_01_2_2 <- auto.adam(ts_train01_2_2,
                             model = "ZZZ",
                             lags = c(1, 12),
                             orders = list(ar = c(3,3), i = 1, ma = c(3,3)),
                             select = TRUE,
                             bootstrap = TRUE)
fitadam3_01_2_2

summary(fitadam3_01_2_2)

par(mfcol=c(2,2))
plot(fitadam3_01_2_2)
par(mfcol=c(1,1))

plot(fitadam3_01_2_2$residuals)

prev_AES_01_2_2 <- forecast(fitadam3_01_2_2,12)
show(prev_AES_01_2_2)
plot(prev_AES_01_2_2)

### .2.5. SSARIMA-

fit_SSARIMA_01_2_2 <- auto.ssarima(ts_train01_2_2, lags=c(1,12), orders=list(ar=c(3,3), i=(2), ma=c(3,3), select=TRUE))
summary(fit_SSARIMA_01_2_2)

par(mfcol=c(2,2))
plot(fit_SSARIMA_01_2_2)

par(mfcol=c(1,1))

plot(fit_SSARIMA_01_2_2$residuals)

prev_SSARIMA_01_2_2 <- forecast(fit_SSARIMA_01_2_2, h=12)
prev_SSARIMA_01_2_2
plot(prev_SSARIMA_01_2_2)

aic_ssarima <- AIC(fit_SSARIMA_01_2_2)
cat("AIC du modèle SSARIMA :", round(aic_ssarima, 2), "\n")

aicc_ssarima <- AICc(fit_SSARIMA_01_2_2)
cat("AICc du modèle SSARIMA :", round(aicc_ssarima, 2), "\n")


#### naïves-

pred_naive_01_2_2 <- naive(ts_train01_2_2, h=12)
show(pred_naive_01_2_2)
plot(pred_naive_01_2_2)




#### 3. évolution des prévisions----

ts_2023_01_2_2 <- window(tsIPC01_2_2_CVS_RJDmetra_corr_diff, start = c(2023, 1), end = c(2023, 12))

# Fonction mise à jour : transforme la date en format mois
extract_forecast_df <- function(fcast_obj, name) {
 df <- data.frame(
  date = as.Date(as.yearmon(time(fcast_obj$mean))),  # conversion en date
  value = as.numeric(fcast_obj$mean),
  model = name
 )
 df %>% slice_head(n = 12)
}

ts_forex13_01_2_2 <- ts(as.numeric(forex13_01_2_2), start = c(2023, 1), frequency = 12)

# 2. Construire un objet forecast manuellement (sans recalculer)
forecast_x13_01_2_2 <- structure(list(
 mean = ts_forex13_01_2_2,  # ici, un vrai ts avec start/end
 lower = matrix(NA, nrow = 12, ncol = 2, dimnames = list(NULL, c("80%", "95%"))),
 upper = matrix(NA, nrow = 12, ncol = 2, dimnames = list(NULL, c("80%", "95%"))),
 level = c(80, 95),
 x = NULL,
 method = "X13",
 fitted = NULL,
 residuals = NULL
), class = "forecast")

dfs_01_2_2 <- list(
 extract_forecast_df(prev_arima_01_2_2, "ARMA"),
 extract_forecast_df(prev_ar1_01_2_2, "AR(1)"),
 extract_forecast_df(prev_arP_01_2_2, "AR(P)"),
 extract_forecast_df(forecast_x13_01_2_2, "X13"),
 extract_forecast_df(forecast_hw01, "HoltWinters"),
 extract_forecast_df(prev_ADAM_ETS_01_2_2, "ADAM_ETS"),
 extract_forecast_df(prev_AES_01_2_2, "ADAM_ETS+SARIMA"),
 extract_forecast_df(prev_SSARIMA_01_2_2, "SSARIMA"),
 extract_forecast_df(pred_naive_01_2_2, "Naïve")
)

#Je vais comparer aveclesvaleur reel grace a ts_test01_2_2
observed_df_01_2_2 <- data.frame(
 date = as.Date(as.yearmon(time(ts_test01_2_2))),
 value = as.numeric(ts_test01_2_2),
 model = "Observée")

# Combine toutes les prévisions et l'observée
df_all_01_2_2 <- bind_rows(dfs_01_2_2, observed_df_01_2_2)
df_all_01_2_2$date <- as.Date(df_all_01_2_2$date, format = "%Y-%m-%d")
library(ggplot2)

# 1. Graphe simple avec axe en numérique
ggplot(df_all_01_2_2, aes(x = as.numeric(date), y = value, color = model)) +
 geom_line(size = 1.2) +
 scale_x_continuous(
  breaks = as.numeric(df_all_01_2_2$date[1:12]),
  labels = format(df_all_01_2_2$date[1:12], "%b %Y")
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
ggplot(df_all_01_2_2, aes(x = date, y = value, color = model, linetype = model)) +
 geom_line(size = 1.2) +
 labs(
  title = "Prévisions - 01.2.2 :  Eaux minérales boissons et jus - Sur 12 mois (2023) par modèle",
  x = "Mois", y = "Valeur prévue", color = "Modèle", linetype = "Modèle"
 ) +
 scale_color_manual(values = c(
  "ADAM_ETS" = "red",
  "ADAM_ETS+SARIMA" = "orange",
  "HoltWinters" = "yellow",
  "Naïve" = "black",
  "SSARIMA" = "purple",
  "X13" = "darkblue",
  "ARMA" = "forestgreen",
  "AR(1)" = "brown",
  "AR(P)" = "darkcyan",
  "Observée" = "black"
 )) +
 scale_linetype_manual(values = c(
  "ADAM_ETS" = "solid",
  "ADAM_ETS+SARIMA" = "solid",
  "HoltWinters" = "solid",
  "Naïve" = "solid",
  "SSARIMA" = "solid",
  "X13" = "dashed",
  "ARMA" = "dotted",
  "AR(1)" = "dotted",
  "AR(P)" = "dotted",
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

#### 4 qualité de prevision -----

## MSE & R²OOS
## MSE & R²OOS avec le modèle Naïf comme référence

# Observations réelles
actual_values01_2_2 <- ts_test01_2_2

# Liste des prévisions pour chaque modèle
forecasts_01_2_2 <- list(
 prev_arima_01_2_2$mean,
 prev_ar1_01_2_2$mean,
 prev_arP_01_2_2$mean,
 forecast_x13_01_2_2$mean,
 forecast_hw01$mean,
 prev_ADAM_ETS_01_2_2$mean,
 prev_AES_01_2_2$mean,
 prev_SSARIMA_01_2_2$mean,
 pred_naive_01_2_2$mean
)


# Fonction MSE + R²OOS avec benchmark explicite
calculate_metrics_01_2_2 <- function(actual, forecast, benchmark_forecast) {
 mse <- mean((actual - forecast)^2)
 sst <- sum((actual - benchmark_forecast)^2)
 sse <- sum((actual - forecast)^2)
 r2oos <- 1 - (sse / sst)
 return(list(mse = mse, r2oos = r2oos))
}

# Naïve utilisé comme benchmark
benchmark_forecast <- as.numeric(pred_naive_01_2_2$mean)

# Calcul des métriques
metrics <- lapply(forecasts_01_2_2, function(fcast) {
 calculate_metrics_01_2_2(as.numeric(actual_values01_2_2), as.numeric(fcast), benchmark_forecast)
})

# Data frame de résultats
metrics_df_01_2_2 <- as.data.frame(do.call(rbind, metrics))
rownames(metrics_df_01_2_2) <- c("ARMA", "AR(1)", "AR(P)","X13",  "HoltWinters","ADAM_ETS", "ADAM_ETS+SARIMA", "SSARIMA", 
                                 "Naïve")

print(metrics_df_01_2_2)

## CSPE

# Fonction CSPE
calculate_cspe <- function(actual, forecast) {
 errors <- (actual - forecast)^2
 cspe <- cumsum(errors)
 return(cspe)
}

# CSPE pour chaque modèle
cspe_list <- lapply(forecasts_01_2_2, function(fcast) {
 calculate_cspe(as.numeric(actual_values01_2_2), as.numeric(fcast))
})

# Création du data.frame CSPE
cspe_df <- do.call(cbind, cspe_list)
colnames(cspe_df) <- rownames(metrics_df_01_2_2)
cspe_df <- cbind(Date = as.Date(time(actual_values01_2_2)), cspe_df)
cspe_df <- as.data.frame(cspe_df)

# Reshape long format
cspe_df_long <- pivot_longer(cspe_df, cols = -Date, names_to = "Model", values_to = "CSPE")
cspe_df_long$Date <- as.Date(cspe_df_long$Date)

# Affichage CSPE
pp2_2CSPE <- ggplot(cspe_df_long, aes(x = Date, y = CSPE, color = Model, linetype = Model)) +
 geom_line(size = 0.8) +
 labs(title = "Cumulative Squared Prediction Errors (CSPE)",
      x = "Date", y = "CSPE", color = "Modèle", linetype = "Modèle") +
 scale_color_manual(values = c(
  "ARMA" = "forestgreen", "AR(1)" = "brown", "AR(P)" = "darkcyan","X13" = "darkblue",
  "HoltWinters" = "yellow", "ADAM_ETS" = "red", "ADAM_ETS+SARIMA" = "orange",
  "SSARIMA" = "purple", "Naïve" = "black"
 )) +
 scale_linetype_manual(values = c(
  "ARMA" = "dashed", "AR(1)" = "dotted", "AR(P)" = "solid","X13" = "dashed",
  "HoltWinters" = "solid", "ADAM_ETS" = "solid", "ADAM_ETS+SARIMA" = "solid",
  "SSARIMA" = "solid", "Naïve" = "dotdash"
 )) +
 
 
 scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
 theme_minimal(base_size = 13) +
 theme(axis.text.x = element_text(angle = 45, hjust = 1),
       legend.position = "bottom",
       legend.title = element_text(face = "bold"),
       plot.title = element_text(face = "bold"))

pp2_2CSPE

##### 5 Test de précision-----

# Définir la période de temps
start_date <- c(2023, 1)
end_date <- c(2023, 12)

# Fonction pour ajuster les séries temporelles
adjust_time_series <- function(ts_data) {
 return(window(ts_data, start = start_date, end = end_date))
}

# Ajuster les séries temporelles pour chaque modèle
for_observed <- adjust_time_series(ts_2023_01_2_2)
for_ARMA     <- adjust_time_series(prev_arima_01_2_2$mean)
for_AR1      <- adjust_time_series(prev_ar1_01_2_2$mean)
for_ARP      <- adjust_time_series(prev_arP_01_2_2$mean)
for_X13      <- adjust_time_series(forecast_x13_01_2_2$mean)
for_HW       <- adjust_time_series(forecast_hw01$mean)
for_ADAM_ETS     <- adjust_time_series(prev_ADAM_ETS_01_2_2$mean)
for_AES      <- adjust_time_series(prev_AES_01_2_2$mean)
for_SSARIMA  <- adjust_time_series(prev_SSARIMA_01_2_2$mean)
for_NAIVE    <- adjust_time_series(pred_naive_01_2_2$mean)



# Vérifiez que toutes les séries temporelles ont la bonne longueur

ts_2023_01_2_2 <- window(tsIPC01_2_2_CVS_RJDmetra_corr_diff, start = c(2023, 1), end = c(2023, 12))
ts_test01_2_2 <- window(tsIPC01_2_2_CVS_RJDmetra_corr_diff, start = c(2023, 1), end = c(2023, 12))
print(length(ts_2023_01_2_2))

print(length(for_observed))
print(length(for_ARMA))
print(length(for_AR1))
print(length(for_ARP))
print(length(for_X13))
print(length(for_HW))
print(length(for_ADAM_ETS))
print(length(for_AES))
print(length(for_SSARIMA))
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

# Calculer le test DM

dm.test(error_NAIVE, error_ARMA, h = length(for_observed))
dm.test(error_NAIVE, error_AR1, h = length(for_observed))
dm.test(error_NAIVE, error_ARP, h = length(for_observed))
dm.test(error_NAIVE, error_X13, h = length(for_observed))
dm.test(error_NAIVE, error_HW, h = length(for_observed))
dm.test(error_NAIVE, error_ADAM_ETS, h = length(for_observed))
dm.test(error_NAIVE, error_AES, h = length(for_observed))
dm.test(error_NAIVE, error_SSARIMA, h = length(for_observed))


# Test de Diebold-Mariano avec h = 1
dm.test(error_NAIVE, error_ARMA, h = 1)
dm.test(error_NAIVE, error_AR1, h = 1)
dm.test(error_NAIVE, error_ARP, h = 1)
dm.test(error_NAIVE, error_X13, h = 1)
dm.test(error_NAIVE, error_HW, h = 1)
dm.test(error_NAIVE, error_ADAM_ETS, h = 1)
dm.test(error_NAIVE, error_AES, h = 1)
dm.test(error_NAIVE, error_SSARIMA, h = 1)


## le meilleur modèle est X13

# Ici on fait des graphiques 


ts_Taux_directeur <- ts(df_GB$Taux_directeur, start = c(2010, 1), frequency = 12)
ts_IPP <- ts(df_GB$IPP, start = c(2010, 1), frequency = 12)
ts_cout_horaire <- ts(df_GB$cout_horaire, start = c(2010, 1), frequency = 12)
ts_consommation_ménages_alim <- ts(df_GB$consommation_ménages_alim, start = c(2010, 1), frequency = 12)
ts_confiance_menages <- ts(df_GB$confiance_menages, start = c(2010, 1), frequency = 12)
ts_IPPAP <- ts(df_GB$IPPAP, start = c(2010, 1), frequency = 12)

ts_Inflation_US <- ts(df_ARX$Inflation_US, start = c(2010, 1), frequency = 12)


autoplot(ts_Taux_directeur) +
 ggtitle("Taux directeur de la BCE - (2010-2023)") +
 xlab("Date") + ylab("Taux_directeur ")+
 theme_minimal()+
 geom_line(color = "blue")

autoplot(ts_IPP) +
 ggtitle("Indices de prix de production et d’importation - (2010-2023)") +
 xlab("Date") + ylab("IPP ")+
 theme_minimal()+
 geom_line(color = "blue")

autoplot(ts_cout_horaire) +
 ggtitle("Coût horaire du travail - (2010-2023)") +
 xlab("Date") + ylab("cout_horaire ")+
 theme_minimal()+
 geom_line(color = "blue")

autoplot(ts_consommation_ménages_alim) +
 ggtitle("Consommation des ménages en biens alimentaires - (2010-2023)") +
 xlab("Date") + ylab("consommation_ménages_alim ")+
 theme_minimal()+
 geom_line(color = "blue")

autoplot(ts_confiance_menages) +
 ggtitle("Indice de confiance des ménages - (2010-2023)") +
 xlab("Date") + ylab("confiance_menages ")+
 theme_minimal()+
 geom_line(color = "blue")

autoplot(ts_IPPAP) +
 ggtitle("Indice des prix agricoles à la production - (2010-2023)") +
 xlab("Date") + ylab("IPPAP ")+
 theme_minimal()+
 geom_line(color = "blue")

autoplot(ts_Inflation_US) +
 ggtitle("Indice des prix à la consommation aux États-Unis (IPC) - (2010-2023)") +
 xlab("Date") + ylab("IPPAP ")+
 theme_minimal()+
 geom_line(color = "blue")

ts_Climat <- ts(df_ARX$Climat, start = c(2010, 1), frequency = 12)
ts_Geopolitical_Risk <- ts(df_ARX$Geopolitical_Risk, start = c(2010, 1), frequency = 12)
ts_FAO_Food_Index <- ts(df_ARX$FAO_Food_Index, start = c(2010, 1), frequency = 12)
ts_PrixMondialGaz <- ts(df_ARX$PrixMondialGaz, start = c(2010, 1), frequency = 12)
ts_Imporation_combustibles <- ts(df_ARX$Imporation_combustibles, start = c(2010, 1), frequency = 12)
ts_Tx_change_EUR_USD <- ts(df_ARX$Tx_change_EUR_USD, start = c(2010, 1), frequency = 12)
ts_Prix_Petrole <- ts(df_ARX$Prix_Petrole, start = c(2010, 1), frequency = 12)


stat_ts_Inflation_US = basicStats(ts_Inflation_US)
show(stat_ts_confiance_menages)

# Normalité
shapiro.test(ts_Inflation_US)

# Boîte à moustache
boxplot(
 ts_Inflation_US,
 main = "Boîte à moustaches de la série",
 ylab = "Valeurs",
 col = "lightgreen"
)
##---

boxplot(
 ts_IPP,
 main = "Boîte à moustaches de la série",
 ylab = "Valeurs",
 col = "lightgreen"
)

boxplot(
 ts_Taux_directeur,
 main = "Boîte à moustaches de la série",
 ylab = "Valeurs",
 col = "lightgreen"
)

boxplot(
 ts_cout_horaire,
 main = "Boîte à moustaches de la série",
 ylab = "Valeurs",
 col = "lightgreen"
)

boxplot(
 ts_consommation_ménages_alim,
 main = "Boîte à moustaches de la série",
 ylab = "Valeurs",
 col = "lightgreen"
)

boxplot(
 ts_confiance_menages,
 main = "Boîte à moustaches de la série",
 ylab = "Valeurs",
 col = "lightgreen"
)

boxplot(
 ts_IPPAP,
 main = "Boîte à moustaches de la série",
 ylab = "Valeurs",
 col = "lightgreen"
)
### ---- ATT -----

boxplot(
 ts_Climat,
 main = "Boîte à moustaches de la série",
 ylab = "Valeurs",
 col = "lightgreen"
)

boxplot(
 ts_Geopolitical_Risk,
 main = "Boîte à moustaches de la série",
 ylab = "Valeurs",
 col = "lightgreen"
)

boxplot(
 ts_Inflation_US,
 main = "Boîte à moustaches de la série",
 ylab = "Valeurs",
 col = "lightgreen"
)

# Boîte à moustache
boxplot(
 ts_FAO_Food_Index,
 main = "Boîte à moustaches de la série",
 ylab = "Valeurs",
 col = "lightgreen"
)

boxplot(
 ts_PrixMondialGaz,
 main = "Boîte à moustaches de la série",
 ylab = "Valeurs",
 col = "lightgreen"
)

boxplot(
 ts_Imporation_combustibles,
 main = "Boîte à moustaches de la série",
 ylab = "Valeurs",
 col = "lightgreen"
)

boxplot(
 ts_Tx_change_EUR_USD,
 main = "Boîte à moustaches de la série",
 ylab = "Valeurs",
 col = "lightgreen"
)

boxplot(
 ts_Prix_Petrole,
 main = "Boîte à moustaches de la série",
 ylab = "Valeurs",
 col = "lightgreen"
)


ts_Climat_CVS_diff
ts_Geopolitical_Risk
ts_Inflation_US_CVS_log_Double_diff
ts_FAO_Food_Index_diff
ts_PrixMondialGaz_diff
ts_Imporation_combustibles_CVS_diff
ts_Tx_change_EUR_USD_diff
ts_Prix_Petrole_diff
ts_train01


combined_test(ts_Prix_Petrole_diff)
seasdum(ts_Prix_Petrole_diff)
adf.test(ts_Prix_Petrole_diff)
kpss.test(ts_Prix_Petrole_diff)








