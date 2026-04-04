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






