# Prévision de l’IPC alimentaire en France : approche désagrégée vs approche directe

[🇬🇧 English version](README.md) | 🇫🇷 **Version française**

Ce projet de recherche étudie la méthode la plus performante pour prévoir l’**Indice des Prix à la Consommation (IPC) des produits alimentaires et boissons non alcoolisées en France**.

L’étude compare deux stratégies de prévision :

- **Approche directe** : prévision directe de la série agrégée de l’IPC alimentaire
- **Approche désagrégée (bottom-up)** : prévision séparée de **11 sous-composantes de l’IPC**, puis agrégation des prévisions à l’aide des pondérations officielles

À partir de données mensuelles issues de l’**Institut national de la statistique et des études économiques (INSEE)**, couvrant la période de **janvier 2010 à décembre 2022**, les modèles produisent des prévisions pour l’année **2023**.

## Principaux résultats

Les résultats montrent que l’**approche désagrégée obtient de meilleures performances de prévision que l’approche directe**.

En prenant en compte les dynamiques propres à chaque sous-composante ainsi que l’hétérogénéité des évolutions de prix, cette méthode permet d’obtenir des prévisions plus précises de l’inflation alimentaire agrégée.

Les modèles sélectionnés ont ensuite été utilisés dans le cadre d’une **procédure de prévision mensuelle glissante sur l’année 2023**.

Ces résultats montrent l’intérêt de réaliser les prévisions à un niveau plus désagrégé avant de reconstruire l’indice agrégé.

---

## Structure du projet

```bash
.
├── Aggregations_forecasts_best_models_each_series.R
├── Best_model_for_each_comp.R
├── Graphiques.R
├── Site_R-shiny.R
├── data/
├── outputs/
├── README.md
└── README_FR.md
```

---

## Méthodologie

L’approche de prévision désagrégée suit quatre grandes étapes :

1. Sélection du meilleur modèle univarié pour chaque sous-composante de l’IPC  
   *(ARIMA, SARIMA, ETS, Holt-Winters, etc.)*
2. Prévision mensuelle de chaque composante sur l’année 2023
3. Agrégation des prévisions à l’aide des pondérations officielles de l’INSEE
4. Évaluation des prévisions par comparaison avec les valeurs observées de l’IPC

La prévision finale selon l’approche bottom-up est obtenue en agrégeant les prévisions individuelles des différentes composantes de l’IPC.

---

## Résultats

- Meilleure précision de prévision que l’approche directe
- Meilleure prise en compte de l’hétérogénéité des dynamiques de prix entre catégories alimentaires
- Méthode pertinente pour le suivi de l’inflation et la prévision macroéconomique
- Approche facilement extensible à d’autres divisions de l’IPC comme l’énergie, le logement ou les services

---

## Documents du projet

📘 **Mémoire complet — Français**  
[Accéder au mémoire complet](https://drive.google.com/drive/folders/1ZBlRA6VUk9bnLSxVCU7je4cfsgF2AAi6)

📄 **Executive Summary / Research Note — English**  
[Accéder à la synthèse en anglais](https://drive.google.com/drive/folders/1K3jdt2gPLvLqt71xdkoyD7LqQgVXOX1k)

📄 **Synthèse du projet — Français**  
[Accéder à la synthèse en français](https://drive.google.com/drive/folders/1AnykxVroznBY1c_9djnH2cOqKiZ2MbyS)

---

## Technologies utilisées

- **R**
- Modèles ARIMA / SARIMA / ETS / Holt-Winters
- **RJDemetra**
- **Shiny**
- **ggplot2**
- Diagnostics de séries temporelles
- Détection des valeurs aberrantes

---

## Contexte académique

Ce projet a été réalisé dans le cadre du **Master 1 ECAP — Économétrie et Statistiques Appliquées** à **Nantes Université / IAE Nantes**.

Il porte principalement sur l’**économétrie appliquée, la prévision de l’inflation, la modélisation des séries temporelles et l’agrégation de prévisions**.
