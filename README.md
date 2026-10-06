# Food CPI Forecasting in France: Disaggregated vs Direct Approaches

🇬🇧 **English version** | [🇫🇷 Version française](README_FR.md)

This research project investigates the most effective method for forecasting the **Consumer Price Index (CPI) for food and non-alcoholic beverages in France**.

The study compares two forecasting strategies:

- **Direct approach**: forecasting the aggregate food CPI series directly
- **Disaggregated approach (bottom-up)**: forecasting **11 individual CPI sub-components separately**, then aggregating their forecasts using official expenditure weights

Using monthly data from the **French National Institute of Statistics and Economic Studies (INSEE)** from **January 2010 to December 2022**, the models generate forecasts for **2023**.

## Key Findings

The results show that the **disaggregated approach outperforms the direct approach** in terms of forecasting accuracy.

By capturing component-specific dynamics and heterogeneous price movements, the disaggregated framework provides more accurate forecasts of aggregate food inflation.

The selected models were subsequently used in a **monthly rolling forecasting procedure over 2023**.

These results highlight the potential benefits of forecasting inflation at a more granular level before aggregating individual component forecasts.

---

## Project Structure

```bash
.
├── Aggregations_forecasts_best_models_each_series.R
├── Best_model_for_each_comp.R
├── Graphiques.R
├── Site_R-shiny.R
├── data/
├── outputs/
└── README.md
```

---

## Methodology

The disaggregated forecasting framework follows four main steps:

1. Selection of the best univariate forecasting model for each CPI sub-component  
   *(ARIMA, SARIMA, ETS, Holt-Winters, etc.)*
2. Monthly forecasts for each component over 2023
3. Aggregation using official INSEE expenditure weights
4. Evaluation against observed CPI values

The resulting bottom-up forecast is obtained by aggregating the forecasts of the individual CPI components.

---

## Results

- Higher forecasting accuracy than the direct aggregate approach
- Better representation of heterogeneous price dynamics across food categories
- Relevant for inflation monitoring and macroeconomic forecasting
- Easily extendable to other CPI divisions such as energy, housing, or services

---

## Project Documents

📘 **Full Master's Thesis — French**  
[Access the full thesis](https://drive.google.com/drive/folders/1ZBlRA6VUk9bnLSxVCU7je4cfsgF2AAi6)

📄 **Executive Summary / Research Note — English**  
[Access the English synthesis](https://drive.google.com/drive/folders/1K3jdt2gPLvLqt71xdkoyD7LqQgVXOX1k)

📄 **Synthèse du projet — Français**  
[Access the French synthesis](https://drive.google.com/drive/folders/1AnykxVroznBY1c_9djnH2cOqKiZ2MbyS)

---

## Technologies Used

- **R**
- ARIMA / SARIMA / ETS / Holt-Winters
- **RJDemetra**
- **Shiny**
- **ggplot2**
- Time-series diagnostics
- Outlier detection

---

## Academic Context

This project was completed as part of the **Master 1 ECAP (Econometrics and Applied Statistics)** program at **Nantes Université / IAE Nantes**.

The project focuses on **applied econometrics, inflation forecasting, time-series modeling, and forecast aggregation**.
