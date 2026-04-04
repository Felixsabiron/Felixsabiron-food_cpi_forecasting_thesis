# Food CPI Forecasting in France: Disaggregated vs Direct Approaches

This research project investigates the most effective method for forecasting the **Consumer Price Index (CPI) for food and non-alcoholic beverages in France**.

The study compares two forecasting strategies:

- **Direct approach**: modeling the aggregated food CPI series directly
- **Disaggregated approach**: forecasting **11 individual CPI sub-components separately**, then aggregating their weighted forecasts

Using monthly data from the **official French statistical system (INSEE)** from **January 2010 to December 2022**, the models generate forecasts for the year **2023**.

## Key Findings

The results demonstrate that the **disaggregated approach significantly outperforms the direct method** in terms of forecasting accuracy.

Because it better captures sector-specific dynamics and heterogeneous price movements, it was selected for a **monthly rolling-window forecasting procedure over 2023**.

This confirms that forecasting CPI at a more granular level can improve predictive performance for sectoral inflation analysis.

---

## Project Structure

```bash
.
├── Aggregations_orecasts_best_models_each_series.R
├── Best_model_for_each_comp.R
├── Graphiques.R
├── Site_R-shiny.R
├── data/
├── outputs/
└── README.md
```

---

## Methodology

The disaggregated framework follows four main steps:

1. Selection of the best univariate model for each CPI sub-component  
   *(ARIMA, SARIMA, Holt-Winters, etc.)*
2. Monthly forecasts for each component over 2023
3. Aggregation using official INSEE expenditure weights
4. Evaluation against observed CPI values

---

## Results

- Better forecasting performance than direct aggregation
- More robust to heterogeneous sectoral shocks
- Suitable for public institutions and macroeconomic monitoring
- Extendable to other CPI divisions (energy, housing, services)

---

## Project Documents

📘 **Full Master’s Thesis (French):**  
[Access the full thesis here](https://drive.google.com/drive/folders/1ZBlRA6VUk9bnLSxVCU7je4cfsgF2AAi6)

📄 **Executive Summary / Research Note (PDF):**  
[Access the synthesis note here](https://drive.google.com/drive/folders/1K3jdt2gPLvLqt71xdkoyD7LqQgVXOX1k)

---

## Technologies Used

- **R**
- Forecasting models: ARIMA / SARIMA / ETS / Holt-Winters
- **RJDemetra**
- **Shiny**
- Data visualization with **ggplot2**
- Time series diagnostics and outlier detection

---

## Academic Context

This project was completed as part of the **M1 ECAP program**, focusing on applied econometrics, inflation forecasting, and sectoral time series modeling.
