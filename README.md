# COVID-19 Testing Forecast – Iceland (2020–2022)

This project analyzes and forecasts the number of daily COVID-19 tests in **Iceland** using **R** and time series modeling. We compare several forecasting techniques including:

- ✅ Exponential Smoothing (ETS)
- ✅ Seasonal ARIMA (SARIMA)
- ✅ Automatic ARIMA selection

Forecast accuracy, residual behavior, and transferability across countries are evaluated using the `fpp3` forecasting framework.

---

## 📁 Project Structure

- `scripts/` – Main R script (`covid19_forecast_iceland.R`) with full workflow
- `data/` – Contains source data ( OWID dataset)
- `results/` – Plots and CSVs summarizing model results
- `figures/` – Decompositions, ACF/PACF, and residual diagnostics

---

## 🧪 Methodology Overview

1. **Data Cleaning & Imputation**
   - Filtered OWID dataset for Iceland
   - Interpolated missing data using seasonal-aware methods

2. **Aggregation & Visualization**
   - Aggregated series to daily, weekly, and monthly resolutions
   - Visualized testing trends and seasonality

3. **Decomposition**
   - STL decomposition to extract trend/seasonal/remainder components

4. **Modeling**
   - Compared ETS(A,A,A), ETS(A,A,M), and ETS(M,N,M)
   - Compared SARIMA(3,0,1)(0,1,2)₇ vs Auto ARIMA
   - Residual diagnostics (ACF/PACF, Ljung–Box test)

5. **Forecasting**
   - Forecasted 4 weeks ahead using top-performing models
   - Evaluated RMSE, MAE, MAPE for out-of-sample accuracy

6. **Cross-Country Testing**
   - Applied Iceland-trained SARIMA to **UAE** data
   - Compared local vs. transferred model performance

---

## 📈 Forecast Accuracy Summary

| Model                        | RMSE  | MAPE  | Comments                          |
|-----------------------------|-------|-------|-----------------------------------|
| ETS(M,N,M) (Auto)           | 542   | 70.3% | Best in-sample accuracy           |
| SARIMA(3,0,1)(0,1,2)[7]     | 814   | 111%  | Strong residual diagnostics       |
| Auto ARIMA (0,1,2)(2,0,0)[7]| 594   | 85.1% | Best for UAE, risk of overfitting |

---

## 🔍 Key Insights

- Weekly seasonality is strong and persistent.
- Residual analysis revealed that **SARIMA** provides the best balance between fit and interpretability.
- The Iceland-tuned SARIMA model adapted well to **UAE** data, demonstrating cross-country robustness.

---

## 📦 Tools & Libraries

- `tidyverse`
- `fpp3` (includes `tsibble`, `feasts`, `fable`)
- `lubridate`, `ggplot2`

---

## 📌 Author

**Guadalupe Armenta Mendoza**  
🎓 MSc in Data Science & Analytics | MTU, Ireland  
📬 [lupitaarmen@gmail.com]  
🔗 [LinkedIn](https://linkedin.com/in/YOUR-LINK)

---

## 📄 License

MIT License – feel free to use and adapt this project with attribution.
