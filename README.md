# Retail Sales Volume Forecasting (TÜİK Data) 📈

This project performs a comprehensive time series analysis on monthly **Retail Sales Volume Indices (Food, Drinks, and Tobacco)** sourced from the **Turkish Statistical Institute (TÜİK)**. The primary objective was to identify the most statistically significant model to forecast market trends for the upcoming 5 months.

##  Project Overview
* **Data Source:** TÜİK (Turkish Statistical Institute)
* **Time Range:** Monthly data starting from 2010.
* **Variable:** Retail Sales Volume Index.
* **Tech Stack:** R, `forecast`, `fpp2`, `lmtest`, `tseries`.

##  Methodology & Analytical Process

A rigorous **Box-Jenkins** approach was followed, and multiple statistical methods were evaluated to ensure model robustness.

### 1. Exploratory Data Analysis (EDA) & Preprocessing
* **Stationarity:** The series was tested for stationarity using **ACF/PACF plots** and **Unit Root Tests**.
* **Differencing:** First-order difference (`d=1`) and seasonal difference (`D=1`) were applied to stabilize the mean and variance.

### 2. Model Evaluation (Benchmarking)
Several modeling techniques were tested and compared:

* **Decomposition Methods:** Both Additive and Multiplicative decomposition were tested.
    * *Result:* Rejected. Residuals failed the White Noise test (significant autocorrelation remained).
* **Harmonic Regression:** A regression model with trigonometric terms (Cosine) was built to capture seasonality (`y ~ t + cos`).
    * *Result:* Rejected due to Durbin-Watson test results indicating autocorrelation in residuals.
* **Exponential Smoothing (ETS):** Holt-Winters Additive (`AAA`) and Multiplicative (`MAM`) models were fitted.
* **ARIMA / SARIMA Modeling:** A comprehensive grid search was conducted, testing over **30 different hyperparameter combinations**.

### 3. Model Selection
Each candidate ARIMA model was evaluated based on:
1.  **Statistical Significance:** Significance of coefficients (`coeftest`).
2.  **Information Criteria:** Minimizing **AIC** and **BIC** values.
3.  **Residual Diagnostics:** **Box-Ljung Test** was applied to ensure residuals were White Noise.

| Model Candidate | AIC Score | Status |
| :--- | :--- | :--- |
| ARIMA(3,1,0)(1,1,0) | 854.33 | Significant |
| ARIMA(0,1,1)(1,1,1) | 839.34 | Significant |
| **ARIMA(0,1,1)(0,1,1)[12]** | **838.49** | **Selected (Best Fit)** |
| ARIMA(1,1,0)(0,1,1) | 847.68 | Significant |

** Winning Model:** `ARIMA(0,1,1)(0,1,1)[12]` was selected as the optimal model with the lowest AIC score (838.49) and clean residual diagnostics.

##  Results & Forecasting
A **5-month forecast** was generated using the selected SARIMA model. The results provide actionable insights into the short-term future of retail sales volume in the food and tobacco sector.

##  How to Run
1. Clone this repository.
2. Open the script file in **RStudio**.
3. Install the required libraries:
   ```r
   install.packages(c("forecast", "fpp2", "readxl", "lmtest"))
 Run the script to reproduce the stationarity tests, model summaries, and forecast plots.
 # Contact
Fatih Efe Kavakli

LinkedIn: https://www.linkedin.com/in/fatihefekavakl%C4%B1/

GitHub: @FatihEfeKavakli
