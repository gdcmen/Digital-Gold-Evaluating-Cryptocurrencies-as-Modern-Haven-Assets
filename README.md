# Digital-Gold-Evaluating-Cryptocurrencies-as-Modern-Haven-Assets

### Description:
This project explores whether cryptocurrencies, particularly Bitcoin and Ethereum, can act as safe-haven assets similar to gold. Using historical data and economic indicators (e.g., M1, M2, inflation rate, gold prices), I conducted a thorough analysis of cryptocurrencies’ stability and resilience using time series transformations, forecasting, and volatility assessments.

- Repository Structure
   1. Project Proposal (Phase 1)
       - *Code:* no code for this part, mainly documentation.
       - *Description:* This document, project_proposal.md, outlines the project’s aim, hypothesis, and potential data resources.
       - *Document content:*
          - Project title and description
          - Research questions and hypotheses
          - Identified data sources (public information related to cryptocurrencies, and finances related to the US) and preliminary analysis plan.

   2. Data Preparation (Phase 2)
       - *Code:* data_preparation.R
       - *Description:* This script cleans the dataset, removes unnecessary columns, and converts it into a time series format, saved as a CSV file.
       - *Key Packages:* dplyr, tsibble

   3. Time Series Analysis (Phase 3)
       - *Code:* time_series_analysis.R
       - *Description:* Plots each variable as a time series, decomposes trends, seasonality, and residuals, and applies Box-Cox transformations.
       - *Key Packages:* ggplot2, tsibble, fable

   4. Forecasting (Phase 4)
       - *Code:* forecasting_methods.R
       - *Description:* Implements Mean, Naïve, and Drift forecasting models over a 14-month period for selected assets.
       - *Key Packages:* fpp3, forecast

   5. ARIMA Modeling and Validation (Phase 5)
        - *Code:* arima_modeling.R
        - *Description:* Forecasts price movements using ARIMA, evaluates with RMSE, MAPE, and CVI. Stationarity and differencing are used to enhance model robustness.
        - *Key Packages:* forecast, tsibble, fable

   6. Cryptocurrency Volatility Index (CVI) Analysis
        - *Code:* cvi_analysis.R
        - *Description:* Analyzes the CVI to determine market sentiment and potential stability, showing how cryptocurrencies might evolve as stable assets.
        - *Key Packages:* fabletools, ggplot2

Conclusion:
- This repository offers a comprehensive look at cryptocurrencies’ potential as safe-haven assets through predictive and sentiment analysis. Each script provides insights into the evolving role of cryptocurrencies in financial markets, with detailed commentary and visualizations.
