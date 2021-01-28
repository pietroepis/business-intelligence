# business-intelligence
"Business Intelligence per i Servizi Finanziari" Project - UniMiB

The project is focalized on data acquisition, visualization, exploratory analysis, predictive analysis and (simulated) portfolio management, web applet

Following stocks have been chosen:
| Corporation | Ticker | Sector |
|-------------|--------|--------|
| Starbucks Corp. | SBUX | Consumer Discretionary |
| McDonald's Corp. | MCD | Consumer Discretionary |
| American Express Co | AXP | Financials |
| Aon plc | AON | Financials |
| Coca Cola | KO | Consumer Staples |
| PepsiCo, Inc. | PEP | Consumer Staples |

**Data Summary**\
Data is loaded from _Yahoo! Finance_ and presented through simple charts

**Descriptive Analytics**\
Computation of simple and continuously componded returns, basic analysis of trends, peaks and valleys detection\
Generation of diagnostic plots for every stock (Histogram, PDF, Boxplot and Q-Q Plot)\
Calculation of descriptive statistic indices (mean, variance, standard deviation, quantiles, skewness, kurtosis)\
Computation of covariance matrix, correlation heatmap and pair-wise scatter plots\

**Predictive Analytics**\
Forecasting with **ARIMA** (Autoregressive Integrated Moving Average)\
Training on n = 80 months (_November 2010_ to _June 2017_), Test on m = 30 months (_July 2017_ to _December 2019_)\
Best parameters for _AR_ and _MA_ are defined with grid search, by assessing RMSE (Root Mean Square Error)\
Final prediction of l = 10 months (_January 2020_ to _October 2020_) and calculation of Markowitz Optimal Portfolio basing on predicted values


