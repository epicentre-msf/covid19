
## How we estimated the trends

---

Trends were estimated on the number of cases (or deaths) in ECDC data observed over a period of the last 12 days. 

On the 12-day time-series we first carry a smoothing using a 3-day moving average. As a result, we obtain 10-day smoothed values for which we run a linear regression of the values in the natural logarithm scale using the following formula:

> `lm(ln(smoothed values) ~ 10 days)`

The standard error of the model was used to calculate the confidence intervals. 

Trends presented in the report were defined using the coefficients of the linear regression as follows:

- **Increasing trend:** an upward trend that was statistically significant; this means a positive coefficient and the confidence intervals do not include 0. 
- **Declining trend:** a downward trend that was statistically significant; this means a negative coefficient and the confidence intervals do not include 0. 
- **Stable trend:** an upward or downward trend that was not statistically significant; this means an either positive or negative coefficient, but the confidence intervals include 0. 

To ensure reliable estimates we estimate the trend only if the cumulative number cases (or deaths) during the 12-day period was higher than 50.

We opted to model the slope on window of 12 days because it includes 3 generation time of 4 days. 

