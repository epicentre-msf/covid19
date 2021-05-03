
## How we estimated the trends

---

Trends are estimated daily on the daily number of cases and deaths in JHU CSSE data observed over a period of  14 days. The most recent 2 days of data are not considered in the 14 day period, as counts may not yet be complete due to reporting delays. For example, if data is available as of the 16th of a given month, trends will be calculated on the period from 1st-14th of that month.

On the 14-day time-series we first carry a smoothing using a 3-day moving average. As a result, we obtain 12-day smoothed values for which we run a linear regression of the values in the natural logarithm scale using the following formula:

> `lm(ln(smoothed values) ~ 12 days)`

The standard error of the model was used to calculate the confidence intervals. 

Trends presented in the report were defined using the coefficients of the linear regression as follows:

- **Increasing:** an upward trend that was statistically significant; this means a positive coefficient and the 95% confidence interval (CI) is all above 0. 
- **Likely increasing:** an upward trend (positive coefficient) where the 95% CI includes <=0 values but the 80% CI does not.
- **Decreasing:** a downward trend that was statistically significant; this means a negative coefficient and the 95% CI is all above 0.
- **Likely decreasing:** an downward trend (negative coefficient) where the 95% CI includes >=0 values but the 80% CI does not.
- **Stable trend:** an upward or downward trend that was not statistically significant; this means an either positive or negative coefficient, but the 80% CI includes 0. 

To ensure reliable estimates we estimate the trend only if the cumulative number cases (or deaths) during the 14-day period was higher than 30.
