# Project-7-Electricity-Prices-Forecasting

# Abstract
Much of the electricity prices forecasting has been presented for short-term forecasting horizon.
However, forecasting electricity prices for longer forecasting horizons is becoming more important
especially for risk management, investment planning processes and derivatives pricing.
The research presented in this dissertation lies in the area of electricity prices forecasting. In particular,
we study the performance of various forecasting models when applied to electricity prices including
TBATS, STLM, DR, GAM, GAMM and RF model. We focus on analysing the performance
of these models for different forecasting horizons. The analyses are performed in R programming
language.

We study the electricity prices forecasting for three forecasting horizons: day-ahead, week-ahead
and month-ahead. We investigate the impact of calendar effects on forecasting the electricity prices.
Calendar effects include hour of the day, day of the week, day of the year, month of the year and
whether the day is a normal weekday, a weekend or a public holiday. Historical electricity prices
is our main predictor. For each model, we use different calendar adjustments and mathematical
transformations on the data to make the forecasting simpler. Then we examine the performance of
each model for the three forecasting horizons. We evaluate the performance of each model using
time series cross validation, rolling forecasting origin technique. We measure the forecast accuracy
in terms of root mean square error (RMSE) and mean absolute error (MAE).

Our results show that the GAMM and GAM models have the best performance in day-ahead
forecasting as well as week-ahead forecasting in terms of RMSE and MAE. While other models
have comparable performance. As we increase the forecasting horizon, the forecasting error also
increases. However, we find that the TBATS model is more robust against increasing forecasting
horizons when compared to other models. For month-ahead forecasting, the TBATS model has the
best forecasting accuracy compared to other models. The DR model comes after TBATS model
with a slight difference in MAE and RMSE.

Then we study the statistical significance of the difference in forecasting accuracy between all
considered forecasters using Diebold-Mariano test. The TBATS model proves to have statistically
significantly better accuracy than all other models for month-ahead forecasting. While in weekahead,
GAM and TBATS models have statistically significantly better forecasting accuracy compared
to other models. In day-ahead forecasting, the accuracy of the GAM model is statistically
significantly better than all models except for the TBATS model.
Our results confirm and improve on the previous investigations of the electricity prices forecasting.
In particular, we thoroughly investigate three forecasting horizons for six different models and
compare them rather than just focusing on day-ahead forecasting. In addition, we investigate the
differences in forecasting accuracies using the Diebold-Mariano test to confirm our results.
