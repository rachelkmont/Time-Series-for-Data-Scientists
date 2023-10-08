#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### DS5740: Week 4 | ARIMA Script ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# Load {fpp3}
library(fpp3)
library(readr)

# Set seed for reproducibility
set.seed(1234)

# Random noise
y_wn <- tsibble(sample = 1:50, wn = rnorm(50, 0, 1), index = sample)
y_wn %>% autoplot(wn) +
  labs(x = "", y = "")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# KPPS test
y_wn %>%
  features(wn, unitroot_kpss) %>%
  as.matrix()

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Original time series
aus_production %>%
  autoplot(Beer)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Need differencing?
aus_production %>%
  features(Beer, unitroot_kpss) %>%
  as.matrix()

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Number of seasonal differencings
aus_production %>%
  features(Beer, unitroot_nsdiffs) %>%
  as.matrix()

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Plot with seasonal differencing
aus_production %>%
  autoplot(difference(Beer, lag = 4))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Need differencing?
aus_production %>%
  features(difference(Beer, lag = 4), unitroot_kpss) %>%
  as.matrix()

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Number of differencings
aus_production %>%
  features(difference(Beer, lag = 4), unitroot_ndiffs) %>%
  as.matrix()

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Plot with seasonal + one differencing
aus_production %>%
  autoplot(difference(difference(Beer, lag = 4)))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# KPSS
aus_production %>%
  features(difference(difference(Beer, lag = 4)), unitroot_kpss) %>%
  as.matrix()

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Load data
nashville_housing <- read_csv("assignment4/nashville_housing.csv")

# Convert date
nashville_housing$date <- yearmonth(
  nashville_housing$date
)

# Convert to `tsibble`
housing_ts <- nashville_housing %>%
  as_tsibble(
    index = date
  )

# Plot time series
housing_ts %>%
  autoplot() +
  labs(
    y = "Number of Houses",
    x = "Year and Month"
  ) +
  scale_y_continuous(
    limits = c(0, 12500),
    breaks = seq(0, 12500, 2500)
  )

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Number of differencings
housing_ts %>%
  features(housing, unitroot_kpss) %>%
  as.matrix()

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Number of differencings
housing_ts %>%
  features(housing, unitroot_ndiffs) %>%
  as.matrix()

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Plot time series
housing_ts %>%
  autoplot() +
  labs(
    y = "Number of Houses",
    x = "Year and Month"
  ) +
  scale_y_continuous(
    limits = c(0, 12500),
    breaks = seq(0, 12500, 2500)
  )

# Plot time series
housing_ts %>%
  autoplot(difference(housing)) +
  labs(
    y = "Difference in\nNumber of Houses",
    x = "Year and Month"
  )

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Fit AR model
fit <- housing_ts %>%
  mutate(diff_housing = difference(housing)) %>%
  na.omit() %>%
  model(ar = AR(diff_housing))

# Report fit
report(fit)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Plot autocorrelations
housing_ts %>%
  gg_tsdisplay(difference(housing), plot_type = "partial")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Fit AR model
fit <- housing_ts %>%
  mutate(diff_housing = difference(housing)) %>%
  na.omit() %>%
  model(
    ar_best = AR(diff_housing),
    ar_1 = AR(diff_housing ~ order(1))
  )

# Report fit
glance(fit) %>%
  select(AIC, AICc, BIC)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Fit MA model
fit <- housing_ts %>%
  mutate(diff_housing = difference(housing)) %>%
  na.omit() %>%
  model(
    ma_best = ARIMA(
      diff_housing ~ pdq(p = 0, d = 0) + PDQ(0, 0, 0)
    ),
    ma_1 = ARIMA(diff_housing ~ pdq(0, 0, 1) + PDQ(0, 0, 0))
  )

# Report fit
glance(fit) %>%
  select(AIC, AICc, BIC)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Fit ARIMA model
fit <- housing_ts %>%
  mutate(diff_housing = difference(housing)) %>%
  na.omit() %>%
  model(
    # AR model
    ar_best = ARIMA(
      diff_housing ~ pdq(d = 0, q = 0) + PDQ(0, 0, 0)
    ),
    # MA model
    ma_best = ARIMA(
      diff_housing ~ pdq(p = 0, d = 0) + PDQ(0, 0, 0)
    ),
    # ARIMA model
    arima_best = ARIMA(diff_housing ~ PDQ(0, 0, 0))
  )

# Report fit
glance(fit) %>%
  select(AIC, AICc, BIC)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Plot autocorrelations
housing_ts %>%
  gg_tsdisplay(difference(housing), plot_type = "partial")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Fit ARIMA model
fit <- housing_ts %>%
  model(
    arima_best = ARIMA(housing ~ PDQ(0, 0, 0))
  )

# Report fit
report(fit)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Residuals
fit %>% gg_tsresiduals()

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Forecast next two years
fc <- fit %>% forecast(h = 24)

# Plot forecast
housing_ts %>% autoplot(housing) + autolayer(fc)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Identify outlier
housing_diff <- housing_ts %>%
  mutate(diff_housing = difference(housing))

# Outlier dummy variable
housing_ts$outlier <- rep(0, nrow(housing_ts))

# Set outlier to 1
housing_ts$outlier[which.min(housing_diff$diff_housing)] <- 1

# Re-model
fit <- housing_ts %>%
  model(arima_best = ARIMA(housing ~ outlier + PDQ(0, 0, 0)))

# Report fit
report(fit)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Residuals
fit %>% gg_tsresiduals()

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Forecast next two years
no_outlier <- new_data(housing_ts, 24) %>% mutate(outlier = 0)
fc_outlier <- fit %>% forecast(new_data = no_outlier)

# Plot forecast
housing_ts %>% autoplot(housing) + autolayer(fc_outlier)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Autocorrelations
housing_ts %>% filter(outlier == 0) %>%
  fill_gaps() %>% ACF(housing) %>% autoplot() + ggtitle("Autocorrelation")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Partial Autocorrelations
housing_ts %>% filter(outlier == 0) %>%
  fill_gaps() %>% PACF(housing) %>% autoplot() + ggtitle("Partial Autocorrelation")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Re-model
fit <- housing_ts %>%
  model(sarima_best = ARIMA(housing ~ outlier))

# Report fit
report(fit)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Residuals
fit %>% gg_tsresiduals()

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Forecast next two years
no_outlier <- new_data(housing_ts, 24) %>% mutate(outlier = 0)
fc_outlier <- fit %>% forecast(new_data = no_outlier)

# Plot forecast
housing_ts %>% autoplot(housing) + autolayer(fc_outlier)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# USE SARIMAX instead

# Load {TSA}
library(TSA)

# SARIMAX
fit <- arimax(x = housing_ts$housing, order = c(1, 1, 1),
              seasonal = list(order = c(0, 0, 1), period = 12),
              xreg = housing_ts$outlier)

# Report fit
fit

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Plot

# Forecast next two years
sarimax_prediction <- fit %>%
  predict(
    n.ahead = 24,
    newxreg = no_outlier$outlier
  ) %>% as.data.frame()

# Set prediction invervals
## 95%
sarimax_prediction$low_95 <- sarimax_prediction$pred - sarimax_prediction$se * qnorm(0.975)
sarimax_prediction$high_95 <- sarimax_prediction$pred + sarimax_prediction$se * qnorm(0.975)
## 80%
sarimax_prediction$low_80 <- sarimax_prediction$pred - sarimax_prediction$se * qnorm(0.900)
sarimax_prediction$high_80 <- sarimax_prediction$pred + sarimax_prediction$se * qnorm(0.900)

# Add dates
sarimax_prediction$date <- yearmonth(fc_outlier$date)

# Convert to `tsibble`
sarimax_ts <- sarimax_prediction %>%
  as_tsibble(index = date)

# Plot forecast
housing_ts %>% autoplot(housing) +
  autolayer(fc_outlier, alpha = 0.5) +
  geom_line(data = sarimax_ts, aes(y = pred), color = "red") +
  geom_line(data = sarimax_ts, aes(y = low_95), color = "red") +
  geom_line(data = sarimax_ts, aes(y = high_95), color = "red") +
  geom_line(data = sarimax_ts, aes(y = low_80), color = "red") +
  geom_line(data = sarimax_ts, aes(y = high_80), color = "red")

