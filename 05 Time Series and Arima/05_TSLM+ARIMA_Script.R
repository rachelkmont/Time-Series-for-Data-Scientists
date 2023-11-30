#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### DS5740: Week 5 | Dynamic Script ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# Load packages
library(fpp2); library(fpp3); library(TSA)

# Set seed for reproducibility
set.seed(1234)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Load data
nashville_housing <- read.csv("./assignment5/nashville_housing.csv")

# Convert date
nashville_housing$date <- yearmonth(
  nashville_housing$date
)

# Convert to `tsibble`
housing_ts <- nashville_housing %>%
  as_tsibble(
    index = date
  )

# Identify outlier
housing_diff <- housing_ts %>%
  mutate(diff_housing = difference(housing))

# Outlier dummy variable
housing_ts$outlier <- rep(0, nrow(housing_ts))

# Set outlier to 1
housing_ts$outlier[which.min(housing_diff$diff_housing)] <- 1

# TSLM with ARIMA errors

# Model with outlier
fit <- housing_ts %>%
  model(sarima_best = ARIMA(housing ~ outlier))

# Report fit
report(fit)

# SARIMAX

# Using {TSA}
fit <- arimax(
  x = housing_ts$housing, order = c(1, 1, 1),
  seasonal = list(order = c(0, 0, 1), period = 12),
  xreg = housing_ts$outlier
)

# Report fit
fit

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Load data
nashville_housing <- read.csv("../data/nashville_housing.csv")

# Convert date
nashville_housing$date <- yearmonth(nashville_housing$date)

# Convert to `tsibble`
housing_ts <- nashville_housing %>%
  as_tsibble(index = date)

# Plot
housing_ts %>% autoplot(housing)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Outlier dummy variable
housing_ts$outlier <- rep(0, nrow(housing_ts))

# Set outlier to 1
housing_ts$outlier[
  which.min(difference(housing_ts$housing))
] <- 1

# Final model
fit <- housing_ts %>%
  model(sarima_best = ARIMA(housing ~ outlier))

# Report fit
report(fit)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Forecast next two years
new_two_years <- new_data(housing_ts, 24) %>% mutate(outlier = 0)
fc_two_years <- fit %>% forecast(new_data = new_two_years)

# Plot forecast
housing_ts %>% autoplot(housing) + autolayer(fc_two_years)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Visualize time series of covariates
housing_ts %>%
  gather(
    "Measure", "Change",
    housing, unemployment,
    median_days, price_increased,
    price_decreased, pending_listing,
    median_price
  ) %>%
  ggplot(aes(x = date, y = Change, colour = Measure)) +
  geom_line() +
  facet_grid(vars(Measure), scales = "free_y") +
  labs(y="") +
  guides(colour="none")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Correlations
housing_ts[,-1] %>%
  GGally::ggpairs()

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Set up training and testing indices
train <- 1:which(as.character(housing_ts$date) == "2021 Jun")
test <- setdiff(1:nrow(housing_ts), train)

# Initialize training and testing data
housing_train <- housing_ts[train,]
housing_test <- housing_ts[test,]

# Plot housing
housing_train %>% autoplot(housing)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Fit linear model
fit_tslm <- housing_train %>%
  model(tslm = TSLM(
    housing ~ unemployment + median_days + price_increased +
      price_decreased + pending_listing + median_price + outlier))

# Report fit
report(fit_tslm)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Multicolinearity?
fit <- lm(
  housing ~ unemployment + median_days + price_increased +
    price_decreased + pending_listing + median_price + outlier,
  data = housing_train
)

# VIF
regclass::VIF(fit)

# Coefficients
round(coefficients(fit), 5)[c("pending_listing", "median_price")]

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Multicolinearity?
fit <- lm(
  housing ~ unemployment + median_days + price_increased +
    price_decreased + pending_listing + outlier,
  data = housing_train
)

# VIF
regclass::VIF(fit)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Remove price increases
fit_increase <- housing_train %>%
  model(
    tslm_all = TSLM(
      housing ~ unemployment + median_days + price_increased +
        price_decreased + pending_listing + outlier
    ),
    tslm_sig = TSLM(
      housing ~ unemployment + median_days +
        price_decreased + pending_listing + outlier
    )
  )

# Report fit
glance(fit_increase) %>%
  select(.model, AIC, AICc, BIC)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Check best fit
fit_tslm <- housing_train %>%
  model(
    tslm_sig = TSLM(
      housing ~ unemployment + median_days +
        price_decreased + pending_listing + outlier
    )
  )

# Report fit
report(fit_tslm)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Residuals
fit_tslm %>% gg_tsresiduals()

# Ljung-Box
fit_tslm %>% augment() %>% features(.innov, ljung_box, lag = 12, dof = 5)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Plot housing
housing_train %>% autoplot(housing)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Set pandemic
housing_train$pandemic <- rep(0, nrow(housing_train))
housing_train$pandemic[
  which(
    as.character(housing_ts$date) == "2020 May"
  ):nrow(housing_train)
] <- 1

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Check best fit
fit_tslm <- housing_train %>%
  model(
    tslm_sig = TSLM(
      housing ~ unemployment + median_days + price_decreased +
        pending_listing + outlier + pandemic
    )
  )

# Report fit
report(fit_tslm)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Residuals
fit_tslm %>%
  gg_tsresiduals()

# Ljung-Box (set the number of degrees of freedom `dof`)
fit_tslm %>% augment() %>% features(.innov, ljung_box, lag = 12, dof = ) # set

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Fit ARIMA model
fit_arima <- housing_train %>%
  model(arima = ARIMA(housing ~ outlier + pandemic))

# Report fit
report(fit_arima)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Residuals
fit_arima %>% gg_tsresiduals()

# Ljung-Box
fit_arima %>% augment() %>% features(.innov, ljung_box, lag = 12, dof = 4)

# Why four `dof`?

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Fit TSLM with ARIMA errors
fit_dynamic <- housing_train %>%
  model(
    dynamic = ARIMA(
      housing ~ unemployment + median_days + price_decreased +
        pending_listing + outlier + pandemic
    )
  )

# Report fit
report(fit_dynamic)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Residuals
fit_dynamic %>% gg_tsresiduals()

# Ljung-Box
fit_dynamic %>% augment() %>% features(.innov, ljung_box, lag = 12, dof = 11)

# Why 11 `dof`?

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Update test data with outlier and pandemic
housing_test <- housing_test %>%
  mutate(
    outlier = rep(0, nrow(housing_test)),
    pandemic = rep(0, nrow(housing_test))
  )

# Combine all models
all_models <- housing_train %>%
  model(
    tslm_sig = TSLM(
      housing ~ unemployment + median_days + price_decreased +
        pending_listing + outlier + pandemic
    ),
    arima_outlier = ARIMA(housing ~ outlier),
    dynamic = ARIMA(
      housing ~ unemployment + median_days + price_decreased +
        pending_listing + outlier + pandemic
    )
  )

# Forecast models
fc <- all_models %>% forecast(new_data = housing_test)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Plot
housing_train %>%
  autoplot(housing) +
  autolayer(fc, alpha = 0.5, size = 1.5) +
  geom_line(
    data = housing_test,
    aes(y = housing),
    color = "orange",
    size = 1.5
  )

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Point estimates
fc %>% accuracy(housing_test) %>%
  select(.model, RMSE, ME, MAE)

# Distributional estimates
fc %>% accuracy(
  housing_test,
  list(winkler = winkler_score, crps = CRPS)
)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Outlier dummy variable
housing_ts$outlier <- rep(0, nrow(housing_ts))

# Set outlier to 1
housing_ts$outlier[
  which.min(difference(housing_ts$housing))
] <- 1

# Set pandemic
housing_ts$pandemic <- rep(0, nrow(housing_ts))
housing_ts$pandemic[
  which(
    as.character(housing_ts$date) == "2020 May"
  ):nrow(housing_ts)
] <- 1

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Combine all models
all_models <- housing_ts %>%
  model(
    tslm_sig = TSLM(
      housing ~ unemployment + median_days + price_decreased +
        pending_listing + outlier + pandemic
    ),
    arima = ARIMA(
      housing ~ outlier + pandemic
    ),
    dynamic = ARIMA(
      housing ~ unemployment + median_days + price_decreased +
        pending_listing + outlier + pandemic
    )
  )

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Unemployment
unemployment_ts <- housing_ts %>% select(date, unemployment)

# Plot
unemployment_ts %>% autoplot(unemployment)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Pandemic variable
unemployment_ts$pandemic <- rep(0, nrow(unemployment_ts))

# April 2020-
unemployment_ts$pandemic[46:51] <- 6:1

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Fit ARIMA
fit_unemployment <- unemployment_ts %>%
  model(ARIMA(unemployment ~ pandemic))

# Report fit
report(fit_unemployment)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Residuals
fit_unemployment %>% gg_tsresiduals()

# Ljung-Box
fit_unemployment %>% augment() %>% features(.innov, ljung_box, lag = 12, dof = 4)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## New data
new_unemployment <- new_data(housing_test, n = 24)

## Add outlier
new_unemployment$pandemic <- rep(0, 24)

## Forecast
fc_unemployment <- fit_unemployment %>%
  forecast(new_data = new_unemployment)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Below is not in slides
# Forecasts are made for *each* individual variable using
# the best fitting ARIMA model

## Median days
median_days_ts <- housing_ts %>% select(date, median_days)

## Fit ARIMA
fit_median_days <- median_days_ts %>% model(ARIMA(median_days))

## New data
new_median_days <- new_data(housing_test, n = 24)

## Forecast
fc_median_days <- fit_median_days %>%
  forecast(new_data = new_median_days)

## Price decrease
price_decreased_ts <- housing_ts %>% select(date, price_decreased)

## Fit ARIMA
fit_price_decreased <- price_decreased_ts %>%
  model(ARIMA(price_decreased))

## New data
new_price_decreased <- new_data(housing_test, n = 24)

## Forecast
fc_price_decreased <- fit_price_decreased %>%
  forecast(new_data = new_price_decreased)

## Pending Listing
pending_listing_ts <- housing_ts %>% select(date, pending_listing)

## Add outlier(s)
pending_listing_ts$outlier <- rep(0, nrow(pending_listing_ts))
pending_listing_ts$outlier[1:10] <- 1

## Fit ARIMA
fit_pending_listing <- pending_listing_ts %>%
  model(ARIMA(pending_listing ~ outlier))

## New data
new_pending_listing <- new_data(housing_test, n = 24)

## Add outlier
new_pending_listing$outlier <- rep(0, 24)

## Forecast
fc_pending_listing <- fit_pending_listing %>%
  forecast(new_data = new_pending_listing)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Make new data
new_final <- new_data(housing_ts, n = 24)

# Add variables
new_final <- new_final %>%
  mutate(
    unemployment = fc_unemployment$.mean,
    median_days = fc_median_days$.mean,
    price_decreased = fc_price_decreased$.mean,
    pending_listing = fc_pending_listing$.mean,
    outlier = 0,
    pandemic = 0
  )

# Forecast models
fc <- all_models %>% forecast(new_data = new_final)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Plot
housing_ts %>%
  autoplot(housing) +
  autolayer(fc, alpha = 0.5, size = 1.5)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Actual data from the last year

# Load data
housing_validation <- read.csv("../data/housing_validation.csv")

# Convert date
housing_validation$date <- yearmonth(housing_validation$date)

# Convert to `tsibble`
housing_valid <- housing_validation %>%
  as_tsibble(index = date)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Plot
housing_ts %>%
  autoplot(housing) +
  autolayer(fc, alpha = 0.5, size = 1.5) +
  geom_line(
    data = housing_valid, aes(y = housing),
    color = "black", linewidth = 1
  )

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Point estimates
fc %>%
  accuracy(housing_valid)

# Distributional estimates
fc %>% accuracy(
  housing_valid,
  list(winkler = winkler_score, crps = CRPS)
)
