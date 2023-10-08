#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### DS5740: Week 6 | VAR Script ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# Load packages
library(vars)
library(urca)
library(fpp2)
library(fpp3)

# Set seed for reproducibility
set.seed(1234)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Load data
nashville_housing <- read.csv("./assignment6/nashville_housing.csv")

# Convert date
nashville_housing$date <- yearmonth(nashville_housing$date)

# Convert to `tsibble`
housing_ts <- nashville_housing %>%
  as_tsibble(index = date)

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
  ):which(
    as.character(housing_ts$date) == "2021 Jun"
  )
] <- 1

# Set up training and testing indices
train <- 1:which(as.character(housing_ts$date) == "2021 Jun")
test <- setdiff(1:nrow(housing_ts), train)

# Initialize training and testing data
housing_train <- housing_ts[train,]
housing_test <- housing_ts[test,]

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

# Create forecast for `unemployment`

## Unemployment
unemployment_ts <- housing_ts %>% dplyr::select(date, unemployment)

## Pandemic variable
unemployment_ts$pandemic <- 0

## April 2020-September 2020
unemployment_ts$pandemic[46:51] <- 6:1

## Fit ARIMA
fit_unemployment <- unemployment_ts %>%
  model(ARIMA(unemployment ~ pandemic))

## New data
new_unemployment <- new_data(housing_test, n = 24)

## Add outlier
new_unemployment$pandemic <- rep(0, 24)

## Forecast
fc_unemployment <- fit_unemployment %>%
  forecast(new_data = new_unemployment)

# Create forecast for `median_days`

## Median days
median_days_ts <- housing_ts %>% 
  dplyr::select(date, median_days, pandemic)

## Fit ARIMA
fit_median_days <- median_days_ts %>% model(
  ets = ETS(median_days)
)

## New data
new_median_days <- new_data(housing_test, n = 24)
new_median_days$pandemic <- 0

## Forecast
fc_median_days <- fit_median_days %>%
  forecast(new_data = new_median_days)

# Create forecast for `price_decreased`

## Price decrease
price_decreased_ts <- housing_ts %>% 
  dplyr::select(date, price_decreased, pandemic)

## Fit ARIMA
fit_price_decreased <- price_decreased_ts %>%
  model(
    dynamic = ARIMA(price_decreased ~ pandemic)
  )

## New data
new_price_decreased <- new_data(housing_test, n = 24)
new_price_decreased$pandemic <- 0

## Forecast
fc_price_decreased <- fit_price_decreased %>%
  forecast(new_data = new_price_decreased)

# Create forecast for `pending_listing`



## Pending Listing
pending_listing_ts <- housing_ts %>% 
  dplyr::select(date, pending_listing, pandemic)

## Add outlier(s)
pending_listing_ts$outlier <- rep(0, nrow(pending_listing_ts))
pending_listing_ts$outlier[1:10] <- 1

## Fit ARIMA
fit_pending_listing <- pending_listing_ts %>%
  model(
    arima = ARIMA(pending_listing ~ outlier)
  )

## New data
new_pending_listing <- new_data(housing_test, n = 24)

## Add outlier
new_pending_listing$outlier <- 0

## Forecast
fc_pending_listing <- fit_pending_listing %>%
  forecast(new_data = new_pending_listing)

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



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Autocorrelation of residuals
fit_var %>% augment() %>%
  ACF(.innov) %>% autoplot()

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Vector autoregression
fit_var <- housing_ts %>%
  model(
    lag_2 = VAR(
      vars(housing, unemployment, median_days,
           price_decreased, pending_listing) ~
        xreg(outlier, pandemic, season(period = 12))
    )
  )

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Autocorrelation of residuals
fit_var %>% augment() %>%
  ACF(.innov) %>% autoplot()

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Fit VAR(2)
var_2 <- vars::VAR(
  housing_ts[,-c(1, 9:10)], p = 2, type = "none", 
  exogen = housing_ts[,c(9:10)]
)
serial.test(var_2, lags.pt = 10, type = "PT.adjusted")

# Fit VAR(2) with season
var_2_season <- vars::VAR(
  housing_ts[,-c(1, 9:10)], p = 2, type = "none", 
  exogen = housing_ts[,c(9:10)], season = 12
)
serial.test(var_2_season, lags.pt = 10, type = "PT.adjusted")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Vector autoregression
fit_var <- housing_ts %>%
  model(
    lag_2 = VAR(
      vars(housing, unemployment, median_days,
           price_decreased, pending_listing) ~
        xreg(outlier, pandemic)
    ),
    lag_10 = VAR(
      vars(housing, unemployment, median_days,
           price_decreased, pending_listing) ~
        xreg(outlier, pandemic) + AR(0:10)
    )
  )

# Report fit
glance(fit_var) %>% select(.model, AIC, AICc, BIC)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Fit VAR(10)
fit_var <- housing_ts %>%
  model(
    lag_10 = VAR(
      vars(housing, unemployment, median_days,
           price_decreased, pending_listing) ~
        xreg(outlier, pandemic) + AR(10)
    )
  )

# Autocorrelation of residuals
fit_var %>% augment() %>%
  ACF(.innov) %>% autoplot()

# Vector autoregression
fit_var <- housing_ts %>%
  model(
    lag_2 = VAR(
      vars(housing, unemployment, median_days,
           price_decreased, pending_listing) ~
        xreg(outlier, pandemic)
    ),
    lag_10 = VAR(
      vars(housing, unemployment, median_days,
           price_decreased, pending_listing) ~
        xreg(outlier, pandemic) + AR(10)
    )
  )

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

# Forecast
fc_var <- fit_var %>%
  forecast(new_data = new_final)

# Plot forecasts
fc_var %>% autoplot(alpha = 0.5)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Fit VAR(2)
fit_var <- housing_ts %>%
  model(
    lag_2 = VAR(
      vars(housing, unemployment, median_days,
           price_decreased, pending_listing) ~
        xreg(outlier, pandemic)
    )
  )

# Forecast
fc_var <- fit_var %>%
  forecast(new_data = new_final)

# Plot forecasts
fc_var %>% autoplot()

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# VAR model with {vars}
vars_var <- vars::VAR(
  y = housing_ts[,c(
    "housing", "unemployment", "median_days",
    "price_decreased", "pending_listing"
  )],
  exogen = housing_ts[,c("outlier", "pandemic")],
  type = "none", # same as {fpp3}'s `VAR`
  p = 2 # lag
)

# Make dummy variable matrix
dummat <- matrix(
  rep(0, 2 * 24), nrow = 24,
  dimnames = list(NULL, c("outlier", "pandemic"))
)

# Forecast
var_fc <- predict(vars_var, n.ahead = 24, dumvar = dummat)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Get forecast values
fc_housing <- var_fc$fcst$housing

# Set up forecast as {fpp3} does 
var_fc <- data.frame(
  .model = "VAR",
  date = fc_var$date,
  housing = distributional::dist_normal(
    mean = fc_housing[,"fcst"],
    sd = fc_housing[,"CI"]
  ),
  .mean = fc_housing[,"fcst"],
  fc[fc$.model == "tslm_sig", -c(1:4)]
) %>% as_tsibble(index = date)

# Add "housing" to dimnames
dimnames(var_fc$housing) <- "housing"

# Add to original forecast
fc <- bind_rows(fc, var_fc)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Plot
housing_ts %>%
  autoplot(housing) +
  autolayer(fc, alpha = 0.5, size = 1.5)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Load data
housing_validation <- read.csv("./assignment6/housing_validation.csv")

# Convert date
housing_validation$date <- yearmonth(housing_validation$date)

# Convert to `tsibble`
housing_valid <- housing_validation %>%
  as_tsibble(index = date)

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
fc %>% accuracy(housing_valid)

# Distributional estimates
fc %>% accuracy(housing_valid, list(crps = CRPS))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#%%%%%%%%%%%%%%%
## Activity ----
#%%%%%%%%%%%%%%%

# Goal: Is there Granger causality from `median_days`?

#   1. Check for stationary of all variables (if not, then make them stationary)

#   2. Check for a linear relationship (use `GGally::ggpairs`)

#   3. Perform `causality` on model with `cause = "median_days"`

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Cointegration
co_test <- ca.jo(
  # variables
  x = housing_ts[,c(
    "housing", "unemployment", "median_days",
    "price_decreased", "pending_listing"
  )],  
  type = "trace", # tends to be more conservative
  K = 2, # lag -- same as your VAR model
  spec = "longrun", # generally use "longrun"
  ecdet = "trend", # trend-stationary
  # exogeneous dummy variables
  dumvar = housing_ts[,c("outlier", "pandemic")]
)

# Summary
co_summ <- summary(co_test)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Convert VECM to VAR
vecm <- vars::vec2var(co_test, r = 1)

# Make dummy variable matrix
dummat <- matrix(
  rep(0, 2 * 24), nrow = 24
)
colnames(dummat) <- c("outlier", "pandemic")

# Forecast
vecm_fc <- predict(vecm, n.ahead = 24, dumvar = dummat)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Get forecast values
fc_housing <- vecm_fc$fcst$housing

# Set up forecast as {fpp3} does 
fc_vecm <- data.frame(
  .model = "VECM",
  date = var_fc$date,
  housing = distributional::dist_normal(
    mean = fc_housing[,"fcst"],
    sd = fc_housing[,"CI"]
  ),
  .mean = fc_housing[,"fcst"],
  fc[fc$.model == "tslm_sig", -c(1:4)]
) %>% as_tsibble(index = date)

# Add "housing" to dimnames
dimnames(fc_vecm$housing) <- "housing"

# Add to original forecast
fc <- bind_rows(fc, fc_vecm)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Plot
housing_ts %>%
  autoplot(housing) +
  autolayer(fc, alpha = 0.5, size = 1.5)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Create average of all models
average_fc <- fc %>%
  mutate(date_factor = factor(date)) %>%
  group_by(date_factor) %>%
  summarize(all_average = as.numeric(mean(.mean))) %>%
  select(all_average) %>%
  as_tsibble(key = NULL, index = date)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Plot average forecast over all forecasts
housing_ts %>%
  autoplot(housing) +
  autolayer(fc, alpha = 0.5, size = 1.5) +
  geom_line(
    data = average_fc, aes(y = all_average),
    color = "black", size = 1.5, alpha = 0.5
  ) +
  labs(
    x = "Date\n(year and month)",
    y = "Number of Houses Available",
    title = "Houses on the Market",
    subtitle = "Nashville-Davidson–Murfreesboro–Franklin, TN"
  ) +
  theme(
    panel.background = element_blank(),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16),
    axis.line = element_line(size = 1),
    axis.ticks = element_line(size = 1),
    plot.title = element_text(size = 20, hjust = 0.5),
    plot.subtitle = element_text(size = 16, hjust = 0.5),
    panel.grid.major = element_line(
      color = "lightgrey"
    ),
    legend.position = "none"
  )

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Plot average forecast over all forecasts
housing_ts %>%
  autoplot(housing) +
  autolayer(
    fc %>%
      filter(.model != "tslm_sig" | .model != "VAR"), 
    alpha = 0.5, size = 1.5
  ) +
  labs(
    x = "Date\n(year and month)",
    y = "Number of Houses Available",
    title = "Houses on the Market",
    subtitle = "Nashville-Davidson–Murfreesboro–Franklin, TN"
  ) +
  theme(
    panel.background = element_blank(),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16),
    axis.line = element_line(size = 1),
    axis.ticks = element_line(size = 1),
    plot.title = element_text(size = 20, hjust = 0.5),
    plot.subtitle = element_text(size = 16, hjust = 0.5),
    panel.grid.major = element_line(
      color = "lightgrey"
    ),
    legend.position = "none"
  )

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Load data
housing_validation <- read.csv("../data/housing_validation.csv")

# Convert date
housing_validation$date <- yearmonth(housing_validation$date)

# Convert to `tsibble`
housing_valid <- housing_validation %>%
  as_tsibble(index = date)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Plot average forecast over all forecasts
housing_ts %>%
  autoplot(housing) +
  autolayer(
    fc %>%
      filter(.model != "tslm_sig" & .model != "VAR"), 
    alpha = 0.5, size = 1.5
  ) +
  geom_line(
    data = housing_valid, aes(y = housing),
    color = "black", size = 0.5, alpha = 1
  ) +
  labs(
    x = "Date\n(year and month)",
    y = "Number of Houses Available",
    title = "Houses on the Market",
    subtitle = "Nashville-Davidson–Murfreesboro–Franklin, TN"
  ) +
  theme(
    panel.background = element_blank(),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16),
    axis.line = element_line(size = 1),
    axis.ticks = element_line(size = 1),
    plot.title = element_text(size = 20, hjust = 0.5),
    plot.subtitle = element_text(size = 16, hjust = 0.5),
    panel.grid.major = element_line(
      color = "lightgrey"
    )
  )

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Point estimates
fc %>%
  filter(.model != "tslm_sig" & .model != "VAR") %>%
  accuracy(housing_valid)

# Distributional estimates
fc %>%
  filter(.model != "tslm_sig" & .model != "VAR") %>%
  accuracy(housing_valid, list(crps = CRPS))