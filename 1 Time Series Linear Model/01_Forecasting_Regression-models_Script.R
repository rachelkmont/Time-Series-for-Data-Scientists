#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### DS5740: Week 1 | TSLM Script ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# Load {fpp3}
library(fpp3)

# Load US Consumption Expenditure
data("us_change")

# Plot multiple variables' time series
us_change %>%
  gather("Measure", "Change", Consumption, Income, Production, Savings, Unemployment) %>%
  ggplot(aes(x = Quarter, y = Change, colour = Measure)) +
  geom_line() +
  facet_grid(vars(Measure), scales = "free_y") +
  labs(y = "") +
  guides(colour="none")

# Plot pairwise correlations
us_change %>%
  as_tibble() %>%
  select(-Quarter) %>%
  GGally::ggpairs()

# Fit TSLM
fit <- us_change %>%
  model(lm = TSLM(
    Consumption ~ Income + Production + Unemployment + Savings
  ))

# Report fit
report(fit)

# Length of time series
ts_length <- nrow(us_change)

# Remove last five years (we'll make a prediction later) 
us_prediction <- us_change[
  -c((ts_length - 19):ts_length), # remove last 5 years
]

# Save last five years (we'll compare with prediction)
us_actual <- us_change[
  c((ts_length - 19):ts_length), # keeps last 5 years
]

# Fit linear model (write the code; adjust code from above)
fit_us_lm <- us_prediction %>%
    model( tslm = TSLM( Consumption ~ Income + Production + Savings + Unemployment))


# Report fit (write the code)
report(fit_us_lm)


# Plot model
augment(fit_us_lm) %>%
  # Plot quarter on x-axis
  ggplot(aes(x = Quarter)) +
  # Plot actual values
  geom_line(aes(y = Consumption, colour = "Data")) +
  # Plot fit values
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  labs(
    # No y-axis label
    y = NULL, 
    # Change title
    title = "Percent change in US consumption expenditure"
  ) +
  # Change colors
  scale_colour_manual(
    values = c(
      Data = "black", # Make data line black
      Fitted = "orange" # Make fitted line orange
    )
  ) +
  # No title for legend
  guides(colour = guide_legend(title = NULL))


# Forecast
fc <- forecast(fit_us_lm, new_data = us_actual)

# Plot forecast
us_change %>%
  # Plot quarter on x-axis
  ggplot(aes(x = Quarter)) +
  # Plot actual values
  geom_line(aes(y = Consumption, colour = "Data")) +
  # Plot predicted values
  geom_line(
    data = fc,
    aes(y = .mean, colour = "Fitted"),
    size = 1
  ) +
  labs(
    # No y-axis label
    y = NULL, 
    # Change title
    title = "Percent change in US consumption expenditure"
  ) +
  # Change colors
  scale_colour_manual(
    values = c(
      Data = "black", # Make data line black
      Fitted = "orange" # Make forecasted line orange
    )
  ) +
  # No title for legend
  guides(colour = guide_legend(title = NULL))

# R-squared (write the code)
cor(fc$.mean, us_actual$Consumption)^2


# MAE (write the code)
mean(abs(fc$.mean - us_actual$Consumption))


# RMSE (write the code)
sqrt(mean((fc$.mean - us_actual$Consumption)^2))


# MBE (write the code)
mean(fc$.mean - us_actual$Consumption)

# General function for many measures
accuracy(fc, us_change)

# Check residuals
gg_tsresiduals(fit_us_lm)

# Future scenarios
future_scenarios <- scenarios( # Create future scenarios
  increase_income = new_data( # Create new data
    us_prediction,  # Original data
    nrow(us_actual) # Number of new data
  ) %>%
    mutate(
      Income = mean(us_prediction$Income) + # Add to mean Income
        seq(0, 1, length = nrow(us_actual)), # Increase from 0 to 1
      # with a length equal to the number of actual data
      Production = mean(us_prediction$Production) + 
        rep(0, nrow(us_actual)), # No increase/decrease
      # Repeat 0 with a length equal to the number of actual data
      Savings = mean(us_prediction$Savings) + 
        rep(0, nrow(us_actual)),
      Unemployment = mean(us_prediction$Unemployment) +
        rep(0, nrow(us_actual))
    ),
  decrease_income = new_data(
    us_prediction, nrow(us_actual)
  ) %>%
    mutate(
      Income = mean(us_prediction$Income) + 
        seq(0, -1, length = nrow(us_actual)),
      Production = mean(us_prediction$Production) + 
        rep(0, nrow(us_actual)),
      Savings = mean(us_prediction$Savings) + 
        rep(0, nrow(us_actual)),
      Unemployment = mean(us_prediction$Unemployment) +
        rep(0, nrow(us_actual))
    )
)

# Forecast
fc_us <- fit_us_lm %>% 
  forecast(new_data = future_scenarios)

# Plot
autoplot(us_prediction, Consumption) +
  autolayer(fc_us)


# Fit linear model with trend
fit_us_trend <- us_prediction %>%
  model( # model for time series
    tslm = TSLM( # time series linear model
      Consumption ~ trend() # trend component
    )
  )

# Report fit (write code)


# Plot model
augment(fit_us_trend) %>%
  # Plot quarter on x-axis
  ggplot(aes(x = Quarter)) +
  # Plot actual values
  geom_line(aes(y = Consumption)) +
  labs(
    # No y-axis label
    y = NULL, 
    # Change title
    title = "Percent change in US consumption expenditure"
  ) +
  # No title for legend
  guides(colour = guide_legend(title = NULL))


# Fit linear model with trend and season
fit_us_season <- us_prediction %>%
  model( # model for time series
    tslm = TSLM( # time series linear model
      Consumption ~ trend() + # trend component
        season() # season component
    )
  )

# Report fit (write code)


# Plot model (write code; adjust code from above)


# Australian beer example
data("aus_production")

# Australian beer production
recent_production <- aus_production %>% 
  filter(year(Quarter) >= 1992)

# Plot
recent_production %>% 
  autoplot(Beer) +
  labs(y = "Megalitres", title = "Australian quarterly beer production")

# Fit model with trend and season (write code)
## Call it `fit_beer`


# Report fit (write code)




# Plot residuals
fit_beer %>%
  gg_tsresiduals()

# Plot fitted model
augment(fit_beer) %>%
  ggplot(aes(x = Quarter)) +
  geom_line(aes(y = Beer, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  labs(y="Megalitres",title ="Australian quarterly beer production") +
  scale_colour_manual(values = c(Data = "black", Fitted = "#D55E00"))


# Examining seasonality
augment(fit_beer) %>%
  ggplot(aes(x=Beer, y=.fitted, colour=factor(quarter(Quarter)))) +
  geom_point() +
  labs(y="Fitted", x="Actual values", title = "Quarterly beer production") +
  scale_colour_brewer(palette="Dark2", name="Quarter") +
  geom_abline(intercept=0, slope=1)


# Forecasting prediction
fc <- fit_beer %>% forecast

# Plot forecast
fc %>% autoplot(recent_production)
