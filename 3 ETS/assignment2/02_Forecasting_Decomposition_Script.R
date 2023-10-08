#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### DS5740: Week 2 | TSLM & Decomposition Script ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# Load {fpp2} and {fpp3}
library(fpp2); library(fpp3)

# Set seed for reproducibility
set.seed(1234)

# Random noise
y_wn <- tsibble(sample = 1:50, wn = rnorm(50, 0, 1), index = sample)

# Plot
y_wn %>% autoplot(wn) + labs(x = "", y = "")

# Random noise
y_wn %>% ACF(wn) %>% autoplot()

# Random noise
round(t(head(y_wn)), 3)

# Make y_wn a matrix
new_y_wn <- as.matrix(y_wn)

# Make with lag = 1 and lag = 2
new_y_wn <- cbind(
  new_y_wn, c(NA, new_y_wn[1:(length(new_y_wn[,2]) - 1),2]),
  c(NA, NA, new_y_wn[1:(length(new_y_wn[,2]) - 2),2])
)

# Change column names
colnames(new_y_wn) <- c("sample", "wn", "wn_lag1", "wn_lag2")

# Set values
wn <- new_y_wn[,"wn"]
wn_lag1 <- new_y_wn[,"wn_lag1"]
wn_lag2 <- new_y_wn[,"wn_lag2"]

# Random noise
round(t(head(new_y_wn)), 3)

# Lag-1 correlation
sum(
  (wn - mean(wn)) *
    (wn_lag1 - mean(wn)),
  na.rm = TRUE
) / sum((wn - mean(wn))^2)

# Lag-2 correlation
sum(
  (wn - mean(wn)) *
    (wn_lag2 - mean(wn)),
  na.rm = TRUE
) / sum((wn - mean(wn))^2)

# `acf` from {tseries}
acf(wn, lag.max = 2, plot = FALSE)

#%%%%%%%%%%%%%%%%%%%%
## TSLM Pipeline ----
#%%%%%%%%%%%%%%%%%%%%

# Load data
emotions <- read.csv("../data/fried_mental_2022.csv")

# Data variables
head(emotions)

# Participant four
participant <- emotions[emotions$ID == unique(emotions$ID)[4],]

# Obtain time and question variables
questions <- data.frame(
  time = participant$time, # time
  participant[,grep(
    "Q", colnames(participant) # questions
  )]
)

# First eight questions
data <- questions[,c(
  1, # time
  2:9 # first eight questions
)]

# Relabel questions
colnames(data)[2:9] <- c(
  "relax", "irritable", "worry",
  "nervous", "future", "anhedonia",
  "tired", "alone"
)

# Remove missing data
data <- na.omit(data)

# Convert to `tsibble`
ts <- data %>%
  mutate(
    time = ymd_hms(time)
  ) %>%
  as_tsibble(
    index = time
  )

# Convert to `tsibble`
ts_fill <- ts %>%
  fill_gaps() # fill in time gaps
# for plotting residuals later

# Length of time series
ts_length <- nrow(ts)
ts_fill_length <- nrow(ts_fill)

# Remove last four time points (we'll make a prediction later) 
prediction <- ts[
  -c((ts_length - 7):ts_length), # remove last 4 points
]

# For modeling residuals 
prediction_fill <- ts_fill[
  1:which(
    ts_fill$time ==
      prediction$time[nrow(prediction)]
  ), # match time points
]

# Save last four time points (we'll compare with prediction)
actual <- ts[
  c((ts_length - 7):ts_length), # keeps last 4 points
] %>%
  fill_gaps()

# Visualize time series
prediction %>%
  gather(
    "Measure", "Change",
    relax, irritable, worry,
    nervous, future, anhedonia,
    tired, alone
  ) %>%
  ggplot(aes(x = time, y = Change, colour = Measure)) +
  geom_line() +
  facet_grid(vars(Measure), scales = "free_y") +
  labs(y="") +
  guides(colour="none")

# Plot correlations
prediction %>%
  select(-time) %>%
  GGally::ggpairs()

# Fit linear model
fit <- prediction_fill %>% # our data
  model( # model for time series
    tslm = TSLM( # time series linear model
      # Fit linear model for `worry` using
      # all other variables
    )
  )

# Report fit
report(fit)

# Glance fit
glance(fit) %>%
  select(
    adj_r_squared, CV, AIC, AICc, BIC, log_lik
  )

# Plot model
augment(fit) %>%
  # Plot quarter on x-axis
  ggplot(aes(x = time)) +
  # Plot actual values
  geom_line(aes(y = worry, colour = "Data")) +
  # Plot fit values
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  labs(
    # No y-axis label
    y = NULL, 
    # Change title
    title = "Worry across time",
    subtitle = "Participant 4"
  ) +
  # Change colors
  scale_colour_manual(
    values = c(
      Data = "black", # Make data line black
      Fitted = "orange" # Make fitted line orange
    )
  ) +
  # No title for legend
  guides(colour = guide_legend(title = NULL)) +
  scale_y_continuous( 
    limits = c(1, 5), # minimum and maximum of y-axis
    breaks = seq(1, 5, 1) # breaks on y-axis
  )

# Plot residuals
fit %>%
  # enter residual plotting function here

  # Plot residuals
fit %>%
  augment() %>%
  na.omit() %>%
  features(.resid, box_pierce, lag = 10)

# Make forecast
fc_actual <- fit %>% 
  forecast(new_data = actual)

# Peek at forecast
head(fc_actual)

# Plot forecast
ts %>%
  # Plot quarter on x-axis
  ggplot(aes(x = time)) +
  # Plot actual values
  geom_line(aes(y = worry, colour = "Data")) +
  # Plot predicted values
  geom_line(
    data = na.omit(fc_actual),
    aes(y = .mean, colour = "Forecast"),
    size = 1
  ) +
  labs(
    # No y-axis label
    y = NULL, 
    # Change title
    title = "Worry across time",
    subtitle = "Participant 4"
  ) +
  # Change colors
  scale_colour_manual(
    values = c(
      Data = "black", # Make data line black
      Forecast = "orange" # Make fitted line orange
    )
  ) +
  # No title for legend
  guides(colour = guide_legend(title = NULL)) +
  scale_y_continuous( 
    limits = c(1, 5), # minimum and maximum of y-axis
    breaks = seq(1, 5, 1) # breaks on y-axis
  )

# Get accuracy measures
accuracy(fc_actual, ts_fill)

# Winkler and Continuous Ranked Probability Scale
fc_actual %>%
  accuracy(ts_fill, list(
    winkler = winkler_score,
    crps = CRPS,
    skill = skill_score(ME)
  ))

# Obtain useful functions
source("../Useful Functions/lm_new_data.R")
# Contains code to generate new variable values
# to forecast with the TLSM
# !! You'll need this code for your assignment this week !!

# Make future possibilities
future_scenarios <- scenarios(
  random = lm_new_data( # Random forecasts
    model = fit, # set model
    df = prediction, # set data
    iterations = 10, # number of iterations
    h = nrow(actual), type = "random"
  ),
  names_to = "Scenario")

# Make forecast
fc <- fit %>% forecast(new_data = future_scenarios)

# Plot forecasts simultaneously
ts %>%
  autoplot(worry) +
  autolayer(fc, alpha = 0.333) +
  labs(
    # No y-axis label
    y = NULL, 
    # Change title
    title = "Worry across time",
    subtitle = "Participant 4"
  ) +
  scale_y_continuous( 
    limits = c(1, 5), # minimum and maximum of y-axis
    breaks = seq(1, 5, 1) # breaks on y-axis
  )

# Obtain metrics
random_scenario <- fc[
  fc$Scenario == "random",
]
random <- random_scenario[
  !is.na(match(random_scenario$time, actual$time)),
]$.mean

# Make data frame table
df_table <- data.frame(
  "Measure" = c(
    "R-squared", "MAE",
    "RMSE", "MBE"
  ),
  "Random" = c(
    cor(random, actual$worry, use = "pairwise")^2, # R-squared
    mean(abs(random - actual$worry), na.rm = TRUE), # MAE
    sqrt(mean((random - actual$worry)^2, na.rm = TRUE)), # RMSE
    mean(random - actual$worry, na.rm = TRUE) # MBE
  )
)

# Print data frame
df_table

# Make data frame table
df_table <- rbind.data.frame(
  random = fc[fc$Scenario == "random",] %>%
    accuracy(ts_fill, list(
      winkler = winkler_score,
      crps = CRPS
    )),
  actual = fc_actual %>%
    accuracy(ts_fill, list(
      winkler = winkler_score,
      crps = CRPS
    ))
)

# Report table
df_table$.type <- c("random", "actual")
df_table

#%%%%%%%%%%%%%%%%%%%%
## Decomposition ----
#%%%%%%%%%%%%%%%%%%%%

# Austrailian beer
aus_production %>% filter(year(Quarter) >= 1992) %>% autoplot() +
  labs(title ="Australian quarterly beer production")

# Austrailian beer
aus_production %>% filter(year(Quarter) >= 1992) %>% 
  ACF(Beer) %>% # Self-quiz: what does `ACF` do again? 
  autoplot()

# Random noise
y_wn %>% autoplot(wn) +
  labs(x = "", y = "")

# Random noise
tsibble(y_wn, index = sample) %>%
  ACF(wn) %>%
  autoplot()

# Antidiabetic
a10 %>% # data
  as_tsibble() %>% # convert to `tsibble` format
  autoplot(value) +
  labs(x = "", y = "")

# Antidiabetic
a10 %>% # data
  as_tsibble() %>% # convert to `tsibble` format
  ACF(
    value, # sales from `tsibble`
    lag_max = 48 # maximum lag
  ) %>%
  autoplot()

# Trend + sine
y <- tsibble(
  sample = 1:30, # time series index
  wn = seq(1, 30, 1) + # linear trend
  sin(rep(c(1, 0, -1), times = 10)) - 10, # sine wave
  index = sample
)

# Plot trend + sine
y %>% autoplot(wn) +
  labs(x = "", y = "")

# Select US retail data
us_retail_employment <- us_employment %>%
  filter(year(Month) >= 1990, Title == "Retail Trade") %>%
  select(-Series_ID)

# US retail employment time series
us_retail_employment %>%
  autoplot(Employed) +
  labs(
    y = "Persons (thousands)",
    title = "Total employment in US retail"
  )

# Store components
us_comps <- us_retail_employment %>%
  model(stl = STL(Employed))

# STL Trend
us_retail_employment %>%
  autoplot(Employed, color = 'gray') +
  autolayer(
    components(us_comps),
    trend, # plot trend
    color = '#D55E00'
  ) +
  labs(
    y = "Persons (thousands)",
    title = "Total employment in US retail"
  )

# STL Trend
us_retail_employment %>%
  autoplot(Employed, color = 'gray') +
  autolayer(
    components(us_comps),
    trend + season_year, # plot trend + seasonlity
    color = '#D55E00'
  ) +
  labs(
    y = "Persons (thousands)",
    title = "Total employment in US retail"
  )

# STL decomposition
us_retail_employment %>% # dataset
  model(stl = STL(Employed)) %>% # model (STL)
  components() %>% # components of decomposition
  autoplot() # plot

# Monthly STL decomposition
us_retail_employment %>% # dataset
  model(stl = STL(Employed)) %>% # model (STL)
  components() %>% # components of decomposition
  gg_subseries(season_year) # broken down by month

# STL Season Adjustment
us_retail_employment %>%
  autoplot(Employed, color = 'gray') +
  autolayer(
    components(us_comps),
    season_adjust, # plot season adjustment
    color = '#D55E00'
  ) +
  labs(
    y = "Persons (thousands)",
    title = "Total employment in US retail"
  )

# X-13ARIMA-SEATS
us_retail_employment %>% # data
  model(X_13ARIMA_SEATS(Employed)) %>%  # X13 decomposition
  components() %>% # get components from decomposition
  autoplot() # plot decomposition

# X-13ARIMA-SEATS
us_retail_employment %>%
  model(X_13ARIMA_SEATS(Employed)) %>%
  report()