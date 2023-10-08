#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### DS5740: Week 3 | ETS Script ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# Load {fpp3} and {zoo}
library(fpp3)
library(zoo)

# Load data
co2 <- read.csv("./data/co2_mm_gl.csv")

# Convert year and month to date format
co2$yearmonth <- yearmonth(
  as.yearmon(paste(co2$year, co2$month), "%Y %m")
)

# Convert to `tsibble`
co2_ts <- as_tsibble(
  co2, index = yearmonth
)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Plot time series
co2_ts %>%
  autoplot(average) +
  labs(
    y = "Average CO2 (ppm)",
    x = "Year and Month "
  )

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Estimate parameters
fit <- co2_ts %>%
  model(ANN = # Set up model)
          
          # Report fit
          report(fit)
        
        #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        
        # Plot components
        components(fit) %>% autoplot()
        
        #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        
        # Plot residuals
        fit %>% gg_tsresiduals()
        
        #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        
        # Estimate parameters
        fit <- co2_ts %>%
          model(AAN = # Set up model)
                  
                  # Report fit
                  report(fit)
                
                #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                
                # Plot components
                components(fit) %>% autoplot()
                
                #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                
                # Plot residuals
                fit %>% gg_tsresiduals()
                
                #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                
                # Estimate parameters
                fit <- co2_ts %>%
                  model(AAdN = # Set up model)
                          
                          # Report fit
                          report(fit)
                        
                        #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                        
                        # Estimate parameters
                        fit <- co2_ts %>%
                          model(AAA = # Set up model)
                                  
                                  # Report fit
                                  report(fit)
                                
                                #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                                
                                # Plot components
                                components(fit) %>% autoplot()
                                
                                #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                                
                                # Plot residuals
                                fit %>% gg_tsresiduals()
                                
                                #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                                
                                # Estimate parameters
                                fit <- co2_ts %>%
                                  model(
                                    ANN = ETS(average ~ error("A") + trend("N") + season("N")),
                                    AAN = ETS(average ~ error("A") + trend("A") + season("N")),
                                    AAdN = ETS(average ~ error("A") + trend("Ad") + season("N")),
                                    AAA = ETS(average ~ error("A") + trend("A") + season("A")),
                                    AAdA = ETS(average ~ error("A") + trend("Ad") + season("A"))
                                  )
                                
                                # Report fit
                                report(fit)
                                
                                #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                                
                                # Estimate parameters
                                fit <- co2_ts %>%
                                  model(
                                    MNN = ETS(average ~ error("M") + trend("N") + season("N")),
                                    MAN = ETS(average ~ error("M") + trend("A") + season("N")),
                                    MAdN = ETS(average ~ error("M") + trend("Ad") + season("N")),
                                    MAA = ETS(average ~ error("M") + trend("A") + season("A")),
                                    MAdA = ETS(average ~ error("M") + trend("Ad") + season("A"))
                                  )
                                
                                # Report fit
                                report(fit)
                                
                                #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                                
                                # Estimate parameters
                                fit <- co2_ts %>%
                                  model(
                                    AAA = # Set up model)
                                  )
                                
                                # Components
                                components(fit) %>% autoplot
                                
                                #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                                
                                # Plot model
                                augment(fit) %>%
                                  ggplot(aes(x = yearmonth)) +
                                  geom_line(aes(y = average, colour = "Data")) +
                                  geom_line(aes(y = .fitted, colour = "Fitted")) +
                                  labs(y = "Average CO2 (ppm)", x = "Year and Month ") +
                                  scale_colour_manual(values = c(Data = "black", Fitted = "orange"))
                                
                                #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                                
                                # Automatic best fit parameters
                                fit <- co2_ts %>%
                                  model(ETS(average))
                                
                                # Report fit
                                report(fit)
                                
                                #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                                
                                # Difference in fit
                                co2_ts %>%
                                  model(
                                    auto = ETS(average),
                                    AAA = ETS(average ~ error("A") + trend("A") + season("A"))
                                  ) %>%
                                  accuracy()
                                
                                #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                                
                                # Forecast to 2050 from end of 2015
                                co2_train %>%
                                  model(ETS(average ~ error("A") + trend("A") + season("A"))) %>%
                                  forecast(h = 12 * 34)
                                
                                #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                                
                                # Plot forecast
                                co2_train %>%
                                  model(ETS(average ~ error("A") + trend("A") + season("A"))) %>%
                                  forecast(h = 12 * 34) %>% autoplot(co2_train) +
                                  geom_line(data = co2_test, aes(y = average), color = "orange") +
                                  labs(
                                    y = "Average CO2 (ppm)", x = "Year and Month ",
                                    title = "Forecasted CO2 for the Year 2050"
                                  ) +
                                  theme(
                                    axis.text = element_text(size = 14),
                                    axis.title = element_text(size = 16),
                                    plot.title = element_text(size = 20, hjust = 0.5),
                                    legend.title = element_text(size = 14),
                                    legend.text = element_text(size = 12)
                                  )
                                
                                #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                                
                                # Load validation data
                                co2_validation <- read.csv("../data/co2_mm_gl_validate.csv")
                                
                                # Convert year and month to date format
                                co2_validation$yearmonth <- yearmonth(
                                  as.yearmon(paste(co2_validation$year, co2_validation$month), "%Y %m")
                                )
                                
                                # Convert to `tsibble`
                                co2_validation <- as_tsibble(
                                  co2_validation, index = yearmonth
                                )
                                
                                #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                                
                                model %>%
                                  accuracy(data = co2_test) %>%
                                  select(RMSE, MAE, ME)
                                
                                #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                                
                                # Point estimates
                                co2_train %>%
                                  model(ETS(average ~ error("A") + trend("A") + season("A"))) %>%
                                  forecast(h = nrow(co2_test) + 12 * 1) %>%
                                  accuracy(data = co2_validation) %>%
                                  select(RMSE, MAE, ME)
                                
                                #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                                
                                # Winkler and Continuous Ranked Probability Scale
                                model %>%
                                  accuracy(
                                    data = co2_test,
                                    list(
                                      winkler = winkler_score,
                                      crps = CRPS
                                    )
                                  ) %>%
                                  select(winkler, crps)
                                
                                #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                                
                                # Point estimates
                                co2_train %>%
                                  model(ETS(average ~ error("A") + trend("A") + season("A"))) %>%
                                  forecast(h = nrow(co2_test) + 12 * 1) %>%
                                  accuracy(
                                    data = co2_validation,
                                    list(
                                      winkler = winkler_score,
                                      crps = CRPS
                                    )
                                  ) %>%
                                  select(winkler, crps)
                                
                                #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                                
                                # Plot forecast
                                co2_train %>%
                                  model(ETS(average ~ error("A") + trend("A") + season("A"))) %>%
                                  forecast(h = 12 * 34) %>% autoplot(co2_train) +
                                  geom_line(data = co2_test, aes(y = average), color = "orange") +
                                  geom_line(data = co2_validation, aes(y = average), color = "red") +
                                  labs(
                                    y = "Average CO2 (ppm)", x = "Year and Month ",
                                    title = "Forecasted CO2 for the Year 2050"
                                  ) +
                                  theme(
                                    axis.text = element_text(size = 14),
                                    axis.title = element_text(size = 16),
                                    plot.title = element_text(size = 20, hjust = 0.5),
                                    legend.title = element_text(size = 14),
                                    legend.text = element_text(size = 12)
                                  )
                                