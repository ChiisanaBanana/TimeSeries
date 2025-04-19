#################
# Code to create Figure 3-1 in PTSF book (NOTE: there are several packages needed for this to work...see separate script file)

Amtrak <- read.csv("Data/Amtrak.csv") #change to match your file path

# create tsibble for ridership variable in Amtrak dataset
ridership <- Amtrak |>
  mutate(Month = yearmonth(as.character(Amtrak$Month))) |>
  as_tsibble(index = Month)

# create plot note choices of training and validation partition lengths
ridership |>
  autoplot(Ridership) +
  xlab("Time") + ylab("Ridership")  +
  geom_vline(xintercept = as.numeric(as.Date(yearmonth("2001-April"))), linetype = "solid", color = "grey55", size = 0.6)+
  geom_segment(aes(x = yearmonth("2001-May"), y = 2250, 
                   xend = yearmonth("2004-Mar"), yend = 2250),
               arrow = arrow(length = unit(0.25, "cm"), ends = "both") , size = 0.3, color = "grey55")+ 
  annotate(geom = "text", x = yearmonth("2002-Aug"), y = 2280, label = "Validation", color = "grey37") +
  
  geom_segment(aes(x = yearmonth("1991-Jan"), y = 2250, 
                   xend = yearmonth("2001-Mar"), yend = 2250),
               arrow = arrow(length = unit(0.25, "cm"), ends = "both"), size = 0.3, color = "grey55")+ 
  annotate(geom="text", x = yearmonth("1996-Aug"), y = 2280, label = "Training", color = "grey37")+
  scale_x_yearmonth(date_breaks = "2 years", date_labels = "%Y")
dev.off()

# create training and validation partition objects (note lengths in environment window when complete)
train.ridership <- ridership |> filter_index(~ "2001 Mar") 
valid.ridership <- ridership |> filter_index("2001 Apr" ~ .)

# Fit a quadratic trend model to training period
ridership.lm <- train.ridership |>
  model(trend_model = TSLM(Ridership ~ trend() + I(trend()^2)))
augment(ridership.lm) # print table with fitted values and residuals

# Get forecasts for validation period
fc <- ridership.lm |> 
  forecast(h = nrow(valid.ridership))

# Compute validation errors
fc <- fc |>
  mutate(fc.error = valid.ridership$Ridership - fc$.mean) |>
  as_tsibble(index = Month)

# Plot 1: actuals and forecasts (save plot as p.model)
p.model <- autoplot(ridership, Ridership) +
  autolayer(fitted(ridership.lm), .fitted, color = "blue", size = 1.25) + 
  autolayer(fc, .mean, linetype = "dashed", color = "blue", size = 1.25) +
  scale_x_yearmonth(date_breaks = "2 years", date_labels = "%Y") +
  labs(title = "Actual and forecasted ridership", y = "Ridership") +
  geom_vline(xintercept = as.numeric(as.Date(yearmonth("2001-April"))), linetype = "solid", color = "grey55", size = 0.6) +
  geom_segment(aes(x = yearmonth("2001-May"), y = 2250, 
                   xend = yearmonth("2004-Mar"), yend = 2250),
               arrow = arrow(length = unit(0.25, "cm"), ends = "both") , color = "grey55") +  
  annotate(geom = "text", x = yearmonth("2002-Aug"), y=2290, label = "Validation", color = "grey37") +
  geom_segment(aes(x = yearmonth("1991-Jan"), y = 2250, 
                   xend = yearmonth("2001-Mar"), yend = 2250),
               arrow = arrow(length = unit(0.25, "cm"), ends = "both"), color = "grey55") +  
  annotate(geom = "text", x = yearmonth("1996-Aug"), y=2290, label = "Training", color = "grey37")
p.model

# Plot 2: errors (save plot as p.errors)
p.errors <- autoplot(resid(ridership.lm), .resid) + 
  autolayer(fc, fc.error, linetype = "dashed") + 
  geom_hline(yintercept = 0) + 
  scale_x_yearmonth(date_breaks = "2 years", date_labels = "%Y") +
  labs(title = "Errors", y = "Error") +
  geom_vline(xintercept = as.numeric(as.Date(yearmonth("2001-April"))), linetype = "solid", color = "grey55", size = 0.6)
p.errors

#### Figure 3.4 (histogram):
fc |> 
  ggplot(aes(x = fc.error)) + # validation errors
  geom_histogram(bins = 7, color = "black", fill = "white") +
  xlab("Forecast Error") + ylab("Frequency")
dev.off()
