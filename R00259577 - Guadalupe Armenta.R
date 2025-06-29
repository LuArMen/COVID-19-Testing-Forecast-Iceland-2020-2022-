library(tidyverse)
library(lubridate)
library(readxl)
library(zoo)
library(forecast)
library(fpp3)
library(scales)
setwd("C:/Users/lupit/Downloads")

#Loading the dataset
#COVID<-read_excel("TS-covid-data_Full.xlsx")  
#Quick check of the general data
#glimpse(COVID)

#When uploading, I notice that the new test variable is interpreted as logic variables. I have to adjust the reader's depth to ensure it processes the number correctly.
COVID <- read_excel(
  "TS-covid-data_Full.xlsx",
  guess_max = 100000
) %>%
  # new_tests sometimes comes in a weird format; force to numeric
  mutate(
    new_tests = as.character(new_tests),
    new_tests = parse_number(new_tests)
  )
#----------------
#1.Data cleaning
#----------------
###### Selecting  Iceland and check the reporting window #######
#Filering to Iceland location
Iceland <- COVID %>%
  filter(location == "Iceland") %>%
  mutate(
    date      = as.Date(date),
    new_tests = as.numeric(new_tests)
  ) %>%
  arrange(date)

glimpse(Iceland) #Overview of the choosen location

# Descriptive statitivs and Missing Values 
summary(Iceland$new_tests)
sum(is.na(Iceland$new_tests))

# The data source shows the last real report was on 2022-06-23
Iceland_window <- Iceland %>%
  filter(date >= as.Date("2020-01-03"), #limit to the real reporting window (2020‑01‑03 to 2022‑06‑23)
         date <= as.Date("2022-06-23"))

#Complete every day
Iceland_complete <- Iceland_window %>%
  #ensure every calendar day from start to end is present
  complete(date = seq(min(date), max(date), by = "day"))

#How many NAs do i need to fill?
  sum(is.na(Iceland_complete$new_tests))  
  

#### 1.1 Fill missing values using seasonal ARIMA interpolation ####
#In order to fill the missing values, I will apply the inter method as  uses a seasonal ARIMA internally (as the general data is prone to trend and seasonality).  
Iceland_full <- Iceland_complete %>%
    mutate(
      # na.interp() fills all NAs  using a seasonal ARIMA approach
     new_tests_interp = na.interp(new_tests)
    )
  
# To make sure there is no more NA on the data
sum(is.na(Iceland_full$new_tests_interp)) 

#Plot before vs. after imputation
plot_df <- Iceland_full %>%
  select(date, original = new_tests, filled = new_tests_interp) %>%
  pivot_longer(
    cols      = c(original, filled), # Reshape to logern pivot  for ggplot
    names_to  = "Series",
    values_to = "Tests")

#Plot both the raw (original) and imputed (filled) series together for a final sanity check.
ggplot(plot_df, aes(x = date, y = Tests, colour = Series)) +
  geom_line() +
  scale_colour_manual(values = c(original = "violet", filled = "green"),
    labels = c("Original (NA's)", "After Imputation (No NA's)")) +
  labs(title = "Daily COVID-19 Tests in Iceland: Before vs. After Filling Missing Values",
          x = "Date",y= "Number of Tests") 


#### 1.2  Weekly and Monthly Aggregations Data  ####
#We star with the Weekly aggregation
Iceland_weekly <- Iceland_full %>%
  mutate(week_start = floor_date(date, unit = "week", week_start = 1)) %>%   #to snap each day to the start of its week (Monday)
  # floor_date(week_start = 1) makes weeks start on Monday
  group_by(week_start) %>%
  summarise(tests_weekly = sum(new_tests_interp, na.rm = TRUE),
    .groups= "drop")
# Monthly data 
Iceland_monthly <- Iceland_full %>%
  mutate(month_start = floor_date(date, unit = "month")) %>% #to snap each day to the start of its  month
  group_by(month_start) %>%
  summarise(tests_monthly = sum(new_tests_interp, na.rm = TRUE),
    .groups= "drop")

#Quick look at counts
nrow(Iceland_weekly)   #how many weeks?
nrow(Iceland_monthly)  #how many monts?
# Check weekly & monthly range to double check the dates we seized correctly 
range(Iceland_weekly$week_start)
range(Iceland_monthly$month_start)

#---------------------------------
#2.Preliminary analysis
#---------------------------------

#### 2.1 Preliminary descriptive analysis on the data ####
# Daily data #
summary(Iceland_full$new_tests_interp)
sd(Iceland_full$new_tests_interp)
# Plotting daily
autoplot(as_tsibble(Iceland_full, index = date), new_tests_interp) +
  labs(title = "Daily COVID-19 Tests in Iceland",
       x= "Date", y = "Tests")

# Weekly data #
summary(Iceland_weekly$tests_weekly)
sd(Iceland_weekly$tests_weekly)
#Plotting  weekly
autoplot(as_tsibble(Iceland_weekly, index = week_start), tests_weekly) +
  labs(title = "Weekly COVID_19 Tests in Iceland",
       x= "Week start", y = "Tests")

# Monthly series #
summary(Iceland_monthly$tests_monthly)
sd(Iceland_monthly$tests_monthly)
autoplot(as_tsibble(Iceland_monthly, index = month_start), tests_monthly) +
  labs(title = "Monthly COVID-19 Tests in Iceland",
       x= "Month start", y = "Tests")

### 2.2 Time - Series Decomposition###
#STL to start a basic decomposition the series into trend (T), seasonal (S) and remainder (R).STL can follow gradual changes in the pandemic’s testing patterns over time. 
#Daily decomposition
Iceland_full |>
  as_tsibble(index = date) |>
  model(STL(new_tests_interp ~ season(window = "periodic"), robust = TRUE)) |>
  components() |>
  autoplot() + ggtitle("Daily STL Decomposition")
acf(Iceland_full$new_tests_interp, main = "ACF - Daily Tests") -> acf1
acf1
#Autocorrelation in the weekly ACF plot exhibits strong spikes at lag 7 (p=0.88), confirming weekly periodicity.

#Weekly decomposition
Iceland_weekly |>
  as_tsibble(index = week_start)|>
  model(STL(tests_weekly ~ season(window = "periodic"),  robust=TRUE)) |>
  components() |>
  autoplot() + ggtitle("Weekly STL Decomposition")

acf(Iceland_weekly$tests_weekly, main = "ACF - Weekly Tests")
#The autocorrelation decays rapidly toward zero by lag 5 and remains insignificant at higher lags (up to lag 20), implying no long-term persistence or seasonal patterns beyond a few weeks.

# Montlhy decomposition
Iceland_monthly %>%
  mutate(month_start = yearmonth(month_start)) %>%
  as_tsibble(index = month_start) %>%
  fill_gaps() %>%
  model(STL(tests_monthly ~ season(window = "periodic"), robust = TRUE)) %>%
  components() %>%
  autoplot() +
  ggtitle("Monthly STL Decomposition")


acf(Iceland_monthly$tests_monthly, main = "ACF - Montly Tests")
#This strong negative autocorrelation at alternating lags suggests a seasonal or periodic reversal effect, 
#where high testing volumes in one month are followed by disproportionately low volumes in the next, repeating every two months. 

#With the previews STL and ACF plot we can suggest that our daily data in general
#follows a high weekly seasonality

### Three periods for different waves of COVID (daily data set)###
#1st wave: First outbreak, early lock downs, the very first mass testing starts
#Dates: 	2020-03-01 to 2020-06-30
wave1 <- Iceland_full %>%
  filter(date >= as.Date("2020-03-01") & date <= as.Date("2020-06-30")) %>% 
  mutate(wave = "Wave 1: Mar–Jun 2020") #to latter plot in the ggplot

#2nd wave: 2020 Autumn wave, after the first lock down summer reopening
#Dates: 	2020-10-01 to 2021-01-31
wave2 <- Iceland_full %>%
  filter(date >= as.Date("2020-10-01") & date <= as.Date("2021-01-31")) %>% 
  mutate(wave = "Wave 2: Oct–Jan 2021") #to latter plot in the ggplot

#3rd wave: 	Omicron variant, likely again a peak in testing and due to the rapid spread
#Dates: 		2022-01-01 to 2022-03-31
wave3 <- Iceland_full %>%
  filter(date >= as.Date("2022-01-01") & date <= as.Date("2022-03-31")) %>% 
  mutate(wave = "Wave 3: Jan–Mar 2022") #to latter plot in the ggplot

#Mixing the 3 waves
waves <- list(Wave1 = wave1, Wave2 = wave2, Wave3 = wave3) #listing the waves so we can output a single descriptive summary
lapply(waves, function(wave) {
  list(Summary = summary(wave$new_tests_interp),
    SD      = sd(wave$new_tests_interp),
    N       = nrow(wave))})

#Plotting the 3 waves
waves_all <- bind_rows(wave1, wave2, wave3) #to fit the 3 lines into the plot
ggplot(waves_all, aes(x = date, y = new_tests_interp, color = wave)) +
  geom_line(size = 0.6) +
  labs(title = "Daily COVID-19 Tests in Iceland During the Three Waves",
    x = "Date", y = "Tests Number",
    color = "Waves") +
  theme_minimal() +theme(legend.position = "top") +
  scale_color_manual(values = c(
    "Wave 1: Mar–Jun 2020"= "red",
    "Wave 2: Oct–Jan 2021"= "blue",
    "Wave 3: Jan–Mar 2022"= "green"))

#---------------------------------
#3.Time series modelling
#---------------------------------
### 3.1 Exponational Smoothing Modelling ###
# As per the assignment request we are only using daily data
daily_ts <- Iceland_full |>
  as_tsibble(index = date)#conv. to tsibble 

#Fitting  4  ETS models:
ets_models <- daily_ts |>
  model(SES = ETS(new_tests_interp ~ error("A") + trend("N") + season("N")),
    Holt    = ETS(new_tests_interp ~ error("A") + trend("A") + season("N")),
    HW_Add  = ETS(new_tests_interp ~ error("A") + trend("A") + season("A")),
    HW_Mult = ETS(new_tests_interp ~ error("A") + trend("A") + season("M")))

#Comparing the models using evaluation 
glance(ets_models) |> arrange(AICc) |> select(.model:BIC)
#After checking comparing the AICs models,  the the best one was the multiplicative Holt–Winters (ETS(A, A, M))

#Checking the residuals plot of the Multiplicative method 
ets_models |>
  select(HW_Mult) |> gg_tsresiduals()

#Checking residuals with Ljung test
augment(ets_models["HW_Mult"]) |> 
  features(.innov, ljung_box, lag = 14)

#Checking the residual plots of the additive method, second best method (Additive one )
ets_models |>
  select(HW_Add) |> gg_tsresiduals()
#Checking residuals with Ljung test
augment(ets_models["HW_Add"]) |> 
  features(.innov, ljung_box, lag = 14)

### 3.2 Stationary ###

###Daily time- series##
# Start with full daily testing series (already imputed)
daily_ts <- Iceland_full %>%
  as_tsibble(index = date)

#Kwiatkowski-Phillips-Schmidt-Shin (KPSS) test. The H0 hypothesis is that the data are stationary.
daily_ts |> features(new_tests_interp, unitroot_kpss)  
#In this case, the p-value is shown as 0.01, indicating that the HO hypothesis is rejected. #That is, the data are not stationary.
#ACF & PACF of the original data 
daily_ts   |>
  gg_tsdisplay(new_tests_interp, plot_type='partial')+
  labs(title = "ACF & PACF  - Daily Time Series", y="")

#First difference 
daily_diff<-daily_ts |>
  mutate(diff_tests = difference(new_tests_interp)) 

#let's carry out the KPSS test again
daily_diff |> features(diff_tests , unitroot_kpss)

#Let's also use the unitroot_ndiffs to see if there is any need more differentiation
daily_diff |> features(diff_tests , unitroot_ndiffs)

#AFC & PACF after differentiation
daily_diff |>
  gg_tsdisplay(diff_tests, plot_type='partial')+
  labs(title = "Daily series  - Single differenced", y="")
#ACF decays rapidly to zero (except minor seasonal spikes at lags 7, 14, 21, 28).PACF shows no persistent autocorrelation
#Spikes at lags 7, 14, 21, 28 suggest weekly seasonality, but this does not violate stationarity. Seasonal patterns can be modeled separately (SARIMA with S=7).

#Even though the series is stationary in trend, it still shows strong seasonal autocorrelation, as seen in the differenced ACF plot. 
#The unitroot_ndiffs & unitroot_kpss both shows that the daily time series is already stationary, despite the
#ACF and PACF plotting showing some correlation 


###Weekly Series##

#Formatting the weekly series 
weekly_ts <- Iceland_weekly %>%
  as_tsibble(index = week_start)
##KPSS test (H0: stationary)
weekly_ts |>
  features(tests_weekly, unitroot_kpss) 
#According to the test outcome the data is non stationary p value = 0.01

#AFC & PACF after differenciation
weekly_ts  |>
  gg_tsdisplay(tests_weekly, plot_type='partial')+
  labs(title = "ACF & PACF  - Weekly Series", y="")
#The ACF support the idea that the data is clearly non stationarity as is higly correlated (ACF shows)

##First difference
weekly_diff <- weekly_ts |>
  mutate(diff_week = difference(tests_weekly))
#KPSS test again
weekly_diff |>
  features(diff_week, unitroot_kpss)
#P- value =  0.1 -> stationary data
#AFC & PACF after differenciation
weekly_diff |>
  gg_tsdisplay(diff_week, plot_type='partial')+
  labs(title = "Single differenced", y="")
#looking to the results we still can observed lags that look very correlated, let's apply a second differenciaton

#Double differenciation
weekly_2diff <- weekly_ts |>
  mutate(diff_week = difference(difference(tests_weekly)))

weekly_2diff |>
  features(diff_week, unitroot_kpss)
#P- value =  0.1 -> stationary data

#AFC & PACF after differenciation
weekly_2diff |>
  gg_tsdisplay(diff_week, plot_type='partial')+
  labs(title = "Double differenced", y="")

#After checkinh the results of the ACF & PACF plots Single differencing achieves stationarity while preserving interpretability. 
#The residual autocorrelation in the single-differenced series indicates potential seasonal ARIMA components (e.g., SARIMA(1,1,1)(1,0,1)[7]). 
#Double differencing is unnecessary and risks overfitting.

### Monthly Series ##

# Start with monthly data
monthly_ts <- Iceland_monthly %>%
  mutate(month_start = yearmonth(month_start)) %>% #coverting data into the correct format
  as_tsibble(index = month_start) %>%
  fill_gaps()  #only fills real time index gaps


#KPSS test (H0: stationary)
monthly_ts |> features(tests_monthly, unitroot_kpss)
#P- value = 0.0277

#ACF & PACF PLOTS
monthly_ts |>
  gg_tsdisplay(tests_monthly, plot_type = 'partial') +
  labs(title = "ACF & PACF - Monthly Series", y = "")
#the data seems to be  not very strong stationarity and indicated the p value and the ACF plots as only
#spike 1 and spike 2 are out of the correlation blue dot line

#First difference
monthly_diff <- monthly_ts |>
  mutate(diff_month = difference(tests_monthly))

#KPSS test again
monthly_diff |> features(diff_month, unitroot_kpss)

#ACF and PACF plots
monthly_diff |>
gg_tsdisplay(diff_month, plot_type='partial')+
  labs(title = "Differenced Monthly Series",y="")
#The data now is fully stationary, only spike 3 is outside of the dashed blue line 


## Waves data ##
#Wave 1: Mar–Jun 2020#
wave1_ts <- wave1 %>%
  as_tsibble(index = date)

# KPSS test (H0: stationary)
wave1_ts |>
  features(new_tests_interp, unitroot_kpss)
#P= 0.01 -> non stationary 

#This process of using a sequence of KPSS tests to determine the appropriate number of first differences is carried out using the unitroot_ndiffs() feature. 
#so now we will applied a new method to determine the differenciation needed
wave1_ts  |>
  features(new_tests_interp, unitroot_ndiffs)

#Because unitroot_ndiffss() returns 1 (indicating one seasonal difference is required), we apply one seasonal diff and log
wave1_log_diff<-  wave1_ts |>
  mutate(log_tests = difference(log(new_tests_interp), 7)) #7 because is daily data 

#Plotting ACF & PACF 
wave1_log_diff |>
  gg_tsdisplay(log_tests , plot_type = 'partial') +
  labs(title = "ACF & PACF - Wave 1 (Oct–Jan 2021), after log & difference", y = "")

#The differenced series appears stationary, but with remaining weekly seasonality (spikes 1,2 and 3) in the ACF PLOT. 

###Unitroot_ndiffs KPSS test once again
wave1_log_diff |>
  features(log_tests , unitroot_ndiffs)
#Return out 0, so that there is not more need for differentiation 

wave1_log_diff |>
  features(log_tests, unitroot_kpss) #P value = 0.10
#The differenced and log-transformed series for Wave 1 is stationary based on the KPSS test results and ACF/PACF analysis.

#Wave 2: Oct–Jan 2021##
wave2_ts <- wave2 %>%
  as_tsibble(index = date)

##KPSS test (H0: stationary)
wave2_ts |>
  features(new_tests_interp, unitroot_kpss)
#P= 0.01 -> non stationary 

#This process of using a sequence of KPSS tests to determine the appropriate number of first differences is carried out using the unitroot_ndiffs() feature. 
#so now we will applied a new method to determine the differenciation needed
wave2_ts |>
  features(new_tests_interp, unitroot_ndiffs)

#Because unitroot_ndiffss() returns 1 (indicating one seasonal difference is required), we apply one seasonal diff and log
wave2_log_diff<-  wave2_ts |>
  mutate(log_tests = difference(log(new_tests_interp), 7)) #7 because is daily data 
#Plotting ACF & PACF 
wave2_log_diff |>
  gg_tsdisplay(log_tests , plot_type = 'partial') +
  labs(title = "ACF & PACF - Wave 2 (Oct–Jan 2021), after log & difference", y = "")
#The differenced series appears stationary, but with remaining weekly seasonality (lag 7 and 14). 

###Unitroot_ndiffs KPSS test once again
wave2_log_diff |>
  features(log_tests , unitroot_ndiffs)
#Return out 0, so that there is not more need for differentiation 
wave2_log_diff |>
  features(log_tests, unitroot_kpss) #P value = 0.10
#The differenced and log-transformed series for Wave 2 is stationary based on the KPSS test results and ACF/PACF analysis.

### Wave 3: Jan–Mar 2022 ##
wave3_ts <- wave3 %>%
  as_tsibble(index = date)

##KPSS test (H0: stationary)
wave3_ts |>
  features(new_tests_interp, unitroot_kpss)
#P= 0.01 -> non stationary 

#This process of using a sequence of KPSS tests to determine the appropriate number of first differences is carried out using the unitroot_ndiffs() feature. 
#so now we will applied a new method to determine the differentiation needed
wave3_ts |>
  features(new_tests_interp, unitroot_ndiffs)

#Because unitroot_ndiffss() returns 1 (indicating one seasonal difference is required), we apply one seasonal diff and log
wave3_log_diff<-  wave3_ts |>
  mutate(log_tests = difference(log(new_tests_interp))) #For this time I am not fitting any number, because I tried to fit number 7 but it still look stationary

#Plotting ACF & PACF 
wave3_log_diff |>
  gg_tsdisplay(log_tests , plot_type = 'partial') +
  labs(title = "ACF & PACF - Wave 3 (Oct–Jan 2021), after log & differentiation", y = "")
#For the ACF, lag 7  and lag 14 are significant Seasonal Spikes. The series is stationary, but the weekly seasonal pattern (7-day cycle) remains.

###Unitroot_ndiffs KPSS test once again
wave3_log_diff |>
  features(log_tests , unitroot_ndiffs)
#Return out 0, so that there is not more need for differentiation 
wave3_log_diff |>
  features(log_tests, unitroot_kpss) #The differenced and log-transformed series (wave3_log_diff) is stationary at the 5% significance level
#The wave 3 series is stationary, but the ACF indicates residual weekly seasonality.

### 3.3 ARIMA model  ###

# Convert full daily series to tsibble
daily_ts <- Iceland_full %>% 
  as_tsibble(index = date)

#Book extract: A seasonal ARIMA model is formed by including additional seasonal terms in the ARIMA models we have seen so far. 
#It is written as follows: ARIMA (p,d,q) non seasonal part and (P,D Q)m seasonal part of the model
#m=seasonal period


#Hand‑specified SARIMA models with seasonal period = 7and no constan (0)
arima_models <- daily_ts %>%
  model(SARIMA_012_011 = ARIMA(new_tests_interp ~ pdq(0,1,2) + PDQ(0,1,1, period = 7)),
    SARIMA_011_001 = ARIMA(new_tests_interp ~ pdq(0,1,1) + PDQ(0,0,1, period = 7)),
    SARIMA_010_101 = ARIMA(new_tests_interp ~ pdq(0,1,0) + PDQ(1,0,1, period = 7) + 0),
    SARIMA_301_012 = ARIMA(new_tests_interp ~ pdq(3,0,1) + PDQ(0,1,2, period = 7) + 0),#This new model was mostly took from the book discussed in class as they were discussing a data set with very strong weekly seasonality, I decided to give it a chance
    auto = ARIMA(new_tests_interp, stepwise = FALSE, approx = FALSE))      ##By setting stepwise=FALSE and approximation=FALSE, we are making sure the model  work extra hard to find a good model.

#Let's see the resulting parameters of the three fitted models 
arima_models|> pivot_longer(everything(), names_to = "Model name",
                    values_to = "Orders")

#Comparing the models by AICc
glance(arima_models) |> arrange(AICc) |> select(.model:BIC)

#Best three models according to the evaluation metrics are SARIMA_301_012 , SARIMA_012_011  and the auto 

#Plot the residuals for the best model -> SARIMA_301_012
arima_models |> select(SARIMA_301_012)|> gg_tsresiduals(lag_max=14) +
  ggtitle("SARIMA(3,0,1)(0,1,2) - Residuals")


#Plot the residuals for the second -> SARIMA_012_011
arima_models |> select(SARIMA_012_011) |> gg_tsresiduals(lag=14) +
  ggtitle("SARIMA(0,1,2)(0,1,1) - Residuals") #

#Plot the residuals for the third model -> auto 
arima_models |> select(auto) |> gg_tsresiduals(lag=14) +
  ggtitle("AUTO ~ ARIMA(3,1,1)(2,0,0) - Residuals")#

#Residuals for the three models 
#SARIMA_012_011 ARIMA(0,1,2)(0,1,1)
augment(arima_models) |>
  filter(.model == "SARIMA_012_011") |>
  features(.innov, ljung_box, lag=7) 

#Auto model ARIMA(3,1,1)(2,0,0)
augment(arima_models) |>
  filter(.model == "auto") |>
  features(.innov, ljung_box, lag=7)

#model SARIMA_301_012
augment(arima_models) |>
  filter(.model == "SARIMA_301_012") |>
  features(.innov, ljung_box, lag=7)

#---------------
# Forecasting 
#--------------
#ETS and ARIMA forecasting 

### 4.1 ETS ###
#daily_ts <- Iceland_full %>%
 # as_tsibble(index = date) #again convert to tsibble

#Now  we split into training (all but last 28 days) and test (last 28 days)
#Using 28 ensures the test split and forecast horizon exactly cover that 4-week period.
horizon <- 28 #define the forecast horizon (4 weeks = 28 days)

# Split the data into training and test sets
# Assume the last 28 days are reserved for testing
train <- daily_ts|>
  filter(date < max(date) - horizon)

test <- daily_ts |>
  filter(date >= max(date) - horizon)


#Now we refit the two chosen ETS models on the training set
ets_models_train <- train  |>
  model(HW_Add  = ETS(new_tests_interp ~ error("A") + trend("A") + season("A")), #best model ETS in the 3.1 section
    HW_Mult = ETS(new_tests_interp ~ error("A") + trend("A") + season("M")), #second best model in the 3.1 section
    ETS_auto = ETS(new_tests_interp) #train a new auto ETS 
  )

#Let's see the resulting parameters of the three fitted models 
ets_models_train|> pivot_longer(everything(), names_to = "Model name",
                            values_to = "Orders")
##Comparing the models by AICc
#glance(ets_models_train) |> arrange(AICc) |> select(.model:BIC)

#best model ETS_auto (16199) and the second is HW_Mult (16901)

#The autoETS(M,N,M) model is multiplicative in both its error and seasonal components (the Ms), with no trend component (n for none). 
#Both the random fluctuations and the seasonal swings scale in proportion to the level of the series.

#In a way this a good choice for my type of data,  strong seasonality that changes with scale, but no clear long‑term upward or downward movement beyond the seasonal cycle.
#I should have done the AUTO ETS in the section 3.1, that would simplified the task


#Produce 28‑day forecasts
ets_fc <- ets_models_train |>
  forecast(h = horizon)

#computing the  accuracy metrics on the test set
ets_fc |>
  accuracy(test) |>
  select(.model, RMSE, MAE, MAPE)
#Model metrics output:
#.model     RMSE   MAE  MAPE
#ETS_auto   542.  485.  70.3
#HW_Add     560.  490.  63.6
#HW_Mult  20927. 4585. 726.


# Plot each model separately VS the actuals
#ETS_auto (ETS(M,N,M))
ets_fc |>
  filter(.model == "ETS_auto") |>
  autoplot(train, size=1.2) +
  autolayer(test, new_tests_interp, colour = "violet") +
  labs(title = "ETS(M,N,M) Forecast vs Actual (4‑Week)",
    subtitle = "Automatic ETS (multiplicative errors & seasonality, no trend)",
    x = "Years", y = "Daily Tests") + theme_minimal()


#Holt–Winters Additive (ETS(A,A,A))
ets_fc |>
  filter(.model == "HW_Add") |>
  autoplot(train, size=1.2) +
  autolayer(test, new_tests_interp, colour = "green") +
  labs(title = "ETS(A,A,A) Forecast vs Actual (4 Week)",
    subtitle = "Holt-Winters Additive",
    x = "Years", y = "Daily Tests") + theme_minimal()

#Holt–Winters Multiplicative (ETS(A,A,M))
ets_fc |>
  filter(.model == "HW_Mult") |>
  autoplot(train, size=1.2) +
  autolayer(test, new_tests_interp, colour = "red") +
  labs(title = "ETS(A,A,M) Forecast vs Actual (4 Week)",
    subtitle = "Holt-Winters Multiplicative",
    x = "Years", y = "Daily Tests") + theme_minimal()

#Plot the three forecasts over the original data 
ets_fc %>%
  autoplot(train, size=1.2) +
  autolayer(test, new_tests_interp) +
  facet_wrap(~.model, ncol = 1) +
  labs(title = "Four-Week ETS Forecasts vs Actuals",
    x     = "Year",y     = "Daily Tests") +theme_minimal()
#The four-week forecasts from the ETS(M,N,M) model dshows moderate alignment with actual daily COVID-19 test volumes in the Iceland. 
#The multiplicative seasonality assumption captures the weekly testing patterns observed in the STL decompositio, 
#but the absence of a trend component limits its ability to adapt to gradual shifts in testing demand over the 2020–2022 period. 
#Compared to the additive Holt-Winters (HW_Add) and multiplicative Holt-Winters (HW_Mult) models, the ETS_auto forecasts show tighter adherence to short-term fluctuations, 
#though all models underestimate extreme peaks during pandemic waves. 

### 4.2 ARIMA forecast ###

#Using same training and test split as in ETS
h <- 28
train_ARIMA <- daily_ts %>% filter(date < max(date) - h)
test_ARIMA  <- daily_ts %>% filter(date >= max(date) - h)

# Fit two models: Auto ARIMA and SARIMA(3,0,1)(0,1,2)[7]
arima_models_train <- train_ARIMA |>
  model(ARIMA_auto      = ARIMA(new_tests_interp), #This time I am not using the stepwise function, cause  complx models is not always good
    SARIMA_301_012  = ARIMA(new_tests_interp ~ pdq(3,0,1) + PDQ(0,1,2, period = 7) + 0))
#Let's see the resulting parameters of the three fitted models 
arima_models_train|> pivot_longer(everything(), names_to = "Model name",
                                values_to = "Orders")

#Model name                     Orders
#ARIMA_auto     <ARIMA(0,1,2)(2,0,0)>
#SARIMA_301_012 <ARIMA(3,0,1)(0,1,2)>

#Forecasting 28 days ahead
arima_fc <- arima_models_train|>
  forecast(h = h)

#Accuracy metrics
arima_fc |>
  accuracy(test_ARIMA) |>
  select(.model, RMSE, MAE, MAPE)

##ARIMA accuracy results
#model          RMSE   MAE  MAPE
#ARIMA_auto      594.  497.  85.1
#SARIMA_301_012  814.  650. 111. 

#Individual forecast plots
#Auto ARIMA(0,1,2)(2,0,0)
arima_fc %>%
  filter(.model == "ARIMA_auto") %>%
  autoplot(train_ARIMA, size=1.2) +
  autolayer(test_ARIMA, new_tests_interp, colour = "pink") +
  labs(title = "ARIMA(0,1,2)(2,0,0) Forecast vs Actual",
    subtitle = "Auto ARIMA (28-day horizon)",
    y = "Daily Tests", x= "Years") +theme_minimal()


#SARIMA (3,0,1)(0,1,2)
arima_fc %>%
  filter(.model == "SARIMA_301_012") %>%
  autoplot(train_ARIMA, size=1.2) +
  autolayer(test_ARIMA, new_tests_interp, colour = "gold") +
  labs(title = "SARIMA(3,0,1)(0,1,2) Forecast vs Actual",
    subtitle = "Manual SARIMA Model (28-day horizon)",
    y = "Daily Tests", x= "Years") +theme_minimal()

# Plot both models for comparison
arima_fc %>%
  autoplot(train_ARIMA, size=1.2) +
  autolayer(test_ARIMA, new_tests_interp) +
  facet_wrap(~.model, ncol = 1) +
  labs(title = "Four-Week ARIMA Forecasts vs Actuals",
       y = "Daily Tests", x = "Years") +theme_minimal()
#While both models reflect the general decline in testing post-2021 peaks, the SARIMA model’s explicit accommodation of dayli data weekly operational rhythms ensures greater accuracy.
#the manually tuned SARIMA model is superior, as it directly addresses the seasonal dynamics and volatility inherent to the series,
#wereas the Auto ARIMA’s generic structure sacrifices responsiveness for simplicity.

### 4.3 Comparison between models ###
#Prepare data and split into training/test sets
#h <- 28
train_comp <- daily_ts |> filter(date < max(date) - h)
test_comp  <- daily_ts |> filter(date >= max(date) - h)

#We fit  the two automatic models on the training data
comparison_models <- train_comp |>
  model(
    ETS_auto   = ETS(new_tests_interp), # ETS(M,N,M)
    ARIMA_auto = ARIMA(new_tests_interp ~ pdq(3,1,1) + PDQ(2,0,0, period = 7))#ARIMA(3,1,1)(2,0,0)
  )

#checking the coefficients of the model 
#ETS auto
comparison_models|> select(ETS_auto) |> report()|> 
  gg_tsresiduals(lag_max = 28) # residuals plots
#lag 7(0.1), 14(less than 0.10) and 21 (around 0.11) go out from the blue dotted line, but not significantly 
#spike 1 go out more significantly around 0.20

#Ljung_box test
comparison_models|> select(ETS_auto) |> augment() |>
  features(.innov, ljung_box, lag = 7)
#P value 1.45e-12

#ARIMA
comparison_models|> select(ARIMA_auto) |>report()|> 
  gg_tsresiduals(lag_max = 28) # residuals plots
#lag 14 (-0.06) and 21 (0.16) go out from the dotted line , lag 7 stays inside the blue dotted line
#Ljung_box test
comparison_models|> select(ARIMA_auto) |> augment() |>
  features(.innov, ljung_box, lag = 7)
#p_value = 0.974

#28‑day forecasts from each model
comparison_fc <- comparison_models %>%
  forecast(h = h)

#computing and display accuracy metrics on the test set
accuracy(comparison_fc, test_comp) |>
  select(.model, RMSE, MAE, MAPE) |>
  print()

#results
#model      RMSE   MAE  MAPE
#ARIMA_auto  562.  486.  78.9
#ETS_auto    542.  485.  70.3

##Plot fitted vs actual
comparison_fc |>
  autoplot(train_comp, level = NULL, size=1.2) +
  autolayer(test_comp, new_tests_interp) +
  facet_wrap(~ .model, ncol = 1) +
  labs(title = "ETS(M,N,M) vs ARIMA(3,1,1)(2,0,0) 4-Week Forecast Comparison",
    x = "Years", y = "Daily Tests") + theme_minimal()

###4.4 Different Country##
#I decided to go for UAE

#First we define the reporting windows as we did prev
start_date <- as.Date("2020-01-03")
end_date   <- as.Date("2022-06-23")

#Extracting UAE data
UAE<- COVID %>%
  filter(location == "United Arab Emirates") %>% #choosing the UAE
  mutate(date = as.Date(date)) %>%
  filter(date >= start_date, date <= end_date) %>% #slicing the data into the start and end window
  arrange(date) %>%
  mutate(new_tests_interp = na.interp(as.numeric(new_tests))) %>% #handling missing values
  as_tsibble(index = date) #to fit into the ARIMA models

#Quick look at missingness and summary
sum(is.na(UAE$new_tests_interp))   
#0 after the interppolation
summary(UAE$new_tests_interp)

#Lets start by checking the general data 
autoplot(UAE, new_tests_interp) + scale_y_continuous(labels = comma_format())
  labs(title = "Daily COVID‑19 Tests in UAE", x = "Date", y = "Tests")

#Let's start by checking the components of the this daily time series with a STL decompositions 
#STL decomposition
#Fit the STL model
stl_fit <- UAE |>
  model(STL(new_tests_interp))
#Plot the decomposition to see the components 
stl_fit |>
  components() |> autoplot() +
  scale_y_continuous(labels = comma_format()) + # to make sure the number are well represented as some of them test daily go around the mean 191032
  labs(title = "STL Decomposition of UAE Daily Tests",
       x= "Date", y= "Number of Tests") + theme_minimal()
#Interpretation of  time series: While yearly seasonality is less pronounced due to the short timeframe, the weekly component dominates, 
#reinforcing findings from Section 3.2 on the importance of weekly cycles in modeling this series.
#The trend component shows a fluctuating pattern, peaking at approximately 400,000 tests during major pandemic waves and declining during periods of reduced transmission.
#Strong 7-day cycles are evident, with testing volumes dipping on weekends (30,000 fewer tests) and peaking on weekdays
#Subtler annual patterns suggest slight increases in testing during cooler months (late 2021), possibly tied to seasonal respiratory virus trends or policy adjustments.
#Let's fit the AUTO ARIMA on full UAE series
#OutofSample Forecast
#we proceed with the same test set of 28 days 
#h=28
train_UAE <- UAE |> filter(date < max(date) - h)
test_UAE  <- UAE |> filter(date >= max(date) - h)

UAE_fit <- train_UAE|>
  model(Auto = ARIMA(new_tests_interp))
#Check models parameters
UAE_fit|> pivot_longer(everything(), names_to = "Model name",
                       values_to = "Orders")
#both models are the same , even I tried to make the function to see more in detailed 

#Forecasting 28 days ahead
UAE_fc <- UAE_fit|>
  forecast(h = h)

#Check the accuracy metrics on the hould out test
UAE_fc |>
  accuracy(test_UAE) |>
  select(.model, RMSE, MAE, MAPE)

#Because the models are the same I will only plot one of the,
UAE_fc  |>
  filter(.model == "Auto") %>%
  autoplot(train_UAE, size=1.2) + 
  scale_y_continuous(labels = comma_format()) +
  autolayer(test_UAE, new_tests_interp, colour = "blue") +
  facet_wrap(~ .model, ncol = 1) +
  labs(title = "ARIMA(1,1,1)(2,0,0)[7] 4-Week Forecast Comparison",
       x = "Years", y = "Daily Tests") + theme_minimal()


#Now I will try to fit the best fitted model of the Iceland dataset into the UAE dataset , to see if that model could work goor or not
#in this data structure
#Fit the Iceland “best” ARIMA(3,1,1)(2,0,0)[7] to the UAE data:
UAE_arima_Iceland <- train_UAE |>
  model(SARIMA_301_012 = ARIMA(new_tests_interp ~ 
        pdq(3, 0, 1) +           #non‑seasonal (p=3, d=0, q=1)
        PDQ(0, 1, 2, period = 7) + #seasonal (P=0, D=1, Q=2) with period 7
        0))                      # no constant term

##Inspect the fitted model
report(UAE_arima_Iceland)

##Check residual diagnostics (up to 28 lags to match one‐month horizon)
UAE_arima_Iceland |> gg_tsresiduals(lag_max = 28)
#In this case only one spike goees out of the blue dotted line and all other remains inside , 
#the innovation residuas stay around 0 but they seems a bit noisy


#Ljung_box test for the models
UAE_arima_Iceland |> augment() |>
  features(.innov, ljung_box, lag = 28)
#P value = 0.100

#The Auto Arima fitted before 
UAE_fit |> select(Auto) |> augment() |>
  features(.innov, ljung_box, lag = 28)
#0.00000000651

#Forecasting 28 days ahead
UAE_fc_Iceland <- UAE_arima_Iceland |>
  forecast(h = h)

#Check the accuracy metrics on the hould out test
UAE_fc_Iceland  |>
  accuracy(test_UAE) |>
  select(.model, RMSE, MAE, MAPE)

## Plot the 4-week forecast
UAE_fc_Iceland |>
  autoplot(train_UAE, level = NULL) + 
  scale_y_continuous(labels = comma_format()) +
  autolayer(test_UAE, new_tests_interp) +
  facet_wrap(~ .model, ncol = 1) +
  labs( title = "Four-Week Forecast: SARIMA(3,0,1)(0,1,2)[7] on UAE Data",
        x = "Years",y = "Daily Tests") + theme_minimal()

#Bind them and to plot them
comparison_fc <- bind_rows(UAE_fc , UAE_fc_Iceland ) 

#Plot both forecasts against training data and test data
comparison_fc  %>%
  autoplot(train_UAE, size   = 1.2) +                
  autolayer(test_UAE, new_tests_interp, colour = "black") + 
  facet_wrap(~ .model, ncol = 1) +
  scale_y_continuous(labels = comma_format()) +
  labs(title    = "Four-Week Forecasts on UAE Data",
    subtitle = "Auto-selected ARIMA vs Iceland-tuned SARIMA",
    x= "Year", y= "Daily Tests") +theme_minimal()

#Sarima MODELS from the Iceland time series seems a better fit as it follows better the trend, better forecats
#The four-week forecasts for UAE daily COVID-19 testing data, comparing the auto-selected and the manually tuned SARIMA -Iceland  , show distinct performance. 
#The SARIMA model produces forecasts that closely track the observed volatility in testing demand, capturing short-term spikes and weekly seasonality more effectively than the auto ARIMA.
#Its prediction intervals (not shown but inferred) are likely narrower, reflecting greater confidence in seasonal patterns identified in prior analyses (STL decomposition’s strong weekly cycles). 
#In contrast, the auto ARIMA’s forecasts appear smoother, underestimating abrupt changes seen during pandemic waves. Given the UAE data’s operational weekly rhythms and responsiveness to outbreaks, the SARIMA model’s incorporation of seasonal differencing 
#(D=1) and tailored parameters aligns better with the data’s inherent dynamics, making it the more reliable choice for short-term forecasting.

