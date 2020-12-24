### Fitbit.R - Olof Hoverfält - 2020

# Playground for manipulating and plotting Fitbit data
# Input: personal Fitbit profile data
# Using the fitbitr package: https://github.com/teramonagi/fitbitr

####################################################
################ SET UP ENVIRONMENT ################
####################################################

# Remove all objects from workspace
rm(list = ls())

# Load required packages
library("fitbitr")
library("ggplot2")
library("dplyr")
library("tidyr")
library("roll")
library("tidyverse")
library("lubridate")


# Source required files
source("Fitbit-API-Key.R")

# Set Fitbit key and secret
FITBIT_KEY    <- get_fitbit_key()
FITBIT_SECRET <- get_fitbit_secret()
FITBIT_CALLBACK <- "http://localhost:1410/" 

# Authenticate and get token
token <- fitbitr::oauth_token()


##########################################
################ ACTIVITY ################
##########################################

# Fitbit API: Time series options are 1d, 7d, 30d, 1w, 1m, 3m, 6m, 1y
# Fitbit API: Intraday time series number of data points to include either 1min or 15min.
# Fitbit API: https://dev.fitbit.com/build/reference/web-api/activity/


# Set test date
date <- "2020-12-20"

# Get daily activity summary
activity_summary <- get_activity_summary(token, date)
activity_summary$activities

# Get daily step data for entire item data period and remove duplicates
steps_2020 <- get_activity_time_series(token, "steps", date=date, period="1y")
steps_2019 <- get_activity_time_series(token, "steps", date="2019-12-31", period="1y")
steps_2018 <- get_activity_time_series(token, "steps", date="2018-12-31", period="1y")
steps <- rbind(steps_2020, rbind(steps_2018, steps_2019))
steps <- steps[!duplicated(steps$dateTime),]

# Remove temporary variables
rm(steps_2020)
rm(steps_2019)
rm(steps_2018)

# Convert variables to correct type and arrange by date
steps <- steps %>%
  #mutate(date = as.POSIXct(strptime(steps$dateTime, "%Y-%m-%d"))) %>%
  mutate(steps = as.numeric(value)) %>%
  select(-dateTime, -value) %>%
  arrange(date)

# Save steps data.frame to file for easier retrieval
save(steps,file="Data/steps-2020-12-03.Rda")

# Load data from file
load("Data/steps-2020-12-03.Rda")

# Plot steps
ggplot2::ggplot(steps, aes(x=date, y=steps)) + geom_col()

# Histogram of steps
steps %>%
  filter(date >= "2016-01-01" & date <= "2020-12-31") %>%
  ggplot(aes(steps)) +
  geom_histogram(binwidth = 1000)

# Summarise steps by week 
steps_by_week <- steps %>% group_by(year= year(date), week = week(date)) %>% summarise(steps = sum(steps)) %>% as.data.frame()

p <- steps_by_week %>% filter(year == 2020) %>%
  ggplot(aes(x=week, y=steps)) +
  geom_line(color='steelblue', size=1) +
  coord_polar() +
  scale_y_continuous(limits=c(0, NA)) +
  scale_x_continuous(limits=c(1, 52))
  #scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m") +
  #labs(x = "Date", y = "Resting heart rate with 7-day and 30-day rolling average") +
  #theme(axis.text.x = element_text(angle = 90))
#ggsave(filename = "Plots/Resting_HR.png", p, width = 40, height = 6, dpi = 300, units = "in", device=png())


# Plot with rolling averages 

rolling_average_window1 <- 7
rolling_average_window2 <- 30
ymax <- 25000

p <- steps %>% mutate(date = as.Date(date), year = year(date), day = yday(date)) %>% # Strip POSIXct formatting to be interpretable by ggplot
  filter(!is.na(steps)) %>% 
  mutate(averageSteps1 = roll_mean(steps, rolling_average_window1)) %>%
  mutate(averageSteps1 = lead(averageSteps1, round(rolling_average_window1/2), digits = 0)) %>% # Shift rolling avg to midpoint of sample
  mutate(averageSteps2 = roll_mean(steps, rolling_average_window2)) %>%
  mutate(averageSteps2 = lead(averageSteps2, round(rolling_average_window2/2), digits = 0)) %>% # Shift rolling avg to midpoint of sample
  filter(year(date) == 2020) %>%
  ggplot(aes(x=date, y=steps)) +
  #geom_line(color='lightgray', size=1) +
  geom_line(aes(x = date, y = averageSteps1), color='steelblue', size=1) +
  geom_line(aes(x = date, y = averageSteps2), color='red', size=1) +
  coord_polar() +
  scale_y_continuous(limits=c(0,ymax), breaks=c(10000, 14286, 25000)) +
  scale_x_date(limits=c(as.Date("2020-01-01"), as.Date("2020-12-31")), date_breaks = "1 month", date_labels = "%Y-%m") +
  labs(x = "Date", y = "Dail ysteps with 7-day and 30-day rolling average")

temp <- steps %>% mutate(date = as.Date(date))
month(temp$date)





# Get activity intraday time series
steps <- get_activity_intraday_time_series(token, "steps", date, detail_level="1min")
steps$time <- as.POSIXct(strptime(paste0(steps$dateTime, " ", steps$dataset_time), "%Y-%m-%d %H:%M:%S"))
ggplot2::ggplot(steps, aes(x=time, y=dataset_value)) + geom_col()


# Get Activity Types (complicated nested list)
get_activity_types(token)

# Get Activity Type (Walk=90013)
get_activity_type(token, 90013)

# Get Frequent Activities
get_frequent_activities(token)

# Get Recent Activities
get_recent_activity_types(token)

# Get Lifetime Stats
str(get_lifetime_stats(token))



############################################
################ HEART RATE ################
############################################

# Fitbit API: https://dev.fitbit.com/build/reference/web-api/heart-rate/


# Set test date
date <- "2020-12-20"

# Get heart rate time series
heart_rate <- get_heart_rate_time_series(token, date=date, period="7d")
heart_rate$value$restingHeartRate



# Get intraday heart rate time series
heart_rate <- get_heart_rate_intraday_time_series(token, date=date, detail_level="1min")

# Add date
heart_rate$date = date

# Convert variables to correct type
heart_rate <- heart_rate %>%
  mutate(dateTime = as.POSIXct(strptime(paste0(date, " ", time), "%Y-%m-%d %H:%M:%S"))) %>%
  mutate(heart_rate = as.numeric(value)) %>%
  select(-date, -time, -value)

# Plot heart rate
ggplot2::ggplot(heart_rate, aes(x=dateTime, y=heart_rate)) + geom_line()



## Resting heart rate time plot #######################################################

# Get daily resting heart rate data for entire data period and remove duplicates
heart_rate_2020 <- get_heart_rate_time_series(token, date=date, period="1y")
heart_rate_2019 <- get_heart_rate_time_series(token, date="2019-12-31", period="1y")
heart_rate_2018 <- get_heart_rate_time_series(token, date="2018-12-31", period="1y")
heart_rate_2017 <- get_heart_rate_time_series(token, date="2017-12-31", period="1y")
heart_rate_2016 <- get_heart_rate_time_series(token, date="2016-12-31", period="1y")
heart_rate_2015 <- get_heart_rate_time_series(token, date="2015-12-31", period="1y")

# Remove 2019 dates from 2020
heart_rate_2020 <- heart_rate_2020 %>% filter(dateTime >= as.Date("2020-01-01"))

resting_HR_2020 <- heart_rate_2020 %>% select(dateTime) %>% mutate(restingHeartRate = heart_rate_2020$value$restingHeartRate)
resting_HR_2019 <- heart_rate_2019 %>% select(dateTime) %>% mutate(restingHeartRate = heart_rate_2019$value$restingHeartRate)
resting_HR_2018 <- heart_rate_2018 %>% select(dateTime) %>% mutate(restingHeartRate = heart_rate_2018$value$restingHeartRate)
resting_HR_2017 <- heart_rate_2017 %>% select(dateTime) %>% mutate(restingHeartRate = heart_rate_2017$value$restingHeartRate)
resting_HR_2016 <- heart_rate_2016 %>% select(dateTime) %>% mutate(restingHeartRate = heart_rate_2016$value$restingHeartRate)
resting_HR_2015 <- heart_rate_2015 %>% select(dateTime) %>% mutate(restingHeartRate = heart_rate_2015$value$restingHeartRate)

resting_HR <- rbind(resting_HR_2020, resting_HR_2019, resting_HR_2018, resting_HR_2017, resting_HR_2016, resting_HR_2015)

# Remove temporary variables
rm(resting_HR_2020)
rm(resting_HR_2019)
rm(resting_HR_2018)
rm(resting_HR_2017)
rm(resting_HR_2016)
rm(resting_HR_2015)

resting_HR <- resting_HR %>% mutate(date = as.Date(dateTime)) %>% select(date, restingHeartRate) %>% arrange(desc(date))

# Only include dates on and after 2015-04-23 for which there is HR data
resting_HR <- resting_HR %>% filter(date > "2015-04-22")

# Save hearrate data frame to file for easier retrieval
save(resting_HR,file="Data/resting_HR-2020-12-10.Rda")

# Load data from file
load("Data/resting_HR-2020-12-10.Rda")
resting_HR %>% arrange(desc(date))

# Plot with rolling averages 

rolling_average_window1 <- 7
rolling_average_window2 <- 30

p <- resting_HR %>% filter(!is.na(restingHeartRate)) %>%
  mutate(averageHeartRate1 = roll_mean(restingHeartRate, rolling_average_window1)) %>%
  mutate(averageHeartRate1 = lead(averageHeartRate1, round(rolling_average_window1/2), digits = 0)) %>% # Shift rolling avg to midpoint of sample
  mutate(averageHeartRate2 = roll_mean(restingHeartRate, rolling_average_window2)) %>%
  mutate(averageHeartRate2 = lead(averageHeartRate2, round(rolling_average_window2/2), digits = 0)) %>% # Shift rolling avg to midpoint of sample
  ggplot(aes(x=date, y=restingHeartRate)) +
  geom_line(color='lightgray', size=1) +
  geom_line(aes(x = date, y = averageHeartRate1), color='steelblue', size=1) +
#  geom_line(aes(x = date, y = averageHeartRate2), color='red', size=1) +
  scale_y_continuous(limits=c(65,90)) +
  scale_x_date(limits=c(as.Date("2015-04-22"),NA), date_breaks = "1 month", date_labels = "%Y-%m") +
  labs(x = "Date", y = "Resting heart rate with 7-day and 30-day rolling average") +
  theme(axis.text.x = element_text(angle = 90))
ggsave(filename = "Plots/Resting_HR.png", p, width = 40, height = 6, dpi = 300, units = "in", device=png())



# Histogram of resting heartrate
p <- resting_HR %>%
  ggplot(aes(restingHeartRate)) +
  geom_histogram(binwidth = 1)







#######################################
################ SLEEP ################
#######################################

# Fitbit API: https://dev.fitbit.com/build/reference/web-api/sleep/


# Get Sleep Logs(date is character or Date)
x <- get_sleep_logs(token, date)

# Set test date
date <- "2020-12-20"

#Get Sleep Time Series
#get_sleep_time_series(token, "timeInBed", date, period="7d")
#get_sleep_time_series(token, "efficiency", date, period="1y")


# Get daily sleep efficiency data for entire data period and remove duplicates
sleep_efficiency_2020 <- get_sleep_time_series(token, "efficiency", date, period="1y")
sleep_efficiency_2019 <- get_sleep_time_series(token, "efficiency", date="2019-12-31", period="1y")
sleep_efficiency_2018 <- get_sleep_time_series(token, "efficiency", date="2018-12-31", period="1y")
sleep_efficiency_2017 <- get_sleep_time_series(token, "efficiency", date="2017-12-31", period="1y")
sleep_efficiency_2016 <- get_sleep_time_series(token, "efficiency", date="2016-12-31", period="1y")
sleep_efficiency_2015 <- get_sleep_time_series(token, "efficiency", date="2015-12-31", period="1y")

# Remove 2019 dates from 2020
sleep_efficiency_2020 <- sleep_efficiency_2020 %>% filter(dateTime >= as.Date("2020-01-01"))

sleep_efficiency <- rbind(sleep_efficiency_2020, sleep_efficiency_2019, sleep_efficiency_2018, sleep_efficiency_2017, sleep_efficiency_2016, sleep_efficiency_2015)

# Remove temporary variables
rm(sleep_efficiency_2020)
rm(sleep_efficiency_2019)
rm(sleep_efficiency_2018)
rm(sleep_efficiency_2017)
rm(sleep_efficiency_2016)
rm(sleep_efficiency_2015)

# Format variables
sleep_efficiency <- sleep_efficiency %>%
  mutate(date = as.Date(dateTime), efficiency = as.numeric(value)) %>%
  select(date, efficiency) %>%
  arrange(desc(date))

# Replace zeros with NAs
sleep_efficiency$efficiency[sleep_efficiency$efficiency == 0] <- NA

# Only include dates on and after 2015-04-23 to match the resting HR data
sleep_efficiency <- sleep_efficiency %>% filter(date > "2015-04-22")

# Set efficiency score to NA for dates prior to algorithm change around 2017-06-01
sleep_efficiency$efficiency[sleep_efficiency$date < "2017-06-01"] <- NA

# Save hearrate data frame to file for easier retrieval
save(sleep_efficiency,file="Data/sleep_efficiency.Rda")
# Load data from file
load("Data/sleep_efficiency.Rda")



# Plot with rolling averages 

rolling_average_window1 <- 7
rolling_average_window2 <- 30

p <- sleep_efficiency %>% filter(!is.na(efficiency)) %>%
  mutate(averageEfficiency1 = roll_mean(efficiency, rolling_average_window1)) %>%
  mutate(averageEfficiency1 = lead(averageEfficiency1, round(rolling_average_window1/2), digits = 0)) %>% # Shift rolling avg to midpoint of sample
  mutate(averageEfficiency2 = roll_mean(efficiency, rolling_average_window2)) %>%
  mutate(averageEfficiency2 = lead(averageEfficiency2, round(rolling_average_window2/2), digits = 0)) %>% # Shift rolling avg to midpoint of sample
  ggplot(aes(x=date, y=efficiency)) +
  geom_line(color='lightgray', size=1) +
  geom_line(aes(x = date, y = averageEfficiency1), color='steelblue', size=1) +
  #  geom_line(aes(x = date, y = averageHeartRate2), color='red', size=1) +
  scale_y_continuous(limits=c(75,100)) +
  scale_x_date(limits=c(as.Date("2015-04-22"),NA), date_breaks = "1 month", date_labels = "%Y-%m") +
  labs(x = "Date", y = "Sleep efficiency with 7-day and 30-day rolling average") +
  theme(axis.text.x = element_text(angle = 90))
ggsave(filename = "Plots/Sleep_Efficiency.png", p, width = 40, height = 6, dpi = 300, units = "in", device=png())


# Histogram of sleep efficiency (only include dates after algorithm change around 2017-06-01)
p <- sleep_efficiency %>% filter(date > "2017-06-01") %>%
  ggplot(aes(efficiency)) +
  geom_histogram(binwidth = 1)

dev.off()





########################################
################ WEIGHT ################
########################################

get_body_fat_logs(token, date)
get_weight_logs(token, date)

get_body_time_series(token, "weight", date = date, period="1y")

weight_2020 <- get_body_time_series(token, "weight", date=date, period="1y")
weight_2019 <- get_body_time_series(token, "weight", date="2019-12-31", period="1y")
weight_2018 <- get_body_time_series(token, "weight", date="2018-12-31", period="1y")
weight_2017 <- get_body_time_series(token, "weight", date="2017-12-31", period="1y")
weight_2016 <- get_body_time_series(token, "weight", date="2016-12-31", period="1y")

# Remove 2019 dates from 2020
weight_2020 <- weight_2020 %>% filter(dateTime >= as.Date("2020-01-01"))
#weight <- weight[!duplicated(weight$dateTime),] 

weight <- rbind(weight_2020, weight_2019, weight_2018, weight_2017, weight_2016)

# Remove temporary variables
rm(weight_2020)
rm(weight_2019)
rm(weight_2018)
rm(weight_2017)
rm(weight_2016)

# Convert variables to correct type and arrange by date
weight <- weight %>%
  mutate(date =  as.Date(dateTime)) %>%
  mutate(weight = as.numeric(value)) %>%
  select(-dateTime, -value) %>%
  arrange(date)

# Set weight to NA for dates prior to Aria use start 2017-02-09
weight$weight[weight$date < "2017-02-09"] <- NA


# Save weight data.frame to file for easier retrieval
save(weight,file="Data/weight.Rda")

# Load data from file
load("Data/weight.Rda")


# Plot with rolling averages 

rolling_average_window1 <- 7
rolling_average_window2 <- 30

p <- weight %>% filter(!is.na(weight)) %>%
  mutate(averageWeight1 = roll_mean(weight, rolling_average_window1)) %>%
  mutate(averageWeight1 = lead(averageWeight1, round(rolling_average_window1/2), digits = 0)) %>% # Shift rolling avg to midpoint of sample
  mutate(averageWeight2 = roll_mean(weight, rolling_average_window2)) %>%
  mutate(averageWeight2 = lead(averageWeight2, round(rolling_average_window2/2), digits = 0)) %>% # Shift rolling avg to midpoint of sample
  ggplot(aes(x=date, y=weight)) +
  geom_line(color='lightgray', size=1) +
  geom_line(aes(x = date, y = averageWeight1), color='steelblue', size=1) +
  #  geom_line(aes(x = date, y = averageHeartRate2), color='red', size=1) +
  scale_y_continuous(limits=c(72,80)) +
  scale_x_date(limits=c(as.Date("2015-04-22"),NA), date_breaks = "1 month", date_labels = "%Y-%m") +
  labs(x = "Date", y = "Sleep efficiency with 7-day and 30-day rolling average") +
  theme(axis.text.x = element_text(angle = 90))
ggsave(filename = "Plots/Weight.png", p, width = 40, height = 6, dpi = 300, units = "in", device=png())

#max(weight$weight, na.rm = TRUE)


# Histogram of weight
weight %>%
  filter(date >= "2016-01-01" & date <= "2020-12-31") %>%
  ggplot(aes(weight)) +
  geom_histogram(binwidth = 1)




#################################################################
################ Fitbit management and resources ################
#################################################################

# https://dev.fitbit.com/apps
# https://dev.fitbit.com/build/reference/web-api/


### Resources for the use of fitibitr 

# Alternative for setting key
# Sys.setenv(FITBIT_KEY = "<your-fitbit-key>", FITBIT_SECRET = "<your-firbit-secret>")

# OAuth 2.0: Authorization URI: https://www.fitbit.com/oauth2/authorize
# OAuth 2.0: Access/Refresh Token Request URI: https://api.fitbit.com/oauth2/token

## Installing the fitbitr package, not available on CRAN
# fitbitr package: https://github.com/teramonagi/fitbitr
# install.packages("devtools")
# devtools::install_github("teramonagi/fitbitr")



