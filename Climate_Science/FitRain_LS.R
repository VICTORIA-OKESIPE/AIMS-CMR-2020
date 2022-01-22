### This is a template for fitting the chance of rain to daily rainfall data
### Some of the lines are incomplete and needed to be filled in.
### Comments are added to give hints on how to complete the code
### The code may need to be adapted depending on the data you use
### e.g. if column names are different.
### The models also can be adapted depending on your data
### e.g. number of harmonics and orders used.
rm(list=ls())
library(ggplot2) 
library(dplyr)

# confirm working directory
getwd()


# read data
# your data must have at least two columns: date and rain
#station <- read.csv("Choma_Rain_tidy_reduced.csv", header = TRUE, sep=",")
station <- read.csv("Livingstone_Rain_tidy_subset_reduced.csv", header = TRUE, sep=",")


#View first 6 lines of dataset
head(station)
tail(station)



# check structure of dataset
str(station)



# check the data you imported
summary(station)


# change year and month column to factor
#stationCHOMA$year = as.factor(stationCHOMA$year)
#stationCHOMA$month = as.factor(stationCHOMA$month)          
#stationLS$year = as.factor(stationLS$year)
#stationLS$month = as.factor(stationLS$month)

# check the changes
#str(stationCHOMA)

# count the number of missing values in each columns
sapply(station, function(x) sum(is.na(x)))  



# if the date column is not of type Date, convert it with as.Date()


# create a 'rainday' column: w if wet (> 0.85) or d if dry
station$rainday <- ifelse(station$daily_rainfall_LS > 0.85,"w","d")


# Then calculate the "lags" of this, which gives you the value for the 
# previous day. Look at ?dplyr::lag to do this.
# Do not use stats::lag, which does something different!!
# You should get lags 1, 2 and 3 which correspond to yesterday, 2 days ago, 
# and 3 days ago.
station$lag1 <-  lag(station$rainday)
station$lag2 <-  lag(station$lag1)
station$lag3 <-  lag(station$lag2)
head(station)

# Then you need to combine the lags. 
# So you want the previous 2 days i.e. "ww", "wd", "dw" or "dd". 
# To do this you can paste together lag 2 and lag 1. 
# You can do the same for previous 3 days.
station$prev1 <- station$lag1
station$prev2 <- paste0(station$lag2, station$lag1)
station$prev3 <- paste0(station$lag3, station$lag2, station$lag1)

# Be careful as some lags are NA so the prev columns need to be corrected
station$prev2 <- ifelse(is.na(station$lag1) | is.na(station$lag2), NA, station$prev2)
station$prev3 <- ifelse(is.na(station$lag1) | is.na(station$lag2) | is.na(station$lag3), NA, station$prev3)

# Calculate a day of the year column using the function below
#yday_366 <- function(date) {
#  temp_doy <- lubridate::yday(date)
#  temp_leap <- lubridate::leap_year(date)
#  temp_doy[(!is.na(temp_doy)) & temp_doy > 59 & (!temp_leap)] <- 1 + temp_doy[(!is.na(temp_doy)) & temp_doy > 59 & (!temp_leap)]
#  return(temp_doy)
#}
# To run this make sure your date column is of type Date (and not factor)
#station$doy <- yday_366(station$date)

# Make the rainday and prev columns factor columns
station$rainday <- factor(station$rainday)
station$prev1 <- factor(station$prev1)
station$prev2 <- factor(station$prev2)
station$prev3 <- factor(station$prev3)

#rainday <- as.data.frame(station$rainday)
#doy <- as.data.frame(stationCHOMA$doy)

rainday <- station$rainday
doy <- station$doy
prev1 <- station$prev1
prev2 <- station$prev2
prev3 <- station$prev3

head(station)
#stationCHOMA$doy <- as.data.frame(stationCHOMA$doy)
#attach(station$rainday)
#stationCHOMA$doy
# Fitting zero-order MC with 2 harmonics
form_zero_2_harm <- rainday ~ (cos(doy * 1 * 2 * pi/366) + sin(doy * 1 * 2 * pi/366) +
                                 cos(doy * 3 * 2 * pi/366) + sin(doy* 3 * 2 * pi/366))
fit_zero_2_harm <- glm(as.formula(form_zero_2_harm), data = station, family = binomial, na.action = na.exclude)
head(station)
anova(fit_zero_2_harm, test = "Chisq")

# Fitting zero-order MC with 3 harmonics
form_zero_3_harm <- rainday ~ (cos(doy * 1 * 2 * pi/366) + sin(doy * 1 * 2 * pi/366) + 
                          cos(doy * 2 * 2 * pi/366) + sin(doy * 2 * 2 * pi/366)+
                          cos(doy * 3 * 2 * pi/366) + sin(doy* 3 * 2 * pi/366))
fit_zero_3_harm <- glm(as.formula(form_zero_3_harm), data = station, family = binomial, na.action = na.exclude)
head(station)
anova(fit_zero_3_harm, test = "Chisq")

# Fitting zero-order MC with 4 harmonics
# Modify the code above to add a forth harmonic
# Look at the output from anova to see how many harmonics might be needed
# Fitting zero-order MC with 4 harmonics
form_zero_4_harm <- rainday ~ (cos(doy * 1 * 2 * pi/366) + sin(doy * 1 * 2 * pi/366) + 
                                 cos(doy * 2 * 2 * pi/366) + sin(doy * 2 * 2 * pi/366)+
                                 cos(doy * 3 * 2 * pi/366) + sin(doy* 3 * 2 * pi/366)+
                                 cos(doy * 4 * 2 * pi/366) + sin(doy* 4 * 2 * pi/366))
fit_zero_4_harm <- glm(as.formula(form_zero_4_harm), data = station, family = binomial, na.action = na.exclude)

anova(fit_zero_4_harm, test = "Chisq")



# Fitting first-order MC
form_one <- rainday ~ (cos(doy * 1 * 2 * pi/366) + sin(doy * 1 * 2 * pi/366) +
                         cos(doy * 3 * 2 * pi/366) + sin(doy* 3 * 2 * pi/366) +
                         cos(doy * 3 * 2 * pi/366) + sin(doy* 3 * 2 * pi/366) +
                         prev1)

fit_one <- glm(as.formula(form_one), data = station, family = binomial, na.action = na.exclude)
anova(fit_one, test = "Chisq")


# With * instead of + for interaction
form_one_int <- rainday ~ ((cos(doy * 1 * 2 * pi/366) + sin(doy * 1 * 2 * pi/366) +
                              cos(doy * 3 * 2 * pi/366) + sin(doy* 3 * 2 * pi/366) +
                              cos(doy * 3 * 2 * pi/366) + sin(doy* 3 * 2 * pi/366)) * prev1)

fit_one_int <- glm(as.formula(form_one_int), data = station, family = binomial, na.action = na.exclude)
anova(fit_one_int, test = "Chisq")

# Follow a similar process to fit second order model with and without interactions
# Fitting second-order MC
form_two <- rainday ~ (cos(doy * 1 * 2 * pi/366) + sin(doy * 1 * 2 * pi/366) +
                         cos(doy * 3 * 2 * pi/366) + sin(doy* 3 * 2 * pi/366) +
                         cos(doy * 3 * 2 * pi/366) + sin(doy* 3 * 2 * pi/366) + prev1 +
                         prev2)
fit_two <- glm(as.formula(form_two), data = station, family = binomial, na.action = na.exclude)
anova(fit_two, test = "Chisq")


# With * instead of + for interaction
form_two_int <- rainday ~ ((cos(doy * 1 * 2 * pi/366) + sin(doy * 1 * 2 * pi/366) +
                              cos(doy * 3 * 2 * pi/366) + sin(doy* 3 * 2 * pi/366) +
                              cos(doy * 3 * 2 * pi/366) + sin(doy* 3 * 2 * pi/366)) * prev2)

fit_two_int <- glm(as.formula(form_two_int), data = station, family = binomial, na.action = na.exclude)
anova(fit_two_int, test = "Chisq")


# Fitting third-order MC
# Follow a similar process to fit third order model with and without interactions
form_three <- rainday ~ (cos(doy * 1 * 2 * pi/366) + sin(doy * 1 * 2 * pi/366) +
                           cos(doy * 3 * 2 * pi/366) + sin(doy* 3 * 2 * pi/366) +
                           cos(doy * 3 * 2 * pi/366) + sin(doy* 3 * 2 * pi/366) + prev1 +
                           prev2 + prev3)

form_three_int <- rainday ~ ((cos(doy * 1 * 2 * pi/366) + sin(doy * 1 * 2 * pi/366) +
                                cos(doy * 3 * 2 * pi/366) + sin(doy* 3 * 2 * pi/366) +
                                cos(doy * 3 * 2 * pi/366) + sin(doy* 3 * 2 * pi/366)) * prev3)

fit_three <- glm(as.formula(form_three), data = station, family = binomial, na.action = na.exclude)
fit_three_int <- glm(as.formula(form_three_int), data = station, family = binomial, na.action = na.exclude)


anova(fit_three, test = "Chisq")
anova(fit_three_int, test = "Chisq")


# AIC Analysis
AIC(fit_zero_2_harm, fit_zero_3_harm, fit_zero_4_harm, fit_one, fit_one_int, fit_two, fit_two_int, fit_three, fit_three_int)

# BIC Analysis
# Do the same for BIC
BIC(fit_zero_2_harm, fit_zero_3_harm, fit_zero_4_harm, fit_one, fit_one_int, fit_two, fit_two_int, fit_three, fit_three_int)

## Plotting zero-order MC model
# add fitted values i.e. probabilities to data
station$fitted_zero <- fitted(fit_zero)
fitted_zero <- station$fitted_zero

## plot zeroorder fitted line
p0 <- ggplot(station, aes(x = doy, y = fitted_zero)) +
  geom_line() + 
  ylab("Probability") + 
  xlab("Day of Year")
p0

## Plotting first-order MC model
# add fitted values i.e. probabilities to data
station$fitted_one <- fitted(fit_one)
fitted_one <- station$fitted_one

p1 <- ggplot(station, aes(x = doy, y = fitted_one, colour = prev1)) + 
  geom_line() + 
  ylab("Probability") + 
  xlab("Day of Year")
p1

# add fitted values i.e. probabilities to data
station$fitted_one_int <- fitted(fit_one_int)
fitted_one_int <- station$fitted_one_int

p1_int <- ggplot(station, aes(x = doy, y = fitted_one_int, colour = prev1)) + 
  geom_line() + 
  ylab("Probability") + 
  xlab("Day of Year")
p1_int

## Plotting second-order MC model
# add fitted values i.e. probabilities to data
station$fitted_two <- fitted(fit_two)
fitted_two <- station$fitted_two

p2 <- ggplot(station, aes(x = doy, y = fitted_two, colour = prev2 )) + 
  geom_line() + 
  ylab("Probability") + 
  xlab("Day of Year")
p2


# add fitted values i.e. probabilities to data
station$fitted_two_int <- fitted(fit_two_int)
fitted_two_int <- station$fitted_two_int

p2_int <- ggplot(station, aes(x = doy, y = fitted_two_int, colour = prev2)) + 
  geom_line() + 
  ylab("Probability") + 
  xlab("Day of Year")
p2_int

## Plotting third-order MC model
# add fitted values i.e. probabilities to data
station$fitted_three <- fitted(fit_three)
fitted_three <- station$fitted_three

# month_abbr <- stationCHOMA$month_abbr
p3 <- ggplot(station, aes(x = doy, y = fitted_three, colour = prev3)) + 
  geom_line() + 
  ylab("Probability") + 
  xlab("Day of Year")
p3

# add fitted values i.e. probabilities to data
station$fitted_three_int <- fitted(fit_three_int)
fitted_three_int <- station$fitted_three_int

p3_int <- ggplot(station, aes(x = doy, y = fitted_three_int, colour = prev3)) + 
  geom_line() + 
  ylab("Probability") + 
  xlab("Day of Year")
p3_int


################################ the amount of rainfall ##################################
rm(list=ls())
library(ggplot2) 
library(dplyr)

# confirm working directory
getwd()


# read data
# your data must have at least two columns: date and rain
#station <- read.csv("Choma_Rain_tidy_reduced.csv", header = TRUE, sep=",")
station <- read.csv("Livingstone_Rain_tidy_subset_reduced.csv", header = TRUE, sep=",")


#View first 6 lines of dataset
head(station)
tail(station)



# check structure of dataset
str(station)



# check the data you imported
summary(station)


# change year and month column to factor
#stationCHOMA$year = as.factor(stationCHOMA$year)
#stationCHOMA$month = as.factor(stationCHOMA$month)          
#stationLS$year = as.factor(stationLS$year)
#stationLS$month = as.factor(stationLS$month)

# check the changes
#str(stationCHOMA)

# count the number of missing values in each columns
sapply(station, function(x) sum(is.na(x)))  



# if the date column is not of type Date, convert it with as.Date()


# create a 'rainday' column: w if wet (> 0.85) or d if dry
station$rainday <- ifelse(station$daily_rainfall_LS > 0.85,"w","d")


# Then calculate the "lags" of this, which gives you the value for the 
# previous day. Look at ?dplyr::lag to do this.
# Do not use stats::lag, which does something different!!
# You should get lags 1, 2 and 3 which correspond to yesterday, 2 days ago, 
# and 3 days ago.
station$lag1 <-  lag(station$rainday)
station$lag2 <-  lag(station$lag1)
station$lag3 <-  lag(station$lag2)
head(station)

# Then you need to combine the lags. 
# So you want the previous 2 days i.e. "ww", "wd", "dw" or "dd". 
# To do this you can paste together lag 2 and lag 1. 
# You can do the same for previous 3 days.
station$prev1 <- station$lag1
station$prev2 <- paste0(station$lag2, station$lag1)
station$prev3 <- paste0(station$lag3, station$lag2, station$lag1)

# Be careful as some lags are NA so the prev columns need to be corrected
station$prev2 <- ifelse(is.na(station$lag1) | is.na(station$lag2), NA, station$prev2)
station$prev3 <- ifelse(is.na(station$lag1) | is.na(station$lag2) | is.na(station$lag3), NA, station$prev3)

# Calculate a day of the year column using the function below
#yday_366 <- function(date) {
#  temp_doy <- lubridate::yday(date)
#  temp_leap <- lubridate::leap_year(date)
#  temp_doy[(!is.na(temp_doy)) & temp_doy > 59 & (!temp_leap)] <- 1 + temp_doy[(!is.na(temp_doy)) & temp_doy > 59 & (!temp_leap)]
#  return(temp_doy)
#}
# To run this make sure your date column is of type Date (and not factor)
#station$doy <- yday_366(station$date)

# Make the rainday and prev columns factor columns
station$rainday <- factor(station$rainday)
station$prev1 <- factor(station$prev1)
station$prev2 <- factor(station$prev2)
station$prev3 <- factor(station$prev3)

#rainday <- as.data.frame(station$rainday)
#doy <- as.data.frame(stationCHOMA$doy)

rainday <- station$rainday
doy <- station$doy
prev1 <- station$prev1
prev2 <- station$prev2
prev3 <- station$prev3

daily_rainfall_LS <- station$daily_rainfall_LS

# getting subset data for rainfall amount before fitting
amt_Rainfall <- subset(station, daily_rainfall_LS > 0.85)
head(amt_Rainfall)
str(prev1)

form_zero_2_harm <- daily_rainfall_LS ~ (cos(doy * 1 * 2 * pi/366) + sin(doy * 1 * 2 * pi/366)+
                                              cos(doy * 3 * 2 * pi/366) + sin(doy* 3 * 2 * pi/366))

fit_zero_2_harm <- glm(as.formula(form_zero_2_harm), data = amt_Rainfall, family = Gamma(link = "log"), na.action = na.exclude)

anova(fit_zero_2_harm, test = "Chisq")

form_zero_3_harm <- daily_rainfall_LS ~ (cos(doy * 1 * 2 * pi/366) + sin(doy * 1 * 2 * pi/366) + 
                                              cos(doy * 2 * 2 * pi/366) + sin(doy * 2 * 2 * pi/366)+
                                              cos(doy * 3 * 2 * pi/366) + sin(doy* 3 * 2 * pi/366))

fit_zero_3_harm <- glm(as.formula(form_zero_3_harm), data = amt_Rainfall, family = Gamma(link = "log"), na.action = na.exclude)

anova(fit_zero_3_harm, test = "Chisq")

# Fitting zero-order MC with 4 harmonics
# Modify the code above to add a forth harmonic
# Look at the output from anova to see how many harmonics might be needed
# Fitting zero-order MC with 4 harmonics
form_zero_4_harm <- daily_rainfall_LS ~ (cos(doy * 1 * 2 * pi/366) + sin(doy * 1 * 2 * pi/366) + 
                                              cos(doy * 2 * 2 * pi/366) + sin(doy * 2 * 2 * pi/366)+
                                              cos(doy * 3 * 2 * pi/366) + sin(doy* 3 * 2 * pi/366)+
                                              cos(doy * 4 * 2 * pi/366) + sin(doy* 4 * 2 * pi/366))
fit_zero_4_harm <- glm(as.formula(form_zero_4_harm), data = amt_Rainfall, family = Gamma(link = "log"), na.action = na.exclude)

anova(fit_zero_4_harm, test = "Chisq")



# Fitting first-order MC
form_one <- daily_rainfall_LS ~ (cos(doy * 1 * 2 * pi/366) + sin(doy * 1 * 2 * pi/366) + 
                                      cos(doy * 3 * 2 * pi/366) + sin(doy* 3 * 2 * pi/366)+
                                      cos(doy * 2 * 2 * pi/366) + sin(doy * 2 * 2 * pi/366) +
                                      prev1)

fit_one <- glm(as.formula(form_one), data = amt_Rainfall, family = Gamma(link = "log"), na.action = na.exclude)
anova(fit_one, test = "Chisq")


# With * instead of + for interaction
form_one_int <- daily_rainfall_LS ~ ((cos(doy * 1 * 2 * pi/366) + sin(doy * 1 * 2 * pi/366) + 
                                           cos(doy * 3 * 2 * pi/366) + sin(doy* 3 * 2 * pi/366)+
                                           cos(doy * 2 * 2 * pi/366) + sin(doy * 2 * 2 * pi/366)) * prev1)

fit_one_int <- glm(as.formula(form_one_int), data = amt_Rainfall, family = Gamma(link = "log"), na.action = na.exclude)
anova(fit_one_int, test = "Chisq")

# Follow a similar process to fit second order model with and without interactions
# Fitting second-order MC
form_two <- daily_rainfall_LS ~ (cos(doy * 1 * 2 * pi/366) + sin(doy * 1 * 2 * pi/366) + 
                                      cos(doy * 3 * 2 * pi/366) + sin(doy* 3 * 2 * pi/366)+
                                      cos(doy * 2 * 2 * pi/366) + sin(doy * 2 * 2 * pi/366) + prev1 +
                                      prev2)
fit_two <- glm(as.formula(form_two), data = amt_Rainfall, Gamma(link = "log"), na.action = na.exclude)
anova(fit_two, test = "Chisq")


# With * instead of + for interaction
form_two_int <- daily_rainfall_LS ~ ((cos(doy * 1 * 2 * pi/366) + sin(doy * 1 * 2 * pi/366) + 
                                        cos(doy * 3 * 2 * pi/366) + sin(doy* 3 * 2 * pi/366)+
                                        cos(doy * 2 * 2 * pi/366) + sin(doy * 2 * 2 * pi/366)) * prev2)

fit_two_int <- glm(as.formula(form_two_int), data = amt_Rainfall, Gamma(link = "log"), na.action = na.exclude)
anova(fit_two_int, test = "Chisq")


# Fitting third-order MC
# Follow a similar process to fit third order model with and without interactions
form_three <- daily_rainfall_LS ~ (cos(doy * 1 * 2 * pi/366) + sin(doy * 1 * 2 * pi/366) + 
                                        cos(doy * 3 * 2 * pi/366) + sin(doy* 3 * 2 * pi/366)+
                                        cos(doy * 2 * 2 * pi/366) + sin(doy * 2 * 2 * pi/366) + prev1 +
                                        prev2 + prev3)

form_three_int <- daily_rainfall_LS ~ ((cos(doy * 1 * 2 * pi/366) + sin(doy * 1 * 2 * pi/366) + 
                                             cos(doy * 3 * 2 * pi/366) + sin(doy* 3 * 2 * pi/366)+
                                             cos(doy * 2 * 2 * pi/366) + sin(doy * 2 * 2 * pi/366)) * prev3)

fit_three <- glm(as.formula(form_three), data = amt_Rainfall, family = Gamma(link = "log"), na.action = na.exclude)
fit_three_int <- glm(as.formula(form_three_int), data = amt_Rainfall, family = Gamma(link = "log"), na.action = na.exclude)


anova(fit_three, test = "Chisq")
anova(fit_three_int, test = "Chisq")


# AIC Analysis
AIC(fit_zero_2_harm, fit_zero_3_harm, fit_zero_4_harm, fit_one, fit_one_int, fit_two, fit_two_int, fit_three, fit_three_int)

# BIC Analysis
# Do the same for BIC
BIC(fit_zero_2_harm, fit_zero_3_harm, fit_zero_4_harm, fit_one, fit_one_int, fit_two, fit_two_int, fit_three, fit_three_int)

## Plotting zero-order MC model
# add fitted values i.e. probabilities to data
amt_Rainfall$fitted_zero <- fitted(fit_zero)
fitted_zero <- amt_Rainfall$fitted_zero

## plot zeroorder fitted line
p0 <- ggplot(amt_Rainfall, aes(x = doy, y = fitted_zero)) +
  geom_line() + 
  ylab("Probability") + 
  xlab("Day of Year")
p0

## Plotting first-order MC model
# add fitted values i.e. probabilities to data
amt_Rainfall$fitted_one <- fitted(fit_one)
fitted_one <- amt_Rainfall$fitted_one

p1 <- ggplot(amt_Rainfall, aes(x = doy, y = fitted_one, colour = prev1)) + 
  geom_line() + 
  ylab("Probability") + 
  xlab("Day of Year")
p1

# add fitted values i.e. probabilities to data
amt_Rainfall$fitted_one_int <- fitted(fit_one_int)
fitted_one_int <- amt_Rainfall$fitted_one_int

p1_int <- ggplot(amt_Rainfall, aes(x = doy, y = fitted_one_int, colour = prev1)) + 
  geom_line() + 
  ylab("Probability") + 
  xlab("Day of Year")
p1_int

## Plotting second-order MC model
# add fitted values i.e. probabilities to data
amt_Rainfall$fitted_two <- fitted(fit_two)
fitted_two <- amt_Rainfall$fitted_two

p2 <- ggplot(amt_Rainfall, aes(x = doy, y = fitted_two, colour = prev2)) + 
  geom_line() + 
  ylab("Probability") + 
  xlab("Day of Year")
p2

# add fitted values i.e. probabilities to data
amt_Rainfall$fitted_two_int <- fitted(fit_two_int)
fitted_two_int <- amt_Rainfall$fitted_two_int

p2_int <- ggplot(amt_Rainfall, aes(x = doy, y = fitted_two_int, colour = prev2)) + 
  geom_line() + 
  ylab("Amount of rain") + 
  xlab("Day of Year")
p2_int

## Plotting third-order MC model
# add fitted values i.e. probabilities to data
amt_Rainfall$fitted_three <- fitted(fit_three)
fitted_three <- amt_Rainfall$fitted_three

# month_abbr <- stationCHOMA$month_abbr
p3 <- ggplot(amt_Rainfall, aes(x = doy, y = fitted_three, colour = prev3)) + 
  geom_line() + 
  ylab("Probability") + 
  xlab("Day of Year")
p3

# add fitted values i.e. probabilities to data
amt_Rainfall$fitted_three_int <- fitted(fit_three_int)
fitted_three_int <- amt_Rainfall$fitted_three_int
fitted_three_int
p3_int <- ggplot(amt_Rainfall, aes(x = doy, y = fitted_three_int, colour = prev3)) + 
  geom_line() + 
  ylab("Probability") + 
  xlab("Day of Year")
p3_int


# Now, the issue of ENSO
