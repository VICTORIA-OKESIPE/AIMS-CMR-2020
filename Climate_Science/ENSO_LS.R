### Now, with ENSO Components
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
getwd()
# read data
# your data must have at least two columns: date and rain
#station <- read.csv("Choma_Rain_tidy_reduced.csv", header = TRUE, sep=",")
station <- read.csv("Livingstone_Rain_tidy_subset_reduced.csv", header = TRUE, sep=",")
head(station)
tail(station)


# Now, ENSO issue

ElNino <- c(1952,1953,1958,1969,1976,1977,1979,2004,2006,2014,2018,1951,1963,1968,1986,1994,2002,2009,1957,1965,1972,1987,1991,1982,1997,2015)
length((ElNino))
LaNina <- c(1954,1964,1971,1974,1983,1984,2000,2005,2008,2016,2017,1955,1970,1995,2011,2020,1973,1975,1988,1998,1999,2007,2010)
length(LaNina)


# create a 'ENSO' column: E if EL-NiNo, L if La-Nina, or N if neutral
station$ENSO <-ifelse(station$year %in% ElNino, "EL Nino",
                      ifelse(station$year %in% LaNina, "La Nina",
                             "Neutral"))

head(station)
sample(station)

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
station$ENSO <- factor(station$ENSO)
#rainday <- as.data.frame(station$rainday)
#doy <- as.data.frame(stationCHOMA$doy)

rainday <- station$rainday
doy <- station$doy
prev1 <- station$prev1
prev2 <- station$prev2
prev3 <- station$prev3
ENSO <- station$ENSO


# Fitting second-order MC
form_three <- rainday ~ (cos(doy * 1 * 2 * pi/366) + sin(doy * 1 * 2 * pi/366) + 
                         cos(doy * 2 * 2 * pi/366) + sin(doy * 2 * 2 * pi/366) +
                         cos(doy * 3 * 2 * pi/366) + sin(doy* 3 * 2 * pi/366) + prev1 +
                         prev2 + prev3 + ENSO)
fit_three <- glm(as.formula(form_three), data = station, family = binomial, na.action = na.exclude)
anova(fit_three, test = "Chisq")



## Plotting second-order MC model
# add fitted values i.e. probabilities to data
station$fitted_three <- fitted(fit_three)
fitted_three <- station$fitted_three

p3 <- ggplot(station, aes(x = doy, y = fitted_three, colour = prev3)) + 
  geom_line() + 
  ylab("Probability") + 
  xlab("Day of Year") + facet_wrap(ENSO)
p3



################################ the amount of rainfall ##################################
### This is a template for fitting the amount of rainfall
### Some of the lines are incomplete and needed to be filled in.
### Comments are added to give hints on how to complete the code
### The code may need to be adapted depending on the data you use
### e.g. if column names are different.
### The models also can be adapted depending on your data
### e.g. number of harmonics and orders used.
rm(list=ls())
library(ggplot2) 
library(dplyr)
getwd()
# read data
# your data must have at least two columns: date and rain
#station <- read.csv("Choma_Rain_tidy_reduced.csv", header = TRUE, sep=",")
station <- read.csv("Livingstone_Rain_tidy_subset_reduced.csv", header = TRUE, sep=",")
head(station)
tail(station)


# Now, ENSO issue

ElNino <- c(1952,1953,1958,1969,1976,1977,1979,2004,2006,2014,2018,1951,1963,1968,1986,1994,2002,2009,1957,1965,1972,1987,1991,1982,1997,2015)
length((ElNino))
LaNina <- c(1954,1964,1971,1974,1983,1984,2000,2005,2008,2016,2017,1955,1970,1995,2011,2020,1973,1975,1988,1998,1999,2007,2010)
length(LaNina)


# create a 'ENSO' column: E if EL-NiNo, L if La-Nina, or N if neutral
station$ENSO <-ifelse(station$year %in% ElNino, "EL Nino",
                      ifelse(station$year %in% LaNina, "La Nina",
                             "Neutral"))

head(station)
sample(station)

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
station$ENSO <- factor(station$ENSO)
#rainday <- as.data.frame(station$rainday)
#doy <- as.data.frame(stationCHOMA$doy)

rainday <- station$rainday
doy <- station$doy
prev1 <- station$prev1
prev2 <- station$prev2
prev3 <- station$prev3
ENSO <- station$ENSO


daily_rainfall_LS <- station$daily_rainfall_LS

# getting subset data for rainfall amount before fitting
amt_Rainfall <- subset(station, daily_rainfall_LS > 0.85)
head(amt_Rainfall)




# Now, the issue of ENSO
# Fitting first-order MC
form_two <- daily_rainfall_LS ~ (cos(doy * 1 * 2 * pi/366) + sin(doy * 1 * 2 * pi/366) +
                                      cos(doy * 2 * 2 * pi/366) + sin(doy * 2 * 2 * pi/366) +
                                      cos(doy * 2 * 2 * pi/366) + sin(doy * 2 * 2 * pi/366) + prev1 + prev2 + ENSO)

fit_two <- glm(as.formula(form_two), data = amt_Rainfall, family = Gamma(link = "log"), na.action = na.exclude)

anova(fit_two, test = "Chisq")

ENSO <- amt_Rainfall$ENSO

## Plotting second-order MC model
# add fitted values i.e. probabilities to data
amt_Rainfall$fitted_two <- fitted(fit_two)
fitted_two <- amt_Rainfall$fitted_two

p2 <- ggplot(amt_Rainfall, aes(x = doy, y = fitted_two, colour = prev2)) + 
  geom_line() + 
  ylab("Amount of rain") + 
  xlab("Day of Year") +
  facet_wrap(ENSO)
p2

