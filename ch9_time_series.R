# Link: https://bookdown.org/ccolonescu/RPoE4/time-series-stationary-variables.html#nonlinear-least-squares-estimation

require(tidyverse)
require(dynlm)
require(lubridate)

require(orcutt) # for 'cochrane.orcutt()' function
library(zoo) # for time series functions (not much used here)
library(lmtest) #for `coeftest()` and `bptest()`.
library(broom) #for `glance(`) and `tidy()`
library(car) #for `hccm()` robust standard errors
library(sandwich)
library(knitr) #for kable()
library(forecast) 

#
## LOAD THE DATA ####
# load the unemployment data
okun = read.csv("unemployment_rate.csv")
okun = okun %>%
  mutate(year = year(dmy(DATE))) %>%
  filter(year >= 1985 & year <= 2002) %>%
  transmute(unrate = UNRATE)

# load the gdp data
gdp = read.csv("gdp.csv")
gdp = gdp %>%
  filter(Year >= 1984 & Year <= 2002) %>%
  mutate(growth = (GDP - lag(GDP, 1))/lag(GDP, 1) ) %>%
  filter(Year != 1984)

# create a fake monthly data for gdp
gdp = as.data.frame(list(growth = rep(gdp$growth, each = 12)))

set.seed(12)

for(i in 1:nrow(gdp))
{
  gdp$fk[i] = gdp$growth[i] + rnorm(1, 0, abs(gdp$growth)/12)
}

okun = cbind(growth = gdp$fk, okun)


#
## 9.2 Finite Distributed Lags ####

# A linear relationship between variable y and several lags of an independent variable x with a lag model of order q (q = how many lags)

okun = okun %>%
  mutate(ul1 = lag(unrate, 1),
         du = unrate - lag(unrate, 1),
         ldu1 = lag(unrate - lag(unrate, 1), 1),
         gl1 = lag(growth, 1),
         gl2 = lag(growth, 2),
         gl3 = lag(growth, 3))


# transform it into time series object, starts from 1985 month 1 to 2002 month 12
okun.ts = ts(okun, start = c(1985,1), end = c(2002,12), frequency = 12)

# see the plot
plot(okun.ts[,1:2])


# plug in the model with 3 year lags
okunL3.dyn = dynlm(d(unrate) ~ L(growth, c(0,12,24,36)), data = okun.ts)
summary(okunL3.dyn)

# with 2 year lags
okunL2.dyn = dynlm(d(unrate) ~ L(growth, c(0,12,24)), data = okun.ts)
summary(okunL2.dyn)

glance(okunL3.dyn)
glance(okunL2.dyn)
# the one with 3 year lags performed better

#
## 9.3 Serial Correlation / Auto Correlation ####


# ACF plot (auto correlation)
acf(okun.ts[,"growth"], lag.max = 2000)


# model this gdp growth with difference of unemployment rate
diffm = dynlm(growth ~ diff(unrate), data = okun.ts)
summary(diffm)

# plot the residual
plot(resid(diffm))
abline(h = 0, lty = 2) # lol.. this is really bad residual

# correlogram of the residual
acf(resid(diffm), lag.max = 300)

#

## 9.4 Estimation with Serially Correlated Errors ####

# Autocorrelation in the errors does not produce biased estimates of the coefficients in linear regression
# It does, however, produce incorrect standard errors.

# We can calculate the correct standard errors using the Newey-West SE
require(sandwich)

coeftest(diffm) # incorrect SE
diffm.nw = coeftest(diffm, vcov. = NeweyWest(diffm)) # Newey-West SE

# Correcting SE in a model with Auto-correlation does not make the estimator of the coefficients a minimum-variance one.
# Therefore, we would like to find a better estimator.

# We can try first order autoregressive process - AR(1) model: more on this later

## 9.5 Nonlinear Least Squares Estimation ####

# model: y_t = b1 (1-rho) + b2 x_t + y_(t-1) - b2 x_(t-1) + error

# Non-linear AR(1) model with 'Cochrane-Orcutt' method
nlmod = nls(growth ~ b1*(1-rho) + b2*du + rho*gl1 - rho*b2*ldu1,
            data = okun,
            start = list(rho = 0.5, b1 = 0.5, b2 = -0.5))

summary(nlmod)
diffm.nw

#
## 9.6 Autoregressive Distributed Lag - General model ####

# Autoregressive distributed lag

genmod = dynlm(growth ~ L(growth) + d(unrate) + L(d(unrate)), data = okun.ts)
summary(genmod)

#
## 9.7 Autoregressive Models ####

# let's build an AR(2) model
okun.ar2 = dynlm(growth ~ L(growth, 12) + L(growth, 24), data = okun.ts)
summary(okun.ar2)

# residual correlogram
Acf(resid(okun.ar2), lag.max = 36)

# Brute force by comparing different lag sequence
aics = rep(0,5)
bics = rep(0,5)

for(i in c(1:5)){
  ari = dynlm(growth ~ L(growth, c(i:5)*12), start = i, data = okun.ts)
  aics[i] = AIC(ari)
  bics[i] = BIC(ari)
}

aics
bics
# AR(4) model seems to have the lowest BIC, and second lowest AIC

## 9.8 Forecasting ####

# let's build an AR(2) model
g.ar2 = dynlm(growth ~ L(growth, c(1, 12)), data = okun.ts)
summary(g.ar2)

# now let's forecast them
ar2g = ar(okun.ts[,"growth"], aic = FALSE, order.max = 2, methods = "ols")
fcst = data.frame(forecast(ar2g, 3))

plot(forecast(ar2g, 50))

# forecast library has an exponential smoothing technique
plot(forecast(ets(okun.ts[,"growth"])), 50)

# let's use HoltWinters method
plot(HoltWinters(okun.ts[,"growth"], beta = FALSE, gamma = FALSE))

forecast(HoltWinters(okun.ts[,"growth"], beta = FALSE, gamma = FALSE), 1)

#








