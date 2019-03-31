

library(tseries) # for `adf.test()`
library(dynlm) #for function `dynlm()`
library(vars) # for function `VAR()`
library(nlWaldTest) # for the `nlWaldtest()` function
library(lmtest) #for `coeftest()` and `bptest()`.
library(broom) #for `glance(`) and `tidy()`
library(car) #for `hccm()` robust standard errors
library(sandwich)
library(knitr) #for `kable()`
library(forecast) 

##### CH 13: VEC and VAR models ####

## Vector AutoRegression (VAR) model
# y_t = B_10 + B_11 y_t-1 + B_12 x_t-1 + v^y_t
# x_t = B_20 + B_21 y_t-1 + B_22 x_t-1 + v^x_t

## If the two variables are cointegrated, then use the Vector Error Correction (VEC) model
# to do this, create a lagged residual series of y_t-1 = b0 + b1 x_t-1 + e_t-1
# then estimate diff(y) and diff(X) by OLS with the estimated lagged residuals from above.

## 13.2 Estimating a VEC model ====

# We will use gdp dataset: GDP series for Australia nad USE from 1970:1 to 2000:4
load("gdp.rda")

gdp = ts(gdp, start = c(1970,1), end = c(2000,4), frequency = 4)

ts.plot(gdp[, "usa"], gdp[,"aus"], type = "l", lty = c(1,2), col = c(1,2))
legend("topleft", border = NULL, legend = c("USA","AUS"), lty = c(1,2), col = c(1,2))

# The figure shows that the two series are nonstationary
acf(gdp)
pacf(gdp)

acf(diff(gdp)) # difference order 1 makes them stationary
pacf(diff(gdp))

# we can test them using augmented Dickey-Fuller test
adf.test(gdp[,"aus"])
adf.test(diff(gdp[,"aus"])) # yep, significant at diff order 1

adf.test(gdp[,"usa"])
adf.test(diff(gdp[,"usa"])) # another significant at diff order 1

## Now, let's test for cointegration
# estimate Australian GDP with USA GDP

cint1.dyn = dynlm(aus ~ usa-1, data = gdp)
summary(cint1.dyn)

# Then estimate the diff error with lag error
ehat = resid(cint1.dyn)
cint2.dyn = dynlm(d(ehat) ~ L(ehat) - 1)
summary(cint2.dyn) # p < 0.05, reject the null of no cointegration


# Because both are cointegrated, we can construct a VEC model
vecaus = dynlm(d(aus) ~ L(ehat), gdp)
vecusa = dynlm(d(usa) ~ L(ehat), gdp)

tidy(vecaus)
tidy(vecusa)
# This suggests that changes in USA economy affect AUS economy but not the other way around.


## 13.3 Estimating a VAR model ====

# VAR Model can be used if the two variables under study are not cointegrated

# Let's look at income-consumption relationship in the fred dataset (c = consumption, y = income)
load("fred.rda")


fred = ts(fred, start = c(1960,1), end = c(2009,4), frequency = 4)
ts.plot(fred[,"c"],fred[,"y"], type="l", 
        lty=c(1,2), col=c(1,2))
legend("topleft", border=NULL, legend=c("c","y"), 
       lty=c(1,2), col=c(1,2))

# see the acf and pacf plots
acf(fred[,"c"])
acf(diff(fred[,"c"]))

acf(fred[,"y"])
acf(diff(fred[,"y"]))

# Augmented Dickey-Fuller Test:
adf.test(fred[,"c"])
adf.test(diff(fred[,"c"]))

adf.test(fred[,"y"])
adf.test(diff(fred[,"y"]))
# both are significant at diff 1

cointcy = dynlm(c ~ y, data = fred)
ehat = resid(cointcy)
adf.test(ehat) # they failed to cointegrate

## now let's use the VAR model
require(vars)

Dc = diff(fred[,"c"])
Dy = diff(fred[,"y"])

varmat = as.matrix(cbind(Dc, Dy))
varfit = VAR(varmat)

summary(varfit)

# now you can see Dy and Dc described in the model


## 13.4 Impluse Response and Variance decompositions
impresp = irf(varfit)
plot(impresp)
# an impulse (shock) to Dc at time zero has large effects the next period, but the effects becomes smaller and smaller as time passes

plot(fevd(varfit))
# this shows that almost 100% variance in Dc is caused by Dc itself
# while only 80% variance in Dy is caused by Dy

