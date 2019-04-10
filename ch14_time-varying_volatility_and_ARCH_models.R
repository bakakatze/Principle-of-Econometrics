
library(rugarch) #for GARCH models
library(tseries) # for `adf.test()`
library(dynlm) #for function `dynlm()`
library(vars) # for function `VAR()`
library(nlWaldTest) # for the `nlWaldtest()` function
library(lmtest) #for `coeftest()` and `bptest()`.
library(broom) #for `glance(`) and `tidy()`
library(PoEdata) #for PoE4 datasets
library(car) #for `hccm()` robust standard errors
library(sandwich)
library(knitr) #for `kable()`
library(forecast) 

## 14.1 The Autoregressive conditional heteroskedasticity (ARCH) model ====

# The ARCH model assumse that the unconditional mean of the error term in a time series model is constant (zero),
# but its conditional variance is not.

# To test for ARCH:
# Regress Y over a constant or other variables. Get the errors, square them, and regress them with the squared lag errors (higher order lags as well).
# Test if the lag terms are significant.

# load the data
# Return to shares in BrightenYourDay Lighting company over 500 observations.
load("byd.rda")

rTS = ts(byd$r)
plot(rTS)
hist(rTS)

# Now let's test for ARCH
byd.mean = dynlm(rTS ~ 1)
summary(byd.mean)

ehatsq = ts(resid(byd.mean)^2)
byd.ARCH = dynlm(ehatsq ~ L(ehatsq))
summary(byd.ARCH)

T = nobs(byd.mean)
q = length(coef(byd.ARCH))-1
Rsq = glance(byd.ARCH)[[1]]
LM = (T-q)*Rsq
alpha = 0.05
qchisq(1-alpha, q)
# the LM statistics 62.16 is above the critical value of chisq with 1 df
# Which concludes that the series has ARCH effects


# garch() function in the tseries package becomes an ARCH model when used with the order = c(0,1)
byd.ach = garch(rTS, order = c(0,1))

summary(byd.ach)

hhat = ts(2*byd.ach$fitted.values[-1,1]^2)
plot(hhat)

#
## 14.2 The Generalised Autoregressive Conditional Heteroskedasticity (GARCH) model ####
require(rugarch)

# Standard GARCH model
garchSpec = ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
  mean.model = list(armaOrder = c(0,0)),
  distribution.model = "std"
)

garchFit = ugarchfit(spec = garchSpec, data = rTS)
coef(garchFit)

rhat = garchFit@fit$fitted.values
plot.ts(rhat)

hhat = ts(garchFit@fit$sigma^2)
plot.ts(hhat)


# treshold GARCH model (tGARCH)
garchMod = ugarchspec(
  variance.model = list(model = "fGARCH",
                        garchOrder = c(1,1),
                        submodel = "TGARCH"),
  mean.model = list(armaOrder = c(0,0)),
  distribution.model = "std"
)

garchFit = ugarchfit(spec = garchMod, data = rTS)
coef(garchFit)


# GARCH-in-mean model
garchMod <- ugarchspec(
  variance.model=list(model="fGARCH",
                      garchOrder=c(1,1),
                      submodel="APARCH"),
  mean.model=list(armaOrder=c(0,0),
                  include.mean=TRUE,
                  archm=TRUE,
                  archpow=2
  ), 
  distribution.model="std"
)
garchFit <- ugarchfit(spec=garchMod, data=rTS)
coef(garchFit)


