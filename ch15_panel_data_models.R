rm(list=ls()) #Removes all items in Environment!
library(plm)  # to structure panel data
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
library(systemfit)
library(AER)
library(xtable)
require(lme4)

#
## PANEL DATA

# Panel data = pooled cross-sectional
# can be balanced if the series for each patient is complete, otherwise it is unbalanced


## load the panel data
load("nls_panel.rda")

nlspd = pdata.frame(nls_panel, index = c("id", "year"))
smpl = nlspd[nlspd$id %in% c(1,2), c(1:6, 14:15)]

# info of the panel data
pdim(nlspd)

#
#### 15.2 The Pooled Model ####

# assuming that each cross-section had a different sample
# we cannot tie the individual together across time

wage.pooled = plm(lwage ~ educ + exper + I(exper^2) + tenure + I(tenure^2) + black + south + union, model = "pooling", data = nlspd)
summary(wage.pooled)

wage.pooled.lm = lm(lwage ~ educ + exper + I(exper^2) + tenure + I(tenure^2) + black + south + union, data = nlspd)
summary(wage.pooled.lm)

#
#### 15.3 Fixed Effects Model ####

wage.fixed <- plm(lwage ~ exper + I(exper^2) + tenure + I(tenure^2) + union, model = "within", data=nlspd)
summary(wage.fixed)

wage.fixed.lm = lm(lwage ~ exper + I(exper^2) + tenure + I(tenure^2) + union + factor(id)-1, data = nlspd)
summary(wage.fixed.lm)

#
#### 15.4 Random Effects Model ####

# same as lme with random intercept








